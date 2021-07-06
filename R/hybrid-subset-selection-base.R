##' Authors: Meelad Amouzgar and David Glass
##' date: July 5th, 2021
##' Description: defines several functions required for hybrid subset selection:
##' 1. hsslda: the hybrid subset selection (HSS)
##' 2.downsampleBalance: downsampling function for faster HSS using the separation metric of choice with balanced downsampling of input class labels
##' 3. various separation metric functions:
##' 3a. Euclidean
##' 3b. Silhouette score
##' 3c. Pixel entropy
##' 3d. Pixel density
##'


#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_point
#'
#' @import magrittr
#'
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr rename
#' @importFrom dplyr summarize
#' @importFrom dplyr n
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom tidyr spread
#' @importFrom dplyr rowwise
#' @importFrom tidyr gather
#' @importFrom dplyr bind_rows
#' @importFrom dplyr left_join
#' @importFrom dplyr right_join
#' @importFrom dplyr full_join
#' @importFrom dplyr case_when
#'
#' @importFrom MASS lda
#' @importFrom stats dist
#' @importFrom TFBSTools shannon.entropy
#' @importFrom cluster silhouette




#' @title plotElbow
#' @description plotElbow: This function generates an Elbow plot of the scores for each # of features
#' @param results the final results table from computing all HSS combinations
#' @param elbow elbow value outputted from getElbow during HSS. Use to color the automatically computed elbow point.
#' @noRd
plotElbow <- function(results, elbow = NULL){
  dfElbow =split(results, results$no.markers)  %>% lapply(., function(x) { x[which.max(x$score),]})  %>% do.call(rbind, .)

  if (is.null(elbow)) {
    p.Elbow = ggplot2::ggplot(dfElbow, ggplot2::aes(x=no.markers, y = score)) +
      ggplot2::geom_line() +
      ggplot2::geom_point()

    return(p.Elbow)
  }
  p.Elbow = ggplot2::ggplot(dfElbow, ggplot2::aes(x=no.markers, y = score)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(color = ifelse(dfElbow$no.markers == elbow, "red", "black"))

  return(p.Elbow)
}



#################################
#################################
#################################
## SEPARATION METRIC FUNCTIONS ##
#################################
#################################
#################################

##############################
## PIXEL ANALYSIS FUNCTIONS ##
##############################
# create_pixel_grid <- function(xbreaks =100, ybreaks = 100) {
#   pixel.grid = base::expand.grid(1:xbreaks,1:ybreaks) %>% data.frame(.)
#   base::colnames(pixel.grid) = c("x","y")
#   pixel.grid$pixel = base::paste(pixel.grid$x,pixel.grid$y, sep =".")
#   return(pixel.grid)
# }
#
# generate_density_map <- function(data, pixel.grid = pixel.grid, xbreaks = 100, ybreaks = 100) {
#   ## returns a density map only of pixels that have cells in them
#   xbin <- base::cut(data$x, xbreaks, include.lowest = TRUE)
#   ybin <- base::cut(data$y, ybreaks, include.lowest = TRUE)
#
#   data_pixel_counts = cbind(data, xout = as.numeric(xbin), yout = as.numeric(ybin) )
#   data_pixel_counts["pixel"] = paste(data_pixel_counts$xout, data_pixel_counts$yout, sep=".")
#   data_pixel_counts = table(pixel = data_pixel_counts$pixel, labels = data_pixel_counts$labels) %>% as.data.frame() %>% .[.$Freq !=0, ] # %>% base::merge(.,pixel.grid, by = "pixel", all.y = TRUE)
#   data_pixel_counts["count.0"] = ifelse(is.na(data_pixel_counts$Freq), 0,data_pixel_counts$Freq )
#   data_pixel_counts = data_pixel_counts %>% split(., .$labels) %>% lapply(., function(dp){dp["percent.0"] = dp$count.0/sum(dp$count.0) ;return(dp) }) %>% do.call("rbind", .)
#   return(data_pixel_counts)
# }


#' @title create_pixel_grid
#' @description create_pixel_grid: This function generates a pixel grid template, defaults to 10,000 pixels
#' @param xbreaks the # of pixels to break the x-axis into. Defaults to 100.
#' @param ybreaks the # of pixels to break the y-axis into. Defaults to 100.
#' @keywords internal
#' @export
create_pixel_grid <- function(xbreaks =100, ybreaks = 100) {
  xbreaks <-xbreaks
  ybreaks <-ybreaks
  pixel.grid = expand.grid(1:xbreaks,1:ybreaks) %>% data.frame(.) %>%
    rename(x=Var1, y = Var2) %>%
    mutate(pixel = paste(x, y,sep= "."))
  return(pixel.grid)
}

#' @description: generate_density_map: This function computes the proportion (density) of the class labels in each pixel of the pixel grid generated using create_pixel_grid.
#' @param data: a dataframe with 3 columns: x-axis coordinates (labeled x), y-axis coordinates(labeled y), and the class labels (labeled as labels)
#' @param pixel.grid: the output from the create_pixel_grid function.
#' @param xbreaks: the # of pixels to break the x-axis into. Defaults to 100.
#' @param ybreaks: the # of pixels to break the y-axis into. Defaults to 100.
#' @noRd
generate_density_map <- function(data, pixel.grid = pixel.grid, xbreaks = 100, ybreaks = 100) {

  # data_pixel_counts <- lapply(unique(data$labels), function(class.label) {
  #   print(class.label)



  xbin <- cut(data$x, xbreaks, include.lowest = TRUE)
  ybin <- cut(data$y, ybreaks, include.lowest = TRUE)

  data_pixel_counts <- data %>%
    ungroup() %>%
    mutate(xout = as.numeric(xbin),
           yout = as.numeric(ybin),
           pixel = paste(xout,yout,sep=".")) %>%
    group_by(pixel,labels) %>%
    summarize(count = n())  %>%
    ungroup() %>%
    right_join(.,pixel.grid,by="pixel") %>%
    mutate(count.0 = ifelse(is.na(count), 0, count)) %>%
    ungroup() %>%
    group_by(labels) %>%
    mutate(percent.0 = count.0 / sum(count.0))
  return(data_pixel_counts)
}


###############################
## PIXEL CLASS ENTROPY SCORE ##
###############################
# calculate_pceScore <- function(data) {
#   pce.score = split(data, data$pixel) %>% lapply(., function(dp) {data.frame(entropy = TFBSTools::shannon.entropy(dp$count), num.of.labels = length(dp$labels)) } ) %>% do.call("rbind",.)
#   pce.score["pce.score"] = 1-(pce.score$entropy/log2(pce.score$num.of.labels))
#   pce.score["pce.score"] = ifelse(is.na(pce.score$pce.score), 1 , pce.score$pce.score)
#   pce.score["pixel"] = rownames(pce.score)
#   return(pce.score)
# }

#' @title calculate_pceScore
#' @description calculate_pceScore: This function computes the pixel class entropy score.
#' @param density_metric_output the output from the function, density_metric_output
#' @noRd
calculate_pceScore <- function(data = density_metric_output) {
  data <- na.omit(data) %>% ungroup()
  pce.score <- data %>%
    group_by(x,y,pixel) %>%
    summarize(entropy = shannon.entropy(count),
              num.of.labels = n()) %>%
    ungroup() %>%
    mutate(pce.score = 1-(entropy/log2(num.of.labels)),
           pce.score = case_when(is.na(pce.score) ~ 1,
                                 TRUE ~ pce.score)) %>%
    dplyr::select(pixel,x,y, num.of.labels,entropy, pce.score)

  return(pce.score)
}

#' @title computePCEscore
#' @description computePCEscore: computes the pixel class entropy score for any biaxial dataset with class labels
#' @param data a dataframe with 3 columns: x-axis coordinates (labeled `x`), y-axis coordinates(labeled `y`), and the class labels (labeled as `labels`)
#' @export
computePCEscore <- function(data) {
  ## data in format of x(axis1), y(axis2), class label of interest

  if (!exists("pixel.grid")){
    pixel.grid <<- create_pixel_grid()
  }

  density_metric_output <- generate_density_map(data = data, pixel.grid = pixel.grid)
  pce.score_output <- calculate_pceScore(data = density_metric_output)
  pce.score <-  mean(pce.score_output$pce.score)
  return(pce.score)
}

##########################
## PIXEL DENSITY SCORE  ##
##########################
# description: calculate_pixelDensityScore: computes the pixel density score for any biaxial dataset with class labels. Must finish recoding. FYI see lapply commented out for reminder on what needs to change
# param data: a dataframe with 3 columns: x-axis coordinates (labeled `x`), y-axis coordinates(labeled `y`), and the class labels (labeled as `labels`)
# calculate_pixelDensityScore <- function(data = density_metric_output) {
#
#   data <- na.omit(data) %>% ungroup()
#   # print("calculate-pixel-clonality")
#
#   pixelDensity.score <- data %>%
#     dplyr::ungroup() %>%
#     dplyr::mutate(binary.labels = ifelse(labels == class.label, "class.of.interest", "other")) %>%
#     dplyr::select(pixel, x, y, percent.0, binary.labels) %>%
#     dplyr::group_by(pixel, x, y, binary.labels) %>%
#     dplyr::summarize(count = sum(count.0),
#               percent = sum(percent.0)) %>%
#     tidyr::gather(key = "approach", value = "quantity", -pixel,-x,-y,-binary.labels) %>%
#     tidyr::spread(key = "binary.labels", value = "quantity") %>%
#     dplyr::rowwise() %>%
#     dplyr::mutate(class.of.interest = ifelse(is.na(class.of.interest), 0, class.of.interest),
#            other = ifelse(is.na(other), 0, other),
#            density.metric = (class.of.interest / (other+class.of.interest)),
#            # density.metric = ifelse()
#            labels = class.label)  %>%
#     # dplyr::select(pixel,x,y, approach, class.of.interest, other,density.metric, labels) %>%
#     dplyr:: group_by(approach, labels) %>%
#     dplyr::summarize(density.summary = sum(density.metric)) %>%
#     dplyr::mutate(density.summary.normalized = density.summary / 10000)
#   return(pixelDensity.score)
# }


########################
## EUCLIDEAN DISTANCE ##
########################
#' @description getScore_euclidean: An aggregate function to compute LDA and euclidean distance score for HSS.
#' @param x dataframe of training data
#' @param y vector of class labels matching training data rows
#' @param cols vector of column names
#' @noRd
getScore_euclidean <- function(x, y, cols) {
  # performs LDA using columns provided and returns lowest euclidean distance between pop means
  lda.out <- lda(y~., data=x[, cols])
  eucl_score = min(dist(lda.out$means %*% lda.out$scaling[,1:2]))
  print(eucl_score)
  return(eucl_score)
}

#######################
## SILHOUETTE SCORE  ##
#######################
#' @description getScore_silhouette: An aggregate function to compute LDA and silouette score for HSS.
#' @param x dataframe of training data
#' @param y vector of class labels matching training data rows
#' @param cols vector of column names
#' @noRd
#' @keywords internal
getScore_silhouette  <- function(x, y, cols) {
  df = x[, cols]
  lda.out <- lda(y~., data=df)

  dist.matrix = df %>% dist() %>% as.matrix()
  silhoutte.output = silhouette(x = as.numeric(factor(y)), dmatrix = dist.matrix, do.clus.stat = TRUE, do.n.k=TRUE )

  silh.result.all = df %>% cbind(., data.frame(cluster = silhoutte.output[,1], neighbor = silhoutte.output[,2], sil_width = silhoutte.output[,3] ))
  sum.sil = summary(silhoutte.output)
  silhouette.score = mean(sum.sil$clus.avg.widths)
  print(silhouette.score)
  return(silhouette.score)
}

##########################################
## PIXEL CLONALITY ENTROPY (PCE) SCORE  ##
##########################################
#' @description getScore_pce: An aggregate function to compute LDA and the PCE score for HSS.
#' @param x dataframe of training data
#' @param y vector of class labels matching training data rows
#' @param cols vector of column names
#' @noRd
#' @keywords internal
getScore_pce <- function(x, y, cols) {
  ## pixel clonality scoring method
  lda.out <- lda(y~., data=x[, cols])

  if (!exists("pixel.grid")){
    pixel.grid <- create_pixel_grid()
  }
  data.pixels <- as.matrix(x[, cols]) %*% lda.out$scaling
  data.pixels <- data.pixels[ , c("LD1","LD2" )]%>% data.frame()
  data.pixels["labels"] <- y
  colnames(data.pixels) <- c("x","y","labels")

  pce.score = computePCEscore(data = data.pixels)
  print(pce.score)
  return(pce.score)
}

# getScore_pixelDensity <- function(x, y, cols) {
#   ## pixel clonality scoring method
#   # performs LDA using columns provided and returns lowest euclidean distance between pop means
#   lda.out <- lda(y~., data=x[, cols])
#
#   if (!exists("pixel.grid")){
#     pixel.grid <- create_pixel_grid()
#   }
#   data.pixels <- as.matrix(x[, cols]) %*% lda.out$scaling
#   data.pixels <- data.pixels[ , c("LD1","LD2" )]%>% data.frame()
#   data.pixels["labels"] <- y
#   colnames(data.pixels) <- c("x","y","labels")
#
#   density_metric_output <- generate_density_map(data = data.pixels, pixel.grid = pixel.grid)
#   pixelDensity_output <- calculate_pixelDensityScore(data = density_metric_output)
#   pixelDensity.score <-  mean(pixelDensity_output$density.summary.normalized)
#   # print(pixelDensity.score)
#   return(pixelDensity.score)
# }

###############################
## CUSTOM TEMPLATE FUNCTION  ##
###############################
#' @title getScore_custom
#' @description getScore_custom: placeholder for a custom metric
#' @param x dataframe of training data
#' @param y vector of class labels matching training data rows
#' @param cols vector of column names
#' @param custom.score.method a custom-function that takes in x, y, and cols, and outputs a score where the larger the value, the more optimal your separation criteria is.
getScore_custom <- function(x, y, cols, custom.score.method, ...) {
  df = x[, cols, with=F]
  lda.out <- lda(y~., data=df)
  custom_output = custom.score.method(x, y, cols, lda.out, ...)
  return(custom_output)
}

#####################################
## aggregate function for getScore ##
#####################################
#' @description getScore: wrapper function for each getScore metric
#' @param x dataframe of training data
#' @param y vector of class labels matching training data rows
#' @param cols vector of column names
#' @param score.method the scoring method to use.
#' @noRd
getScore <- function(x , y, cols, score.method) {
  if (length(cols) > 1) {
    if (score.method == "euclidean") {
      # print("euclidean")
      scoreFunction <- getScore_euclidean(x, y, cols)
      return(scoreFunction)
    } else if (score.method == "silhouette") {
      # print("silhouette")
      ## pixel clonality scoring method
      scoreFunction <- getScore_silhouette(x, y, cols)
      return(scoreFunction)
    } else if (score.method == "pixel.entropy") {
      # print("pixel.entropy")
      ## pixel clonality scoring method
      scoreFunction <- getScore_pce(x, y, cols)
      return(scoreFunction)
    } else if (score.method == "pixel.density") {
      # print("pixel.density")
      ## pixel clonality scoring method
      scoreFunction <- getScore_pixelDensity(x, y, cols)
      return(scoreFunction)
    } else if (score.method == "custom") {

      if (is.null(custom.score.method)) {
        stop("Must provide a custom score method")
      }
      # print("custom")
      scoreFunction <- getScore_custom(cols, x, y, custom.score.method)
      return(scoreFunction)
    }
  }
}

#' @description hybridSubsetSelection: function that performs hybrid stepwise subset selection
#' @param x dataframe of training data
#' @param y vector of class labels matching training data rows
#' @param score.method the scoring method to use.
#' @param custom.score.method optional input, a custome scoring function (see getScore_custom)
#' @noRd
hybridSubsetSelection <- function(x, y, score.method , custom.score.method = NULL) {
  options(dplyr.summarise.inform = FALSE)

    # performs hybrid stepwise subset selection on LDA reduced dimensions
    # Inputs:
    #   x - data.table to evaluate, rows are cells, columns are columns to evaluate
    #   y - vector of observation classes
    #   two.d - logical if true creates two new axes, if false only one
    # Outputs:
    #   matrix of coefficients where rows are markers and columns are axes

    ### global data structures ###
    keep <- NULL
    channels <- colnames(x)
    n.channels <- length(channels)
    current.score <- 0
    continue <- TRUE
    results <- setNames(data.frame(matrix(nrow=1, ncol=n.channels)), channels)
    results[1,] <- as.list(rep(F, n.channels))
    subtract.log <- results[0,] # record of keep values inputted into subtractOne
    results$score <- 0

    hss.results = list() ## final output

    #############################
    #############################
    #############################
    ##### SCORING FUNCTIONS #####
    #############################
    #############################
    #############################



    # #######################
    # ## SILHOUETTE SCORE  ##
    # #######################
    # getScore_silhouette  <- function(x, y, cols) {
    #   df = x[, cols, with=F]
    #   lda.out <- lda(y~., data=df)
    #   dist.matrix <- df %>% dist() %>% as.matrix()
    #   silhoutte.output <- cluster::silhouette(x = as.numeric(factor(y)), dmatrix = dist.matrix, do.clus.stat = TRUE, do.n.k=TRUE )
    #
    #   silh.result.all<- df %>%
    #     bind_cols(., data.frame(cluster = silhoutte.output[,1], neighbor = silhoutte.output[,2], sil_width = silhoutte.output[,3] ))
    #   sum.sil <- summary(silhoutte.output)
    #   silhouette.score = mean(sum.sil$clus.avg.widths)
    #   print(silhouette.score)
    #   return(silhouette.score)
    # }

    ######################################
    ## PIXEL CLASS ENTROPY (PCE) SCORE  ##
    ######################################
    # getScore_pce <- function(x, y, cols) {
    #   ## pixel clonality scoring method
    #   # performs LDA using columns provided and returns lowest euclidean distance between pop means
    #   lda.out <- lda(y~., data=x[, cols, with=F])
    #   data.pixels <- makeAxes(dt = x[, cols, with=F], co=lda.out$scaling)
    #   data.pixels <- data.pixels[ , c("ld1","ld2" )]
    #   data.pixels$labels <- y
    #   colnames(data.pixels) <- c("x","y","labels")
    #
    #   if (!exists("pixel.grid")){
    #     pixel.grid <<- create_pixel_grid()
    #   }
    #
    #   density_metric_output <- generate_density_map(data = data.pixels, pixel.grid = pixel.grid)
    #   pce.score_output <- calculate_pceScore(data = density_metric_output)
    #   pce.score <-  mean(pce.score_output$pce.score)
    #   print(pce.score)
    #   return(pce.score)
    # }







    ####################
    ## begin analysis ##
    ####################

    ## original, euclidean distance based function
    # } else if (score.method == "pixel.density") {
    #   ## pixel density scoring method
    #   getScore <<- function(cols) {
    #     # performs LDA using columns provided and returns lowest euclidean distance between pop means
    #     print(length(cols))
    #     lda.out <<- lda(y~., data=x[, cols, with=F])
    #     # print(lda.out)
    #     df.density <<- makeAxes(dt = x[, cols, with=F], co=lda.out$scaling)
    #     df.density <<- df.density[ , c("ld1","ld2" )]
    #     df.density$labels <- factor(dat$labels)
    #     colnames(df.density) <- c("x","y","labels")
    #     coordinate.density <- list()
    #     pixel.label.density_sc <- list()
    #
    #     pixel.grid <- create_pixel_grid()
    #     density_metric_output <- generate_density_map(data = df.density, pixel.grid = pixel.grid)
    #     # pixel.clonality_output <- calculate_entropy_metric(data = density_metric_output) %>% mutate(filename = file.name)
    #     # pixel.clonality.metric <<- dplyr::bind_rows(pixel.clonality.metric, pixel.clonality_output)
    #     coordinate.density <- dplyr::bind_rows(coordinate.density, density_metric_output )
    #     density_metric_output <- calculate_pixel_density_metric(data=density_metric_output)
    #     pixel.label.density_sc <-  dplyr::bind_rows(pixel.label.density_sc, density_metric_output)
    #     density_metric_output <- aggregate_pixel_density_metric(data=density_metric_output)
    #     # density_metric_output$filename = file.name
    #     ave.density_metric_output <- density_metric_output %>% dplyr::filter(approach == "percent")
    #     mean.score = mean(ave.density_metric_output$density.summary.normalized)
    #
    #     print(mean.score)
    #     if (two.d) return(mean.score)
    #     return(min(dist(lda.out$means %*% lda.out$scaling[,1])))
    #   }

    ##subset functions
    addOne <- function() {
      # Evaluates the addition of each channel not in keep to keep. Adds best and updates current.score
      temp.results <- results[0,]
      # print(keep)
      # print(channels)
      for (channel in channels[!channels %in% keep]) {
        temp.keep <- c(keep, channel)
        temp.score <- getScore(x, y, cols = temp.keep, score.method)
        temp.results <- rbind(temp.results, as.list(channels %in% temp.keep) %>% append(temp.score))
        # print(temp.results)
        # temp.results <<-temp.results
      }
      colnames(temp.results) = colnames(results)
      current.score <<- max(temp.results$score)
      new.keep <- temp.results[temp.results$score == current.score, channels]
      if (nrow(new.keep) > 1) new.keep <- new.keep[sample(.N,1)]
      keep <<- channels[as.logical(new.keep)]
      results <<- unique(rbind(results, temp.results))
    }

    subtractOne <- function() {
      # Evaluates the subtraction of each channel from keep. Removes worst if it improves score and updates current.score
      # If a better subset is found, it calls itself.
      # If this keep has been evaluted before, exits
      # print("test")
      subtract.log <<- rbind(subtract.log, as.list(channels %in% keep))
      if (anyDuplicated(subtract.log) > 0) {
        subtract.log <<- unique(subtract.log)
        return()
      }
      temp.results <- results[0,]
      # temp.results <<- temp.results
      for (channel in keep) {
        temp.keep <- keep[!keep %in% channel]
        temp.score <- getScore(x, y, cols = temp.keep, score.method)
        temp.results <- rbind(temp.results, as.list(channels %in% temp.keep) %>% append(temp.score))
      }
      # print(colnames(temp.results))
      # print( colnames(results))
      colnames(temp.results) = colnames(results)
      # temp.results <<- temp.results
      # current.score <<- current.score

      if (max(temp.results$score) > current.score) {
        # current.score <<- base::max(temp.results$score)
        current.score <- max(temp.results$score)
        new.keep <- temp.results[temp.results$score == current.score, channels]
        if (nrow(new.keep) > 1) new.keep <- new.keep[1, ]
        keep <<- channels[as.logical(new.keep)]
        results <<- unique(rbind(results, temp.results))
        subtractOne()
      } else results <<- unique(rbind(results, temp.results))
    }


    #############################
    #############################
    #############################
    initializeKeep <- function() {
      # chooses the best scoring pair of markers to initialize keep
      temp.results <- results[0,]

      myChannels = expand.grid(channel.1 = channels, channel.2 = channels) %>% .[.$channel.1 != .$channel.2, ]
      # print(myChannels)
      temp.results = apply(myChannels, 1, function(row.temp.keep){
        temp.keep = row.temp.keep %>% unlist()
        temp.score = getScore(x, y, cols = temp.keep, score.method)
        temp.result = as.list(channels %in% temp.keep) %>% append(temp.score)
        return(temp.result)
      })
      # temp.results <<- temp.results
      temp.results <- do.call("rbind", lapply(temp.results, unlist)) %>% data.frame()
      colnames(temp.results) = colnames(results)

      current.score <<- max(temp.results$score)
      new.keep <- temp.results[temp.results$score==current.score, channels]
      old.keep <<- new.keep
      if (nrow(new.keep) > 1)  new.keep <- new.keep[ which.max(apply(new.keep, 1, sum)), ]
      keep <<- channels[as.logical(new.keep)]
      results <<- unique(rbind(results, temp.results))
    }

    getElbow <- function(res) {
      # takes results and returns the elbow point
      res.lite <- res[ , "no.markers"] %>% unique() %>% .[-1 ] %>% data.frame(no.markers = .)
      res.lite[, "V1"] <- lapply(split(res, res$no.markers) , function(df) {max(df$score)}) %>% unlist(.) %>% .[-1]
      res.lite<<-res.lite
      slope <- (res.lite$V1[nrow(res.lite)] - res.lite$V1[1]) / (res.lite$no.markers[nrow(res.lite)] - res.lite$no.markers[1])
      intercept <- res.lite$V1[1] - slope * res.lite$no.markers[1]
      perp.slope <- -1 / slope
      perp.int <- res.lite$V1 - (perp.slope * res.lite$no.markers)
      xcross <- (intercept - perp.int) / (perp.slope - slope)
      ycross <- slope * xcross + intercept
      dists <-  sqrt((res.lite$no.markers - xcross)^2 + (res.lite$V1 - ycross)^2)
      elbowi <- max(which(dists==max(dists))) # if dists are tie, take the largest number of channels
      return(elbowi+1)
    }

    ### main ###
    initializeKeep()
    while(continue) {
      print(paste("Number of markers:", length(keep)))
      addOne()
      print(paste("Number of markers:", length(keep)))
      if (length(keep) > 3) subtractOne()
      if (length(keep)==length(channels)) continue <- FALSE
    }
    results["no.markers"] = apply(results[, channels], 1, sum)
    elbow <- getElbow(res=results)

    markers <- results[results$no.markers==elbow, ] %>%
      .[.$score==max(.$score), colnames(.) %in% channels] %>%
      unlist() %>%
      .[.==1] %>%
      names(.)
    # print(markers)
    lda.out <- lda(y~., data=x[, markers])


    ## save lda.out results
    hss.results[["method"]] = score.method
    hss.results[["finalMarkers"]] = markers
    hss.results[["HSSscores"]] = results
    hss.results[["ElbowPlot"]] = plotElbow(results = results, elbow = elbow)
    hss.results[["HSS-LDA-model"]] = lda.out
    ## restore options
    options(dplyr.summarise.inform = TRUE) ## turn it back on

    return(hss.results)
  }

#' @title makeAxes
#' @description makeAxes: function that generates new LDA axes given an LDA model coefficients
#' @param df a dataframe of cells (rows) by markers/genes (columns)
#' @param co the dataframe of LDA coefficients from MASS::lda.
#' @keywords internal
#' @export
makeAxes <- function(df=dat, co=coefficient) {
  # makes new axes based on coefficients
  # Inputs:
  #   df - data.frame of data
  #   co - matrix of coefficients
  #   axis.name - character vector of new axis name (e.g. "ld" results in "ld1" and "ld2")
  # Outputs:
  #   df - data.frame
  x <- as.matrix(df[, rownames(co)])
  df = cbind(df, x %*% co)
  return(df)
}


#' @title runHSS
#' @description This function runs hybrid subset selection.
#' @param x table with predictors of interest
#' @param y vector of class labels
#' @param score.method scoring metric for feature selection using HSS. Options include: 'euclidean', 'silhouette', 'pixel.density', 'pixel.entropy', or 'custom'.
#' @param custom.score.method function for your custom scoring metric. Score.method must be 'custom'
#' @keywords internal
#' @export
runHSS <- function(x, y, score.method, custom.score.method = NULL){
  if (!score.method %in% c("euclidean","silhouette", "pixel.density","pixel.entropy")){
    stop("score.method method must be: 'euclidean', 'silhouette', 'pixel.density', 'pixel.entropy', or 'custom'.")
  }

  print(score.method)
  hss.results <- hybridSubsetSelection(x, y, score.method = score.method, custom.score.method = custom.score.method)
  hss.results <<-hss.results
  coefficients = hss.results[["HSS-LDA-model"]]$scaling
  dat <- makeAxes(df=x, co=coefficients)
  dat[["labels"]] = y
  hss.results[["HSS-LDA-result"]] = dat
  return(hss.results)
}


# setwd("~/phd-projects")
# library(dplyr)
# library(magrittr)
# library(ggplot2)
# path  <- "~/phd-projects/sc-lda/data/analysis-ready/metabolism-CD8-naive-data_cell-allmarkers.csv"
# channels <- c('GLUT1', 'HK2', 'GAPDH', 'LDHA', 'MCT1', 'PFKFB4', 'IDH2', 'CyclinB1',
#               'GLUD12', 'CS', 'OGDH', 'CytC', 'ATP5A', 'S6_p', 'HIF1A', 'PDK1_p', 'NRF1',
#               'NRF2_p', 'XBP1', 'VDAC1', 'OPA1', 'DRP1', 'ASCT2', 'GLS', 'GOT2', 'CPT1A',
#               'ACADM', 'IdU', 'BrU', 'Puromycin', 'H3_p',
#               "CD45RA",
#               # "CD69","CD25"
#               "CD69","CD3","CD98","CD25","CD27","CD137","CD57"
#
#               # 'DNA','barium'
# )
# dat_input <- data.table::fread(path)
# dat <- dat_input %>%
#   filter(H3_p < 0.05,
#          dead < 0.3)  %>%
#   group_by(labels) %>%
#   sample_n(1000) %>%
#   data.frame()
# # fwrite(dat, file = output_path)
# # training data with appropriate channels
# train.x <- dat[, channels[1:12]]
# # training class labels - must be 3+ unique classes
# train.y <- dat$labels
# #
# # # x=train.x
# # # y=train.y
# #
# start.time = Sys.time()
# # hss.results=runHSS(x = train.x, y = train.y, score.method = "euclidean")
# # hss.results=runHSS(x = train.x, y = train.y, score.method = "silhouette")
# hss.results=runHSS(x = train.x, y = train.y, score.method = "pixel.entropy")
# end.time = Sys.time()
# coefficients <- hybridSubsetSelection(x=train.x, y=train.y, score.method = "pixel.entropy")
# #
# end.time-start.time
#
# start.time.1 = Sys.time()
# hss.results=runHSS(x = train.x, y = train.y, score.method = "euclidean")
# end.time.1 = Sys.time()
# end.time.1-start.time.1
#
# start.time.2 = Sys.time()
# hss.results=runHSS(x = train.x, y = train.y, score.method = "silhouette")
# end.time.2 = Sys.time()
# end.time.2-start.time.2
#
# start.time.3 = Sys.time()
# hss.results=runHSS(x = train.x, y = train.y, score.method = "pixel.entropy")
# end.time.3 = Sys.time()
# end.time.3-start.time.3
# end.time.1-start.time.1
# end.time.2-start.time.2
# end.time.3-start.time.3
# start.time.4 = Sys.time()
# hss.results=runHSS(x = train.x, y = train.y, score.method = "pixel.density")
# end.time.4 = Sys.time()
# hss.results=runHSS(x = train.x, y = train.y, score.method = "silhouette")







# remotes::install_github("mamouzgar/hsslda")







# dat <- makeAxes()
# # cluster::silhouette(x = y)
# # dist.matr = dist(train.x) %>% as.matrix()
# # silhoutte.output <- cluster::silhouette(x = as.numeric(factor(train.y)), dmatrix = dist.matr, do.clus.stat = TRUE, do.n.k=TRUE )
# ggplot(hss.results$`HSS-LDA-result`, aes(x = ld1, y= ld2)) +
#   geom_point(aes(color = labels)) +
#   viridis::scale_color_viridis(discrete=TRUE)


# Once you’ve got your documentation completed, you can simply run:
#
#   devtools::document()
# This will generate the load_mat.Rd file in the man folder:
#
# You will get one .Rd file for each function in your R package.
#
# Each time you add new documentation to your R function, you need to run devtools::document() again to re-generate the .Rd files.




