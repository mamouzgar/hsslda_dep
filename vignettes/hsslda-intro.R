## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----eval=FALSE---------------------------------------------------------------
#  remotes::install_github("mamouzgar/hsslda")

## ---- warning=FALSE, include=FALSE--------------------------------------------
library(magrittr)

## ---- warning=FALSE, include=TRUE---------------------------------------------
library(hsslda)

## ---- echo=TRUE, results = 'hide'---------------------------------------------
Tcell_intro = read.csv("~/phd-projects/Rpackage/hsslda/data/TcellHartmann2020_sampleData.txt",sep="\t", stringsAsFactors = T)

## -----------------------------------------------------------------------------
head(Tcell_intro)
colnames(Tcell_intro)

## ---- echo=TRUE, results = 'hide'---------------------------------------------
channels = c('GLUT1', 'HK2', 'GAPDH', 'LDHA', 'MCT1', 'PFKFB4', 'IDH2', 'CyclinB1', 'GLUD12', 'CS', 'OGDH', 'CytC', 'ATP5A', 'S6_p', 'HIF1A')
train.x = Tcell_intro[channels]
train.y = Tcell_intro[['labels']]
hss.result = runHSS(x = train.x, y = train.y, score.method = 'euclidean')


## -----------------------------------------------------------------------------
hss.result$ElbowPlot

## -----------------------------------------------------------------------------
lda.df = hss.result$`HSS-LDA-result`
head(lda.df)

## ---- echo=FALSE--------------------------------------------------------------
lda.df$labels = train.y

## ---- echo=FALSE--------------------------------------------------------------
ggplot2::ggplot(lda.df,ggplot2::aes(x = LD1, y = LD2, color = labels)) +
  ggplot2::geom_point() +
  viridis::scale_color_viridis(discrete = TRUE, option = "viridis") 

## ---- echo=TRUE---------------------------------------------------------------
hss.result$`HSS-LDA-model`

## ---- echo=FALSE, results = 'hide'--------------------------------------------
newdata = train.x

## ---- echo=TRUE, results = 'hide'---------------------------------------------
lda.df.newData = makeAxes(df = newdata, co =hss.result$`HSS-LDA-model`$scaling)

## ----eval=FALSE---------------------------------------------------------------
#  hss.resultCustom = runHSS(x = train.x, y = train.y, score.method = 'custom', custom.score.method = myCustomFunction)

