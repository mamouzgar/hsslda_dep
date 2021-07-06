





# save(mydata, file="data/mydata.RData")
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
#   data.frame() %>%
#   dplyr::select(all_of(channels[1:15]), labels)
#
# save(dat, file="data/TcellHartmann2020_sampleData.RData")
# write.table(dat, "data/TcellHartmann2020_sampleData.txt", sep="\t",row.names = FALSE, col.names = TRUE)
# save(dat, file="data/TcellHartmann2020_sampleData.RData")

#
# # fwrite(dat, file = output_path)
# # training data with appropriate channels
# train.x <- dat[, channels[1:15]]
# # training class labels - must be 3+ unique classes
# train.y <- dat$labels
# #
# # # x=train.x
# # # y=train.y
# #
# start.time = Sys.time()
# # hss.results=runHSS(x = train.x, y = train.y, score.method = "euclidean")
#
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
#
#
# df2 = makeAxes(train.x, hss.results$`HSS-LDA-model`$scaling)
# df2$labels = train.y
# ggplot(df2 ) +
#   geom_point(aes(x =LD1,y = LD2, color = labels )) +
#   viridis::scale_color_viridis(option = "viridis",discrete = TRUE)









