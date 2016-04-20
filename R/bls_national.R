
library(openxlsx)
library(reshape2)
library(foreach)

bls.employment <- list()
bls.employment$`2012`<- readWorksheet(loadWorkbook("data/bls/national_M2012_dl.xls"), 1)
bls.employment$`2013`<- readWorksheet(loadWorkbook("data/bls/national_M2013_dl.xls"), 1)
bls.employment$`2014`<- readWorksheet(loadWorkbook("data/bls/national_M2014_dl.xlsx"), 1)
bls.employment$`2015`<- readWorksheet(loadWorkbook("data/bls/national_M2015_dl.xlsx"), 1)
bls.employment <- do.call(rbind, bls.employment)
bls.employment$year <- substr(rownames(bls.employment), 1,4)
bls.employment <- subset(bls.employment, OCC_GROUP %in% c("", "detailed"))
bls.employment$year <- as.numeric(bls.employment$year)
bls.employment$OCC_CODE <- as.factor(bls.employment$OCC_CODE)
bls.employment$A_MEAN <- gsub(",", "", bls.employment$A_MEAN)
bls.employment$A_MEAN <- as.numeric(bls.employment$A_MEAN)

colors <- foreach(i = unique(bls.employment$OCC_CODE), .combine = rbind) %do% {
  data.frame(occ.code = i,
             value = coef(lm(log(TOT_EMP) ~ year, data = subset(bls.employment, OCC_CODE == i)))[2])
}
colors$value[is.na(colors$value)] <- 0



plot(career.graph, 
     vertex.size = sizes,
     edge.arrow.size = 0.05,
     edge.width = 0.1,
     vertex.label = labs,
     vertex.color = rgb(pmin(cbind(1 - colors$value / 0.2), 1), 
                        pmin(cbind(1 + colors$value / 0.2), 1),
                        pmin(cbind(1 - abs(colors$value) / 0.2), 1))
)


colors2 <- foreach(i = unique(bls.employment$OCC_CODE), .combine = rbind) %do% {
  data.frame(occ.code = i,
             value = mean(bls.employment$A_MEAN[bls.employment$OCC_CODE == i]))
}
colors2$value[is.na(colors2$value)] <- 0

plot(career.graph, 
     vertex.size = sizes,
     edge.arrow.size = 0.05,
     edge.width = 0.1,
     vertex.label = labs,
     vertex.color = rgb(pmin(1 - log(colors2$value / 70000)/3, 1), 
                        pmin(1 + log(colors2$value / 70000)/3, 1),
                        pmin(1 - abs(log(colors2$value / 70000)/3), 1))
)


colors3 <- foreach(i = unique(bls.employment$OCC_CODE), .combine = rbind) %do% {
  data.frame(occ.code = i,
             value = tryCatch(
               coef(lm(log(A_MEAN * TOT_EMP) ~ year, data = subset(bls.employment, OCC_CODE == i)))[2],
               error = function(e) NA
             ))
}
colors3$value[is.na(colors3$value)] <- 0

png("")
plot(career.graph, 
     vertex.size = sizes,
     edge.arrow.size = 0.05,
     edge.width = 0.1,
     vertex.label = labs,
     vertex.color = rgb(pmin(cbind(1 - colors3$value / 0.35), 1), 
                        pmin(cbind(1 + colors3$value / 0.35), 1),
                        pmin(cbind(1 - abs(colors3$value) / 0.35), 1))
)

colors4 <- colors3[V(career.graph) %in% neighborhood(career.graph, 1, nodes = "519151")[[1]],]
plot(induced_subgraph(career.graph, neighborhood(career.graph, 1, nodes = "519151")[[1]]),
     vertex.label = as.character(lab.tab[V(career.graph) %in% neighborhood(career.graph, 1, nodes = "519151")[[1]], 2]),
     edge.arrow.size = 0.05,
     edge.width = 0.1,
     vertex.color = rgb(pmin(cbind(1 - colors4$value / 0.35), 1), 
                        pmin(cbind(1 + colors4$value / 0.35), 1),
                        pmin(cbind(1 - abs(colors4$value) / 0.35), 1))
     )


plot(career.graph, 
     vertex.size = sizes,
     edge.arrow.size = 0.05,
     edge.width = 0.1,
     vertex.label = labs,
     vertex.color = as.factor(substr(colors3$occ.code, 1, 2)))
)