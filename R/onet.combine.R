# Author: Ryan Brady

onet.all <- readRDS("data/processed/onet_all_years.RDS")

onet.all.combined <- foreach(j  = 1:length(onet.all)) %do% {
  onet <- onet.all[[j]]
  onet.combined <- list()
  for(i in 1:length(onet)) {
    if(all(c("O.NET.SOC.Code", "Element.ID") %in% names(onet[[i]]))){
      onet.combined[[names(onet)[i]]] <- onet[[i]]
    }
  }
  res <- do.call(plyr::rbind.fill, onet.combined)
  res$onet.source <- names(onet.all)[[j]]
  res
}
onet.all.combined <- do.call(plyr::rbind.fill, onet.all.combined)
onet.all.combined$Category <- as.numeric(onet.all.combined$Category)
onet.temp <-
  onet.all.combined[!is.na(onet.all.combined$Category), ] %>% 
  group_by(O.NET.SOC.Code, Element.ID, Element.Name, Scale.ID, Date, onet.source) %>% 
  dplyr::summarize(Data.Value = weighted.mean(Category, w = Data.Value))
onet.all.combined <- plyr::rbind.fill(onet.all.combined[is.na(onet.all.combined$Category), names(onet.temp)], 
                                      onet.temp)
onet.all.combined$Date[onet.all.combined$onet.source == "June.2002"] <- "3/2002"
onet.all.combined$onet.source <- NULL
onet.all.combined <- unique(onet.all.combined)
saveRDS(onet.all.combined, "data/processed/onet_combined.RDS")
