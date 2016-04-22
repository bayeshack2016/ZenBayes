# author: Ryan Brady
library(data.table)


cps.data <- read.csv("http://download.bls.gov/pub/time.series/ln/ln.data.1.AllData", sep = "\t")
cps.dict <- read.csv("http://download.bls.gov/pub/time.series/ln/ln.series", sep = "\t", skip=2, header=F)
names(cps.dict) <- strsplit(paste0(readLines("http://download.bls.gov/pub/time.series/ln/ln.series", n = 2), collapse = "\t"), "\t")[[1]]
cps.data <- data.frame(sapply(cps.data, gsub, pattern = " +", replacement = ""))
cps.dict <- data.frame(sapply(cps.dict, gsub, pattern = " +", replacement = ""))
cps.data <- cps.data[,-5]
cps.data$year <- as.numeric(as.character(cps.data$year))
cps <- list(data = cps.data, dict = cps.dict)
cps$data <- data.table(cps$data, key = "series_id")
cps$dict <- data.table(cps$dict, key = "series_id")
saveRDS(cps, file = "data/processed/cps.RDS")
