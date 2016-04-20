library(reshape2)

# Ingest ONET data
onet <- lapply(list.files("data/O_NET/", full.names = TRUE), 
               read.csv, fill = TRUE, header = TRUE, sep = '\t')
names(onet) <- gsub(".txt", "", list.files("data/O_NET/"))



bls.oc <- read.csv("http://download.bls.gov/pub/time.series/oe/oe.data.0.Current", 
                 fill = T, 
                 header = T, 
                 sep = '\t')
bls.oc$datatype <- as.numeric(substr(bls.oc$series_id, 24,25))
bls.oc$occupation_code <- factor(substr(bls.oc$series_id, 18,23))
bls.oc$value <- gsub("[:blank:]*-", NA, bls.oc$value)
bls.oc$value <- gsub(",", "", bls.oc$value)
bls.oc$value <- as.numeric(as.character(bls.oc$value))
bls.oc.employment <- dcast(subset(bls.oc, datatype == 1), 
                     occupation_code + year + period ~ datatype, 
                     value.var = "value", 
                     fun.aggregate = sum, na.rm = TRUE)


bls.ln <- read.csv("http://download.bls.gov/pub/time.series/ln/ln.data.1.AllData",
                   fill = T, 
                   header = T, 
                   sep = '\t')
bls.ln.series <- read.csv("http://download.bls.gov/pub/time.series/ln/ln.series",
                   fill = T, 
                   header = T, 
                   sep = '\t', skip = 2)
names(bls.ln.series) <- strsplit(
  paste0(
    readLines("http://download.bls.gov/pub/time.series/ln/ln.series", n=2),
  collapse = '\t'),
  '\t')[[1]]
bls.ln.series$series_id <- substr(bls.ln.series$series_id, 1, 11)
bls.ln$series_id <- substr(bls.ln$series_id, 1, 11)
bls.ln$series_title <- bls.ln.series[match(bls.ln$series_id, bls.ln.series[,1]), "series_title"]
bls.ln$periodicity <- substr(bls.ln$period, 1, 1)
bls.ln$period <- substr(bls.ln$period, 2, 3)
bls.ln$value <- as.character(bls.ln$value)
bls.ln$value <- gsub("[:space:]*-", "", bls.ln$value)
bls.ln$value <- gsub(",", "", bls.ln$value)
bls.ln$value <- as.numeric(bls.ln$value)

subset(bls.ln, series_title == "(unadj) Employed - Chief executives" )
