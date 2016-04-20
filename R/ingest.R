library(openxlsx)
library(reshape2)
library(foreach)
library(gdata)

# Ingest ONET data
onet <- lapply(list.files("data/O_NET/", full.names = TRUE), 
               read.csv, fill = TRUE, header = TRUE, sep = '\t')
names(onet) <- gsub(".txt", "", list.files("data/O_NET/"))


# Ingest State BLS OES data
bls.state <- list()
bls.state$`1997` <- read.xls("data/bls/state_1997_dl.xls", 1, skip = 40, blank.lines.skip = FALSE)
bls.state$`1998` <- read.xls("data/bls/state_1998_dl.xls", 1, skip = 41, blank.lines.skip = FALSE)
bls.state$`1999` <- read.xls("data/bls/state_1999_dl.xls", 1, skip = 43, blank.lines.skip = FALSE)
bls.state$`2000` <- read.xls("data/bls/state_2000_dl.xls", 1, skip = 42, blank.lines.skip = FALSE)
bls.state$`2001` <- read.xls("data/bls/state_2001_dl.xls", 1)
bls.state$`2002` <- read.xls("data/bls/state_2002_dl.xls", 1)
bls.state$`2003` <- read.xls("data/bls/state_may2003_dl.xls", 1)
bls.state$`2004` <- read.xls("data/bls/state_may2004_dl.xls", 1)
bls.state$`2005` <- read.xls("data/bls/state_may2005_dl.xls", 1)
bls.state$`2006` <- read.xls("data/bls/state_may2006_dl.xls", 1)
bls.state$`2007` <- read.xls("data/bls/state_May2007_dl.xls", 1)
bls.state$`2008`<- read.xls("data/bls/state__M2008_dl.xls", 1)
bls.state$`2009`<- read.xls("data/bls/state_dl.xls", 1)
bls.state$`2010`<- read.xls("data/bls/state_M2010_dl.xls", 1)
bls.state$`2011`<- read.xls("data/bls/state_M2011_dl.xls", 1)
bls.state$`2012`<- read.xls("data/bls/state_M2012_dl.xls", 1)
bls.state$`2013`<- read.xls("data/bls/state_M2013_dl.xls", 1)
bls.state$`2014`<- read.xlsx("data/bls/state_M2014_dl.xlsx", 1)
bls.state$`2015`<- read.xlsx("data/bls/state_M2015_dl.xlsx", 1)


# Ingest Industry BLS OES data
to.get <- list.files("data/bls/", pattern = "nat3d.*xls$", full.names = TRUE)
bls.industry <- lapply(to.get, read.xls, sheet = 1)
to.get2 <- list.files("data/bls/", pattern = "nat3d.*xlsx$", full.names = TRUE)
bls.industry <- c(bls.industry, lapply(to.get2, read.xlsx, sheet = 1))
names(bls.industry) <- c(str_extract(to.get, "[0-9]{4}"), str_extract(to.get2, "[0-9]{4}"))


# Process state
state.cols <- c("state", 
                "occ_code", 
                "a_mean",
                "a_median")
bls.state <- 
  foreach(bls.year = bls.state) %do% {
    names(bls.year) <- tolower(names(bls.year))
    names(bls.year)[names(bls.year) == "group"] <- "occ_group"
    bls.year <- subset(bls.year, occ_group %in% c("", "detailed"), select = state.cols)
  }
names(bls.state) <- 1997:2015
bls.state <- do.call(rbind, bls.state)
bls.state$year <- substr(rownames(bls.state), 1,4)
rownames(bls.state) <- NULL
saveRDS(bls.state, "data/processed/bls_state.RDS")
