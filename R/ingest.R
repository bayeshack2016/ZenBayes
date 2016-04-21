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
bls.state$`1997` <- subset(bls.state$`1997`, !is.na(occ_code))
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
bls.industry$`1997` <- read.xls("data/bls/nat3d_sic_1997_dl.xls", 1, skip = 32, blank.lines.skip = FALSE)
bls.industry$`1998` <- read.xls("data/bls/nat3d_sic_1998_dl.xls", 1, skip = 32, blank.lines.skip = FALSE)
bls.industry$`1999` <- read.xls("data/bls/nat3d_sic_1999_dl.xls", 1, skip = 35, blank.lines.skip = FALSE)
bls.industry$`2000` <- read.xls("data/bls/nat3d_sic_2000_dl.xls", 1, skip = 34, blank.lines.skip = FALSE)


# Process state
state.cols <- c("state", 
                "occ_code", 
                "tot_emp",
                "annual_wages_mean",
                "annual_wages_median")
bls.state <- 
  foreach(bls.year = bls.state) %do% {
    names(bls.year) <- tolower(names(bls.year))
    bls.year$tot_emp <- as.numeric(as.character(bls.year$tot_emp))
    bls.year$annual_wages_mean <- as.numeric(as.character(bls.year$a_mean))
    bls.year$annual_wages_median <- as.numeric(as.character(bls.year$a_median))
    bls.year <- bls.year[ , state.cols]
    
  }
names(bls.state) <- 1997:2015
bls.state <- do.call(rbind, bls.state)
bls.state$year <- substr(rownames(bls.state), 1,4)
rownames(bls.state) <- NULL
saveRDS(bls.state, "data/processed/bls_state.RDS")
  
# Process industry
ind.cols <- c("naics", 
              "naics_title", 
              "occ_code",
              "tot_emp",
              "annual_wages_mean",
              "annual_wages_median")
ind.yrs <- names(bls.industry)
bls.industry <- 
  foreach(bls.year = bls.industry) %do% {
    names(bls.year) <- tolower(names(bls.year))
    names(bls.year)[names(bls.year) == "sic"] <- "naics"
    names(bls.year)[names(bls.year) == "sic_title"] <- "naics_title"
    bls.year$occ_code  <- as.character(bls.year$occ_code)
    bls.year$tot_emp <- as.numeric(as.character(bls.year$tot_emp))
    bls.year$annual_wages_mean <- as.numeric(as.character(bls.year$a_mean))
    bls.year$annual_wages_median <- as.numeric(as.character(bls.year$a_median))
    bls.year <- bls.year[ , ind.cols]
  }
names(bls.industry) <- ind.yrs
bls.industry <- do.call(rbind, bls.industry)
bls.industry$year <- substr(rownames(bls.industry), 1,4)
rownames(bls.industry) <- NULL
saveRDS(bls.industry, "data/processed/bls_industry.RDS")