# Author: Ryan Brady

library(openxlsx)
library(reshape2)
library(foreach)
library(gdata)
library(stringr)

# Ingest ONET data
onet <- lapply(list.files("data/O_NET/", full.names = TRUE), 
               read.csv, fill = TRUE, header = TRUE, sep = '\t')
names(onet) <- gsub(".txt", "", list.files("data/O_NET/"))

crosswalk <- list()
crosswalk[[1]] <- read.csv("data/crosswalks/2000_to_2006_Crosswalk.csv")
crosswalk[[2]] <- read.csv("data/crosswalks/2006_to_2009_Crosswalk.csv")
crosswalk[[3]] <- read.csv("data/crosswalks/2009_to_2010_Crosswalk.csv")
crosswalk <- Reduce(function(...) merge(..., all = T), crosswalk)
crosswalk <- as.data.frame(sapply(crosswalk, gsub, pattern = "[.][0-9]{2}", replacement = ""))

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


bls.metro <- list()
bls.metro$`1997` <- rbind(read.xls("data/bls/oes97ma1.xls", skip = 31, blank.lines.skip = FALSE),
                          read.xls("data/bls/oes97ma2.xls", skip = 32, blank.lines.skip = FALSE))
bls.metro$`1998` <- rbind(read.xls("data/bls/MSA_1998_dl_1.xls", skip = 42, blank.lines.skip = FALSE),
                          read.xls("data/bls/MSA_1998_dl_2.xls", skip = 42, blank.lines.skip = FALSE),
                          read.xls("data/bls/MSA_1998_dl_3.xls", skip = 42, blank.lines.skip = FALSE)[,1:25])
bls.metro$`1999` <- rbind(read.xls("data/bls/MSA_1999_dl_1.xls", skip = 43, blank.lines.skip = FALSE),
                          read.xls("data/bls/MSA_1999_dl_2.xls", skip = 43, blank.lines.skip = FALSE))
bls.metro$`2000` <- rbind(read.xls("data/bls/MSA_2000_dl_1.xls", skip = 42, blank.lines.skip = FALSE),
                          read.xls("data/bls/MSA_2000_dl_2.xls", skip = 34, blank.lines.skip = FALSE))
bls.metro$`2001` <- rbind(read.xls("data/bls/MSA_2001_dl_1.xls"),
                          read.xls("data/bls/MSA_2001_dl_2.xls"),
                          read.xls("data/bls/MSA_2001_dl_3.xls"))
bls.metro$`2002` <- rbind(read.xls("data/bls/MSA_2002_dl_1.xls"),
                          read.xls("data/bls/MSA_2002_dl_2.xls"))
bls.metro$`2003` <- rbind(read.xls("data/bls/MSA_may2003_dl_1.xls"),
                          read.xls("data/bls/MSA_may2003_dl_2.xls"))
bls.metro$`2004` <- rbind(read.xls("data/bls/MSA_may2004_dl_1.xls"),
                          read.xls("data/bls/MSA_may2004_dl_2.xls"),
                          read.xls("data/bls/MSA_may2004_dl_3.xls"))
bls.metro$`2005` <- rbind(read.xls("data/bls/MSA_may2005_dl_1.xls"),
                          read.xls("data/bls/MSA_may2005_dl_2.xls"),
                          read.xls("data/bls/MSA_may2005_dl_3.xls"))
bls.metro$`2006` <- rbind(read.xls("data/bls/MSA_may2006_dl_1.xls"),
                          read.xls("data/bls/MSA_may2006_dl_2.xls"),
                          read.xls("data/bls/MSA_may2006_dl_3.xls"))
bls.metro$`2007` <- rbind(read.xls("data/bls/MSA_May2007_dl_1.xls"),
                          read.xls("data/bls/MSA_May2007_dl_2.xls"),
                          read.xls("data/bls/MSA_May2007_dl_3.xls"))
bls.metro$`2008` <- rbind(read.xls("data/bls/MSA_M2008_dl_1.xls"),
                          read.xls("data/bls/MSA_M2008_dl_2.xls"),
                          read.xls("data/bls/MSA_M2008_dl_3.xls"))
bls.metro$`2009` <- rbind(read.xls("data/bls/MSA_dl_1.xls"),
                          read.xls("data/bls/MSA_dl_2.xls"),
                          read.xls("data/bls/MSA_dl_3.xls"))
bls.metro$`2010` <- rbind(read.xls("data/bls/MSA_M2010_dl_1.xls"),
                          read.xls("data/bls/MSA_M2010_dl_2.xls"),
                          read.xls("data/bls/MSA_M2010_dl_3.xls"))
bls.metro$`2011` <- rbind(read.xls("data/bls/MSA_M2011_dl_1_AK_IN.xls"),
                          read.xls("data/bls/MSA_M2011_dl_2_KS_NY.xls"),
                          read.xls("data/bls/MSA_M2011_dl_3_OH_WY.xls"))
bls.metro$`2012` <- rbind(read.xls("data/bls/MSA_M2012_dl_1_AK_IN.xls"),
                          read.xls("data/bls/MSA_M2012_dl_2_KS_NY.xls"),
                          read.xls("data/bls/MSA_M2012_dl_3_OH_WY.xls"))
bls.metro$`2013` <- rbind(read.xls("data/bls/MSA_M2013_dl_1_AK_IN.xls"),
                          read.xls("data/bls/MSA_M2013_dl_2_KS_NY.xls"),
                          read.xls("data/bls/MSA_M2013_dl_3_OH_WY.xls"))
bls.metro$`2014` <- read.xlsx("data/bls/MSA_M2014_dl.xlsx")
bls.metro$`2015` <- read.xlsx("data/bls/MSA_M2015_dl.xlsx")

# To get consistent occupation names:
bls.crosswalk <- function(bls.set){
  bls.set$O.NET.SOC.Code[bls.set$year %in% 2000:2005] <- 
    as.character(crosswalk$O.NET.SOC.2010.Code[match(bls.set$occ_code[bls.set$year %in% 2000:2005], crosswalk$O.NET.SOC.2000.Code)])
  bls.set$O.NET.SOC.Code[bls.set$year %in% 2006:2008] <- 
    as.character(crosswalk$O.NET.SOC.2010.Code[match(bls.set$occ_code[bls.set$year %in% 2006:2008], crosswalk$O.NET.SOC.2000.Code)])
  bls.set$O.NET.SOC.Code[bls.set$year == 2009] <- 
    as.character(crosswalk$O.NET.SOC.2010.Code[match(bls.set$occ_code[bls.set$year == 2009], crosswalk$O.NET.SOC.2000.Code)])
  bls.set$O.NET.SOC.Code[bls.set$year >= 2010] <- as.character(bls.set$occ_code[bls.set$year >= 2010])
  return(bls.set)
}


# Process state
state.cols <- c("state", 
                "occ_code", 
                "tot_emp",
                "annual_wages_mean",
                "annual_wages_median")
bls.state <- 
  foreach(bls.year = bls.state) %do% {
    names(bls.year) <- tolower(names(bls.year))
    bls.year$tot_emp <- as.numeric(gsub(",", "", (bls.year$tot_emp)))
    bls.year$annual_wages_mean <- as.numeric(gsub(",", "", (bls.year$a_mean)))
    bls.year$annual_wages_median <- as.numeric(gsub(",", "", (bls.year$a_median)))
    bls.year[ , state.cols]
  }
names(bls.state) <- 1997:2015
bls.state <- do.call(rbind, bls.state)
bls.state$year <- substr(rownames(bls.state), 1,4)
rownames(bls.state) <- NULL
bls.state <- bls.crosswalk(bls.state)
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
    bls.year$tot_emp <- as.numeric(gsub(",", "", (bls.year$tot_emp)))
    bls.year$annual_wages_mean <- as.numeric(gsub(",", "", (bls.year$a_mean)))
    bls.year$annual_wages_median <- as.numeric(gsub(",", "", (bls.year$a_median)))
    bls.year[ , ind.cols]
  }
names(bls.industry) <- ind.yrs
bls.industry <- do.call(rbind, bls.industry)
bls.industry$year <- substr(rownames(bls.industry), 1,4)
rownames(bls.industry) <- NULL
bls.industry <- bls.crosswalk(bls.industry)
saveRDS(bls.industry, "data/processed/bls_industry.RDS")

# Process industry
metro.cols <- c("state", 
              "area", 
              "area_name",
              "occ_code",
              "tot_emp",
              "annual_wages_mean",
              "annual_wages_median")
metro.yrs <- names(bls.metro)
bls.metro <- 
  foreach(bls.year = bls.metro) %do% {
    names(bls.year) <- tolower(names(bls.year))
    names(bls.year)[names(bls.year) == "prim_state"] <- "state"
    bls.year$occ_code  <- as.character(bls.year$occ_code)
    bls.year$tot_emp <- as.numeric(gsub(",", "", (bls.year$tot_emp)))
    bls.year$annual_wages_mean <- as.numeric(gsub(",", "", (bls.year$a_mean)))
    if("a_median" %in% names(bls.year)) {
      bls.year$annual_wages_median <- as.numeric(gsub(",", "", (bls.year$a_median)))
    }
    else {
      bls.year$annual_wages_median <- NA
    }
    bls.year[ , metro.cols]
  }
names(bls.metro) <- metro.yrs
bls.metro <- do.call(rbind, bls.metro)
bls.metro$year <- substr(rownames(bls.metro), 1,4)
rownames(bls.metro) <- NULL
bls.metro <- bls.crosswalk(bls.metro)
saveRDS(bls.metro, "data/processed/bls_metro.RDS")


bls.project  <- read.csv("data/bls/Employment Projections.csv")
bls.project <- bls.project[, c("SOC.Code", "X2014", "X2024")]
bls.project$X2014 <- as.numeric(gsub(",", "", bls.project$X2014)) * 1000
bls.project$X2024 <- as.numeric(gsub(",", "", bls.project$X2024)) * 1000
names(bls.project) <- c("SOC.Code", "employment.2014", "employment.2024")
saveRDS(bls.project, "data/processed/bls_project.RDS")
