rm(list = ls())
graphics.off()
set.seed(10)

library(plyr)
library(dplyr)
library(data.table)
options(dplyr.width = Inf)

# Load functions
source("onet-skill-difference-functions.R")
source("bls_functions.R")

# Load Ankur's processed and averaged onet data
onet <- readRDS("../data/processed/onet_combined_averaged.RDS")

# Get biggest loser occupation
bls <- readRDS("../data/processed/bls_project_gain_loss_sorted_titles.rds")
bls[['difference.10yrs']] <-  bls[['employment.2024']] - bls[['employment.2014']]
bls <- bls[order(bls['difference.10yrs']), ]
bls <- bls[1:30, ]

# Get scores for all top 30 losers
top.losers.soc.vec <- as.character(bls$SOC.Code)
print(top.losers.soc.vec)

# Get score.df for each top loser soc
top.losers.score.df.list <- lapply(top.losers.soc.vec, function(socA) {
    print(socA)
    score.df <- get.score.df(socA, onet)
    new.score.df <- add.titles.to.score.df(score.df, onet)
    saveRDS(new.score.df, file = paste0(socA, "-score-df.rds"))
    return(new.score.df)
})
names(top.losers.score.df.list) <- top.losers.soc.vec
saveRDS(top.losers.score.df.list, "../data/processed/top-losers-score-df-list.rds")