rm(list = ls())
graphics.off()
set.seed(10)

library(plyr)
library(dplyr)
library(data.table)
options(dplyr.width = Inf)

# Run only if you need to create onet_combined_averaged.RDS
# source("create-onet-average.R")

# Load functions
source("onet-skill-difference-functions.R")

onet <- readRDS("../data/processed/onet_combined_averaged.RDS")
# onet.orig <- readRDS("../data/processed/")

# We will use these two soc codes as an example.
socA <- "51-4121" # Welders
socB <- "51-4041" # Machinists
# socB <- "43-9031" # Desktop publishers
# socA <- "51-4041" # Machinists
# socA <- "43-9031" # Desktop publishers
# socB <- "51-4121" # Welders

df <- get.BminusA(socA, socB, onet)
head(df)
print(compute.workstyles.diff(df))
print(compute.workvalues.diff(df))
print(compute.workcontext.diff(df))
print(compute.knowledge.diff(df))
print(compute.skills.diff(df))
print(compute.abilities.diff(df))
print(compute.work.activities.diff(df))
print(compute.score(df))


# # In bayeshack2016/R folder
# library(reshape2)
# onet <- readRDS("../data/processed/onet_combined_averaged.RDS")
# source("onet-skill-difference-functions.R")
# socA <- "51-4121" # Welders
# socB <- "51-4041" # Machinists
# compute.score(get.BminusA(socA, socB, onet))

# In bayeshack2016/R folder
library(reshape2)
onet <- readRDS("../data/processed/onet_combined_averaged.RDS")
socA <- "51-4121" # Welders
source("onet-skill-difference-functions.R")
# source("")
x <- get.score.df(socA, onet)
socB <- "11-1031"
df <- get.BminusA(socA, socB, onet)
compute.workstyles.diff(df)