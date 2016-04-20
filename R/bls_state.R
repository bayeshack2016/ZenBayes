
library(openxlsx)
library(reshape2)
library(foreach)
library(gdata)
library(choroplethr)
library(ggplot2)


bls.employment <- list()
bls.employment$`2012`<- read.xls("data/bls/state_M2012_dl.xls", 1)
bls.employment$`2013`<- read.xls("data/bls/state_M2013_dl.xls", 1)
bls.employment$`2014`<- read.xlsx("data/bls/state_M2014_dl.xlsx", 1)
bls.employment$`2015`<- read.xlsx("data/bls/state_M2015_dl.xlsx", 1)
bls.employment <- do.call(rbind, bls.employment)
bls.employment$year <- substr(rownames(bls.employment), 1,4)
bls.employment <- subset(bls.employment, OCC_GROUP %in% c("", "detailed"))
bls.employment$year <- as.numeric(bls.employment$year)
bls.employment$OCC_CODE <- as.factor(bls.employment$OCC_CODE)
bls.employment$A_MEAN <- gsub(",", "", bls.employment$A_MEAN)
bls.employment$A_MEAN <- as.numeric(bls.employment$A_MEAN)
bls.employment$TOT_EMP <- gsub(",", "", bls.employment$TOT_EMP)
bls.employment$TOT_EMP <- as.numeric(bls.employment$TOT_EMP)


test.data <- subset(bls.employment, OCC_CODE == "53-7062")
test.data <- test.data %>% 
  group_by(STATE) %>% 
  dplyr::summarize(region = tolower(STATE[1]), value = TOT_EMP[year == 2015] / TOT_EMP[year == 2014] - 1)
p1 <- state_choropleth(test.data) + ggtitle("% Change in Employment") + scale_fill_brewer(type = "div", type = 2)

test.data <- subset(bls.employment, OCC_CODE == "53-7062")
test.data <- test.data %>% 
  group_by(STATE) %>% 
  dplyr::summarize(region = tolower(STATE[1]), value = A_MEAN[year == 2015] / A_MEAN[year == 2014] -1)
p2 <- state_choropleth(test.data) + ggtitle("% Change in Salary") + scale_fill_brewer(type = "div")

grid.arrange(p1, p2)
