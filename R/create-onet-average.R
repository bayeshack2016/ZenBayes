rm(list = ls())
graphics.off()
set.seed(10)

library(plyr)
library(dplyr)
library(data.table)
options(dplyr.width = Inf)

onet.orig <- readRDS("../data/processed/onet_combined.RDS")
print(head(onet.orig))
print(lapply(onet.orig, class))

# We do not need the Element.ID column anymore.
onet <- onet.orig[setdiff(names(onet.orig), "Element.ID")]

# We rename some columns to be more meaningful.
onet <- plyr::rename(onet, replace = c("set.source" = "domain"))

# Some Element.Name values differ by only whitespace.
# We will trim all whitespace on both sides.
onet$Element.Name <- trimws(onet$Element.Name)

# We can get rows that have the same Date but belong to different years.
# This is most likely a bug in the data itself.
# In such a case, we are going to select the row that corresponds to the latest year.
onet$year <- as.numeric(sub("^\\w+.", "", onet$onet.source))

# The domain column has some variation. We are going to homogenize this variation.
domain.variants <- list("WorkValues" = c("Work Values", "WorkValue", "WorkValue.TXT"),
                            "Skills" = c("Skills", "Skills.TXT"),
                            "Knowledge" = c("Knowledge", "Knowledge.TXT"),
                            "WorkActivities" = c("Work Activities", "WorkActivity", "WorkActivity.TXT"),
                            "Abilities" = c("Ability", "Ability.TXT", "Abilities"),
                            "Interest" = c("Interest", "Interest.TXT", "Interests"),
                            "WorkContext" = c("Work Context", "WorkContext", "WorkContext.TXT"),
                            "EducTrainExp" = c("Education, Training, and Experience"),
                            "WorkStyles" = c("Work Styles"))
for (i in seq_along(domain.variants)) {
    onet$domain[onet$domain %in% domain.variants[[i]]] <- names(domain.variants)[[i]]
}

# Different domains have different Scales
# Very important: https://www.onetonline.org/help/online/scales
domains.imp <- c("WorkStyles") # Importance (IM)
domains.imp.lev <- c("Knowledge", "Skills", "Abilities", "WorkActivities")  # Importance (IM), Level (LV)
domains.ext <- c("WorkValues")  # Extent (EN, EX)
domains.con <- c("WorkContext") # Context

# Importance (IM)
# IM is on the scale of 1--5
# "Not Important" (1) to "Extremely Important" (5)

# Extent (EN, EX)
# EN, EX are both on the scale 1--5
# No guidance on what the values mean

# Context (CX, CXP, CT, CTP)
# CX - Context 1--5
# CXP - Context (Categories 1-5) 0--100
# CT - Context 1--3
# CTP - Context (Categories 1-3) 0--100
# No guidance on what these values mean
# We will interpret these as follows:
# Different values of CX form distinct unnamed Categories.
# For each Category, CXP is the level of the context we need, with the understanding
# that higher value means higher requirement.
# The exact same understanding applies to CT, CTP.

# Knowledge, Skills, Abilities, WorkActivities (IM, LV)
# IM - Importance 1--5
# LV - Level 0--7
# IM : "Not Important" (1) to "Extremely Important" (5)
# LV: Continuum scale
# We judge by IM * LV

# For each occupation code (fully qualified), quality (combination of Element.Name, domain),
# and requirement (Scale.ID), get only one row. This one row should correspond to the
# latest date. This way we have the latest data to work with.
onet <- onet %>% group_by_("O.NET.SOC.Code", "Element.Name", "Scale.ID", "domain") %>%
            filter(Date == max(Date)) %>% filter(year == max(year))

# We do not care about the last two digits in the O.NET.SOC.Code. We will take a mean
# a mean of the Data.Value values for all sub-specializations of the SOC code.
onet <- onet %>% group_by_("SOC", "Element.Name", "Scale.ID", "domain") %>%
        dplyr::summarize(mean.value = mean(Data.Value))

saveRDS(onet, file = "../data/processed/onet_combined_averaged.RDS")
