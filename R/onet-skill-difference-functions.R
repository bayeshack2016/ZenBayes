# Concept:
# We need to transfer an employee from SOCA to SOCB.
# Which skills does SOCB require but are not required for SOCA?
# These skills are the ones that the employee will need to acquire in order to
# successfully complete the transfer.

get.requirements.for.soc <- function(soc, onet) {
    library(testthat)
    expect_false("soc" %in% names(onet))
    return(subset(onet, SOC == soc))
}

get.BminusA <- function(socA, socB, onet) {
    # Get the requirements for both SOCs
    reqA <- get.requirements.for.soc(socA, onet)
    reqB <- get.requirements.for.soc(socB, onet)

    # Create a dataframe that contains all the requirements from B
    # but not all the requirements from A.
    df <- base::merge(reqA, reqB, by = c("Element.Name", "Scale.ID", "domain"),
                      all.y = TRUE, suffixes = c(".A", ".B"))
    df$mean.value.A[is.na(df$mean.value.A)] <- 0
    df$BminusA <- df$mean.value.B - df$mean.value.A
    return(df)
}

compute.workstyles.diff <- function(df) {
    df.subset <- subset(df, domain == "WorkStyles")
    library(testthat)
    expect_equal(unique(df.subset$Scale.ID), "IM")
    return(mean(df.subset$BminusA[df.subset$BminusA > 0]))
}

compute.workvalues.diff <- function(df) {
    df.subset <- subset(df, domain == "WorkValues")
    library(testthat)
    expect_true(all(unique(df.subset$Scale.ID) %in% c("EN", "EX")))
    return(mean(df.subset$BminusA[df.subset$BminusA > 0]))
}

compute.workcontext.diff <- function(df) {
    df.subset <- subset(df, domain == "WorkContext")
    library(testthat)
    expect_true(all(unique(df.subset$Scale.ID) %in% c("CX", "CXP", "CT", "CTP")))
    # Disable these checks because the data sucks:
    # expect_equal(sum(df.subset$Scale.ID == "CX"), sum(df.subset$Scale.ID == "CXP"))
    # expect_equal(sum(df.subset$Scale.ID == "CT"), sum(df.subset$Scale.ID == "CTP"))

    # For now, we do not care about the categories themselves (i.e. CX, CT) as long as we
    # subtract the required level at the same scale.
    df.subset <- subset(df.subset, Scale.ID %in% c("CXP", "CTP"))
    return(mean(df.subset$BminusA[df.subset$BminusA > 0]))
}

compute.imp.lvl.diff <- function(df, this.domain) {
    library(testthat)
    expect_false("this.domain" %in% names(df))
    df.subset <- subset(df, domain == this.domain)
    expect_true(all(unique(df.subset$Scale.ID) %in% c("IM", "LV")))
    expect_equal(sum(df.subset$Scale.ID == "IM"), sum(df.subset$Scale.ID == "LV"))

    # Unmelt the dataframes for each soc codes
    dfA <- dcast(df.subset, Element.Name ~ Scale.ID, value.var = "mean.value.A")
    dfB <- dcast(df.subset, Element.Name ~ Scale.ID, value.var = "mean.value.B")
    BminusA <- merge(dfA, dfB, by = "Element.Name", suffixes = c(".A", ".B"))

    # Compute the score as (IM * LV)_B - (IM * LV)_A
    BminusA$IM.LV.A <- BminusA$IM.A * BminusA$LV.A
    BminusA$IM.LV.B <- BminusA$IM.B * BminusA$LV.B
    BminusA$IM.LV.B.minus.IM.LV.A <- BminusA$IM.LV.B - BminusA$IM.LV.A
    return(mean(BminusA$IM.LV.B.minus.IM.LV.A[BminusA$IM.LV.B.minus.IM.LV.A > 0]))
}

compute.knowledge.diff <- function(df) {
    compute.imp.lvl.diff(df, "Knowledge")
}

compute.skills.diff <- function(df) {
    compute.imp.lvl.diff(df, "Skills")
}

compute.abilities.diff <- function(df) {
    compute.imp.lvl.diff(df, "Abilities")
}

compute.work.activities.diff <- function(df) {
    compute.imp.lvl.diff(df, "WorkActivities")
}

compute.score <- function(df, w = rep(1, 7)) {
    workstyles <- compute.workstyles.diff(df) # 0 - 4
    workvalues <- compute.workvalues.diff(df) # 0 - 4
    workcontext <- compute.workcontext.diff(df) # 0 -100
    knowledge <- compute.knowledge.diff(df) # 0 - 35
    skills <- compute.skills.diff(df) # 0 - 35
    abilities <- compute.abilities.diff(df) # 0 - 35
    works.activities <- compute.work.activities.diff(df) # 0 - 35

    x <- c(workstyles/4, workvalues/4, workcontext/100, knowledge/35, skills/35, abilities/35, works.activities/35)
    score  <- sum(x * w)/sum(w)
    return(score)
}


# prakash <- function(socA, <list-odatasets>) {

#     # Return df with nice columns
# }
