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
    if (nrow(df.subset) == 0) return(as.numeric(NA))
    expect_equal(unique(df.subset$Scale.ID), "IM")
    return(mean(df.subset$BminusA[df.subset$BminusA >= 0]))
}

compute.workvalues.diff <- function(df) {
    df.subset <- subset(df, domain == "WorkValues")
    library(testthat)
    expect_true(all(unique(df.subset$Scale.ID) %in% c("EN", "EX")))
    return(mean(df.subset$BminusA[df.subset$BminusA >= 0]))
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
    return(mean(df.subset$BminusA[df.subset$BminusA >= 0]))
}

compute.imp.lvl.diff <- function(df, this.domain) {
    library(testthat)
    expect_false("this.domain" %in% names(df))
    df.subset <- subset(df, domain == this.domain)
    expect_true(all(unique(df.subset$Scale.ID) %in% c("IM", "LV")))
    expect_equal(sum(df.subset$Scale.ID == "IM"), sum(df.subset$Scale.ID == "LV"))

    # Quick return
    if (nrow(df.subset) == 0) return(NA)

    # Unmelt the dataframes for each soc codes
    dfA <- reshape2::dcast(df.subset, Element.Name ~ Scale.ID, value.var = "mean.value.A")
    dfB <- reshape2::dcast(df.subset, Element.Name ~ Scale.ID, value.var = "mean.value.B")
    BminusA <- merge(dfA, dfB, by = "Element.Name", suffixes = c(".A", ".B"))

    # Compute the score as (IM * LV)_B - (IM * LV)_A
    BminusA$IM.LV.A <- BminusA$IM.A * BminusA$LV.A
    BminusA$IM.LV.B <- BminusA$IM.B * BminusA$LV.B
    BminusA$IM.LV.B.minus.IM.LV.A <- BminusA$IM.LV.B - BminusA$IM.LV.A
    return(mean(BminusA$IM.LV.B.minus.IM.LV.A[BminusA$IM.LV.B.minus.IM.LV.A >= 0]))
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

    # Compute the mean treating for NAs
    not.na.logical <- !is.na(x)
    score <- sum(x[not.na.logical] * w[not.na.logical])/sum(w[not.na.logical])
    if (length(score) == 0) {
        return(NA)
    } else {
        return(score)
    }
}

get.all.soc.codes <- function(onet) sort(unique(onet$SOC))

get.score.df <- function(socA, onet) {
    soc.all <- get.all.soc.codes(onet)
    socB.vec <- setdiff(soc.all, socA)

    output.list <- lapply(socB.vec, function(socB) {
        # print(socB)
        df <- get.BminusA(socA, socB, onet)
        c(compute.workstyles.diff(df), compute.workvalues.diff(df),
          compute.workcontext.diff(df), compute.knowledge.diff(df),
          compute.skills.diff(df), compute.abilities.diff(df),
          compute.work.activities.diff(df), compute.score(df))
    })
    names(output.list) <- socB.vec

    # Return df with nice columns
    output.df <- plyr::ldply(output.list, .id = "soc")
    names(output.df) <- c("soc", "workstyles", "workvalues", "workcontext",
                          "knowledge", "skills", "abilities",
                          "work.activities", "score")
    output.df <- dplyr::arrange(output.df, dplyr::desc(score))
    output.df$soc <- as.character(output.df$soc)
    return(output.df)
}

get.title.df <- function(onet, path.to.occ = "../data/O_NET/Occupation Data.txt") {
    # Read the data file as a data table
    library(data.table)
    occ.dt <- get.onet.data.table(path.to.occ)

    # Get the titles
    soc.vec <- get.all.soc.codes(onet)
    title.vec <- vapply(soc.vec, get.titles, "example output", occ.dt)

    # Make a dataframe and return
    library(testthat)
    expect_identical(soc.vec, names(title.vec))
    names(title.vec) <- NULL
    return(data.frame(soc = soc.vec, title = title.vec, stringsAsFactors = FALSE))
}

add.titles.to.score.df <- function(score.df, onet, path.to.occ = "../data/O_NET/Occupation Data.txt") {
    title.df <- get.title.df(onet, path.to.occ)

    library(testthat)
    expect_false("title" %in% names(score.df))

    # Merge and return
    new.score.df <- base::merge(score.df, title.df, by = "soc", all.x = TRUE, all.y = FALSE)
    expect_equal(nrow(score.df), nrow(new.score.df))
    expect_false(any(is.na(new.score.df$title)))
    new.score.df <- dplyr::arrange(new.score.df, dplyr::desc(score))
    return(new.score.df)
}

get.closest.df <- function(socA, onet, score.df.list = NULL) {
    if (!is.null(score.df.list) && (as.character(socA) %in% names(score.df.list))) {
        # If score.df.list is provided and it contains the necessary score.df dataframe,
        # we simply use it.
        score.df <- score.df.list[socA]
    } else {
        # If we are here, we need to generate the score.df dataframe ourselves.
        # No problem, except that this will take ~2 minutes.
        score.df <- get.score.df(socA, onet)
        score.df <- add.titles.to.score.df(score.df, onet)
    }

    # Sort by increasing values of score.
    # Score values should be between 0 and 1.
    # Lower score value means that the corresponding row of score.df is more closely
    # related to the socA.
    score.df <- dplyr::arrange(score.df, score)

    # Rename the columns so that they are fancy and have the correct capitalization.
    replace.vec <- c("title" = "Title", "score" = "Total Score", "skills" = "Skills Score",
                     "knowledge" = "Knowledge Score", "abilities" = "Abilities",
                     "workstyles" = "Work Styles", "workvalues" = "Work Values",
                     "workcontext" = "Work Context", "works.activities" = "Work Activities")
    score.df <- plyr::rename(score.df, replace = replace.vec)
    score.df <- score.df[replace.vec]
    return(score.df)
}

