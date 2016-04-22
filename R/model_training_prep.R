library(reshape2)

#' Get training data from a bls metro data set.
#' @param metro character metro name. 'Phoenix' will get all with name phoenix in it (3, over time)
#' @param metro.data the data.frame containig the bls metro data.
#' @return list x data frame of individual occupational employments, y total employment
#' @examples get.training.data.metro("Phoenix", bls.metro)
get.training.data.metro <- function(metro, metro.data) {
  mdl.data <- subset(metro.data, !is.na(O.NET.SOC.Code) & 
                       grepl(metro, area_name, ignore.case = TRUE) &
                       substr(occ_code, 6,7) != "00" &
                       !is.na(tot_emp))
  mdl.data <- dcast(mdl.data, O.NET.SOC.Code ~ year, value.var = "tot_emp", fun.aggregate = sum)
  list(x = mdl.data, y = colSums(mdl.data[,-1]))
}
