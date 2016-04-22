# author: Ryan Brady
cps <- readRDS("data/processed/cps.RDS")

#' Get cps data series
#' @param series.name character case-sensitive part of series name
#' @param year.range numeric vector of years
#' @return period A for annual, M for monthly, Q for quarterly
#' @examples get.cps.series("PopulationLevel", year.range = 2014:2015, period = "Q")
get.cps.series <- function(series.name, year.range = 1900:2020, period = c("A", "M", "Q")) {
  series.ids <- cps$dict[grepl(series.name, series_title) & (periodicity_code %in% period), 
                         c("series_id", "series_title"), with = FALSE]
  merge(series.ids, cps$data[year %in% year.range])
}
