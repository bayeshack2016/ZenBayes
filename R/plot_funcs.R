diff.or.na <- function(x, y){
  diff <- x - y
  if(length(diff) == 0) {
    NA
  }
  else diff
}


get.scale <- function(type, 
                      .state = NULL, 
                      .industry = NULL, 
                      .metro = NULL, 
                      .year = NULL, 
                      onet.value = NULL, 
                      onet.scale = "LV"){
  # get correct data subset
  if(type == "skill") {
    data <- onet[grepl(onet.value, Element.Name, ignore.case = TRUE) | grepl(onet.value, Element.ID, ignore.case = TRUE)]
    data <- onet[Element.ID== Element.ID[1]]
    data <- data[Scale.ID == onet.scale] %>% 
      group_by(SOC) %>%
      summarize(value = Data.Value[which.max(Date)])
  }
  else if(!is.null(.state)){
    data <- bls.state[grepl(paste0("^", .state), state, ignore.case = TRUE)]
  }
  else if(!is.null(.industry)){
    data <- bls.industry[grepl(.industry, industry, ignore.case = TRUE)]
  }
  else if(!is.null(.metro)){
    data <- bls.state[grepl(.metro, area_name, ignore.case = TRUE)]
  }
  else{
    data <- bls.national
  }
  

  if(type == "employment"){
    data <- data[year == .year] %>% group_by(SOC) %>% 
      summarize(value = tot_emp)
  }
  if(type == "employment change"){
    data <- data[year %in% (.year - (0:1))] %>% group_by(SOC) %>% 
      summarize(value = diff.or.na(sum(tot_emp[year == .year], na.rm = TRUE), sum(tot_emp[year == .year - 1], na.rm = TRUE)))
    }
  if(type == "employment change %"){
    data <- data[year %in% (.year - (0:1))] %>% group_by(SOC) %>% 
      summarize(value = diff.or.na(sum(tot_emp[year == .year], na.rm = TRUE), sum(tot_emp[year == .year - 1], na.rm = TRUE)) / sum(tot_emp[year == .year]))
  }
  if(type == "salary"){
    data <- data[year == .year] %>% group_by(SOC) %>% 
      summarize(value = annual_wages_median)
  }
  if(type == "salary change"){
    data <- data[year %in% (.year - (0:1))]  %>% group_by(SOC) %>% 
      summarize(value = diff.or.na(mean(annual_wages_median[year == .year], na.rm = TRUE), mean(annual_wages_median[year == .year - 1], na.rm = TRUE)) / sum(tot_emp[year == .year]))
  }
  if(type == "salary change %"){
    data <- data[year %in% (.year - (0:1))]  %>% group_by(SOC) %>% 
      summarize(value = diff.or.na(mean(annual_wages_median[year == .year], na.rm = TRUE), mean(annual_wages_median[year == .year - 1], na.rm = TRUE)) / sum(tot_emp[year == .year]))
  }
  return(data)
}

make.subgraph <- function(career.graph, SOC, distance = 1){
  SOC <- SOC[SOC %in% names(V(career.graph))]
  if(length(SOC) == 1) {
    induced_subgraph(career.graph, v = neighborhood(career.graph, order = distance, nodes = SOC)[[1]])
  }
  else {
    induced_subgraph(career.graph, v = SOC)
  }
}

plot.subgraph <- function(...) {
  subgraph <- make.subgraph(...)
  plot(subgraph, 
       edge.width = 1,
       edge.arrow.size = 0.1,
       vertex.label.dist = 1)
}

table.subgraph <- function(...) {
  subgraph <- make.subgraph(...)
  data.frame(V(subgraph)$names, V(subgraph)$titles)
}

make.subgraph <- function(career.graph, SOC, distance = 1){
  SOC <- SOC[SOC %in% names(V(career.graph))]
  if(length(SOC) == 1) {
    induced_subgraph(career.graph, v = neighborhood(career.graph, order = distance, nodes = SOC)[[1]])
  }
  else {
    induced_subgraph(career.graph, v = SOC)
  }
}

