setwd("../")


# bls.metro <- data.table(readRDS("data/processed/bls_metro.RDS"), key = "O.NET.SOC.Code")
# names(bls.metro)[names(bls.metro) == "O.NET.SOC.Code"] <- "SOC"
# bls.state <- data.table(readRDS("data/processed/bls_state.RDS"), key = "O.NET.SOC.Code")
# names(bls.state)[names(bls.state) == "O.NET.SOC.Code"] <- "SOC"
# bls.industry <- data.table(readRDS("data/processed/bls_industry.RDS"), key = "O.NET.SOC.Code")
# names(bls.industry)[names(bls.industry) == "O.NET.SOC.Code"] <- "SOC"
# bls.national <- data.table(readRDS("data/processed/bls_national.RDS"), key = "O.NET.SOC.Code")
# names(bls.national)[names(bls.national) == "O.NET.SOC.Code"] <- "SOC"
mid.skill <- readRDS("data/processed/mid_skill.RDS")
mid.skill$SOC <- substr(mid.skill$O.NET.SOC.Code, 1, 7)
mid.skill <- data.table(mid.skill, key = "SOC")
# onet <- readRDS("data/processed/onet_combined.RDS")
# onet <- data.table(onet, key = "SOC")
bls.project <- readRDS("data/processed/bls_project_gain_loss_sorted_titles.rds")
names(bls.project)[names(bls.project) == "SOC.Code"] <- "SOC"
bls.project <- data.table(bls.project, key = "SOC")
bls.project[, change := (employment.2024 - employment.2014)]
pressure.data <- readRDS("data/processed/pressure_output.RDS")
### Functions to make plots.
## Functions to make graph plots
# Probably do this in the data preload:
graph.data <- read.csv("data/O_NET/Career Changers Matrix.txt", sep = '\t')
graph.data$O.NET.SOC.Code <- gsub("[.][0-9]{2}", "", graph.data$O.NET.SOC.Code)
graph.data$Related.O.NET.SOC.Code <- gsub("[.][0-9]{2}", "", graph.data$Related.O.NET.SOC.Code)
graph.data <- unique(graph.data[graph.data[,1] != graph.data[,2],1:2])
# Takes no time.
career.graph <- graph.data.frame(graph.data)
V(career.graph)$change <- bls.project[ match(names(V(career.graph)), SOC), gain.loss.10yrs]
V(career.graph)$titles <- bls.project[ match(names(V(career.graph)), SOC), title]
V(career.graph)$size <- log(bls.project[ match(names(V(career.graph)), SOC), employment.2024])
V(career.graph)$color <- ifelse(V(career.graph)$change > 0, "forest green", "dark red")
edges.to.delete <- get.edges(career.graph, E(career.graph))
to.delete <- sign(as.numeric((V(career.graph)$change[edges.to.delete[,1]]))) == 
  sign(as.numeric(V(career.graph)$change[edges.to.delete[,2]])) |
  V(career.graph)$change[edges.to.delete[,1]] == 0|
  V(career.graph)$change[edges.to.delete[,2]] == 0
career.graph <- delete.edges(career.graph, edges = E(career.graph)[which(to.delete)])
setwd("shiny")