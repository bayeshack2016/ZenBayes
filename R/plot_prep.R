# Author: Ryan Brady
library(igraph)
library(dplyr)
bls.metro <- readRDS("data/processed/bls_metro.RDS")
bls.state <- readRDS("data/processed/bls_state.RDS")
bls.industry <- readRDS("data/processed/bls_industry.RDS")
mid.skill <- readRDS("data/processed/mid_skill.RDS")


### Functions to make plots.
## Functions to make graph plots
# Probably do this in the data preload:
graph.data <- read.csv("data/O_NET/Career Changers Matrix.txt", sep = '\t')
graph.data$O.NET.SOC.Code <- gsub("[.][0-9]{2}", "", graph.data$O.NET.SOC.Code)
graph.data$Related.O.NET.SOC.Code <- gsub("[.][0-9]{2}", "", graph.data$Related.O.NET.SOC.Code)
graph.data <- unique(graph.data[graph.data[,1] != graph.data[,2],1:2])
# Takes no time.
career.graph <- graph.data.frame(graph.data)


# Change these
size = 5
color = 1
label = NA

test.graph <- induced_subgraph(career.graph, neighborhood(career.graph, 1, "51-4022")[[1]])

plot(test.graph, 
     vertex.size = size,
     edge.width = 0.5,
     edge.arrow.size = 0.01,
     vertex.color = color,
     vertex.label = label)