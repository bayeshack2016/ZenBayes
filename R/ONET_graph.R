library(igraph)
library(dplyr)
attach(onet)

graph.data <- onet$`Career Changers Matrix`[,-3]
graph.data$O.NET.SOC.Code <- gsub("[.][0-9]{2}", "", graph.data$O.NET.SOC.Code)
graph.data$Related.O.NET.SOC.Code <- gsub("[.][0-9]{2}", "", graph.data$Related.O.NET.SOC.Code)
graph.data$O.NET.SOC.Code <- gsub("-", "", graph.data$O.NET.SOC.Code)
graph.data$Related.O.NET.SOC.Code <- gsub("-", "", graph.data$Related.O.NET.SOC.Code)
graph.data <- graph.data[graph.data[,1] != graph.data[,2],]
career.graph <- graph.data.frame(graph.data)
career.cluster <- cluster_spinglass(career.graph)

# Do one label per group
to.lab <- data.frame(cluster = career.cluster$membership, 
                     label = career.cluster$names,
                     size = sizes) %>% 
  group_by(cluster) %>% 
  summarize(label = label[which.max(size)])
labs <- names(V(career.graph))
labs[!labs %in% to.lab$label] <- NA
lab.tab <- `Occupation Data`[,-3]
lab.tab[,1]  <- gsub("[.][0-9]{2}", "", lab.tab[,1] )
lab.tab[,1] <- gsub("-", "", lab.tab[,1])
labs <- as.character(lab.tab[match(labs, lab.tab[,1]),2])

# Size by logscale employment
sizes <- bls.oc.employment[match(names(V(career.graph)), bls.oc.employment[,1]),4]
sizes <- sizes / max(sizes, na.rm=T)
sizes <- log(sizes) + 11
sizes[is.na(sizes)] <- 1

png("plots/career_mobility.png", width = 800, height = 800)
plot(career.graph, 
     vertex.size = sizes,
     edge.arrow.size = 0.05,
     edge.width = 0.5,
     vertex.label = labs,
     vertex.color = membership(career.cluster))
dev.off()

subnames <- names(which(membership(career.cluster) == 6))
labs <- as.character(lab.tab[match(subnames, lab.tab[,1]),2])
career.subgraphs <- induced_subgraph(career.graph, subnames)
plot(career.subgraphs,
     vertex.color = 1,
     vertex.size = sizes[membership(career.cluster) == 6],
     edge.arrow.size = 0.05,
     edge.width = 0.5,
     vertex.label = labs)


sizes <- bls.oc.employment[match(names(V(career.graph)), bls.oc.employment[,1]),4]

to.lab <- data.frame(cluster = career.cluster$membership, 
                     label = career.cluster$names) %>% 
  group_by(cluster) %>% 
  summarize(label = sample(label, 1))
labs <- names(V(career.graph))
labs[!labs %in% to.lab$label] <- NA
lab.tab <- `Occupation Data`[,-3]
lab.tab[,1] <- gsub("-.*", "", lab.tab[,1])
labs <- as.character(lab.tab[match(labs, lab.tab[,1]),2])
