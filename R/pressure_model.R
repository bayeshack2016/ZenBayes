# occ.data <- occ.data %>% group_by(SOC) %>% summarize(title = Title[1], desc = Description[1])


V(career.graph)$change <- bls.project[match(names(V(career.graph)), SOC) , change]
V(career.graph)$capacity <- bls.project[match(names(V(career.graph)), SOC) , employment.2024]
V(career.graph)$label <- bls.project[match(names(V(career.graph)), SOC) , title]

pressure.mdl <- get.edges(career.graph, E(career.graph))
pressure.mdl <- apply(pressure.mdl, c(1,2), function(x) get.vertex.attribute(career.graph, name = "change", index = x))
pressure.mdl[,1] <- ifelse(pressure.mdl[,2] < 0, 0, pressure.mdl[,1])
pressure.mdl[,2] <- ifelse(pressure.mdl[,1] > 0, 0, pressure.mdl[,2])
pressure.mdl[,1] <- pmin(pressure.mdl[,1], 0)
pressure.mdl[,2] <- pmax(pressure.mdl[,2], 0)
pressure.mdl <- apply(pressure.mdl, 1, diff)
E(career.graph)$weight <- pressure.mdl
career.graph <- delete_edges(career.graph, edges = E(career.graph)[(E(career.graph)$weight == 0) | is.na(E(career.graph)$weight)])
career.graph <- delete.vertices(career.graph, which(is.na(V(career.graph)$change) | V(career.graph)$change == 0))
career.graph <- delete.vertices(career.graph, which(degree(career.graph) < 1))

edges <- get.edges(career.graph, E(career.graph))
vertex.sizes <- V(career.graph)$change
pressure.mdl <- data.table(edges)
names(pressure.mdl) <- c("from", "to")
pressure.mdl[, from.capacity := V(career.graph)$capacity[from]]
pressure.mdl[, to.capacity := V(career.graph)$capacity[to]]
pressure.mdl[, from.value := - V(career.graph)$change[from] + from.capacity]
pressure.mdl[, to.value := -V(career.graph)$change[to] + to.capacity]
pressure.mdl[, outflows := 0]
pressure.mdl[, resistance := .resistance]


sizes <- pressure.mdl[, from.value[1], by = "from"]
names(sizes) <- c("to", "V1")
sizes <- rbind(sizes, pressure.mdl[, to.value[1], by = "to"])
sizes$iter = 0
pressure = data.table(to = 0, V1 = 0, iter = 0)

for(i in 1:1000){
  pressure.mdl <- pressure.mdl %>% group_by(from) %>% mutate(pressure.out = (from.value / from.capacity))
  pressure.mdl <- pressure.mdl %>% group_by(to) %>% mutate(pressure.in = (to.value / to.capacity))
  pressure.mdl <- pressure.mdl[, force := ifelse(pressure.out > 1 & pressure.in < 1, 100 * (pressure.out - pressure.in) / resistance, 0)]
  pressure.mdl <- pressure.mdl[, outflows := outflows + force]
  pressure.mdl <- pressure.mdl[, from.value := from.value - sum(force), by = "from"]
  pressure.mdl <- pressure.mdl[, to.value := to.value + sum(force), by = "to"]
  print(sum(pressure.mdl$force))
  sizes.new <- pressure.mdl[, from.value[1], by = "from"]
  names(sizes.new) <- c("to", "V1")
  sizes.new <- rbind(sizes.new, pressure.mdl[, to.value[1], by = "to"])
  pressure.new <- pressure.mdl[, pmax(1, pressure.out[1]), by = "from"]
  names(pressure.new) <- c("to", "V1")
  pressure.new <- rbind(pressure.new, pressure.mdl[, pmin(1, pressure.in[1]), by = "to"])
  sizes.new$iter = i
  pressure.new$iter = i
  sizes <- rbind(sizes, sizes.new)
  pressure <- rbind(pressure, pressure.new)
}

pressure.mdl <- pressure.mdl[,unemployment := pmax(0, from.value - from.capacity)]
pressure.mdl <- pressure.mdl[, from.SOC:=names(V(career.graph))[from]]
pressure.mdl <- pressure.mdl[, to.SOC:=names(V(career.graph))[to]]
pressure.mdl <- merge(pressure.mdl, occ.data[,list(SOC, title)], by.x = "from.SOC", by.y = "SOC")
setnames(pressure.mdl, old = "title", new = "from.title")
pressure.mdl <- merge(pressure.mdl, occ.data[,list(SOC, title)], by.x = "to.SOC", by.y = "SOC")
setnames(pressure.mdl, old = "title", new = "to.title")
pressure.export <- pressure.mdl[, list(from.SOC, to.SOC, from.title, to.title, outflows, unemployment)]
saveRDS(pressure.export, "data/processed/pressure_output.RDS")

unemployment <- pressure.mdl[, list(from, unemployment)]
unemployment <- unique(unemployment)
unemployment <- unemployment[unemployment > 0]
unemployment <- unemployment[, SOC:=names(V(career.graph))[from]]



system("convert -delay 1 animation/*.png bookkeeper.gif")



sizes <- pressure.mdl[, from.value[1], by = "from"]
names(sizes) <- c("to", "V1")
sizes <- rbind(sizes, pressure.mdl[, to.value[1], by = "to"])

pressure <- pressure.mdl[, pmax(1, pressure.out[1]), by = "from"]
names(pressure) <- c("to", "V1")
pressure <- rbind(pressure, pressure.mdl[, pmin(1, pressure.in[1]), by = "to"])


v.sizes <- sizes[, V1[1], by = to] 
v.sizes <- v.sizes[order(to),]
v.pressure <- pressure[, V1[1], by = to] 
v.pressure <- v.pressure[order(to)[-1],]
V(career.graph)$size <- pmax(0, v.sizes[order(to), V1])
V(career.graph)$pressure <- v.pressure[order(to), V1]
data.subgraph <-induced_subgraph(career.graph, neighborhood(career.graph, 1, 262)[[1]])
#pressure.mdl.layout <- layout_(pressure.mdl.graph, with_fr())
# pressure.mdl.labels <- ifelse(1:26 %in% order(V(data.subgraph)$size, decreasing = T)[1:5], gsub("^(.{1,30} ).*", "\\1", V(data.subgraph)$label), NA)



plot(data.subgraph, 
     vertex.size = sqrt(V(data.subgraph)$size)/ 70,
     vertex.color = rgb(pmin(V(data.subgraph)$pressure, 1),
                        pmin(1, 2 - V(data.subgraph)$pressure),
                        1 - abs(1 - V(data.subgraph)$pressure)),
     edge.arrow.size = 0.1,
     vertex.label = V(data.subgraph)$label
)
