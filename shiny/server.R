
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(igraph)
library(dplyr)

initialize<-function(){
  bls <- readRDS("../data/processed/bls_project_gain_loss_sorted_titles.rds")
  biggest.loss <- subset(bls, employment.2014 - employment.2024 > 10000)
  
  return(biggest.loss)

}
biggest.loss<-initialize();


plot_graph <-function(graph.data){
  career.graph <- graph.data.frame(graph.data)
  career.cluster <- cluster_spinglass(career.graph)
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
  
  plot(career.graph, 
  vertex.size = sizes,
  edge.arrow.size = 0.05,
  edge.width = 0.5,
  vertex.label = labs,
  vertex.color = membership(career.cluster))
  
  
}

shinyServer(function(input, output) {
 
  ## One time graph initialization   
  print(names(biggest.loss))
  output$jobdist<-renderPlot({
   print(input$select)
   barplot(c(biggest.loss[as.numeric(input$select),]$employment.2014, biggest.loss[as.numeric(input$select),]$employment.2024), 
            main = sprintf("Jobs lost =  %d",biggest.loss[as.numeric(input$select),]$employment.2014 - biggest.loss[as.numeric(input$select),]$employment.2024 ),names.arg = c("2014", "2024"))
    
  })

  output$jobgraph<-renderPlot({
    x <- sample(seq(from = 20, to = 50, by = 5), size = 50, replace = TRUE)
    bins <- seq(min(x), max(x), length.out = 10 + 1)

    par(mfrow=c(2,2),mar=c(0,0,0,0), oma=c(0,0,0,0))
    g <- watts.strogatz.game(1,20,3,0.4)
    # plot(g,layout=layout.fruchterman.reingold,margin=0)   
    plot_graph(graph.data)
  })
  
  output$table <- renderDataTable(biggest.loss)
  
  
})
