
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(igraph)
library(dplyr)

initialize<-function(){
  onet <- lapply(list.files("data/O_NET/", full.names = TRUE), 
                 read.csv, fill = TRUE, header = TRUE, sep = '\t');
  names(onet) <- gsub(".txt", "", list.files("data/O_NET/"))
  graph.data <- onet$`Career Changers Matrix`[,-3]
  graph.data$O.NET.SOC.Code <- gsub("[.][0-9]{2}", "", graph.data$O.NET.SOC.Code)
  graph.data$Related.O.NET.SOC.Code <- gsub("[.][0-9]{2}", "", graph.data$Related.O.NET.SOC.Code)
  graph.data$O.NET.SOC.Code <- gsub("-", "", graph.data$O.NET.SOC.Code)
  graph.data$Related.O.NET.SOC.Code <- gsub("-", "", graph.data$Related.O.NET.SOC.Code)
  return(graph.data)
}

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
  graph.data<-initialize();
 
  
  

  output$distPlot <- renderPlot({

    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')

  })
  
  output$jobdist<-renderPlot({
    x <- sample(seq(from = 20, to = 50, by = 5), size = 50, replace = TRUE)
    bins <- seq(min(x), max(x), length.out = 10 + 1)
    
    # draw the histogram with the specified number of bins
    # hist(x, breaks = bins, col = 'darkgray', border = 'white')
    slices <- c(10, 12, 4, 16, 8)
    lbls <- c("Accountant", "Engineer", "Miner", "Teacher", "Sales Rep")
    pct <- round(slices/sum(slices)*100)
    lbls <- paste(lbls, pct) # add percents to labels
    lbls <- paste(lbls,"%",sep="") # ad % to labels
    pie(slices,labels = lbls, col=rainbow(length(lbls)),
        main="Job distribution") 
    
    #par(mfrow=c(2,2),mar=c(0,0,0,0), oma=c(0,0,0,0))
    #g <- watts.strogatz.game(1,20,3,0.4)
    #plot(g,layout=layout.fruchterman.reingold,margin=0)
    
    
  })

  output$jobgraph<-renderPlot({
    x <- sample(seq(from = 20, to = 50, by = 5), size = 50, replace = TRUE)
    bins <- seq(min(x), max(x), length.out = 10 + 1)

    par(mfrow=c(2,2),mar=c(0,0,0,0), oma=c(0,0,0,0))
    g <- watts.strogatz.game(1,20,3,0.4)
    # plot(g,layout=layout.fruchterman.reingold,margin=0)   
    plot_graph(graph.data)
  })
  
  
})
