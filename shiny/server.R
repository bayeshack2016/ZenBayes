
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(igraph)

shinyServer(function(input, output) {

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
    plot(g,layout=layout.fruchterman.reingold,margin=0)   
    
  })
  
  
})
