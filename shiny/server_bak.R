
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(igraph)
library(dplyr)
library(data.table)
source("../R/plot_funcs.R")

initialize<-function(){
  bls <- readRDS("../data/processed/bls_project_gain_loss_sorted_titles.rds")
  biggest.loss <- subset(bls, employment.2014 - employment.2024 > 10000)
  
  return(biggest.loss)

}

biggest.loss<-initialize();

source("server_init.R")

shinyServer(function(input, output) {
 
  ## One time graph initialization   
  print(names(biggest.loss))
  output$jobdist<-renderPlot({
   print(input$select)
   barplot(c(biggest.loss[as.numeric(input$select),]$employment.2014, biggest.loss[as.numeric(input$select),]$employment.2024), 
            main = sprintf("Jobs lost =  %d",biggest.loss[as.numeric(input$select),]$employment.2014 - biggest.loss[as.numeric(input$select),]$employment.2024 ),names.arg = c("2014", "2024"))
    
  })

  output$jobgraph<-renderPlot({
    input$select
    plot.subgraph(career.graph = career.graph, SOC = as.character(biggest.loss[as.numeric(input$select),]$SOC.Code))
    
    
  })
  
  output$table <- renderDataTable(biggest.loss)
  
  
})
