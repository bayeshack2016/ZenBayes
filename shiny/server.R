
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(igraph)
library(dplyr)
library(ggplot2)
library(stringr)
source("../R/plot_funcs.R")
source("../R/bls_functions.R")

initialize<-function(){
  bls <- readRDS("../data/processed/bls_project_gain_loss_sorted_titles.rds")
  biggest.loss <- subset(bls, employment.2014 - employment.2024 > 10000)
  source("server_init.R")
  biggest.loss$abb.title = str_wrap( biggest.loss$title, width = 40)

  return(biggest.loss)

}

biggest.loss<-initialize();
setwd("../")


#---------------------------------------------------------------------------------
# Load datasets and files for scoring functions.
#---------------------------------------------------------------------------------
# This is a smaller dataset (~1.2 MB) that contains averaged data from ONET.
cat("Loading data/processed/onet_combined_averaged.RDS ... ")
onet.comb.avg <- readRDS("data/processed/onet_combined_averaged.RDS")
cat("done\n")

# Load the score.df for all the top 30 biggest losers (from 2014 to 2024)
cat("Loading data/processed/top-losers-score-df-list.rds ... ")
top.losers.score.df.list <- readRDS("data/processed/top-losers-score-df-list.rds")
cat("done\n")

# Read the file containing functions for scoring.
cat("Sourcing R/onet-skill-difference-functions.R ...")
source("R/onet-skill-difference-functions.R")
cat("done\n")

#---------------------------------------------------------------------------------
# Set the working directory correctly and print final message
#---------------------------------------------------------------------------------
setwd("shiny")
cat("\n---------------------------------------\n")
cat("\nFinished loading data and files. Ready!\n")
cat("\n---------------------------------------\n")





shinyServer(function(input, output) {

  ## One time graph initialization
  print(names(biggest.loss))

  output$overallplot <- renderPlot({
    biggest.loss$raw.losses <- biggest.loss$employment.2014 - biggest.loss$employment.2024
    biggest.loss.new<- biggest.loss[order(biggest.loss$raw.losses),]
    ggplot( biggest.loss.new[1:20,], aes(x = abb.title, y = raw.losses)) + geom_bar(stat = "identity",fill="steelblue") +
      coord_flip()  + ylab("Number of jobs lost") + xlab("") +
      theme(axis.text=element_text(size=12), axis.title.x=element_text(size=18))# + theme(axis.text.y = element_text(angle = 90, hjust = 1))
  })


  output$jobdist<-renderPlot({
   print(input$select)
   barplot(c(biggest.loss[as.numeric(input$select),]$employment.2014, biggest.loss[as.numeric(input$select),]$employment.2024),
            main = sprintf("Jobs lost =  %d",biggest.loss[as.numeric(input$select),]$employment.2014 - biggest.loss[as.numeric(input$select),]$employment.2024 ),names.arg = c("2014", "2024"))

  })

  output$jobgraph<-renderPlot({
    plot.subgraph(career.graph = career.graph, SOC = as.character(biggest.loss[as.numeric(input$select),]$SOC.Code))
    

  })
  
  output$jobgraphtable <- renderDataTable({
    
    table.subgraph(career.graph = career.graph, SOC = as.character(biggest.loss[as.numeric(input$select),]$SOC.Code))
  }, options = list(columnDefs = list( searchable = FALSE), searchable = FALSE, pageLength = 5))



  # Scoring Table
  #output$table <- renderDataTable(biggest.loss)
  output$table <- renderDataTable({
    cat("\n---------------------------------------\n")
    cat("Logging renderDataTable():")
    cat("\n---------------------------------------\n")
    cat(paste0("input$select = ", input$select, "\n"))
    cat("User selected:\n")
    cat(paste0("\tSOC.Code: ", biggest.loss$SOC.Code[as.numeric(input$select)], "\n"))
    cat(paste0("\tTitle: ", biggest.loss$title[as.numeric(input$select)], "\n"))

    # Obtain the output dataframe that contains scores against the user-selected job.
    score.df <- get.closest.df(as.character(biggest.loss$SOC.Code[as.numeric(input$select)]),
                               onet.comb.avg, top.losers.score.df.list)
    return(score.df)
  })



})
