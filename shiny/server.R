
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
  
  biggest.loss$abb.title = str_wrap( biggest.loss$title, width = 40)
  
  return(biggest.loss)

}

biggest.loss<-initialize();
setwd("../")
bls.metro <- data.table(readRDS("data/processed/bls_metro.RDS"), key = "O.NET.SOC.Code")
names(bls.metro)[names(bls.metro) == "O.NET.SOC.Code"] <- "SOC"
bls.state <- data.table(readRDS("data/processed/bls_state.RDS"), key = "O.NET.SOC.Code")
names(bls.state)[names(bls.state) == "O.NET.SOC.Code"] <- "SOC"
bls.industry <- data.table(readRDS("data/processed/bls_industry.RDS"), key = "O.NET.SOC.Code")
names(bls.industry)[names(bls.industry) == "O.NET.SOC.Code"] <- "SOC"
bls.national <- data.table(readRDS("data/processed/bls_national.RDS"), key = "O.NET.SOC.Code")
names(bls.national)[names(bls.national) == "O.NET.SOC.Code"] <- "SOC"
mid.skill <- readRDS("data/processed/mid_skill.RDS")
mid.skill$SOC <- substr(mid.skill$O.NET.SOC.Code, 1, 7)
mid.skill <- data.table(mid.skill, key = "SOC")
onet <- readRDS("data/processed/onet_combined.RDS")
onet <- data.table(onet, key = "SOC")
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
setwd("shiny")





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
    input$select
    plot.subgraph(career.graph = career.graph, SOC = biggest.loss[as.numeric(input$select),]$SOC.Code)
    
    
  })
  
  output$table <- renderDataTable(biggest.loss)
  
  
  
})
