
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

bls <- readRDS("/Users/prakash/bayeshack2016/data/processed/bls_project_gain_loss_sorted_titles.rds")
biggest.loss <- subset(bls, employment.2014 - employment.2024 > 10000)
titles<-lapply(biggest.loss$SOC.Code, get.titles, 
               onet.data = get.onet.data.table("/Users/prakash/bayeshack2016/data/O_NET/Occupation Data.txt"));
choice.list <-seq(1, length(titles))
names(choice.list) <- titles

shinyUI(fluidPage(theme = "darkly.css",
                  navbarPage("My Application",
                             tabPanel("Jobs that are losing",  titlePanel(h1("Jobs with the highest losses")),   sidebarLayout(
                               sidebarPanel(    selectInput("select", label = h3("Select job title"), 
                                                            choices = choice.list, selected = 1)
                               ),
                               
                               
                               mainPanel( tabsetPanel(
                                 tabPanel("Job loss", plotOutput("jobdist")),
                                 tabPanel("Job to move to", dataTableOutput("table"))
                                 )) 
                               
                             )
                             
                             
                             
                             
                             
                             ))))
                  