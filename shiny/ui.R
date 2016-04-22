
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
#library(shinythemes)

shinyUI(fluidPage(theme = "darkly.css",
                  navbarPage("My Application",
                             tabPanel("Mid skill jobs distribution",  titlePanel(h1("Mid skill jobs")),   sidebarLayout(
                               sidebarPanel(    selectInput("select", label = h3("Select state"), 
                                                            choices = list("Texas" = 1, "Arizona" = 2,
                                                                           "Choice 3" = 3), selected = 1),
                                                selectInput("select", label = h3("Select year"), 
                                                            choices = list("2016" = 1, "2017" = 2,
                                                                           "Choice 3" = 3), selected = 1),
                                                numericInput("num", 
                                                             label = h3("Total number of jobs"), 
                                                             value = 100000)),
                               
                               
                               mainPanel(plotOutput("jobdist"))) ),
                             tabPanel("Job graph", titlePanel(h1("Job graph")),   sidebarLayout(
                               sidebarPanel(    selectInput("select", label = h3("Select state 2"), 
                                                            choices = list("Texas" = 1, "Arizona" = 2,
                                                                           "Choice 3" = 3), selected = 1),
                                                selectInput("select", label = h3("Select year"), 
                                                            choices = list("2016" = 1, "2017" = 2,
                                                                           "Choice 3" = 3), selected = 1),
                                                selectInput("select", label = h3("Select job title"), 
                                                            choices = list("2016" = 1, "2017" = 2,
                                                                           "Choice 3" = 3), selected = 1)),
                                                                             
                               
                               mainPanel(plotOutput("jobgraph"))) ),
                             tabPanel("Component 3")
                  )
                  
                  # Application title
                  
                  
                  
                  
))
