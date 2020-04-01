# FIT5147 - Data exploration and visualisation
# Data Visualization Project
# Phat Vinh Ho - 28423186
# Last modified: 4June 2018

library(leaflet)
library(shiny)
library(RColorBrewer)
library(ggplot2)
# install.packages("shinythemes")
library(shinythemes)
# install.packages("shinyWidgets")
library(shinyWidgets)

shinyUI(navbarPage(
  theme = shinytheme("superhero"), title = "Victoria's Road Crashes", id = "nav",
  
  # create first tab of the page
  tabPanel("Interactive Map", 
           sidebarLayout(
             sidebarPanel( # create sidebar tools
               dateRangeInput("dateRange", label = "Date range (yyyy-mm-dd): ", 
                              start="2015-01-01", end = "2017-12-31"),
                              
               selectInput("category", "Categorized by:", 
                           c("None", "Speed Zone" = "speed_zone", "Severity" = "severity",
                             "Fatality" = "no_persons_killed")),
               checkboxInput("cluster", "Cluster data", TRUE),
               plotOutput("bar_plot")
               ),
             
             mainPanel(
               leafletOutput("crash_map", height = "600")
               )
             )
           ),
  
  # create second tab
  tabPanel("Custom Chart", 
           sidebarLayout(
             sidebarPanel( # create sidebar tools
               searchInput(
                 inputId = "search", 
                 label = "Search a region name :", 
                 # placeholder = "Melbourne, Geelong, Monash, Frankston,...",
                 value = "Melbourne",
                 btnSearch = icon("search"), 
                 btnReset = icon("remove"), 
                 width = "100%"),
             
               dateRangeInput("dateRange2", label = "Date range (yyyy-mm-dd): ", start="2015-01-01", end = "2017-12-31")
               ),
             
              mainPanel(
                plotOutput("region_crash"),
                plotOutput("acc_type"))
             )
           )
  )
  )