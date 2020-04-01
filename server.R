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

# load data
crash <- read.csv("./aggregated_data(2).csv", header = TRUE)

# reformat dataframe
crash$date <- as.Date(crash$date, format= "%m/%d/%Y")
crash$time <- format(as.POSIXct(crash$time, format= "%H:%M:%S"), format = "%H:%M:%S")
crash <- crash[order(as.Date(crash$date)),]

shinyServer(function(input, output, session){
  output$bar_plot <- reactivePlot(function(){
    
    if (input$category == "None") {
      print("Figure not available")
    } else {
      tbl <- table(crash[[input$category]])
      myplot <- barplot(tbl, main = paste("Crash distribution by", as.character(input$category)), 
                        col="orange", ylab = "Count of crashes")
      print(myplot)
    }
  })
  
  output$crash_map <- renderLeaflet({
    crash <- crash[crash$date >= as.Date(input$dateRange[1]) & crash$date <= as.Date(input$dateRange[2]),]
    
    leaflet(data = crash) %>% 
      # maptypes: "MapQuestOpen.Aerial", "Stamen.TerrainBackground", "Esri.WorldImagery", "OpenStreetMap", "Stamen.Watercolor"
      addTiles() %>% # default: "OpenStreetMap"
      addCircleMarkers(~long, ~lat, radius = 0.05, label = ~as.character(accident_no)) %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
    })
  
  # change map style
  observe({
    crash <- crash[crash$date >= as.Date(input$dateRange[1]) & crash$date <= as.Date(input$dateRange[2]),]
    
    output$bar_plot <- reactivePlot(function(){
      if (input$category == "None") {
        print("Figure not available")
      } else {
        tbl <- table(crash[[input$category]])
        myplot <- barplot(tbl, main = paste("Crash distribution by", as.character(input$category)), 
                          col="orange", ylab = "Count of crashes")
        print(myplot)
      }
    })
    
    colorBy <- input$category
    
    # categorize data by colors
    if (colorBy == "None") {
      mycol <- c("No category")
      pal <- colorFactor("Orange", mycol)
    } else {
      mycol <- crash[[colorBy]]
      pal <- colorFactor("YlOrRd", mycol)
    }
    
    response <- input$cluster
    if (response == TRUE){    
      leafletProxy("crash_map", data = crash) %>%
        clearMarkers() %>%
        addMarkers(clusterOptions = markerClusterOptions(showCoverageOnHover = T,
                                                         zoomToBoundsOnClick = T,
                                                         spiderfyOnMaxZoom = T))
    } else {
      leafletProxy("crash_map", data = crash) %>%
        clearMarkerClusters() %>%
        
        addCircleMarkers(~long, ~lat, radius = 0.05,
                         label = ~paste("Acc_no:", as.character(accident_no), 
                                        "\nDate:", as.character(date),
                                        "\nPostcode:", as.character(postcode)),
                         labelOptions = labelOptions(clickable = TRUE, textsize = "13px"),
                         color = pal(mycol), opacity = 1, fillColor = pal(mycol), fillOpacity = 0.5) %>%
        addLegend("bottomleft", pal=pal, values=mycol, title=colorBy, layerId="colorLegend")
    }
  })
  
  ##### second tab: Custom Chart
  # plot accident by regions
  output$region_crash <- reactivePlot(function() {
    crash <- crash[crash$date >= as.Date(input$dateRange2[1]) & crash$date <= as.Date(input$dateRange2[2]),]
    crash <- crash[crash$region_name != " ",]
    
    myplot <- ggplot(crash, aes(x = region_name, fill=accident_type)) +
      geom_bar() +
      ggtitle("Crash distribution by regions") +
      xlab("Regions") +
      ylab("Count of crashes") +
      theme(axis.text.x=element_text(angle=90, hjust=1))
    print(myplot)
  })
  
  # plot accident by types
  output$acc_type <- reactivePlot(function() {
    crash <- crash[crash$date >= as.Date(input$dateRange2[1]) & crash$date <= as.Date(input$dateRange2[2]),]
    crash <- crash[crash$LGA_name == toupper(input$search),]
    
    idx <- tail(names(sort(table(crash$road_name))),10)
    crash <- subset(crash, road_name==idx)
    
    crash <- crash[crash$road_name != " ",]
    
    myplot <- ggplot(crash, aes(x=road_name, fill=accident_type)) +
      geom_bar() +
      ggtitle("Crash distribution by roads in a suburb") +
      theme(axis.text.x=element_text(angle=90, hjust=1))
    print(myplot)
  })
  
  # modify acc_type plot
  observe({
    crash <- crash[crash$date >= as.Date(input$dateRange2[1]) & crash$date <= as.Date(input$dateRange2[2]),]
    crash <- crash[crash$LGA_name == toupper(input$search),]
    
    output$acc_type <- reactivePlot(function() {
      idx <- tail(names(sort(table(crash$road_name))),10)
      crash <- subset(crash, road_name==idx)
      
      crash <- crash[crash$road_name != " ",]
      
      myplot <- ggplot(crash, aes(x = road_name, fill=accident_type)) +
        geom_bar() +
        ggtitle("Crash distribution by roads in a suburb") +
        xlab("Road name") +
        theme(axis.text.x=element_text(angle=90, hjust=1))
      print(myplot)
    })
  })
})