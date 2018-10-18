#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)

bakery <- read.csv('data.csv')

bakery_raw <- bakery

bakery$Date <- ymd(bakery$Date)

bakery$weekday <- as.factor(weekdays(bakery$Date))

choices <- as.vector(head(bakery$Item, 30))

hourToTimePeriod <- function(x) {
  if (x >= 7 && x < 10) {
    x <- '7am to 10am'
  } else if (x >= 10 && x < 13) {
    x <-'10am to 13pm'
  } else if (x >= 13 && x < 16) {
    x <- '13pm to 16pm'
  } else if (x >= 16 && x < 19) {
    x <- '16pm to 19pm'
  } else if (x >= 19 && x < 21) {
    x <- '19pm to 22pm'
  } else {
    x <- '> 22pm'
  }
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$dataTable <- renderDataTable(
    bakery_raw
  )  
  
  output$overviewPlot <- renderPlotly({
    # draw the histogram with the specified number of bins
    bakery_ranged_day <- bakery %>%
      filter(between(Date, as.Date(input$dateRange[1]),as.Date(input$dateRange[2])))
    
    display <- bakery_ranged_day %>%
      group_by(Date, Item) %>%
      summarize(TotalSales=length(Date))
      
    plot <- ggplot(display, aes(x=Date, y =TotalSales))
      
    if (input$overviewVariation == 'Bar chart') {
      plot <- plot + geom_col(aes(fill=factor(Item)))
    } else {
      plot <- plot + geom_line(aes(color=factor(Item)))
    }
    
    plot <- plot + theme_minimal() + theme(legend.title = element_blank(), axis.title = element_blank())
    
    if(!input$showLegend) {
      plot <- plot + theme(legend.position = 'none')
    }
    
    ggplotly(plot)
  })
  
  output$itemPlot <- renderPlotly({
    item <- bakery %>% 
      filter(between(Date, as.Date(input$dateRange[1]),as.Date(input$dateRange[2])))  %>%
      filter(Item %in% input$showItem) %>%
      group_by(Date, Item) %>% 
      summarise(Total = n())
    
    if (input$displayType == 'Separated chart') {
      plot <- ggplot(item, aes(x=Date, y=Total)) +
        geom_line(aes(col=Item)) +    
        facet_grid(Item ~ .)
    } else {
      plot <- ggplot(item, aes(x=Date, y=Total, group=Item)) +
        geom_line(aes(col=Item))        
    }
    
    plot <- plot + theme_minimal() + theme(legend.title = element_blank(), axis.title = element_blank())
    
    if(!input$showLegendItem) {
      plot <- plot + theme(legend.position = 'none')
    }
    
    ggplotly(plot)    
  })
  
  output$totalItemSold <- renderValueBox({
    bakery_by_day <- bakery %>% 
      filter(between(Date, as.Date(input$dateRange[1]),as.Date(input$dateRange[2])))  %>%
      filter(Item %in% input$showItem) %>%
      group_by(Date) %>% 
      summarise(Total=n())
    
    valueBox(
      sum(bakery_by_day$Total), "Total item sold",
      color = "blue"
    )
  })

  output$meanItemSold <- renderValueBox({
    bakery_by_day <- bakery %>% 
      filter(between(Date, as.Date(input$dateRange[1]),as.Date(input$dateRange[2])))  %>%
      filter(Item %in% input$showItem) %>%
      group_by(Date) %>% 
      summarize(Total=n())  
    
    valueBox(
      round(mean(bakery_by_day$Total), digits = 2), "Average item sold/day",
      color = "green"
    )
  })
  
  output$timePlot <- renderPlotly({
    time <- bakery %>%
      filter(between(Date, as.Date(input$dateRange[1]),as.Date(input$dateRange[2])))
    time$Time <- hms(time$Time)
    time$Hour <- hour(time$Time)
    time$TimePeriod <- mapply(time$Hour, FUN = hourToTimePeriod)
    
    time <- time %>%
      group_by(Date, TimePeriod) %>%
      summarize(sales=length(Date))
    
    time$weekday <- as.factor(weekdays(time$Date))
    
    time <- time %>%
      group_by(weekday, TimePeriod) %>%
      summarize(MeanSales=mean(sales))
    
    plot <- ggplot(time, aes(x = TimePeriod, y = MeanSales)) + 
      geom_col(aes(fill=MeanSales))+
      facet_grid(weekday ~ .)+
      theme_minimal() +
      scale_fill_gradient(low = 'red3', high = 'green3')
    
    plot <- plot + theme(legend.position = 'none', axis.title = element_blank())
    
    ggplotly(plot)
  })
  
  output$bestTime <- renderValueBox({
    time <- bakery %>%
      filter(between(Date, as.Date(input$dateRange[1]),as.Date(input$dateRange[2])))
    
    time$Time <- hms(time$Time)
    
    time$Hour <- hour(time$Time)
    
    time$TimePeriod <- mapply(time$Hour, FUN = hourToTimePeriod)
    
    time <- time %>%
      group_by(weekday, TimePeriod) %>%
      summarize(Total=length(Date)) %>%
      arrange(-Total)
    
    valueBox(
      time[1, 'TimePeriod'], "Best sales time", icon = icon("time"),
      color = "blue"
    )
  })
  
  output$bestDay <- renderValueBox({
    
    time <- bakery %>%
      filter(between(Date, as.Date(input$dateRange[1]),as.Date(input$dateRange[2])))
    
    time$Time <- hms(time$Time)
    
    time$Hour <- hour(time$Time)
    
    time$TimePeriod <- mapply(time$Hour, FUN = hourToTimePeriod)
    
    time <- time %>%
      group_by(weekday, TimePeriod) %>%
      summarize(Total=length(Date)) %>%
      arrange(-Total)
    
    valueBox(
      time[[1, 'weekday']], "Best day for sales",
      color = "yellow"
    )
  })
  
  output$popularItem <- renderInfoBox({
    bakery_ranged_day <- bakery %>%
      filter(between(Date, as.Date(input$dateRange[1]),as.Date(input$dateRange[2])))
    
    item.by.totalSales <- bakery_ranged_day %>%
      group_by(Item) %>%
      summarize(total=length(Item)) %>%
      arrange(-total)
    
    infoBox(
      title = "Most Popular Item", 
      value = item.by.totalSales[1,1], icon=icon("trophy"), 
      subtitle = "Item with the most sales",
      color = "yellow"
    )
  })
  
  output$totalSales <- renderInfoBox({
    
    bakery_ranged_day <- bakery %>%
      filter(between(Date, as.Date(input$dateRange[1]),as.Date(input$dateRange[2])))
    
    item.by.totalSales <- bakery_ranged_day %>%
      group_by(Date) %>%
      summarize(sales=length(Date))
    
    infoBox(
      title = "Average Sales per Day", 
      value = round(mean(item.by.totalSales$sales), digits = 2), icon=icon("money"), 
      subtitle = "average sales/day within period",
      color = "green"
    )
  })
  
  output$itemVariation <- renderInfoBox({
    
    bakery_ranged_day <- bakery %>%
      filter(between(Date, as.Date(input$dateRange[1]),as.Date(input$dateRange[2])))
    
    item.by.totalSales <- bakery_ranged_day %>%
      group_by(Item) %>%
      summarize(total=length(Item)) %>%
      arrange(-total)
    
    infoBox(
      title = "Total Item Variation", 
      value = nrow(item.by.totalSales), icon=icon("apple", class = NULL, lib = "glyphicon"), 
      subtitle = "Type item sold within period",
      color = "blue"
    )
  })
  
})
