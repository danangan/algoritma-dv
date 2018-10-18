#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(lubridate)
library(plotly)
library(shinythemes)

bakery <- read.csv('data.csv')

choices <- as.vector(head(bakery$Item, 30))

bakery$Date <- ymd(bakery$Date)

# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    skin = "red",
    dashboardHeader(
      title="Sales Data"
    ),
    # Sidebar with a slider input for number of bins 
    dashboardSidebar(
      sidebarMenu(
        menuItem("Overview", tabName = "overview", icon=icon("home")),
        menuItem("Explore Item", tabName = "item", icon=icon("apple", class = NULL, lib = "glyphicon")),
        menuItem("Explore Time & Day", tabName = "time", icon=icon("time", class = NULL, lib = "glyphicon"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem("overview",
          fluidRow(
            infoBoxOutput("popularItem"),
            infoBoxOutput("totalSales"),
            infoBoxOutput("itemVariation")
          ),
          fluidRow(
            box(
              solidHeader=T,
              width=9,
              plotlyOutput("overviewPlot") 
            ),
            box(
              width=3,
              title='Chart Control',
              verticalLayout(
                dateRangeInput('dateRange', 'Date range', start = max(bakery$Date)-90, end = max(bakery$Date), max = max(bakery$Date), min = min(bakery$Date)),
                selectInput('overviewVariation', label='Variation', choices = c('Bar chart', 'Line chart')),
                checkboxInput('showLegend', label = 'Show legend')
              )
            ),
            box(
              title='About the data',
              solidHeader = T,
              width=3,
              p('The data is record of a bakery transaction.'),
              p('Data source: kaggle')
            )
          ),
          fluidRow(
            box(
              solidHeader=T,
              width=12,
              h2('Raw Data Preview'),
              br(),
              dataTableOutput('dataTable')
            )     
          )
        ),
        tabItem("item",
          fluidRow(
            valueBoxOutput("totalItemSold"),
            valueBoxOutput("meanItemSold")
          ),
          fluidRow(
            box(
              solidHeader=T,
              width=9,
              plotlyOutput("itemPlot", height = 450)
            ),
            box(
              solidHeader=T,
              width=3,
              title='Chart control',
              dateRangeInput('dateRangeItem', 'Date range', start = max(bakery$Date)-90, end = max(bakery$Date), max = max(bakery$Date), min = min(bakery$Date)),
              selectInput('showItem', 'Item', choices = choices, selected = c('Coffee'), multiple = T),
              radioButtons('displayType', 'Show as', choices = c('One chart', 'Separated chart')),
              checkboxInput('showLegendItem', label = 'Show legend')
            )
          )
        ),
        tabItem("time",
          fluidRow(
            valueBoxOutput("bestDay"),
            valueBoxOutput("bestTime")
          ),
          fluidRow(
            box(
              solidHeader=T,
              width=12,
              plotlyOutput("timePlot", height = 600) 
            )
          )
        )
      )
    )
  )
)
