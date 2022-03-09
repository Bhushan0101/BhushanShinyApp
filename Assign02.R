library(ggplot2)
library(shiny)
library(shinydashboard)
library(PerformanceAnalytics)

ui <- dashboardPage(
  dashboardHeader(title = 'Movie rating Dashboard'),
  dashboardSidebar(
    sidebarMenu(
    menuItem("Histogram ", tabName = "plot01", icon = icon("bolt",lib = "font-awesome")),
    menuItem("Box plot ", tabName = "plot02",icon = icon("bolt",lib = "font-awesome")),
    menuItem("Data Set", tabName = "tableraw",icon = icon("table",lib = "font-awesome")),
    menuItem("Contact Developer", tabName = "developer", icon = icon("envelope",lib = "font-awesome"))
    )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "plot01",
    fluidRow(
      box(title = "Histogram of Audience rating distribution",status = "primary",solidHeader = T,plotOutput("plot1")),
      box(title = "Filters", status = "warning", solidHeader = T,
          selectInput("genre", "Genre (a movie can have multiple genres)",
                      choices = levels(Data$Genre), selected = levels(Data$Genre)[1]),
          selectInput("year", "Select Year",
                      choices = c(2007,2008,2009,2010,2011), selected = 2007),
          wellPanel(paste(
            "A histogram is a bar graph-like representation of data that buckets a range of outcomes into",
            "columns along the x-axis.",
            "The y-axis represents the number count or percentage of occurrences in the data for each column and",
            "can be used to visualize data distributions"
          )
          
          )
    )
  )
  ),
  tabItem(tabName = "plot02",
  fluidRow(
    box(title = "Box Plot - Audience Rating and Critic Rating comparison",status = "primary",solidHeader = T, plotOutput("plot2")),
    box(title = "Filters", status = "warning", solidHeader = T,
        selectInput("genre2", "Genre (a movie can have multiple genres)",
                    choices = levels(Data$Genre), selected = levels(Data$Genre)[1]),
        selectInput("year2", "Select Year",
                    choices = c(2007,2008,2009,2010,2011), selected = 2007),
        wellPanel(
          paste(
            "box plot or boxplot (also known as box and whisker plot) is a type of", 
            "chart often used in explanatory data analysis. Box plots visually show", 
            "the distribution of numerical data and skewness through displaying the", 
            "data quartiles (or percentiles) and averages"
          )
        )
    )
  )
),
tabItem(tabName = "developer",
        fluidRow(
          wellPanel(
            paste(
              "This Dashboard is created by Bhushan Bist ( 21238658) of National University of ireland",
              "As a part of assignemnt, all data used in this dashboard for analysis belongs to the owner",
              "For anu queries please reach out at - b.bist1@nuigalway.ie"
            )
          )
        )),
tabItem(tabName = "tableraw",
        fluidRow(
        box(title = "Data Set Quick view",status = "primary",solidHeader = T,tableOutput("MovieData")),
          box(title = "Filters", status = "warning", solidHeader = T,
              selectInput("tablegenre", "Genre",
                      choices = levels(Data$Genre), selected = levels(Data$Genre)[1]),
              selectInput("yeartable", "Select Year",
                          choices = c(2007,2008,2009,2010,2011), selected = 2007)),
          wellPanel(
            paste("The Dateset used in this analysis downloaded from",
            "- https://sds-platform-private.s3-us-east-2.amazonaws.com/uploads/P2-Movie-Ratings.csv",
            "The purpose of this Dashboard to prove efficiency as an intermnediate developer in R dashboard building",
            "Data rights reserved with the owner and dashboard ownership reserved to developer ( Bhushan Bist )")
          )
          
          )
)
)
)
)

server <- function(input, output){
  output$plot1 <- renderPlot(
    {
      Data <- read.csv("MovieRatingData.csv", stringsAsFactors = T)
      FilteredData <- Data %>% filter(Genre == input$genre,
                                      Year.release == input$year)
      hist(FilteredData$Audienec.rating)
    })
    
    output$plot2 <- renderPlot(
      {
        Data <- read.csv("MovieRatingData.csv", stringsAsFactors = T)
        FilteredData <- Data %>% filter(Genre == input$genre2,
                                        Year.release == input$year2)
        boxplot(FilteredData[c(4,5)], horizontal=TRUE, main="Critic Ratings of selected Filters")
      
        
      })
    output$MovieData <- renderTable({
      FilterData <- Data %>% filter(Genre == input$tablegenre,
                                    Year.release == input$yeartable)
    })
}



shinyApp(ui, server)

# server <- function(input, output)
# shinyApp(server)


