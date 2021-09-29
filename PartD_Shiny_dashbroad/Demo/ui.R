library(shiny)  # shiny makes it easy to build interactive web applications with R
library(shinydashboard) # Create dashboard in the web applications
library(DT)     # Provides a R interface to manipulate data tables
library(ggplot2)    # Makes it easy to plot
library(plotly)     # Plotly creates interactive plots
library(leaflet)    # leaflet enables us to display map with geographical data



# Data preparation
data = readRDS("D:\\Warwick\\Courses\\Term 1\\Data Management\\Group Assignment\\PartD\\Demo\\final_result.rds")

data$Longitude = as.numeric(data$Longitude)
data$Latitude = as.numeric(data$Latitude)
# Change encoding to utf-8
Encoding(data$LocalAuthorityName) = "UTF-8"
Encoding(data$AddressLine) = "UTF-8"



dbHeader = dashboardHeader(title="UK Food Hygiene",
                            tags$li(a(onclick = "openTab('HygieneMap')",
                                      icon("home"),
                                      title = "Back to Home",
                                      style = "cursor: pointer;"),
                                      class = "dropdown",
                                    tags$script(HTML("
                                       var openTab = function(tabName){
                                       $('a', $('.sidebar')).each(function() {
                                       if(this.getAttribute('data-value') == tabName) {
                                       this.click()};});}"))),
                            tags$li(a(href = 'https://www.food.gov.uk/',
                                      icon("utensils"),
                                      title = "Food Hygiene Webpage"),
                                      class = "dropdown"))

ui = dashboardPage(
    dbHeader, 
    dashboardSidebar(  
    
        sidebarMenu(
            menuItem("Food Map", tabName = "HygieneMap", icon = icon("map-marked-alt")),
            menuItem("Plots", tabName = "plotdashboard", icon = icon("th")),
            menuItem("Controls", tabName = "Hygieneplotswithcontrols", icon = icon("filter")),
            menuItem("Data", tabName = "Hygienedata", icon = icon("dashboard"))
    
    )), 
    dashboardBody(  
        tabItems(
            # First tab content
            tabItem(tabName = "HygieneMap",
                    fluidRow(h1(style = "font-family:Times New Roman;padding-left:16px", "Food Map")), 
                    fluidRow(
                        column(
                            4,
                            selectInput('City', 'Select City', unique(data$LocalAuthorityName))
                        ),
                        column(
                            4,
                            selectInput('BusinessType', 'Select Business Type', choices = NULL)
                        ),
                        column(
                            4,
                            selectInput('RatingValue', 'Select Rating Value', choices = NULL)
                        ),
                    ),
                    fluidRow(column(width = 12, leafletOutput("mymap", "100%", height = "500"))),
                    fluidRow(column(width = 12, p(textOutput("desc")))),
                    fluidRow(a(style = "padding-left:25px;font-size:10;font-family:Lato;text-decoration:underline",
                               "Source: Food Standards Agency",
                               href = "https://www.food.gov.uk/uk-food-hygiene-rating-data-api"))
            ),
            
            # Second tab
            tabItem(tabName = "plotdashboard",
                    fluidRow(h1(style = "font-family:Times New Roman;padding-left:16px", "Plots")),
                    
                    fluidRow(h3(style = "font-family:Times New Roman;padding-left:16px", 
                                "The share of Scheme Type")),
                    fluidRow(
                        column(width = 12,
                               box(width = 12,
                                   plotlyOutput("myplot2")))),
                    tags$hr(style="border-color:black"),
                    fluidRow(h3(style = "font-family:Times New Roman;padding-left:16px", 
                                "The share of New Rating Pending")),
                    fluidRow(
                        column(width = 12,
                               box(width = 12,
                                   plotlyOutput("myplot4")))),
                    tags$hr(style="border-color:black"),
                    fluidRow(h3(style = "font-family:Times New Roman;padding-left:16px", 
                                "The Number of Missing values for Scores")),
                    fluidRow(
                        column(width = 12,
                               box(width = 12,
                                   plotlyOutput("myplot5")))),
                    fluidRow(a(style = "padding-left:25px;font-size:10;font-family:Lato;text-decoration:underline",
                               "Source: Food Standards Agency",
                               href = "https://www.food.gov.uk/uk-food-hygiene-rating-data-api"))
            ),
            
            # Third tab
            tabItem(tabName = "Hygieneplotswithcontrols",
                    fluidRow(h1(style = "font-family:Times New Roman;padding-left:16px", "Controls")),
                    fluidRow(h3(style = "font-family:Times New Roman;padding-left:16px", 
                                "The Share of Rating Values in each city")),
                    fluidRow(
                        column(
                            width = 5,
                            selectInput('SchemeForPlot', 'Select Scheme', 
                                        c("Overall", 
                                          ifelse(unique(data$SchemeType) == "FHIS", "FHIS (Scotland)",
                                                 "FHRS (England, Wales and Northern Ireland)")), 
                                        selectize = TRUE)
                        ),
                        column(
                            width = 5,
                            selectInput('CityForPlot', 'Select City', 
                                        choices = NULL, 
                                        selectize = TRUE)
                        ),
                        fluidRow(
                            column(width = 12,
                                   box(width = 12,
                                       plotOutput("myplot1"))))),
                    
                    tags$hr(style="border-color:black"),
                    fluidRow(h3(style = "font-family:Times New Roman;padding-left:16px", 
                                "Business Type Share")),
                    fluidRow(
                        column(
                            width = 4,
                            selectInput('CityForPlot2', 'Select City', 
                                        c("Overall", unique(data$LocalAuthorityName)), 
                                        selectize = TRUE)
                        ),
                        fluidRow(
                            column(width = 12,
                                   box(width = 12,
                                       plotOutput("myplot3"))))),
                    tags$hr(style="border-color:black"),
                    fluidRow(h3(style = "font-family:Times New Roman;padding-left:16px", 
                                "Scores")),
                    fluidRow(
                        column(
                            width = 4,
                            selectInput('ScorePlot', 'Select Score Type', 
                                        c(unique(colnames(data[,c(19:21)]))), 
                                        selectize = TRUE)
                        ),
                        fluidRow(
                            column(width = 12,
                                   box(width = 12,
                                       plotlyOutput("myplot6"))))),
                    fluidRow(a(style = "padding-left:25px;font-size:10;font-family:Lato;text-decoration:underline",
                               "Source: Food Standards Agency",
                               href = "https://www.food.gov.uk/uk-food-hygiene-rating-data-api"))
            ),
            
            # Last tab content
            tabItem(tabName = "Hygienedata",
                    fluidRow(h1(style = "font-family:Times New Roman;padding-left:16px", "Dataset")),
                    fluidRow(
                        column(width = 12, 
                               div(style = 'overflow-x: scroll', DT::dataTableOutput("mydata"),
                                   tags$style(HTML(".dataTables_wrapper .dataTables_length {
                                   float: left;}.dataTables_wrapper .dataTables_filter {
                                   float: right; text-align: left;}"))
                               )
                        )
                    ),
                    fluidRow(a(style = "padding-left:25px;font-size:10;font-family:Lato;text-decoration:underline",
                               "Source: Food Standards Agency",
                               href = "https://www.food.gov.uk/uk-food-hygiene-rating-data-api"))
            )
        )
    )
)