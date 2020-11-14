# Ariana Polanco
# Project 3 UI
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

ui <- dashboardPage(
    dashboardHeader(title = " Project 3: Students"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("About", tabName= "about"),
            menuItem("Data Exploration", tabName = "eda"),
            menuItem("Clustering", tabName = "clustering"),
            menuItem("Modeling", tabName = "modeling"),
            menuItem("Data", tabname = "data")
        )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "About") ,
       # h4("This project is about the student data set. This data set captures many socio-economic and personal variables to determine whether it affects the grades of students in math." )
       tabItem("data",
               numericInput("maxrows", "Rows to show", 25),
               verbatimTextOutput("datTable"),
               downloadButton("downloadCsv", "Download as CSV")
      )
    )
)
)
# Define UI for application that draws a histogram
#shinyUI(fluidPage(

    # Application title
 #   titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
  #  sidebarLayout(
   #     sidebarPanel(
     #       sliderInput("bins",
    #                    "Number of bins:",
      #                  min = 1,
       #                 max = 50,
        #                value = 30)
    #    ),

        # Show a plot of the generated distribution
      #  mainPanel(
         #   plotOutput("distPlot")
       # )
#    )
#))
