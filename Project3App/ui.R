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

shinyUI(
  dashboardPage(
    dashboardHeader(title = " Project 3: Students"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("About", tabName= "about"),
            menuItem("Data Exploration", tabName = "eda"),
            menuItem("Clustering", tabName = "clustering"),
            menuItem("Modeling", tabName = "modeling"),
            menuItem("Data", tabName = "rawdat")
        )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "about",
                h2("About This Project"),
        h4("This project is about the student data set. This data set captures many socio-economic and personal variables to determine whether it affects the grades of students in math.")
        ),
        
        
        tabItem(tabName = "eda",
                 h2("Exploratory Data Analysis"),
        selectizeInput("sex", "sex", selected = "F", choices = levels(as.factor(dat$sex))),
        selectInput("var","Plot Our Y/N Variables",
                    choices = c("schoolsup", "famsup", "paid", "activities", "nursery", "higher", "romantic"), selected = "higher"),
        box(plotOutput("edaPlot")),
        box(tableOutput("edaTable")),
               ),
        tabItem(tabName = "clustering",
                h2("Clustering our data")
                ),
        tabItem(tabName = "modeling",
                h2("Setup Some Models for our data")
                ),
        tabItem(tabName = "rawdat",
                h2("Our Raw Data"),
            #   numericInput("maxrows", "Rows to show", 25),
          fluidRow(
              box(DT::dataTableOutput("datTable"),
                  downloadButton("downloadData", "Download as CSV"))
      )
    )
)
))
)

