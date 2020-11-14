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
          box(selectInput("var","Plot Our Dichotomous Variables",
              choices = c("school","address","Pstatus","schoolsup", "famsup", "paid", "activities", "nursery", "higher", "romantic"), selected = "higher"),
            plotOutput("edaPlot")),
          box(selectInput("var2","Plot Our Ranked Variables",
                        choices = c("famrel", "freetime", "goout", "Dalc", "Walc", "health"), selected = "famrel"),
            plotOutput("edaPlot2")),
          box(selectInput("responses", "Summary of Semester grades and overall grade",
                        choices = c("G1","G2","G3")),
            plotOutput("sumPlot")),
          box(plotOutput("corPlot"))
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
           ) # Close fluid row
        ) # close data tab
    ) # close tab items 
  ) # close dash body
  ) # close dash page
) # close shiny

