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
library(DT)
library(tidyverse)
library(corrplot)
library(rgl)
library(tree)
library(nlme)
library(caret)
library(ggplot2)
library(plotly)
library(ggcorrplot)

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
          fluidPage(
            h2("Exploratory Data Analysis"),
            checkboxInput("schoolFilter", h4("Explore by school?")),
            conditionalPanel(condition = ("input.schoolFilter==true"), 
                               selectizeInput("school", choices = c("GP", "MS"),selected = "GP",
                                              h5("Select School of Interest"))
              ),
            #sidebarMenu(
             #   menuItem("Boxplots", tabName = "boxplots"),
              #  menuItem("Histograms", tabName = "histograms")
              #),
              

            #dashboardBody(
             #   tabItems(
              #    tabItem(tabName = "boxplots",
              
                
         # selectizeInput("sex", "sex", selected = "F", choices = levels(as.factor(dat$sex))),
                  box(selectInput("var","Plot Our Dichotomous Variables",
                        choices = c("school","address","Pstatus","schoolsup", "famsup", "paid", "activities", "nursery", "higher", "romantic"), selected = "higher"),
                      plotOutput("edaPlot"),
                      downloadButton("downloadEdaPlot", "Download as png")),
                  box(selectInput("var2","Plot Our Ranked Variables",
                        choices = c("famrel", "freetime", "goout", "Dalc", "Walc", "health"), selected = "famrel"),
                      plotOutput("edaPlot2"),
                      downloadButton("downloadEdaPlot2", "Download as png")),
                  box(selectInput("responses", "Summary of Semester grades and overall grade",
                        choices = c("G1","G2","G3")),
                      plotOutput("sumPlot"),
                      downloadButton("downloadSumPlot", "Download as png")),
                  box(plotOutput("corPlot"),
                      downloadButton("downloadCorPlot", "Download as png"))
                  #),

                #)
         #)
         
          ) # close fluid page
         ), # close eda tab
        
        
        
        
        
        
        tabItem(tabName = "clustering",
                h2("Clustering our data"),
                pageWithSidebar(
                  headerPanel('K-means clustering'),
                  sidebarPanel(
                    selectInput('xcol', 'X Variable', names(datNum)),
                    selectInput('ycol', 'Y Variable', names(datNum),
                                selected=names(datNum)[[2]]),
                    numericInput('clusters', 'Cluster count', 3,
                                 min = 1, max = 9)
                  ),
                  mainPanel(
                    plotOutput('clusPlot'),
                    plotOutput('dendoPlot')
                  )
                )
                ), #close clustering tab
        
 
        
        tabItem(tabName = "modeling", 
              fluidPage(
                h2("Setup Some Models for our data"),
                box(selectizeInput("response", "Choose  a Response Variable", 
                               choices =  c(names(dat[31:33])), selected = "G3"),
                selectizeInput("allvar1", "Choose the First Predictor Variable", 
                               choices =  c(names(dat[1:30])), selected = "school"),
                selectizeInput("allvar2", "Choose the Second Predictor Variable", 
                               choices =  c(names(dat[1:30])), selected = "sex"),
                selectizeInput("allvar3", "Choose the Third Predictor Variable", 
                               choices =  c(names(dat[1:30])), selected = "higher"),
                selectizeInput("allvar4", "Choose the Fourth Predictor Variable", 
                               choices =  c(names(dat[1:30])), selected = "failures"),
                selectizeInput("allvar5", "Choose the Fifth Predictor Variable", 
                               choices =  c(names(dat[1:30])), selected = "goout")),
                box(selectizeInput("model", "Choose Type of Fit", 
                                   choices =  c("GLM","ClassificationTree", "RandomForest", "BaggedTree")),
                  verbatimTextOutput('modResults')),
                box(tableOutput('pred'))
                   ) #close fluid page
                ), # close modeling tab
        
        
        
        
        
        tabItem(tabName = "rawdat", 
          fluidPage(
                h2("Our Raw Data"),
                checkboxInput("schoolFilter2", h4("Subset by school?")),
                conditionalPanel(condition = ("input.schoolFilter2==true"), 
                                 selectizeInput("school2", choices = c("GP", "MS"),selected = "GP",
                                                h5("Select School of Interest"))),
            #   numericInput("maxrows", "Rows to show", 25),
            fluidRow(
              box(DT::dataTableOutput("datTable"),
                  downloadButton("downloadData", "Download as CSV"))
          )) # Close fluid row and page
        ) # close data tab
    ) # close tab items 
  ) # close dash body
  ) # close dash page
) # close shiny

