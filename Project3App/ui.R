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
library(caret)
library(ggplot2)
library(plotly)
library(ggcorrplot)
library(shinyWidgets)

shinyUI(
  dashboardPage(
    dashboardHeader(title = "Project 3: Students"),
    dashboardSidebar(
      # create tabs
        sidebarMenu(
            menuItem("About", tabName= "about"),
            menuItem("Data Exploration", tabName = "eda"),
            menuItem("Principal Component Analysis", tabName = "pca"),
            menuItem("Modeling", tabName = "modeling"),
            menuItem("Data", tabName = "rawdat")
        )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "about",
                h2("About This Project"),
               
        h4("This project is about the", 
        a(href = "https://archive.ics.uci.edu/ml/machine-learning-databases/00320/","student performance data set."), 
           "This data captures many socio-economic and personal variables to 
           determine whether it affects the grades of students in math and portuguese. 
           The data was separated by subject and I decided to analyze the math subject.
           Grades were collected for each student for the first period, second period, and final grade.
           In addition to the this, 30 other characteristics were captured. 
           The below at the bottom of the page shows all of the variables and their descriptions.
           I have created this app to be able to explore the data and make some predictions."),
        br(),
        h3("How to navigate this app"),
        h4("The", tags$b("Data Exploration"), "tab allows you to see the summaries of the different 
           variables up against the overall grade response and the grade summaries for each school.
           These summaries can be saved to your computer!
           It also shows the correlations of our numeric variables. 
           This interactive correlation plot allows you to view additional details when hovering over it."),
        br(),
        h4("The", tags$b("Principal Component Analysis"),"tab allows you to choose the variables to create principal components. 
           These are then displayed nicely in a biplot."),
        br(),
        h4("The", tags$b("Modeling"), "tab allows you to choose the response variable and the predictor 
           variables to create a model formula. Then you can select the type of model you are interested in:
           a generalized linear model, classification tree, random forrest, and a bagged tree.
           The results for the selected method will be displayed. Using the same model, 
           you can select the desired values for your predictor variables and a prediction will be displayed."),
        br(),
        h4("Lastly, the", tags$b("Data"), "tab allows you to look at the raw data and subset by the school.
           This data can also be downloaded to your computer."),
        h2("Information about the variables"),
        dataTableOutput("infoTab"),
        h4("For more information about this data it can be found at", 
           a(href = "https://archive.ics.uci.edu/ml/datasets/Student+Performance", "UCI machine learning repository")),
  
        ), # close about tab

        
        tabItem(tabName = "eda", 
          fluidPage(
            h2("Exploratory Data Analysis"),
            checkboxInput("schoolFilter", h4("Explore by school?")),
            conditionalPanel(condition = ("input.schoolFilter==true"), 
                               selectizeInput("school", choices = c("GP", "MS"),selected = "GP",
                                              h5("Select School of Interest"))
              ),

                  box(selectInput("var","Plot Our Dichotomous Variables",
                        choices = c("school","address","Pstatus","schoolsup", "famsup", "paid", "activities", "nursery", "higher", "romantic"), selected = "higher"),
                      plotOutput("edaPlot"),
                      downloadBttn("downloadEdaPlot", "Download as png")),
                  box(selectInput("var2","Plot Our Ranked Variables",
                        choices = c("famrel", "freetime", "goout", "Dalc", "Walc", "health"), selected = "famrel"),
                      plotOutput("edaPlot2"),
                      downloadBttn("downloadEdaPlot2", "Download as png")),
                  box(checkboxGroupInput("inCheckboxGroup", "Select the Response Variables to Summarize",
                                         c("G1","G2","G3"),
                                         selected = "G3"),
                      tableOutput("respTab"),
                      selectInput("inSelect", "Summary of Semester grades and overall grade",
                                  choices = c("G1","G2","G3")),
                      plotOutput("sumPlot"),
                      downloadBttn("downloadSumPlot", "Download as png")),
                    box(fluidRow(h4("Correlation Plot for Numeric Variables"),
                                 h6("Hover over points to get more details!"),
                      plotlyOutput("corPlot") 
                      ) # close fluid row
                     ) # close box
          ) # close fluid page
         ), # close eda tab
        
        
        tabItem(tabName = "pca",
                h2("Principal Component Analysis"),
                  box(
                    selectizeInput("pcVars", "Choose the Variables", 
                                   choices =  c(names(datNum[1:12])), 
                                   selected = c("failures","goout","studytime", "freetime"), 
                                   multiple = T)
                  ), # close box
                  mainPanel(
                    verbatimTextOutput('pcOut'),
                    plotOutput('biPlot'),
                    dataTableOutput('pcaTab')
                  )# close mainpanel
                ), # close clustering tab
        

        
        tabItem(tabName = "modeling", 
          fluidPage(
            panel(
             h2("Setup Some Models for our data"),
             # choose between our 3 response variables
             box(selectizeInput("response", "Choose  a Response Variable", 
                               choices =  c(names(dat[31:33])), selected = "G3"),
                 # user to choose the predictor variables
                 selectizeInput("allvar1", "Choose the Predictor Variables", 
                               choices =  c(names(dat[1:30])), 
                               selected = c("school","sex","higher", "failures","goout"), 
                               multiple = T), # select as many as desired
                 h3("Selected Model"),
                 uiOutput('modForm')
                ), # close box
             box(selectizeInput("model", "Choose Method", 
                                   choices =  c("GLM","ClassificationTree", "RandomForest", "BaggedTree")),
                h3("Fit Results"),  
                verbatimTextOutput('modResults')
                  )), # close box and panel
          fluidRow(
            panel(h2("Predictions from the above model"),
              box( # create conditional panel for each variable to be able to input values for prediction
                # must include a null value to avoid any unselected variables to be included
                conditionalPanel(condition=("input.allvar1.includes('school')"), 
                                 selectizeInput("predschool", "Select School", 
                                                choices = c("",levels(as.factor(dat$school))),
                                                selected = NULL)
                                 ),
                conditionalPanel(condition = ("input.allvar1.includes('sex')"),
                                 selectizeInput("predsex", "Select Sex", 
                                                choices = c("",levels(as.factor(dat$sex))),
                                                selected = NULL)
                                 ),
                conditionalPanel(condition = ("input.allvar1.includes('age')"),
                                 selectizeInput("predage", "Select Age", 
                                                choices = c("",levels(as.factor(dat$age))),
                                 selected = NULL)
                ),
              
                conditionalPanel(condition = ("input.allvar1.includes('address')"),
                                 selectizeInput("predaddress", "Select Address", 
                                                choices = c("",levels(as.factor(dat$address))),
                                                selected = NULL)
                ),
                
                conditionalPanel(condition = ("input.allvar1.includes('famsize')"),
                                 selectizeInput("predfamsize", "Select Famsize", 
                                                choices = c("",levels(as.factor(dat$famsize))),
                                                selected = NULL)
                ),
        
                conditionalPanel(condition = ("input.allvar1.includes('Pstatus')"),
                                 selectizeInput("predPstatus", "Select Pstatus", 
                                                choices = c("",levels(as.factor(dat$Pstatus))),
                                                selected = NULL)
                ),
                
                conditionalPanel(condition = ("input.allvar1.includes('Medu')"),
                                 selectizeInput("predMedu", "Select Medu", 
                                                choices = c("",levels(as.factor(dat$Medu))),
                                 selected = NULL)
                ),
                
                conditionalPanel(condition = ("input.allvar1.includes('Fedu')"),
                                 selectizeInput("predFedu", "Select Fedu", 
                                                choices = c("",levels(as.factor(dat$Fedu))),
                                                selected = NULL)
                ),
                
                conditionalPanel(condition = ("input.allvar1.includes('Mjob')"),
                                 selectizeInput("predMjob", "Select Mjob", 
                                                choices = c("",levels(as.factor(dat$Mjob))),
                                                selected = NULL)
                ),
                
                conditionalPanel(condition = ("input.allvar1.includes('Fjob')"),
                                 selectizeInput("predFjob", "Select Fjob", 
                                                choices = c("",levels(as.factor(dat$Fjob))),
                                                selected = NULL)
                ),
                
                conditionalPanel(condition = ("input.allvar1.includes('reason')"),
                                 selectizeInput("predReason", "Select reason", 
                                                choices = c("",levels(as.factor(dat$reason))),
                                                selected = NULL)
                ),
                

                conditionalPanel(condition = ("input.allvar1.includes('guardian')"),
                                 selectizeInput("predGuardian", "Select Guardian", 
                                                choices = c("",levels(as.factor(dat$guardian))),
                                                selected = NULL)
                ),
                
                conditionalPanel(condition = ("input.allvar1.includes('traveltime')"),
                                 selectizeInput("predTraveltime", "Select Traveltime", 
                                                choices = c("",levels(as.factor(dat$traveltime))),
                                                selected = NULL)
                ),
                
                conditionalPanel(condition = ("input.allvar1.includes('studytime')"),
                                 selectizeInput("predStudytime", "Select Studytime", 
                                                choices = c("",levels(as.factor(dat$studytime))),
                                                selected = NULL)
                ),
                
                conditionalPanel(condition = ("input.allvar1.includes('failures')"),
                                 selectizeInput("predFailures", "Select Failures", 
                                                choices = c("",levels(as.factor(dat$failures))),
                                                selected = NULL)
                ),
                
                conditionalPanel(condition = ("input.allvar1.includes('schoolsup')"),
                                 selectizeInput("predSchoolsup", "Select Schoolsup", 
                                                choices = c("",levels(as.factor(dat$schoolsup))),
                                                selected = NULL)
                ),
                
                conditionalPanel(condition = ("input.allvar1.includes('famsup')"),
                                 selectizeInput("predFamsup", "Select Famsup", 
                                                choices = c("",levels(as.factor(dat$famsup))),
                                                selected = NULL)
                ),
                
                conditionalPanel(condition = ("input.allvar1.includes('paid')"),
                                 selectizeInput("predPaid", "Select Paid", 
                                                choices = c("",levels(as.factor(dat$paid))),
                                                selected = NULL)
                ),
                
                conditionalPanel(condition = ("input.allvar1.includes('activities')"),
                                 selectizeInput("predActivities", "Select Activities", 
                                                choices = c("",levels(as.factor(dat$activities))),
                                                selected = NULL)
                ),
                
                conditionalPanel(condition = ("input.allvar1.includes('nursery')"),
                                 selectizeInput("predNursery", "Select Nursery", 
                                                choices = c("",levels(as.factor(dat$nursery))),
                                                selected = NULL)
                ),
                
                conditionalPanel(condition = ("input.allvar1.includes('higher')"),
                                 selectizeInput("predHigher", "Select Higher", 
                                                choices = c("",levels(as.factor(dat$higher))),
                                                selected = NULL)
                ),
                
                conditionalPanel(condition = ("input.allvar1.includes('internet')"),
                                 selectizeInput("predInternet", "Select Internet", 
                                                choices = c("",levels(as.factor(dat$internet))),
                                                selected = NULL)
                ),
                
                conditionalPanel(condition = ("input.allvar1.includes('romantic')"),
                                 selectizeInput("predRomantic", "Select Romantic", 
                                                choices = c("",levels(as.factor(dat$romantic))),
                                                selected = NULL)
                ),
                
                conditionalPanel(condition = ("input.allvar1.includes('famrel')"),
                                 selectizeInput("predFamrel", "Select Famrel", 
                                                choices = c("",levels(as.factor(dat$famrel))),
                                                selected = NULL)
                ),
                
                conditionalPanel(condition = ("input.allvar1.includes('freetime')"),
                                 selectizeInput("predFreetime", "Select Freetime", 
                                                choices = c("",levels(as.factor(dat$freetime))),
                                                selected = NULL)
                ),
                
                conditionalPanel(condition = ("input.allvar1.includes('goout')"),
                                 selectizeInput("predGoout", "Select Goout", 
                                                choices = c("",levels(as.factor(dat$goout))),
                                                selected = NULL)
                ),
                
                
                conditionalPanel(condition = ("input.allvar1.includes('Dalc')"),
                                 selectizeInput("predDalc", "Select Dalc", 
                                                choices = c("",levels(as.factor(dat$Dalc))),
                                                selected = NULL)
                ),
                
                
                conditionalPanel(condition = ("input.allvar1.includes('Walc')"),
                                 selectizeInput("predWalc", "Select Walc", 
                                                choices = c("",levels(as.factor(dat$Walc))),
                                                selected = NULL)
                ),
                
                conditionalPanel(condition = ("input.allvar1.includes('health')"),
                                 selectizeInput("predHealth", "Select Health", 
                                                choices = c("",levels(as.factor(dat$health))),
                                                selected = NULL)
                ),
                
                conditionalPanel(condition = ("input.allvar1.includes('absences')"),
                                 selectizeInput("predAbsences", "Select Absences", 
                                                choices = c("",levels(as.factor(dat$absences))),
                                                selected = NULL)
                )
              
                
              ), # close box
              
              box(h3("Prediction for the selected response:")
                  ,h4("(Be sure to select a value for each predictor, otherwise you will get an error!)"),
                  verbatimTextOutput('pred1'))   
            ) # close panel
          ) # close fluid row
                  
  
              #  box(tableOutput('pred'))
          ) #close fluid page
      ), # close modeling tab
        
        
        tabItem(tabName = "rawdat", 
          fluidPage(
                h2("Our Raw Data"),
                checkboxInput("schoolFilter2", h4("Subset by school?")), #subset our data by school if desired
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

