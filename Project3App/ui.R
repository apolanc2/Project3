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
library(shinyWidgets)

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
                h4("This data set comes from the", 
                   a(href = "https://archive.ics.uci.edu/ml/datasets/Student+Performance", "UCI machine learning repository")),
                br(),
        h4("This project is about the student performance data set. 
           This data captures many socio-economic and personal variables to 
           determine whether it affects the grades of students in math and portuguese. 
           The data was separated by subject and I decided to analyze the math subject.
           Grades were collected for each student for the first period, second period, and final grade."),
        h2("Information about the variables"),
        dataTableOutput("infoTab")
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
                      downloadBttn("downloadEdaPlot", "Download as png")),
                  box(selectInput("var2","Plot Our Ranked Variables",
                        choices = c("famrel", "freetime", "goout", "Dalc", "Walc", "health"), selected = "famrel"),
                      plotOutput("edaPlot2"),
                      downloadBttn("downloadEdaPlot2", "Download as png")),
                  box(selectInput("responses", "Summary of Semester grades and overall grade",
                        choices = c("G1","G2","G3")),
                      plotOutput("sumPlot"),
                      downloadBttn("downloadSumPlot", "Download as png")),
                 
                    box(fluidRow(h4("Correlation Plot for Numeric Variables"),
                                 h6("Hover over points to get more details!"),
                      plotlyOutput("corPlot")#, 
                      #           tags$h4("Download"),
                      #downloadBttn(outputId = "downloadCorPlot", style = "bordered", color = "primary")
                      )
                  )
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
                panel(
                h2("Setup Some Models for our data"),
                box(selectizeInput("response", "Choose  a Response Variable", 
                               choices =  c(names(dat[31:33])), selected = "G3"),
                selectizeInput("allvar1", "Choose the Predictor Variables", 
                               choices =  c(names(dat[1:30])), 
                               selected = c("school","sex","higher", "failures","goout"), 
                               multiple = T),
                uiOutput('modForm'),
                ), # close box
                box(selectizeInput("model", "Choose Type of Fit", 
                                   choices =  c("GLM","ClassificationTree", "RandomForest", "BaggedTree")),
                  
                  verbatimTextOutput('modResults')
                  )), # close box and panel
                fluidRow(
                 # column(
                  #  width = 10, offset = 1,
                    panel(
                      box(selectizeInput("predresponse", "Choose  a Response Variable", 
                                         choices =  c(names(dat[31:33])), selected = "G3"),
                          selectizeInput("predvars", "Choose the Predictor Variables", 
                                         choices =  c(names(dat[1:30])), 
                                         selected = c("school","sex","higher", "failures","goout"), 
                                         multiple = T),
                          uiOutput('predForm')
                          ), # close box
                      #checkboxGroupInput(inputId = "predvars", 
                       #            label = "Choose variables to use", 
                        #           choices = c(names(dat[1:30])),
                         #          selected = c("school", "sex", "age","address"),
                          #         inline = TRUE),
                box(
                conditionalPanel(condition=("input.predvars.includes('school')"), 
                                 selectizeInput("predschool", "Select School", 
                                                choices = c("",levels(as.factor(dat$school))),
                                                selected = NULL)
                                 ),
                conditionalPanel(condition = ("input.predvars.includes('sex')"),
                                 selectizeInput("predsex", "Select Sex", 
                                                choices = c("",levels(as.factor(dat$sex))),
                                                selected = NULL)
                                 ),
                conditionalPanel(condition = ("input.predvars.includes('age')"),
                                 selectizeInput("predage", "Select Age", 
                                                choices = c("",levels(as.factor(dat$age))),
                                 selected = NULL)
                ),
              
                conditionalPanel(condition = ("input.predvars.includes('address')"),
                                 selectizeInput("predaddress", "Select Address", 
                                                choices = c("",levels(as.factor(dat$address))),
                                                selected = NULL)
                ),
                
                conditionalPanel(condition = ("input.predvars.includes('famsize')"),
                                 selectizeInput("predfamsize", "Select Famsize", 
                                                choices = c("",levels(as.factor(dat$famsize))),
                                                selected = NULL)
                ),
        
                conditionalPanel(condition = ("input.predvars.includes('Pstatus')"),
                                 selectizeInput("predPstatus", "Select Pstatus", 
                                                choices = c("",levels(as.factor(dat$Pstatus))),
                                                selected = NULL)
                ),
                
                conditionalPanel(condition = ("input.predvars.includes('Medu')"),
                                 selectizeInput("predMedu", "Select Medu", 
                                                choices = c("",levels(as.factor(dat$Medu))),
                                 selected = NULL)
                ),
                
                conditionalPanel(condition = ("input.predvars.includes('Fedu')"),
                                 selectizeInput("predFedu", "Select Fedu", 
                                                choices = c("",levels(as.factor(dat$Fedu))),
                                                selected = NULL)
                ),
                
                conditionalPanel(condition = ("input.predvars.includes('Mjob')"),
                                 selectizeInput("predMjob", "Select Mjob", 
                                                choices = c("",levels(as.factor(dat$Mjob))),
                                                selected = NULL)
                ),
                
                conditionalPanel(condition = ("input.predvars.includes('Fjob')"),
                                 selectizeInput("predFjob", "Select Fjob", 
                                                choices = c("",levels(as.factor(dat$Fjob))),
                                                selected = NULL)
                ),
                
                conditionalPanel(condition = ("input.predvars.includes('reason')"),
                                 selectizeInput("predReason", "Select reason", 
                                                choices = c("",levels(as.factor(dat$reason))),
                                                selected = NULL)
                ),
                

                conditionalPanel(condition = ("input.predvars.includes('guardian')"),
                                 selectizeInput("predGuardian", "Select Guardian", 
                                                choices = c("",levels(as.factor(dat$guardian))),
                                                selected = NULL)
                ),
                
                conditionalPanel(condition = ("input.predvars.includes('traveltime')"),
                                 selectizeInput("predTraveltime", "Select Traveltime", 
                                                choices = c("",levels(as.factor(dat$traveltime))),
                                                selected = NULL)
                ),
                
                conditionalPanel(condition = ("input.predvars.includes('studytime')"),
                                 selectizeInput("predStudytime", "Select Studytime", 
                                                choices = c("",levels(as.factor(dat$studytime))),
                                                selected = NULL)
                ),
                
                conditionalPanel(condition = ("input.predvars.includes('failures')"),
                                 selectizeInput("predFailures", "Select Failures", 
                                                choices = c("",levels(as.factor(dat$failures))),
                                                selected = NULL)
                ),
                
                conditionalPanel(condition = ("input.predvars.includes('schoolsup')"),
                                 selectizeInput("predSchoolsup", "Select Schoolsup", 
                                                choices = c("",levels(as.factor(dat$schoolsup))),
                                                selected = NULL)
                ),
                
                conditionalPanel(condition = ("input.predvars.includes('famsup')"),
                                 selectizeInput("predFamsup", "Select Famsup", 
                                                choices = c("",levels(as.factor(dat$famsup))),
                                                selected = NULL)
                ),
                
                conditionalPanel(condition = ("input.predvars.includes('paid')"),
                                 selectizeInput("predPaid", "Select Paid", 
                                                choices = c("",levels(as.factor(dat$paid))),
                                                selected = NULL)
                ),
                
                conditionalPanel(condition = ("input.predvars.includes('activities')"),
                                 selectizeInput("predActivities", "Select Activities", 
                                                choices = c("",levels(as.factor(dat$activities))),
                                                selected = NULL)
                ),
                
                conditionalPanel(condition = ("input.predvars.includes('nursery')"),
                                 selectizeInput("predNursery", "Select Nursery", 
                                                choices = c("",levels(as.factor(dat$nursery))),
                                                selected = NULL)
                ),
                
                conditionalPanel(condition = ("input.predvars.includes('higher')"),
                                 selectizeInput("predHigher", "Select Higher", 
                                                choices = c("",levels(as.factor(dat$higher))),
                                                selected = NULL)
                ),
                
                conditionalPanel(condition = ("input.predvars.includes('internet')"),
                                 selectizeInput("predInternet", "Select Internet", 
                                                choices = c("",levels(as.factor(dat$internet))),
                                                selected = NULL)
                ),
                
                conditionalPanel(condition = ("input.predvars.includes('romantic')"),
                                 selectizeInput("predRomantic", "Select Romantic", 
                                                choices = c("",levels(as.factor(dat$romantic))),
                                                selected = NULL)
                ),
                
                conditionalPanel(condition = ("input.predvars.includes('famrel')"),
                                 selectizeInput("predFamrel", "Select Famrel", 
                                                choices = c("",levels(as.factor(dat$famrel))),
                                                selected = NULL)
                ),
                
                conditionalPanel(condition = ("input.predvars.includes('freetime')"),
                                 selectizeInput("predFreetime", "Select Freetime", 
                                                choices = c("",levels(as.factor(dat$freetime))),
                                                selected = NULL)
                ),
                
                conditionalPanel(condition = ("input.predvars.includes('goout')"),
                                 selectizeInput("predGoout", "Select Goout", 
                                                choices = c("",levels(as.factor(dat$goout))),
                                                selected = NULL)
                ),
                
                conditionalPanel(condition = ("input.predvars.includes('goout')"),
                                 selectizeInput("predGoout", "Select Goout", 
                                                choices = c("",levels(as.factor(dat$goout))),
                                                selected = NULL)
                ),
                
                
                conditionalPanel(condition = ("input.predvars.includes('Dalc')"),
                                 selectizeInput("predDalc", "Select Dalc", 
                                                choices = c("",levels(as.factor(dat$Dalc))),
                                                selected = NULL)
                ),
                
                
                conditionalPanel(condition = ("input.predvars.includes('Walc')"),
                                 selectizeInput("predWalc", "Select Walc", 
                                                choices = c("",levels(as.factor(dat$Walc))),
                                                selected = NULL)
                ),
                
                conditionalPanel(condition = ("input.predvars.includes('health')"),
                                 selectizeInput("predHealth", "Select Health", 
                                                choices = c("",levels(as.factor(dat$health))),
                                                selected = NULL)
                ),
                
                conditionalPanel(condition = ("input.predvars.includes('absences')"),
                                 selectizeInput("predAbsences", "Select Absences", 
                                                choices = c("",levels(as.factor(dat$absences))),
                                                selected = NULL)
                ),
                actionButton("update", "Update")
              
                
                ) # close box
                 
                    ) # close panel
                  #) # close column
                ) # close fluid row
                  
                ,
                box(verbatimTextOutput('pred1')),
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

