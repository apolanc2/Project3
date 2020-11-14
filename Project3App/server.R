# Ariana Polanco
# Project 3 server
#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
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

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  # read in our data. local directory not working at the moment and need full path
  dat <- read.csv(file = 'C:/Users/nelso/Documents/NCSU/ST 558/Project3/Project3/student-mat.csv', sep = ";")
  dat$famrel <- as.factor(dat$famrel)
  dat$freetime <- as.factor(dat$freetime)
  dat$goout <- as.factor(dat$goout)
  dat$Dalc <- as.factor(dat$Dalc)
  dat$Walc <- as.factor(dat$Walc)
  dat$health <- as.factor(dat$health)
  
  # output the raw data
  #output$edaTable <- renderTable({
   # var <- input$var
    #datNew <- dat[, c("sex", var),drop = FALSE]
  # tab
 #table(dat$sex, dat[[var]])
  # table(input$sex,dat$failures)
  
  output$edaPlot <- renderPlot({
      ggplot(data = dat,aes_string(x=input$var)) + 
        geom_boxplot(position = "dodge", aes(y = G3, fill = sex))
    }) 
  
  output$edaPlot2 <- renderPlot({
    ggplot(data = dat,aes_string(x=input$var2)) + 
      geom_boxplot(position = "dodge", aes(y = G3, fill = sex))
  })  
  
  output$sumPlot <- renderPlot({
    ggplot(data = dat,aes_string(y=input$responses)) + 
      geom_boxplot(position = "dodge", fill = "purple", aes(x = school))
  })  

  output$corPlot <- renderPlot({
    correlations <- cor(select_if(dat,is.numeric))
    corrplot(correlations)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      "student-data.csv"
    },
    content = function(file) {
      write.csv(dat, file)
    })
})
