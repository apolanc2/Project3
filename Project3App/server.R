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
library(rgl)
library(tree)

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
  
# EDA tab
 # 1st plot
  eda_plot <- reactive({
     ggplot(data = dat,aes_string(x=input$var)) + 
       geom_boxplot(position = "dodge", aes(y = G3, fill = sex))
  })
  output$edaPlot <- renderPlot({
      eda_plot()
    }) 
  
  output$downloadEdaPlot <- downloadHandler(
    filename = function() {
      "EDA-Plot.png"
    },
    content = function(file) {
      ggsave(file, plot= eda_plot(),device = "png")
    })
 # 2nd plot 
  eda_plot2 <- reactive({
    ggplot(data = dat,aes_string(x=input$var2)) + 
      geom_boxplot(position = "dodge", aes(y = G3, fill = sex))    
  })
  output$edaPlot2 <- renderPlot({
    eda_plot2()
  })  
  output$downloadEdaPlot2 <- downloadHandler(
    filename = function() {
      "EDA-Plot2.png"
    },
    content = function(file) {
      ggsave(file, plot= eda_plot2(),device = "png")
    })
 # 3rd plot
  sum_plot <- reactive({
    ggplot(data = dat,aes_string(y=input$responses)) + 
      geom_boxplot(position = "dodge", fill = "purple", aes(x = school))
  })
  output$sumPlot <- renderPlot({
    sum_plot()
  })  
  output$downloadSumPlot <- downloadHandler(
    filename = function() {
      "Summary-Plot.png"
    },
    content = function(file) {
      ggsave(file, plot= sum_plot(),device = "png")
    })
 # 4th plot
  cor_plot <- reactive({
    correlations <- cor(select_if(dat,is.numeric))
    corrplot(correlations)
  })
  output$corPlot <- renderPlot({
    cor_plot()
  })
  output$downloadCorPlot <- downloadHandler(
    filename = function() {
      "Corr-Plot.png"
    },
    content = function(file) {
      ggsave(file, plot= cor_plot(),device = "png")
    })
# Cluster tab
  datNum <- select_if(dat,is.numeric)
  selectedData <- reactive({
    datNum[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$clusPlot <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  output$dendoPlot <- renderPlot({
    hierclust <- hclust(dist(data.frame(input$xcol,input$ycol)))
    plot(hierclust,xlab="")
  })
  
# Modeling tab
  
  
# Raw Data tab
  output$datTable <- DT::renderDataTable({
    datatable(dat)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      "student-data.csv"
    },
    content = function(file) {
      write.csv(dat, file)
    })
  
}) # close shinyserver
