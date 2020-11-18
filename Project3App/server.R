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
library(caret)
library(ggplot2)
library(plotly)
library(ggcorrplot)
library(qtlcharts)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  # read in our info file
  info <- read.csv(file = "student.csv", sep = ";", header = FALSE)
  info$V1<- substr(info$V1,3,nchar(info$V1))
  names(info) <- c("Variable Name", "Description")
  output$infoTab <- renderDataTable(
    datatable(info)
  )
  # read in our data. local directory not working at the moment and need full path
  dat <- read.csv(file = "student-mat.csv", sep = ";")
  # numeric data set for corrlations and PCA
  datNum <- dat %>% select(1:30) %>% select_if(is.numeric)
  # convert numeric values to factors
  dat$age <- as.factor(dat$age)
  dat$Fedu <- as.factor(dat$Fedu)
  dat$Medu <- as.factor(dat$Medu)
  dat$traveltime <- as.factor(dat$traveltime)
  dat$studytime <- as.factor(dat$studytime)
  dat$Fedu <- as.factor(dat$Fedu)
  dat$failures <- as.factor(dat$failures)
  dat$famrel <- as.factor(dat$famrel)
  dat$freetime <- as.factor(dat$freetime)
  dat$goout <- as.factor(dat$goout)
  dat$Dalc <- as.factor(dat$Dalc)
  dat$Walc <- as.factor(dat$Walc)
  dat$health <- as.factor(dat$health)

  
# EDA tab
  getData <- reactive({ 
    if(input$schoolFilter == 0){
      newData <- dat
    } # if desired filter all data by school 
      else{newData <- dat %>% filter(school == input$school)} 
  })
 # 1st plot
  # box plot for binary variables for each sex
  eda_plot <- reactive({
    newData <- getData() # our data or filtered data
     ggplot(data = newData,aes_string(x=input$var)) + 
       geom_boxplot(position = "dodge", aes(y = G3, fill = sex))
  })
  output$edaPlot <- renderPlot({
      eda_plot()
    }) 
  # download plot
  output$downloadEdaPlot <- downloadHandler(
    filename = function() {
      "EDA-Plot.png"
    },
    content = function(file) {
      ggsave(file, plot= eda_plot(),device = "png") 
    })
 # 2nd plot 
  # box plot for numeric variables for each sex
  eda_plot2 <- reactive({
    newData <- getData()
    ggplot(data = newData,aes_string(x=input$var2)) + 
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
  observe({
    # table will update based on the check box options
    x <- input$inCheckboxGroup
    resChoice <- dat %>% select(input$inCheckboxGroup)
    # Can use character(0) to remove all choices
    if (is.null(x))
      x <- character(0)
    # the check box will update the options for the box plot
    updateSelectInput(session, "inSelect",
                      label = paste("Select Response Variable to Plot"),
                      choices = x,
                      selected = tail(x, 1)
    )
    # create summary table
    summ <- lapply(resChoice , function(x) rbind( mean = mean(x) ,
                                                  sd = sd(x) ,
                                                  median = median(x) ,
                                                  minimum = min(x) ,
                                                  maximum = max(x)) 
             ) 
    output$respTab <- renderTable({
      summ <- data.frame(summ)
      summ<- cbind("Stat"=c("Mean", "SD", "Median", "Min", "Max"),summ)
      summ
    })
    sum_plot <- reactive({
      newData <- getData()
      ggplot(data = dat,aes_string(y=input$inSelect)) + 
        geom_boxplot(position = "dodge", fill = "purple", aes(x = school))
    })
    output$sumPlot<- renderPlot({
      sum_plot()
    })
    output$downloadSumPlot <- downloadHandler(
      filename = function() {
        "Summary-Plot.png"
      },
      content = function(file) {
        ggsave(file, plot = sum_plot(),device = "png")
      })
  })

 # 4th plot
  cor_plot <- reactive({
    correlations <- cor(datNum)
    pvals <- cor_pmat(datNum)
    corr.plot <- ggcorrplot(  # used ggcorrplot to be able to make the correlation plot interactive
      correlations, hc.order = TRUE, type = "lower", outline.col = "white",
      p.mat = pvals
    )
    ggplotly(corr.plot) %>% layout(xaxis = list(autorange = TRUE),
                               yaxis = list(autorange = TRUE))
  })
  # render interactive plot
  output$corPlot <- renderPlotly({
    cor_plot()
  })
  

# Cluster tab
  # filter data for selected variables
  datSelect <- reactive({
    datPC <-  datNum %>% select(input$pcVars)
  })
  # create principal components
  output$pcOut <- renderPrint({
    PCs <- prcomp(datSelect(), center = TRUE, scale = TRUE)
    PCs
  })
  # create biplot
  output$biPlot <- renderPlot({
    PCs <- prcomp(datSelect(), center = TRUE, scale = TRUE)
    biplot(PCs,xlabs = rep(",", nrow(datSelect())))
  })

  
# Modeling tab
  # for reproducibility
  set.seed(123)
  # create train and test data sets
  train <- sample(1:nrow(dat), size = nrow(dat)*0.8)
  test <- dplyr::setdiff(1:nrow(dat), train)
  datTrain <- dat[train,]
  datTest <- dat[test,]
  # our formula using user inputs for our variables
  meanform <- reactive({
     as.formula(paste(input$response, "~", paste(input$allvar1, collapse = "+"), sep = ""))
    })
  # used conditional statements to change the fit method based on user input
  fit1<- reactive({
    if(input$model == "GLM"){
      train(data = dat, meanform(), method = "glm", 
            preProcess = c("center", "scale"),
            trControl = trainControl(method="cv",number=10))
    }
    else if(input$model == "ClassificationTree"){
      train(data = dat, meanform(), method = "rpart", 
            preProcess = c("center", "scale"),
            trControl = trainControl(method="cv"))
    }
    else if(input$model == "RandomForest"){
      train(data = dat, meanform(), method = "rf", 
            preProcess = c("center", "scale"),
            trControl = trainControl(method="cv"))
    }
    else {
      train(data = dat, meanform(), method = "treebag", 
            preProcess = c("center", "scale"),
            trControl = trainControl(method="cv"))
   }
  })
  # used mathJax to display the formula
  output$modForm <- renderUI({
    withMathJax(input$response," =", paste(input$allvar1, collapse = "+") )
  })
  # print results of fit
 output$modResults <- renderPrint({
   fit1()$results
 })
 # create the new data to predict on using the user input
 vals <- reactive({
   inputPred <- data.frame(input$predschool,input$predsex,input$predage, input$predaddress, input$predfamsize, 
     input$predPstatus, input$predMedu, input$predFedu,input$predMjob,input$predFjob,
     input$predReason,input$predGuardian, input$predTraveltime, input$predStudytime,
     input$predFailures, input$predSchoolsup, input$predFamsup,input$predPaid, 
     input$predActivities,input$predNursery,input$predHigher,input$predInternet,
     input$predRomantic,input$predFamrel, input$predFreetime,input$predGoout,input$predDalc,
     input$predWalc,input$predHealth,input$predAbsences)
  names(inputPred) <- c(names(dat[1:30]))
  inputPred  
 })
 # use new data to make prediction
 output$pred1 <- renderPrint({
    predDat <- vals() %>% select_if(~ !any(is.na(.)))
    predict(fit1(), newdata = predDat)
   })
 
  
# Data tab
 # create data table with the filter options
  output$datTable <- DT::renderDataTable({
    if(input$schoolFilter2==0){
    datatable(dat,options = list(scrollX = TRUE))
    }
    else{
      schoolDat <- dat %>% filter(school == input$school2)
      datatable(schoolDat,options = list(scrollX = TRUE)) # a lot of variables requires a scrolling option
    }
  })
  # download the data / subset data
  output$downloadData <- downloadHandler(
    filename = function() {
      "student-data.csv"
    },
    content = function(file) {
      write.csv(dat, file)
    })
  
}) # close shinyserver
