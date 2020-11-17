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
library(nlme)
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
  getData <- reactive({
    if(input$schoolFilter == 0){
      newData <- dat
    }
      else{newData <- dat %>% filter(school == input$school)}
  })

  
 # 1st plot
  eda_plot <- reactive({
    newData <- getData()
     ggplot(data = newData,aes_string(x=input$var)) + 
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
  sum_plot <- reactive({
    newData <- getData()
    ggplot(data = newData,aes_string(y=input$responses)) + 
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
      ggsave(file, plot = sum_plot(),device = "png")
    })
 # 4th plot
  cor_plot <- reactive({
    newData <- getData()
    corData <- select_if(newData,is.numeric)
    correlations <- cor(corData)
    pvals <- cor_pmat(corData)
    corr.plot <- ggcorrplot(
      correlations, hc.order = TRUE, type = "lower", outline.col = "white",
      p.mat = pvals
    )
   # iplotCorr(corData,reorder=TRUE,chartOpts=list(cortitle="Correlation matrix",
     #                                             scattitle="Scatterplot"))
    ggplotly(corr.plot) %>% layout(xaxis = list(autorange = TRUE),
                               yaxis = list(autorange = TRUE))
   # cplot <- corrplot(correlations)
    #ggplotly(correlations)
  })
  output$corPlot <- renderPlotly({
    cor_plot()
  })
  output$downloadCorPlot <- downloadHandler(
    filename = function() {
      "Corr-Plot.png"
    },
    content = function(file) {
      ggsave(file, plot = cor_plot(),device = "png")
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
  # for reproducibility
  set.seed(123)
  # create train and test data sets
  train <- sample(1:nrow(dat), size = nrow(dat)*0.8)
  test <- dplyr::setdiff(1:nrow(dat), train)
  datTrain <- dat[train,]
  datTest <- dat[test,]
  # our formula using user inputs for our variables
  meanform <- reactive({
    as.formula(paste(input$response, "~", input$allvar1, "+", input$allvar2, "+", input$allvar3, "+", input$allvar4, "+", input$allvar5))
  })
  # fit1 <- gls(data = dat, model = meanform, method = "REML")
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

  
 output$modResults <- renderPrint({
   fit1()$results
   #testFit <- datTest %>% select(input$response, input$allvar1, input$allvar2, input$allvar3,input$allvar4, input$allvar5)
   #predFit <- predict(fit1(), newdata = testFit,se.fit = TRUE)
   #testResp <- testFit[,1]
   #postResample(predFit$fit, obs = testResp)   
   #confMat <- confusionMatrix(predFit, testFit[,1])
   #confMat
 
 })
 
 output$pred <- renderTable({
   testFit <- datTest %>% select(input$response, input$allvar1, input$allvar2, input$allvar3,input$allvar4, input$allvar5)
   predFit <- predict(fit1(), newdata = testFit)
   predSum <- round(summary(predFit),4)
   resp <- datTrain %>% select(input$response)
   testResp <- testFit[,1]
   trainSum <- summary(resp)
  # cbind(trainSum,predSum)
   confMat <- confusionMatrix(predFit, testFit[,1])
   confMat$overall
    })
 
  
# Raw Data tab
  output$datTable <- DT::renderDataTable({
    if(input$schoolFilter2==0){
    datatable(dat,options = list(scrollX = TRUE))
    }
    else{
      schoolDat <- dat %>% filter(school == input$school2)
      datatable(schoolDat,options = list(scrollX = TRUE))
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      "student-data.csv"
    },
    content = function(file) {
      write.csv(dat, file)
    })
  
}) # close shinyserver
