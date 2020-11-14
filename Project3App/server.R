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

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  # read in our data. local directory not working at the moment and need full path
  dat <- read.csv(file = 'C:/Users/nelso/Documents/NCSU/ST 558/Project3/Project3/student-mat.csv', sep = ";")

  # output the raw data
  #output$edaTable <- renderTable({
   # var <- input$var
    #datNew <- dat[, c("sex", var),drop = FALSE]
  # tab
 #table(dat$sex, dat[[var]])
  # table(input$sex,dat$failures)
  
    output$edaPlot <- renderPlot({
      ggplot(data = dat,aes_string(x=input$var)) + 
        geom_bar(position = "dodge", aes(fill = sex))

    }) 
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
})
