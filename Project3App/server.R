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

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

#    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
 #       x    <- faithful[, 2]
  #      bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
   #     hist(x, breaks = bins, col = 'darkgray', border = 'white')

 #   })
  output$downloadCsv <- downloadHandler(
    filename = "student-mat.csv",
    content = function(file) {
      write.csv(pkgData(), file)
    },
    contentType = "text/csv"
  )
  
  output$datTable <- renderPrint({
    orig <- options(width = 1000)
    print(tail(pkgData(), input$maxrows), row.names = FALSE)
    options(orig)
  })
})
