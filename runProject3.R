# Ariana Polanco
# use this to run the shiny app

# install.packages(c("shiny", "shinydashboard", "DT", "tidyverse", "corrplot", "rgl", "tree", "caret", "ggplot2", "plotly", "ggcorrplot", "qtlcharts","shinyWidgets"))

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
library(shinyWidgets)

runGitHub("Project3", "apolanc2", subdir = "Project3App", ref = "main")

