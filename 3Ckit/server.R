#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

library(shiny)
library(shinyjs)
library(FlowSOM)
library(flowCore)
library(DT)
library(markdown)
library(rhandsontable)
library(flowCore)
library(knitr)
library(shinythemes)
library(shinyFiles)
library(shinyalert)
library(shinyWidgets)
library(shinydashboard)
library(dashboardthemes)
library(ggcyto)
library(ggplot2)
library(JLutils)
library(plotly)
library(flowViz)
library(flowStats)
library(corrplot)
library(CATALYST)
library(flowAssist)
library(shinysky)
library(shinybusy)
library(flowAI)
#library(heatmaply)
#library(d3heatmap)


shinyserver <- function(input, output, session) {
  options(shiny.maxRequestSize = 1024^10)

  source("Panel/server/dataprocessing.R", local = T)
  source("Panel/server/Datatransformation.R", local = T)
  source("Panel/server/selectmarkers.R", local = T)
  source("Panel/server/reduction.R", local = T)
  source("Panel/server/clustering.R", local = T)
  source("Panel/server/progression.R", local = T)
  source("Panel/server/qualitycontrol.R", local = T)
  source("Panel/server/codegenerate.R", local = T)
  source("Panel/server/preprocessing.R", local = T)

}



