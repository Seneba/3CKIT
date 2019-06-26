#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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

source("Panel/ui/dataprocessing_ui.R", local = TRUE)
source("Panel/ui/datatransformation_ui.R", local = TRUE)
source("Panel/ui/qualitycontrol_ui.R", local = TRUE)
source("Panel/ui/selectmarkers_ui.R", local = TRUE)
source("Panel/ui/codegenerate_ui.R", local = TRUE)
source("Panel/ui/preprocessing_ui.R", local = TRUE)


shinyUI <- dashboardPage(skin = "black",
  dashboardHeader(title = "Run Cytofkit"),
  
  dashboardSidebar(useShinyalert(),
    sidebarMenu(id = "sidebarMenu",
                menuItem("Load Data", tabName = "load_data", icon = icon("file-upload")),
                menuItem("Data transformation", tabName = "Datatransformation", icon = icon("edit")),
                menuItem("Preprocessing", tabName = "preprocessing", icon = icon("edit")),
                          menuSubItem("Mass cytometry", tabName = "mass_preprocessing"),
                          menuSubItem("Flow Cytometry", tabName = "flow_preprocessing"),
                menuItem("Quality Control", tabName = "qualitycontrol", icon = icon("gears")),
                #menuItem("Data Visualisation", tabName = "Datavisualisation", icon = icon("chart-area")),
                          menuSubItem("Correlation Matrix", tabName = "qualitycontrol_matrix"),
                          menuSubItem("Density plot", tabName = "qualitycontrol_density"),
                menuItem("Run Cytofkit", tabName = "Runcytofkit", icon = icon("play")),
                menuItem("Code Generate", tabName = "codegenerate", icon = icon("code"))

    )
  ),
  dashboardBody(
    useShinyalert(),
    shinyDashboardThemes(
    theme = "poor_mans_flatly" )
    ,tabItems(
      
      ### Load data page
      tabItem(tabName = "load_data",
              fluidPage(dataprocessing)
      ),
      
      ### Data transformation page
      tabItem(tabName = "Datatransformation",
              fluidPage(datatransformation)
      ),
      ### Preprocessing page
      tabItem(tabName = "preprocessing",
              fluidPage()
      ),
      ### mass preprocessing page
      tabItem(tabName = "mass_preprocessing",
              fluidPage(preprocessingmass)
      ),
      # ### flow preporcessing page
      tabItem(tabName = "flow_preprocessing",
              fluidPage(preprocessingflow)
      ),
      ### Quality control page
      tabItem(tabName = "qualitycontrol",
              fluidPage(qualitycontrol0)
      ),
      ### qualitycontrol_matrix page
      tabItem(tabName = "qualitycontrol_matrix",
              fluidPage(qualitycontrol1)
      ),
      ### qualitycontrol_density page
      tabItem(tabName = "qualitycontrol_density",
              fluidPage(qualitycontrol2)
      ),
      ### cytofkit page
      tabItem(tabName = "Runcytofkit",
              fluidPage((selectMarkers)
              )),
      
      tabItem(tabName = "codegenerate",
              fluidPage((generateCode)
              ))
    )
    
  )
)

