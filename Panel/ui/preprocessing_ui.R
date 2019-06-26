preprocessingmass <-  
  tabsetPanel(id="tabs3",
            tabPanel("Data Transformation",
                     fluidRow(
                       selectInput("beads","",choices = c("Dvs Beads(140, 151, 153, 165, 175)"="dvs","Beta Beads(139, 141, 159, 169, 175)"= "beta"), selected = "Dvs Beads(140, 151, 153, 165, 175)"),
                       hr(),
                       fluidRow(
                         actionBttn(
                           inputId = "Normalize",
                           label = "Visualize Normalize Data",
                           style = "unite", 
                           color = "succes"
                         ),
                         downloadBttn(
                           outputId = "Normalisation",
                           label = "Download Normalized Data",
                           style = "bordered",
                           color = "success"
                         ),
                         shinybusy::add_busy_spinner(spin = "fading-circle"),
                         hr()
                     ),
                     plotOutput("normalisation")
            )
  )
  )

preprocessingflow <- 
  tabsetPanel(id="tabs3",
              tabPanel("flow cytometry preprocessing", 
                       fluidPage(
                         # h4("Input:"),
                         # fileInput('fcsFiles', strong('Choose FCS file:'), multiple = FALSE,
                         #           accept = c('text/fcs', '.fcs')),
                         
                         # hr(),
                         # h4("Summary:"),
                         # textOutput("summaryText1"),
                         # textOutput("summaryText2"),
                         
                         # hr(),
                         # h4("Parameters:"),
                         # numericInput("timeLenth", label = h5("Time step (sec)"), value = 0.1, step = 0.1),
                         # uiOutput("signalBinSize"),
                         
                         hr(),
                         fluidRow(
                           
                           #actionButton("goButton", "Normalize")
                           actionBttn(
                             inputId = "Normalizeflow",
                             label = "Visualize Normalized Data",
                             style = "unite", 
                             color = "succes"
                           ),
                           downloadBttn(
                             outputId = "downloadGoodFCS",
                             label = "Download Normalized Data",
                             style = "bordered",
                             color = "success"
                           ),
                           shinybusy::add_busy_spinner(spin = "fading-circle")
                         ),
                         # textOutput("flowRateSummary"),
                         # hr(),
                         column(4, offset = 1,
                                uiOutput("timeSlider")
                         ),
                         column(4, offset = 2,
                                uiOutput("rateSlider")
                         ),
                         hr(),
                         plotOutput("flowRatePlot")
                         # hr(),
                         # h4("Download Output:"),
                         # downloadButton('downloadQCFCS',   'FCS with QC'),
                         # br(),
                         #downloadButton('downloadGoodFCS', 'High Q FCS')
                         
                         # br(),
                         # downloadButton('downloadBadFCS',  'Low Q FCS')
                         
                         
                       ))
              
  )


  