generateCode <- 
fluidPage(
  tabsetPanel(id="tabs3",
              tabPanel("Generate Report",
                       #uiOutput("Visualisation"),
                       
                       fluidRow(
                         column(1,
                                downloadBttn(
                                  outputId = "generatetransformation"
                                )
                         ),
                         column(5,(
                           titlePanel(("External data Transformation Report"))
                         ))
                       )
              )))


  
  