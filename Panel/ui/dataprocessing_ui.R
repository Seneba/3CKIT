dataprocessing <- tabsetPanel(id="tabs2",
                tabPanel("Data processing",
                         useShinyalert(),
                         fluidRow(
                           column(6, wellPanel(
                             fluidRow(
                               column(2, wellPanel(
                                 actionBttn(
                                   inputId = "preview",
                                   label = "",
                                   style = "material-circle",
                                   color = "succes",
                                   icon = icon("info")
                                 )
                               )),
                               column(7,(
                                 titlePanel("Load data")
                               ))
                             ),
                             textInput("text",label="Project Name",value = "Enter your project Name"),
                             fileInput(inputId = "Files",
                                       label = "Select FCS Files",
                                       multiple = TRUE,
                                       accept = ".fcs"),
                             #uiOutput("Files_selected"),
                             fluidRow(
                               shinydashboard::box(
                               title = "Data information"
                               ,status = "primary"
                               ,solidHeader = TRUE 
                               ,collapsible = TRUE, 
                               uiOutput("Fcsversion"),
                               uiOutput("Cytometer"),
                               uiOutput("Cytometerversion"),
                               uiOutput("Acquisition"),
                               uiOutput("Events")
                             )
                             
                             
                             #  shinydashboard::box(
                             #   title = "Revenue per Product"
                             #   ,status = "primary"
                             #   ,solidHeader = TRUE 
                             #   ,collapsible = TRUE 
                             #   #,plotOutput("revenuebyRegion", height = "300px")
                             # )
                             )
                             #hotable("table.information")
                             
                           ))
                )
              ))