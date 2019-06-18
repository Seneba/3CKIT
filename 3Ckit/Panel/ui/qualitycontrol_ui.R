qualitycontrol0 =
  tabsetPanel(id="tabs4",
              tabPanel("Quality control",
                       
                       fluidRow(
                         column(3, (
                           selectInput("qualitycompensation","Compensation",choices=c("YES"="TRUE","NO"="FALSE"),selected = "FALSE",multiple = FALSE)
                         )),
                         column(3, (
                           selectInput("qualitytransformation","Transformation",
                                       choices=c("cytofAsinh"="cytofAsinh","logicle"="logicle","arcsinh"="arcsinh","none"="none"),
                                       selected = "NONE",multiple = FALSE)
                         )),
                         column(2, (
                           (numericInput("scale1",label = "Choose a cofactor", value=5, min =1, max= 1000))
                         )),
                         column(2, (
                           downloadBttn(
                             outputId = "generatequality",
                             label = "Quality Control Report",
                             style ="jelly"  
                           )
                         )),
                         column(2, (
                           shinybusy::add_busy_spinner(spin = "fading-circle")
                         ))
                       )
              ))
  
qualitycontrol1 = tabsetPanel(id="tabs4",
         tabPanel("Quality control",
                        
                          #plotOutput("flowsetPlot"),
                          titlePanel("Correlation Matrix"),
                          fluidRow(
                            # column(5, (
                            #   #selectInput("marker1","choose marker 1",choices =colnames(read.FCS(input$Files$datapath)))
                            #   uiOutput("marker1")
                            # )),
                            # column(5, (
                            #   #selectInput("marker2","choose marker 2",choices =colnames(read.FCS(input$Files$datapath)))
                            #   uiOutput("marker2")
                            # ))
                            shinydashboard::box(
                              title = "1.Correlation Matirx of untransformed Data"
                              ,status = "primary"
                              ,solidHeader = TRUE 
                              ,collapsible = TRUE,
                              width =6,
                              plotlyOutput("gating")
                            ),
                            shinydashboard::box(
                              title = "2. Correlation Matirx of trnasformed and compensated Data"
                              ,status = "primary"
                              ,solidHeader = TRUE 
                              ,collapsible = TRUE,
                              width =6,
                              plotlyOutput("gating2")
                            )
                            
                            )
    
                  )
)

qualitycontrol2 =
  
  tabsetPanel(id="tabs4",
              tabPanel("Quality control",
                       hr(),
                       h4("Density Plot for all markers in Flowset/Flowframe"),
                       hr(),
                       plotOutput("markertest"),
                       hr(),
                       h4("Visualisation of indivudals markers before and after Files transformation and compensation"),
                       uiOutput("Phenotypic_Markers_plot"),
                       hr(),
                       h4("Density plot of Untransformed Merges Data and Transformed Merged Data"),
                       plotOutput("markersplot")
              ))


