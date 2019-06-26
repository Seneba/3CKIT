selectMarkers <-
  fluidPage(
  tabsetPanel(id="tabs1",
    tabPanel("Markers Selection",
                                  #uiOutput("Visualisation"),
                                  fluidRow(
                                    column(1, wellPanel(
                                      actionBttn(
                                        inputId = "markeralert",
                                        label = "",
                                        style = "material-circle", 
                                        color = "succes",
                                        icon = icon("info")
                                      )
                                    )),
                                    column(6,(
                                      titlePanel("Select Markers")
                                    ))
                                  ),
                                  fluidRow(
                                    column(7,wellPanel(
                                      uiOutput("Phenotypic_Markers"),
                                      textOutput("Pheno_marks_selected"),
                                      uiOutput("Functional_Markers"),
                                      uiOutput("Func_marks_selected"),
                                      
                                      
                                      
                                      fluidRow(
                                        column(2, wellPanel(
                                          actionBttn(
                                            inputId = "panelalert",
                                            label = "",
                                            style = "material-circle", 
                                            color = "succes",
                                            icon = icon("info")
                                          )
                                        )),
                                        column(9,(
                                          titlePanel("OR Define  your Panel")
                                        ))
                                      ),
                                      hotable("hotable1"),
                                      # rHandsontableOutput("panel"),
                                      
                                      
                                      
                                      uiOutput("pheno_panel"),
                                      uiOutput( "func_panel"),
                                      fluidRow(
                                        column(3,(
                                          downloadBttn(
                                            outputId = "paneldefine",
                                            label = "Export Your Panel",
                                            style = "bordered",
                                            color = "primary"
                                          )
                                        ))
                                      )
                                    ))
                                  ),
                                  
                                  fluidRow(
                                    column(7),
                                    column(3,(
                                      actionBttn(
                                        inputId = "dimensionreduction",
                                        label = "NEXT",
                                        style = "unite", 
                                        color = "succes"
                                      )
                                    ))
                                  )
                         )
))