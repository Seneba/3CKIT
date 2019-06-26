datatransformation <- tabsetPanel(id="tabs3",
                               tabPanel("Data Transformation",
                                        fluidRow(
                                          
                                          shinydashboard::box(
                                            title = "1.Compensate Data"
                                            ,status = "primary"
                                            ,solidHeader = TRUE 
                                            ,collapsible = TRUE,
                                            width =4,
                                            column(10,
                                              fluidRow(
                                                column(2, wellPanel(
                                                  actionBttn(
                                                    inputId = "compensationalert",
                                                    label = "",
                                                    style = "material-circle",
                                                    color = "succes",
                                                    icon = icon("info")
                                                  )
                                                )),
                                                column(7,(
                                                  titlePanel("Compensation")
                                                ))
                                              ),
                                              fluidRow(
                                                column(5,
                                                  selectInput("compensationMethods","",choices = c("YES"="TRUE","NO"="FALSE"), selected = "YES")
                                                )    
                                              ),
                                              fluidRow(
                                                column(12,(
                                                  downloadBttn(
                                                    outputId = "compensatedData",
                                                    label = "Compensate Data",
                                                    style = "bordered",
                                                    color = "success"
                                                    
                                                  )
                                                ))
                                                
                                              )
                                            )
                                          ),
                                          shinydashboard::box(
                                            title = "2.Transform Data"
                                            ,status = "primary"
                                            ,solidHeader = TRUE 
                                            ,collapsible = TRUE,
                                            width =4,
                                            column(10,
                                              fluidRow(
                                                fluidRow(
                                                  column(2, wellPanel(
                                                    actionBttn(
                                                      inputId = "transformationalert",
                                                      label = "",
                                                      style = "material-circle",
                                                      color = "succes",
                                                      icon = icon("info")
                                                    )
                                                  )),
                                                  column(7,(
                                                    titlePanel("Transformation")
                                                  ))
                                                ),
                                                
                                                
                                                fluidRow(
                                                  column(3,wellPanel(
                                                    prettyCheckbox(
                                                      inputId = "autoLgcl",
                                                      label = "autoLgcl",
                                                      value = FALSE,
                                                      icon = icon("check"),
                                                      status = "succes",
                                                      animation = "rotate"
                                                    )
                                                  ))
                                                  # column(5,(
                                                  #   (uiOutput("Channels"))
                                                  # ))
                                                ),
                                                fluidRow(
                                                  column(4,wellPanel(
                                                    prettyCheckbox(
                                                      inputId = "cytofAsinh",
                                                      label = "cytofAsinh",
                                                      value = FALSE,
                                                      icon = icon("check"),
                                                      status = "succes",
                                                      animation = "rotate"
                                                    )
                                                  )),
                                                  column(5,(
                                                    (numericInput("scale",label = "Choose a cofactor", value=5, min =1, max= 1000))
                                                  ))
                                                ),
                                                fluidRow(
                                                  column(3,wellPanel(
                                                    prettyCheckbox(
                                                      inputId = "logicle",
                                                      label = "logicle",
                                                      value = FALSE,
                                                      icon = icon("check"),
                                                      status = "succes",
                                                      animation = "rotate"
                                                    )
                                                  ))
                                                  # column(5,(
                                                  #   (uiOutput("Channels"))
                                                  # ))
                                                ),
                                                fluidRow(
                                                  column(5,wellPanel(
                                                    prettyCheckbox(
                                                      inputId = "arcsinh",
                                                      label = "arcsinh",
                                                      value = FALSE,
                                                      icon = icon("check"),
                                                      status = "succes",
                                                      animation = "rotate"
                                                    )
                                                  ))
                                                ),
                                                fluidRow(
                                                  column(5,wellPanel(
                                                    prettyCheckbox(
                                                      inputId = "none",
                                                      label = "none",
                                                      value = FALSE,
                                                      icon = icon("check"),
                                                      status = "succes",
                                                      animation = "rotate"
                                                    )
                                                  ))
                                                ),
                                                fluidRow(
                                                  column(12,(
                                                    downloadBttn(
                                                      outputId = "Transformeddata",
                                                      label = "Transformed Data",
                                                      style = "bordered",
                                                      color = "success"
                                                    )
                                                  ))
                                                  
                                                ),
                                                fluidRow(
                                                  column(5,(
                                                    uiOutput("transformmethod")
                                                  ))
                                                  
                                                  
                                                )
                                                # column(8, wellPanel(
                                                #   selectInput("transformation","", c("autoLgcl"="autoLgcl","cytofAsinh"="cytofAsinh","logicle"="logicle","arcsinh"="arcsinh","none"="none")),
                                                #
                                                #   dataTableOutput("tab")
                                                #   #erbatimTextOutput("test2")
                                                # )),
                                                # column(7,(
                                                #   uiOutput("cond_transformation")
                                                # )
                                                # )
                                              )
                                            )
                                          ),
                                          shinydashboard::box(
                                            title = "3. Merge Data"
                                            ,status = "primary"
                                            ,solidHeader = TRUE 
                                            ,collapsible = TRUE,
                                            width =4,
                                            column(10,
                                              fluidRow(
                                                fluidRow(
                                                  column(2, wellPanel(
                                                    actionBttn(
                                                      inputId = "mergealert",
                                                      label = "",
                                                      style = "material-circle",
                                                      color = "succes",
                                                      icon = icon("info")
                                                    )
                                                  )),
                                                  column(7,(
                                                    titlePanel("Merge methods")
                                                  ))
                                                ),
                                                column(6,
                                                  selectInput("mergemethod", "Select a Merge Method", c("all" = "all","min" = "min","ceil" = "ceil", "fixed"="fixed"))
                                                  #verbatimTextOutput("test1")
                                                ),
                                                column(5,(
                                                  numericInput("fixedNum",label = "Select a fixed Number", value=10000,min =1, max= 10000)
                                                  
                                                ))
                                              ),
                                              fluidRow(
                                                column(12,(
                                                  downloadBttn(
                                                    outputId = "dataMerged",
                                                    label = "Merged Data",
                                                    style = "bordered",
                                                    color = "success"
                                                  )
                                                ))
                                                
                                              ),
                                              fluidRow(
                                                column(12,(
                                                  downloadBttn(
                                                    outputId = "CompensateMergedData",
                                                    label = "Compensated Merged Data",
                                                    style = "bordered",
                                                    color = "success"
                                                  )
                                                )) 
                                              ),
                                              fluidRow(
                                                column(12,(
                                                  downloadBttn(
                                                    outputId = "TransformMergedData",
                                                    label = "Transformed Merged Data",
                                                    style = "bordered",
                                                    color = "success"
                                                  )
                                                )) 
                                              ),
                                              fluidRow(
                                                column(12,(
                                                  downloadBttn(
                                                    outputId = "TransformCompensateMergedData",
                                                    label = "Transformed and Compensated Merged Data",
                                                    style = "bordered",
                                                    color = "success"
                                                  )
                                                )) 
                                              )
                                              
                                            )
                                          )
                                          # shinydashboard::box(
                                          #   title = "4.Trim Data"
                                          #   ,status = "primary"
                                          #   ,solidHeader = TRUE 
                                          #   ,collapsible = TRUE,
                                          #   width =3,
                                          #   column(10,
                                          #          fluidRow(
                                          #            fluidRow(
                                          #              column(2, wellPanel(
                                          #                actionBttn(
                                          #                  inputId = "trimalert",
                                          #                  label = "",
                                          #                  style = "material-circle",
                                          #                  color = "succes",
                                          #                  icon = icon("info")
                                          #                )
                                          #              )),
                                          #              column(7,(
                                          #                titlePanel("Triming")
                                          #              ))
                                          #            ),
                                          #            column(4,
                                          #              shinyDirButton("dir", "Folder select", "Please select a folder")
                                          #              # verbatimTextOutput("teest")
                                          #            ),
                                          #            column(4,(
                                          #              shinybusy::add_busy_spinner(spin = "fading-circle")
                                          #            ))
                                          #          ),
                                          #          fluidRow(
                                          #            column(7,(
                                          #              actionBttn(
                                          #                inputId = "trimflow",
                                          #                label = "Trimed Flow Cytometry Data",
                                          #                style = "bordered", 
                                          #                color = "success",
                                          #                icon = icon("download")
                                          #              )
                                          #            )),
                                          #            column(7,(
                                          #              actionBttn(
                                          #                inputId = "trimmass",
                                          #                label = "Trimed Mass Cytometry Data",
                                          #                style = "bordered", 
                                          #                color = "success",
                                          #                icon = icon("download")
                                          #              )
                                          #            ))
                                          #          )
                                          #          
                                          #   )
                                          # )
                                          
                                        )
                               )
)