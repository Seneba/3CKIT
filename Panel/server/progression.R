##########Cellular progression################
observeEvent(input$visualisation, 
if(input$visualisation == 1)
  # (input$clusteringmethod =="rpheno" && input$Rphenograph_k >= 1 && input$Rphenograph_seed >= 1)||
  #  input$clusteringmethod =="clusterX" || 
  #  (input$clusteringmethod =="FlowSOM" && input$Flowsom_k >=1 && 
  #   input$Flowsom_seed >= 1 && input$Flowsom_X >=1 && input$Flowsom_Y >= 1) || 
  #  input$clusteringmethod =="DensVM" || 
  #  input$clusteringmethod == "None")
   # (input$flowsom == TRUE && input$Flowsom_k >=1 && input$Flowsom_seed >= 1 && input$Flowsom_X >=1 && input$Flowsom_Y >= 1) ||
   # # ((input$rpheno == TRUE && input$Rphenograph_k >= 1 && input$Rphenograph_seed >= 1) && 
   # #   (input$flowsom == TRUE && input$Flowsom_k ==0 && input$Flowsom_seed == 0 && input$Flowsom_X == 0 && input$Flowsom_Y == 0))
   # (input$clusterX== TRUE 
   #  && input$Rphenograph_k <=0 && input$Rphenograph_seed <=0
   #  && input$Flowsom_k <=0 && input$Flowsom_seed <=0 && input$Flowsom_X <=0 && input$Flowsom_Y <=0) ||
   # (input$DensVM== TRUE 
   #  && input$Rphenograph_k <=0 && input$Rphenograph_seed <=0
   #  && input$Flowsom_k <=0 && input$Flowsom_seed <=0 && input$Flowsom_X <=0 && input$Flowsom_Y <=0) ||
   # (input$None== TRUE
   #  && input$Rphenograph_k <=0 && input$Rphenograph_seed <=0
   #  && input$Flowsom_k <=0 && input$Flowsom_seed <=0 && input$Flowsom_X <=0 && input$Flowsom_Y <=0)){
{
  insertTab(inputId = "tabs1",
            tabPanel("Cellular Progression",
                     #uiOutput("Visualisation"),
                     fluidRow(
                       column(1, wellPanel(
                         actionBttn(
                           inputId = "progressionalert",
                           label = "",
                           style = "material-circle", 
                           color = "succes",
                           icon = icon("info")
                         )
                       )),
                       column(9,(
                         titlePanel("Cellular Progression")
                       ))
                     ),
                     wellPanel(
                     #radioButtons("cellularprogression",label=" ",choices=c("diffusion map"="diffusion map","isoMAP"="isoMAP","NULL"="NULL"))),
                     prettyRadioButtons(
                       inputId = "cellularprogression",
                       label = "", 
                       choices = c("Diffusion map"="diffusionmap","isoMAP"="isoMAP","NULL"="NULL"),
                       icon = icon("check"), 
                       bigger = TRUE,
                       status = "info",
                       animation = "jelly"
                     )),
                     
                     textOutput("table"),
                     fluidRow(
                       column(5,(
                         # downloadBttn(
                         #   outputId = "report",
                         #   label = "Run Cytofkit",
                         #   style = "bordered",
                         #   color = "primary"
                         # )
                         downloadBttn(
                           outputId = "generatecytofkit",
                           label = "Generate Cytofkit Report",
                           style = "bordered",
                           color = "succes"
                         )
                       )),
                       column(5,
                         actionBttn(
                           inputId = "runcytofkit",
                           label = "Run Cytofkit Report",
                           style = "unite", 
                           color = "succes"
                         )
                       ),
                       column(2,(
                         actionBttn(
                           inputId = "quit",
                           label = "QUIT",
                           style = "unite", 
                           color = "succes"
                         )
                       ))
                     )
                     
                    
            ),
            
            output$cellularprogression <- renderPrint({input$cellularprogression}),
            position = "after",target = "Clustering"
  )
}else{return()
  })


observeEvent(input$progressionalert, {shinyalert(
  title = "Cellular Progression Methods",
  text = "The method(s) use for cellular progression including 'isoMAP'and 'Diffusion map'
  If 'NULL' is selected, no progression estimation will be performed",
  closeOnEsc = TRUE,
  closeOnClickOutside = FALSE,
  html = FALSE,
  type = "info",
  showConfirmButton = TRUE,
  showCancelButton = FALSE,
  confirmButtonText = "OK",
  confirmButtonCol = "#AEDEF4",
  timer = 0,
  imageUrl = "",
  animation = TRUE
)})

output$quit <-  reactive({input$quit
  if (input$navbar == "stop") 
    stopApp()
})

observeEvent(input$quit, {
  js$closeWindow()
  stopApp()
})


####################################