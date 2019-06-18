#########################dimensoniality Reduction#######


observeEvent(input$dimensionreduction, 
if(input$dimensionreduction == 1){
  insertTab(inputId = "tabs1",
            tabPanel("Dimensionality Reduction",
                     
                     fluidRow(
                       column(1, wellPanel(
                         actionBttn(
                           inputId = "reductionalert",
                           label = "",
                           style = "material-circle", 
                           color = "succes",
                           icon = icon("info")
                         )
                       )),
                       column(9,(
                         titlePanel("Choose a Dimensionality Reduction Methods")
                       ))
                     ),
                     # fluidRow(
                     #   column(3,wellPanel(
                     #     checkboxGroupInput("reductionmethod", label ="", 
                     #                        choices = list("pca" = "pca", "tsne" = "tsne", "isomap" = "isomap"),
                     #                        width ="1000px")
                     #   )),
                     #   column(2,(
                     #     numericInput("tsne_interation",label = "tsne_interation", value = 0,min =0, max= 1000)
                     #   )),
                     #   column(2,(
                     #     numericInput("tsne_perplexity",label = "tsne_perplexity", value= 0, min =0, max= 1000)
                     #   ))
                     # ),
                     fluidRow(
                       column(3,wellPanel(
                         prettyCheckbox(
                           inputId = "pca",
                           label = "pca", 
                           value = FALSE,
                           icon = icon("check"),
                           status = "succes",
                           animation = "rotate" 
                         )
                       ))
                     ),
                     fluidRow(
                       column(3,wellPanel(
                         prettyCheckbox(
                           inputId = "tsne",
                           label = "tsne", 
                           value = FALSE,
                           icon = icon("check"),
                           status = "succes",
                           animation = "rotate" 
                         )
                       )),
                       column(2,(
                         numericInput("tsne_interation",label = "tsne_interation", value = 30,min =0, max= 10000)
                       )),
                       column(2,(
                         numericInput("tsne_perplexity",label = "tsne_perplexity", value= 1000, min =0, max= 10000)
                       ))
                     ),
                     fluidRow(
                       column(3,wellPanel(
                         prettyCheckbox(
                           inputId = "isomap",
                           label = "isomap", 
                           value = FALSE,
                           icon = icon("check"),
                           status = "succes",
                           animation = "rotate" 
                         )
                       )),
                       column(2,(
                         numericInput("isomap_k ",label = "isomap_k ", value = 5,min =0, max= 10000)
                       )),
                       column(2,(
                         numericInput("isomap_ndim",label = "isomap_ndim", value = 5,min =0, max= 10000)
                       )) 
                     ),
                     fluidRow(
                       column(3,wellPanel(
                         prettyCheckbox(
                           inputId = "UMAP",
                           label = "UMAP", 
                           value = FALSE,
                           icon = icon("check"),
                           status = "succes",
                           animation = "rotate" 
                         )
                       )),
                       column(2,(
                         numericInput("umap_neighbor",label = "umap_neighbor", value = 30,min =0, max= 10000)
                       )),
                       column(2,(
                         numericInput("umap_min_dist",label = "umap_min_dist", value= 0.3, min =0, max= 10000)
                       ))
                     ),
                     uiOutput("reductionchoice"),
                     
                     fluidRow(
                       column(7),
                       column(3,(
                         actionBttn(
                           inputId = "clustering",
                           label = "NEXT",
                           style = "unite", 
                           color = "succes"
                         )
                       ))
                     )
                     
                     
            ),
            
            output$reductionmethod <- renderPrint({input$reductionmethod}),
            position = "after",target = "Markers Selection"
  )
  }else { 
  return(NULL)}
)
output$tsne_interation <- renderPrint({input$tsne_interation})
output$tsne_perplexity <- renderPrint({input$tsne_perplexity})



observeEvent(input$reductionalert, {shinyalert(
  title = "Reduction Methods",
  text = "The method used for reduction including 'tsne', 'pca' and 'isomap'
  if you select tsne, you have to customize parameters as tsne max iteration and tsne perplexity",
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

# observeEvent(input$pca,input$tsne,input$isomap,
#              if (input$pca == TRUE){
#                output$Clusterchoice <-renderUI({ TextInput('methodchoose',"methodchoose","pca")})
#              } else if (input$tsne == TRUE){
#                output$Clusterchoice <-renderUI({ TextInput('methodchoose',"methodchoose","tsne")})
#              } else if (input$isomap == TRUE){
#                output$Clusterchoice <-renderUI({ TextInput('methodchoose',"methodchoose","isomap")})
#              }
# )

output$reductionchoice <- renderUI({

  if (input$isomap == TRUE && input$pca == TRUE && input$tsne == TRUE){
    return(textInput('methodchoose',"Methods chosen","isomap,tsne,pca"))
  }
  if (input$pca == TRUE && input$tsne == TRUE){
    return(textInput('methodchoose',"Methods chosen","tsne,pca"))
  }
  if (input$isomap == TRUE && input$pca == TRUE){
    return(textInput('methodchoose',"Methods chosen","isomap,pca"))
  }
  if (input$isomap == TRUE && input$tsne == TRUE){
    return(textInput('methodchoose',"Methods chosen","isomap,tsne"))
  }
  if (input$pca == TRUE){
    return(textInput('methodchoose',"Methods chosen","pca"))
  }
  if (input$tsne == TRUE){
    return(textInput('methodchoose',"Methods chosen","tsne"))
  }
  if (input$isomap == TRUE){
  return(textInput('methodchoose',"Methods chosen","isomap"))
  }
  if (input$UMAP == TRUE){
    return(textInput('methodchoose',"Methods chosen","UMAP"))
  }
  else{
    return (NULL)}
})

# output$Clusterchoice <- renderUI({textInput("ger","he",
#             if (input$pca == TRUE){ return (value = "pca")}
#             if (input$tsne == TRUE) {return(value = "tsne")}
#             if (input$isomap == TRUE) {return(value = "tsne")}
#             else{return(NULL)}
# ) 
# })

########################################################