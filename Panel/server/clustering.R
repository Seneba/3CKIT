############## CLUSTERINGG#################

observeEvent(input$clustering ,
             if(input$clustering==1 
               #(input$reductionmethod== "pca" || input$reductionmethod== "tSNE" || input$reductionmethod== "isomap")
               # (input$PCA == TRUE && input$tSNE ==FALSE && input$isomap == FALSE && (is.na(input$tSNE_interation) || input$tSNE_interation <= 0) && (is.na(input$tSNE_perplexity) || input$tSNE_perplexity <= 0)) ||
               #  (input$PCA == FALSE && input$tSNE ==TRUE && input$isomap == FALSE && input$tSNE_interation >= 1 && input$tSNE_perplexity >= 1) ||
               #  (input$PCA == FALSE && input$tSNE ==FALSE && input$isomap == TRUE && (is.na(input$tSNE_interation) || input$tSNE_interation <= 0) && (is.na(input$tSNE_perplexity) || input$tSNE_perplexity <= 0))
             ){
  insertTab(inputId = "tabs1",
            tabPanel("Clustering",
                     #uiOutput("Clustering"),
                     fluidRow(
                       column(1, wellPanel(
                         actionBttn(
                           inputId = "clusteringalert",
                           label = "",
                           style = "material-circle", 
                           color = "succes",
                           icon = icon("info")
                         )
                       )),
                       column(9,(
                         titlePanel("Clustering methods")
                       ))
                     ),
                     fluidRow(
                       column(3,wellPanel(
                         prettyCheckbox(
                           inputId = "FlowSOM",
                           label = "FlowSOM", 
                           value = FALSE,
                           icon = icon("check"),
                           status = "succes",
                           animation = "rotate" 
                         )
                         )),
                       column(2,(
                         numericInput("Flowsom_k",label = "Flowsom_k", value=20, min =0, max= 1000)
                       )),
                       column(2,(
                         numericInput("Flowsom_seed",label = "Flowsom_seed", value=42, min =0, max= 1000)
                       )),
                       column(2,(
                         numericInput("Flowsom_X",label = "Flowsom_X", value=100, min =0, max= 1000)
                       )),
                       column(2,(
                         numericInput("Flowsom_Y",label = "Flowsom_Y", value=10, min =0, max= 1000)
                       ))
                     ),
                     
                     fluidRow(
                       column(3,wellPanel(
                         prettyCheckbox(
                           inputId = "Rphenograph",
                           label = "Rphenograph", 
                           value = FALSE,
                           icon = icon("check"),
                           status = "succes",
                           animation = "rotate" 
                         )
                       )),
                       column(2,(
                         numericInput("Rphenographgraph_k",label = "Rphenographgraph_k", value=30, min =0, max= 1000)
                       )),
                       column(2,(
                         numericInput("Rphenographgraph_seed",label = "Rphenographgraph_seed", value=42, min =0, max= 1000)
                       ))
                     ),

                     fluidRow(
                       column(3,wellPanel(
                         prettyCheckbox(
                           inputId = "clusterX",
                           label = "clusterX", 
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
                           inputId = "DenseVM",
                           label = "DenseVM", 
                           value = FALSE,
                           icon = icon("check"),
                           status = "succes",
                           animation = "rotate" 
                         )
                       ))
                     ),
                     uiOutput("clusterchoice"),
                     
                     fluidRow(
                       column(10),
                       column(2,
                         actionBttn(
                           inputId = "visualisation",
                           label = "NEXT",
                           style = "unite", 
                           color = "succes"
                         )
                       )
                     )
            ),
            
            output$clusteringmethod <- renderPrint({input$clusteringmethod}),
            position = "after",target = "Dimensionality Reduction"
  )
             } else { 
               return("NULL")}
)

output$value <- renderPrint({input$Rphenographgraph_k})
output$value <- renderPrint({input$Rphenographgraph_seed})
output$value <- renderPrint({input$flowsom})
output$value <- renderPrint({input$flowsom_k})
output$value <- renderPrint({input$flowsom_seed})
output$value <- renderPrint({input$flowsom_X})
output$value <- renderPrint({input$flowsom_Y})


observeEvent(input$clusteringalert, {shinyalert(
  title = "Clustering Methods",
  text = "The method(s) use for clustering including 'DensVM','ClusterX','Rphenographgraph'and 'FlowSOM'
  DenseVM and ClusterX require tSNE reduction method",
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


output$clusterchoice <- renderUI({
  
  if (input$FlowSOM == TRUE && input$clusterX == TRUE && input$Rphenograph == TRUE && input$DenseVM == TRUE){
    return(textInput('methodclusterchoose',"Methods chosen","FlowSOM,ClusterX,Rphenograph,DenseVM"))
  }
  if (input$FlowSOM == TRUE && input$clusterX == TRUE){
    return(textInput('methodclusterchoose',"Methods chosen","FlowSOM,ClusterX"))
  }
  if (input$FlowSOM == TRUE && input$Rphenograph == TRUE){
    return(textInput('methodclusterchoose',"Methods chosen","FlowSOM,Rphenograph"))
  }
  if (input$FlowSOM == TRUE && input$DenseVM == TRUE){
    return(textInput('methodclusterchoose',"Methods chosen","FlowSOM,DenseVM"))
  }
  if (input$clusterX == TRUE && input$DenseVM == TRUE){
    return(textInput('methodclusterchoose',"Methods chosen","clusterX,DenseVM"))
  }
  if (input$clusterX == TRUE && input$Rphenograph == TRUE){
    return(textInput('methodclusterchoose',"Methods chosen","clusterX,Rphenograph"))
  }
  if (input$DenseVM== TRUE && input$Rphenograph == TRUE){
    return(textInput('methodclusterchoose',"Methods chosen","DenseVM,Rphenograph"))
  }
  if (input$FlowSOM == TRUE){
    return(textInput('methodclusterchoose',"Methods chosen","FlowSOM"))
  }
  if (input$clusterX == TRUE){
    return(textInput('methodclusterchoose',"Methods chosen","clusterX"))
  }
  if (input$Rphenograph == TRUE){
    return(textInput('methodclusterchoose',"Methods chosen","Rphenograph"))
  }
  if (input$DenseVM == TRUE){
    return(textInput('methodclusterchoose',"Methods chosen","DenseVM"))
  }
  else{
    return (NULL)}
})
