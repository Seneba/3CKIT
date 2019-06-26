data <- reactive({
  read.table(files = input$panel$datapath)
})

observeEvent(input$preview, {shinyalert(
  title = "FCS Files",
  text = "The fcs files to be analyzed. One or multiple fcs files are allowed.When multiple fcs files are selected, cells from each fcs files are combined for analysis ",
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

observeEvent(input$transformationalert, {shinyalert(
  title = "Transformation",
  text = "Data transformation method ,including 'cytofAsinh'(choose a scale for Asinh transformation depending on the type of cytometry),
  'autoLgcl','logicle'(customize your own paramters for logicle transformation) and none if your data is already transformed ",
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

observeEvent(input$mergealert, {shinyalert(
  title = "Merge method",
  text = "When multiple fcs files are selected, cell expression data can be merges  using one of the four different methods including 'ceil','all','min','fixed',
  Ceil up to fixed a number (specified by FixedNum) of cells are sampled without replacement from each fcs file and combined for analysis.
  Fixed: a fixed number (specified by fixedNum) of cells are samled (with remplacement when the total number of cell is less than fixedNum) from each fcs files and combined for analysis",
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

observeEvent(input$markeralert, {shinyalert(
  title = "Markers",
  text = "You can not choose markers if the FCS files are not loaded",
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

observeEvent(input$panelalert, {shinyalert(
  title = "Panel",
  text = "You can not define a panel if the FCS files are not loaded",
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


# observeEvent(input$phenotypic, {
#   output$phenotypic <- renderPrint({strsplit(input$phenotypic,split =" ")})
# })



output$Files_selected <-renderUI({
  if(!is.null(input$Files)){
    return(textInput('file_selected','',value =(input$Files)))
  }
})


output$Phenotypic_Markers <-renderUI({
  ff = read.FCS((input$Files$datapath)[1])
  pd = pData(parameters(ff))
  channels = paste(pd$name, "<", pd$desc, ">", sep = "")
  if(!is.null(input$Files)){
    return(selectInput('pheno_selected','Select Phenotypic Markers:',
                       choices =channels,
                       multiple= TRUE,width="100%"))
  }
})


observeEvent(input$pheno_selected, {
  
  output$Pheno_marks_selected <- renderText(input$pheno_selected)
  vect <- input$pheno_selected
  #print(vect)
  #vect <- paste(vect,",")
  # vect<- as.vector(vect)
  # vect <- paste(vect,sep=",")
  #cat(paste0('"', paste(vect, collapse="\", \""), '"'))
  # vect<- unlist(vect)
  # vect <- paste(vect,",")
  vect <- strsplit(vect,split="\t")
  # vect <- paste(vect, collapse = ",")
  # vect <- strsplit(vect,split=",")
  print(vect)
})


output$Functional_Markers <-renderUI({
  
  ff = read.FCS((input$Files$datapath)[1])
  pd = pData(parameters(ff))
  channels = paste(pd$name, "<", pd$desc, ">", sep = "")
  if(!is.null(input$Files)){
    return(selectInput('funct_selected','Select Functional Markers:',
                       choices =channels,
                       multiple= TRUE,width="100%"))
  }
})

observeEvent(input$funct_selected, {
  output$Func_marks_selected <- renderText({ input$funct_selected })
})



# output$hotable1 <- renderHotable({Trigger_orders()}, readOnly = F)


# Initiate your table
  
previous <- reactive({
  markers <- colnames(read.FCS((input$Files$datapath)[1]))
  df = data.frame(matrix(vector(), length(markers), 3,
                         dimnames=list(c(), c("Markers", "Functional", "Phenotypic"))),
                  stringsAsFactors=F)
  df$Markers<- markers
  df$Functional <- rep(0,length(markers))
  df$Phenotypic <- rep(1,length(markers))
  df
  })
   
  
Trigger_orders <- reactive({
    
  if(is.null(input$hotable1)){return(previous())}
    
  else if(!identical(previous(),input$hotable1)){
      # hot.to.df function will convert your updated table into the dataframe
      
    as.data.frame(hot.to.df(input$hotable1))
    
    }
  
  })

output$hotable1 <- renderHotable({Trigger_orders()}, readOnly = F)

output$pheno_panel <- renderUI({
  
  if(is.null(input$hotable1)){return(previous())}
  
  else if(!identical(previous(),input$hotable1)){
    # hot.to.df function will convert your updated table into the dataframe
    
    df <- as.data.frame(hot.to.df(input$hotable1))
    
    df_new <- subset(df,df$Phenotypic >= 1)
    pheno <- df_new$Markers
    selectInput("Phenopanel","Phenotypic Markers from Panel",choices =pheno,selected =pheno,multiple = TRUE)
    
  }
})

output$func_panel <- renderUI({
  
  if(is.null(input$hotable1)){return(previous())}
  
  else if(!identical(previous(),input$hotable1)){
    # hot.to.df function will convert your updated table into the dataframe
    
    df <- as.data.frame(hot.to.df(input$hotable1))
    
    df_new <- subset(df,df$Functional >= 1)
    func <- df_new$Markers
    selectInput("funcpanel","Functional Markers from Panel",choices =func,selected =func,multiple = TRUE)
  }
})


# Downloadable txt of selected dataset ----
output$paneldefine <- downloadHandler(
  filename = function() {
    paste("Panel", ".xlsx", sep = "")
  },
  content = function(file) {
    write.table(Trigger_orders(), file,sep ="\t",row.names = FALSE)
  }
)
######################################################
