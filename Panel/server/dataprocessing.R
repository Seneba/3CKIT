data <- reactive({
  read.flowSet(files = input$Files$datapath)
})

shinyDirChoose(input, 'resultdir', roots = getVolumes())
resultdir <- reactive(input$resultdir)
output$dir <- renderPrint(resultdir())

observeEvent(input$mergemethod, {
  output$test1 <- renderPrint({ input$mergemethod })
})

observeEvent(input$transformation, {
  output$test2 <- renderPrint({ input$transformation })
})
observeEvent(input$clusteringmethod, {
  output$test3 <- renderPrint({ input$clusteringmethod })
})
observeEvent(input$resultdir, {
  output$test5 <- renderPrint({ input$resultdir })
})

# df <- data.frame(matrix(nrow = 9, ncol = 5))
# 
# colnames(df) <- c("Plate Name", "Spot position", "Arthropod type", "Body part", "Sample name")

#A[, "replicates_per_sample"] = rep(1, 8)

# output$table.information<- renderHotable({
# 
#   fs <- read.flowSet(input$Files$datapath)
#   
#   df = data.frame(matrix(vector(), 5, length(fs),
#                          dimnames=list(c(), c("FcsVersion", "Acquisition", "cytometer","cytometerversion","events"))),
#                 stringsAsFactors=F)
#   
#   df$FcsVersion<- fsApply(fs,function(key){
#     #extract compensation matrix from keywords
#     keyword(fs, "FCSversion")
#   })
#   
#   df$Acquisition <- fsApply(fs,function(ac){
#     #extract compensation matrix from keywords
#     keyword(fs, "$DATE")
#   })
#   
#   df$cytometer <- fsApply(fs,function(cyt){
#     #extract compensation matrix from keywords
#     keyword(fs, "$CYTSN")})
#     
#   # df$cytometerversion < keyword((fs), "$CYT")
#   df$cytometerversion <- fsApply(fs,function(ver){
#     #extract compensation matrix from keywords
#     keyword(fs, "$CYT")
#   })
#     
#     
#   df$events <- fsApply(read.flowSet(input$Files$datapath),function(frame){
#         #extract compensation matrix from keywords
#     nrow(exprs(frame))
#   })
#       df
#     })
#   
  
  #df$FcsVersion<- fsApply(fs, keyword(fs, "FCSversion"))
  # df$Acquisition <- keyword((fs), "$DATE")
  # df$cytometer <- keyword((fs), "$CYTSN")
  # df$cytometerversion < keyword((fs), "$CYT")


set <- reactive({
  if (!is.null(input$Files))
    return()
  isolate({fcsFiles <- input$Files
  if (is.null(fcsFiles))
    return(NULL)
  set <- read.FCS(fcsFiles$datapath)
  set@description$FILENAME <- fcsFiles$name})
  return(set)
})


get_df_data <- reactive({
  df <- hot.to.df(input$data.table)
  
  return(df)
})

# output$Events <- renderUI({
#   numberEvents <- reactive({fsApply(read.flowSet(input$Files$datapath),function(frame){
#     #extract compensation matrix from keywords
#     nrow(exprs(frame))
#   })})
# 
#   textInput("events","Number of events",value= numberEvents())
# })

output$Fcsversion <- renderUI({
  if(!is.null(input$Files)){
    
    return(textInput("fcsversion","FCS Version",value= keyword((read.FCS(input$Files$datapath[1])), "FCSversion")))
    
  }else{
    
    return ()}
})

output$Cytometer <- renderUI({
  if(!is.null(input$Files)){

    return(textInput("cytometer","Cytometer",value= keyword((read.FCS(input$Files$datapath[1])), "$CYTSN")))

  }else{

    return ()}
})


output$Cytometerversion <- renderUI({
  if(!is.null(input$Files)){
    

    return(textInput("cytometerversion","Cytometerversion",value= keyword((read.FCS(input$Files$datapath[1])), "$CYT")))

  }else{

    return ()}
})

output$Acquisition <- renderUI({
  if(!is.null(input$Files)){

    return(textInput("acquisition","Acquisition",value= keyword((read.FCS(input$Files$datapath[1])), "$DATE")))

  }else{

    return ()}
})


output$Events <- renderUI({
  numberEvents <- fsApply(read.flowSet(input$Files$datapath),function(frame){
    #extract compensation matrix from keywords
    nrow(exprs(frame))
  })
    textInput("events","Number of events",value= numberEvents)
})

output$cond_mergemethod <- renderUI({
  
  if(input$mergemethod == "ceil" || input$mergemethod =="fixed"){
    
    return(numericInput("fixedNum",label = "Select a fixed Number", value=3000,min =1, max= 1000))
    
  }else{
    
    return ()}
  
})


output$cond_transformation <- renderUI({
  
  if(input$transformation =="cytofAsinh"){
    
    return(numericInput("scale",label = "Cofactor", value=5, min =1, max= 1000))
    
  } 
  # if(input$transformation == "fixedlogicle"){
  #   
  #   return(numericInput("dynamic2",label = "Please select a number", value=3000, min =1, max= 1000))
  #   
  # }
  else{
    
    return (NULL)}
})

savetransformation <- reactive({  
    cytof_exprsMerge(input$Files$datapath, comp = FALSE, verbose =FALSE,transformMethod=input$transformation)})


##compensation
# fs <- read.flowSet(fcsfiles)
# x <- fs[[1]] 
# comp_list <- spillover(x) 
# comp_list
# comp <- comp_list[[1]]
# x_comp <- compensate(x, comp)
# comp <- fsApply(fs, function(x) spillover(x)[[1]], simplify=FALSE) 
# fs_comp <- compensate(fs, comp)
# #transformation
# cytof_flowframeLoad <- 
# autoLgcl = {
#   trans <- autoLgcl(fcs, channels = colnames(fcs@exprs)[transMarker_id], q = q)
#   
#   transformed <- flowCore::transform(fcs, trans)
#   exprs <- transformed@exprs[, marker_id, drop=FALSE]
# },
# 
# logicle = {
#   data <- fcs@exprs
#   trans <- transformList(transMarker_id, flowCore::logicleTransform(w = l_w, t = l_t, m = l_m, a = l_a))
#   
#   transformed <- flowCore::transform(fcs, trans)
#   exprs <- transformed@exprs[, marker_id, drop=FALSE]}
# 
# observeEvent(input$compensation, {
#   # browser()
#   if(input$compensation == "YES"){
#     output$datacompensation <-renderText({numberEvents()
#     })
# 
#   }
# })






