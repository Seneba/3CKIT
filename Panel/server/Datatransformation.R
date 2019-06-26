observeEvent(input$Files, {
  output$teest <- renderPrint({ input$Files$datapath })
})

shinyDirChoose(input, 'dir', roots = getVolumes())
dir <- reactive(input$dir)
output$dir <- renderPrint(dir())

observeEvent(input$dir, {
  output$teest <- renderPrint({parseDirPath(roots = getVolumes(), input$dir)})
})




output$transformmethod <- renderUI({
  
  if (input$autoLgcl == TRUE ){
    return(textInput('transformation',"Transform Method","autoLgcl"))
  }
  if (input$cytofAsinh == TRUE ){
    return(textInput('transformation',"Transform Method","cytofAsinh"))
  }
  if (input$logicle == TRUE ){
    return(textInput('transformation',"Transform Method","logicle"))
  }
  if (input$arcsinh == TRUE ){
    return(textInput('transformation',"Transform Method","arcsinh"))
  }
  if (input$none== TRUE ){
    return(textInput('transformation',"Transform Method","none"))
  }
  else{
    return (NULL)}
})



# 
# observeEvent(input$trimmass, 
#              if(input$trim == 1){
#                path_dir <- parseDirPath(roots = getVolumes(), input$dir)
#                print(path_dir)
#                #path_dir <- as.character(path_dir)
#                output$trim <- autoGate::trim.fcs(path_dir,pattern = "*.fcs",flu_channels =c("FSC-A","SSC-A"),
#                                                  do_plot = F)
#                
#              })



output$compensatedData <- downloadHandler(
  # This function returns a string which tells the client
  # browser what name to use when saving the file.
  filename = function() {
    paste("Compensate Data",".Zip" ,sep = "")
  },
  # This function should write data to a file given to it by
  # the argument 'file'.
  content = function(file){
    fnames<- c()
    # tmp.dir <- as.character(trunc(as.numeric(Sys.time())))
    # dir.create(tmp.dir)
    # dir.create(paste0(tmp.dir,"/CTRL"))
    fcs.temp <-read.flowSet(input$Files$datapath)
    sampleNames(fcs.temp)<- keyword(fcs.temp, "$FIL")
    fcs.temp <- fsApply(fcs.temp,function(frame){
      #extract compensation matrix from keywords
      comp <- keyword(frame)$SPILL
      new_frame <- compensate(frame,comp)
      new_frame
    })
    
    
    #fnames <-  file.path(tempdir(), "compensate.fcs")
    tmp.name <- paste0("Flowset_compensated",".fcs")
    fnames <- c(unlist(fnames), tmp.name)
    write.flowSet(fcs.temp,fnames)

    #create the zip file
    zip(file,files=fnames)
  }
)



output$Transformeddata <- downloadHandler(
  # This function returns a string which tells the client
  # browser what name to use when saving the file.
  filename = function() {
    paste("Transforme data",".Zip" ,sep = "")
  },
  # This function should write data to a file given to it by
  # the argument 'file'.
  content = function(file){
    fnames<- c()
    fcs.temp <- read.flowSet(input$Files$datapath)
    sampleNames(fcs.temp)<- keyword(fcs.temp, "$FIL")
    fcs.temp <- fsApply(fcs.temp,function(frame){
      #extract compensation matrix from keywords
      
      applyComp <- function(fcs, compMatrix) {
        
        comp_fcs <- compensate(fcs, compMatrix)
        
      }
      
      
      scaleData <- function(x, range = c(0, 4.5)) {
        
        (x - min(x))/(max(x) - min(x)) * (range[2] - range[1]) + range[1]
        
      }
      
      
      
      cytofAsinh <- function(value, cofactor = input$scale) {
        
        value <- value.
        
        loID <- which(value < 0)
        
        if(length(loID) > 0)
          
          value[loID] <- rnorm(length(loID), mean = 0, sd = 0.01)
        
        value <- value / cofactor
        
        value <- asinh(value) # value <- log(value + sqrt(value^2 + 1))
        
        return(value)
        
      }
      
      
      
      autoLgcl <- function(x, channels, m = 4.5, q = 0.05) {
        
        if (!is(x, "flowFrame")) 
          
          stop("x has to be an object of class \"flowFrame\"")
        
        if (missing(channels)) 
          
          stop("Please specify the channels to be logicle transformed")
        
        indx <- channels %in% colnames(x@exprs)
        
        if (!all(indx)) 
          
          stop(paste("Channels", channels[!indx], "were not found in the FCS file.\n ", 
                     
                     sep = " "))
        
        
        
        trans <- lapply(channels, function(p) {
          
          data <- x@exprs[, p]
          
          w <- 0
          
          t <- max(data)
          
          ndata <- data[data < 0]
          
          ## use 1.5 * IQR to filter outliers in negative values
          
          nThres <- quantile(ndata, 0.25) - 1.5 * IQR(ndata)
          
          ndata <- ndata[ndata >= nThres]
          
          transId <- paste(p, "autolgclTransform", sep = "_")
          
          
          
          if (length(ndata)) {
            
            r <- .Machine$double.eps + quantile(ndata, q)
            
            ## Check to avoid failure of negative w
            
            if (10^m * abs(r) <= t) {
              
              w <- 0  
              
            } else {
              
              w <- (m - log10(t/abs(r)))/2
              
              if(is.nan(w) || w>2) {
                
                warning(paste0("autoLgcl failed for channel: ", p, "; using default logicle transformation!"))
                
                w <- 0.1
                
                t <- 4000 
                
                m <- 4.5 
                
              }
              
            }
            
          }
          
          logicleTransform(transformationId = transId, 
                           
                           w = w, t = t, m = m, a = 0)
          
        })
        
        transformList(channels, trans)
        
      }
      
      
      
      
      
      Transform_files <- function(fcsFile,  
                                  
                                  comp = FALSE, 
                                  
                                  transformMethod = c("autoLgcl", "cytofAsinh", "logicle", "arcsinh", "none"), 
                                  
                                  scaleTo = NULL, 
                                  
                                  q = 0.05,
                                  
                                  l_w = 0.1, l_t = 4000, l_m = 4.5, l_a = 0,
                                  
                                  a_a = 1, a_b = 1, a_c =0) {
        
        
        
        transformMethod <- match.arg(transformMethod)
        
        fcs <- read.FCS(fcsFile)
        
        ## compensation
        
        if(is.matrix(comp) || is.data.frame(comp)){
          
          fcs <- applyComp(fcs, comp)
          
          cat("    Compensation is applied on", fcsFile, "\n")
          
        }else if(isTRUE(comp)) {
          
          if(!is.null(fcs@description$SPILL)) {
            
            fcs <- applyComp(fcs, fcs@description[["SPILL"]])
            
            cat("    Compensation is applied on ", fcsFile, "\n")
            
          }else if(!is.null(fcs@description$SPILLOVER)) {
            
            fcs <- applyComp(fcs, fcs@description[["SPILLOVER"]])
            
            cat("    Compensation is applied on ", fcsFile, "\n")
            
          }else if(!is.null(fcs@description$COMP)) {
            
            fcs <- applyComp(fcs, fcs@description[["COMP"]])
            
            cat("    Compensation is applied on ", fcsFile, "\n")
            
          }else{
            
            warning("Cannot find compensation matrix in the FCS files!
                    
                    Please CHECK the keyword of 'SPILL', 'SPILLOVER', or 'COMP'
                    
                    in the FCS file and make sure it stores the compensation matrix.")
            
          }
          
        }
        
        
        
        ## match marker names to get marker ID, use all if NULL 
        
        pd <- fcs@parameters@data
        
        
        
        ## Exclude "Time", "Event" channel
        
        exclude_channels <- grep("Time|Event", colnames(fcs@exprs), ignore.case = TRUE)
        
        marker_id <- setdiff(seq_along(colnames(fcs@exprs)), exclude_channels)
        
        
        
        size_channels <- grep("FSC|SSC", colnames(fcs@exprs), ignore.case = TRUE)
        
        transMarker_id <- setdiff(marker_id, size_channels)
        
        
        
        ## exprs transformation
        
        switch(transformMethod,
               
               cytofAsinh = {
                 
                 data <- fcs@exprs
                 
                 data[ ,transMarker_id] <- apply(data[ ,transMarker_id, drop=FALSE], 2, cytofAsinh)
                 
                 exprs <- data[ ,marker_id, drop=FALSE]
                 
               },
               
               autoLgcl = {
                 
                 trans <- autoLgcl(fcs, channels = colnames(fcs@exprs)[transMarker_id], q = q)
                 
                 transformed <- flowCore::transform(fcs, trans)
                 
                 exprs <- transformed@exprs[, marker_id, drop=FALSE]
                 
               },
               
               logicle = {
                 
                 data <- fcs@exprs
                 
                 trans <- flowCore::logicleTransform(w = l_w, t = l_t, m = l_m, a = l_a)
                 
                 data[ ,transMarker_id] <- apply(data[ ,transMarker_id, drop=FALSE], 2, trans)
                 
                 exprs <- data[ ,marker_id, drop=FALSE]
                 
               },
               
               arcsinh = {
                 
                 data <- fcs@exprs
                 
                 trans <- flowCore::arcsinhTransform(a = a_a, b = a_b, c = a_c)
                 
                 data[ ,transMarker_id] <- apply(data[ ,transMarker_id, drop=FALSE], 2, trans)
                 
                 exprs <- data[ ,marker_id, drop=FALSE]
                 
               },
               
               none = {
                 
                 data <- fcs@exprs
                 
                 exprs <- data[ ,marker_id, drop=FALSE]
                 
               })
        
        
        
        ## apply linear transformation for the "FSC-x", "SSC-x" channel if exists
        
        if(length(size_channels) > 0){
          
          if(any(size_channels %in% marker_id)){
            
            used_size_channel <- size_channels[size_channels %in% marker_id]
            
            used_size_channel_id <- match(used_size_channel, marker_id)
            
            exprs[ ,used_size_channel_id] <- apply(exprs[ , used_size_channel_id, drop=FALSE], 2, 
                                                   
                                                   function(x) scaleData(x, range=c(0, 4.5)))
            
          }
          
        }
        
        
        
        ## rescale all data to same range
        
        if (!is.null(scaleTo)) {
          
          exprs <- apply(exprs, 2, function(x) scaleData(x, scaleTo))
          
        }
        
        
        
        ## add rownames and colnames   
        
        col_names <- paste0(pd$name, "<", pd$desc,">")
        
        colnames(exprs) <- col_names[marker_id]
        
        row.names(exprs) <- paste(pd$name, 1:nrow(exprs), sep = "_")
        
        
        
        return(exprs)
        
        }

      ff <- keyword(frame)$FILENAME
      frame <- Transform_files(ff,comp=input$compensationMethods,transformMethod =input$transformation)
      frame <- as.data.frame(frame)
      frame <- flowAssist::DFtoFF(frame)
      frame
    })
    #fcs.temp <- fsApply(fcs.temp,cytof_exprsMerge())
    # fcs.temp <- flowAssist::DFtoFF(fcs.temp)
    # fnames <-  file.path(getwd(), "transform.fcs")
    # write.FCS(fcs.temp,fnames)
    tmp.name <- paste0("Flowset_transformed",".fcs")
    fnames <- c(unlist(fnames), tmp.name)
    write.flowSet(fcs.temp,fnames)
    

    #create the zip file
    zip(file,files=fnames)
  }
)



output$dataMerged <- downloadHandler(
  # This function returns a string which tells the client
  # browser what name to use when saving the file.
  filename = function() {
    paste("Merged_data",".fcs" ,sep = "")
  },
  # This function should write data to a file given to it by
  # the argument 'file'.
  content = function(file){
    fnames<- c()
    fcs.temp <- input$Files$datapath
    fcs.temp <- cytofkit2::cytof_exprsMerge(fcs.temp,comp=FALSE,transformMethod ="none",mergeMethod =input$mergemethod, fixedNum = input$fixedNum)
    fcs.temp <- premessa::as_flowFrame(fcs.temp)
    write.FCS(fcs.temp,file)
  }
)

output$CompensateMergedData <- downloadHandler(
  # This function returns a string which tells the client
  # browser what name to use when saving the file.
  filename = function() {
    paste("Compensated_Merged_data",".fcs" ,sep = "")
  },
  # This function should write data to a file given to it by
  # the argument 'file'.
  content = function(file){
    fnames<- c()
    fcs.temp <- input$Files$datapath
    fcs.temp <- cytofkit2::cytof_exprsMerge(fcs.temp,comp=TRUE,transformMethod ="none",mergeMethod =input$mergemethod, fixedNum = input$fixedNum)
    fcs.temp <- premessa::as_flowFrame(fcs.temp)
    write.FCS(fcs.temp,file)
  }
)

output$TransformMergedData <- downloadHandler(
  # This function returns a string which tells the client
  # browser what name to use when saving the file.
  filename = function() {
    paste("Transformed_Merged_data",".fcs" ,sep = "")
  },
  # This function should write data to a file given to it by
  # the argument 'file'.
  content = function(file){
    fnames<- c()
    fcs.temp <- input$Files$datapath
    fcs.temp <- cytofkit2::cytof_exprsMerge(fcs.temp,comp=FALSE,transformMethod =input$transformation,mergeMethod =input$mergemethod, fixedNum = input$fixedNum)
    fcs.temp <- premessa::as_flowFrame(fcs.temp)
    write.FCS(fcs.temp,file)
  }
)

output$TransformCompensateMergedData <- downloadHandler(
  # This function returns a string which tells the client
  # browser what name to use when saving the file.
  filename = function() {
    paste("Transformed_Compensated_Merged_Data",".fcs" ,sep = "")
  },
  # This function should write data to a file given to it by
  # the argument 'file'.
  content = function(file){
    fnames<- c()
    fcs.temp <- input$Files$datapath
    fcs.temp <- cytofkit2::cytof_exprsMerge(fcs.temp,comp=TRUE,transformMethod =input$transformation,mergeMethod =input$mergemethod, fixedNum = input$fixedNum)
    fcs.temp <- premessa::as_flowFrame(fcs.temp)
    write.FCS(fcs.temp,file)
  }
)








                                                               