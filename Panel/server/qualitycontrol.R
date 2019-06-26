
# output$marker1 <-renderUI({
#   if(!is.null(input$Files$datapath)){
#     return(selectInput("marker11","choose marker 1",
#                        choices =colnames(read.FCS(input$Files$datapath)[1]),
#                        multiple= FALSE))
#   }
# })
# 
# output$marker2 <-renderUI({
#   if(!is.null(input$Files$datapath)){
#     return(selectInput("marker22","choose marker 2",
#                        choices =colnames(read.FCS(input$Files$datapath)[1]),
#                        multiple= FALSE))
#   }
# })

# output$flowsetPlot <- renderPlot({
#   fs <- read.flowSet(input$Files$datapath)
#   fr <- fs[[1]]
#   autoplot(fr)
#   
# })

output$gating <- renderPlotly({
  #fs <- read.FCS((input$Files$datapath)[1])
  fs <- premessa::concatenate_fcs_files(input$Files$datapath)
  nms <- (colnames(fs)[colnames(fs)!=c("FSC-A","FSC-H","FSC-W","SSC-A","SSC-H","SSC-W","time","event_length","center","width","offset","residual")]) 
  correlation <- round(cor(exprs(fs)[,nms]), 3)
  
  #ff <- exprs(ff)
  #ff <- as.data.frame(ff)
  plot_ly(x = nms, y = nms, z = correlation, 
          key = correlation, type = "heatmap", source = "heatplot") %>%
    layout(xaxis = list(title = ""), 
           yaxis = list(title = ""))

}
)

output$gating2 <- renderPlotly({

  
  applyComp <- function(fcs, compMatrix) {
    
    comp_fcs <- compensate(fcs, compMatrix)
    
  }
  
  
  scaleData <- function(x, range = c(0, 4.5)) {
    
    (x - min(x))/(max(x) - min(x)) * (range[2] - range[1]) + range[1]
    
  }

  cytofAsinh <- function(value, cofactor = input$scale1) {
    
    value <- value  
    
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
  
  fs_comp <- cytofkit::cytof_exprsMerge(input$Files$datapath,comp =input$qualitycompensation ,transformMethod =input$qualitytransformation)
  fs_comp <- as.data.frame(fs_comp)
  nms <- (colnames(fs_comp)[colnames(fs_comp)!=c("FSC-A<NA>","FSC-H<NA>","FSC-W<NA>","SSC-A<NA>","SSC-H<NA>","SSC-W<NA>","Time<NA>","Event_length<NA>","Center<NA>","Width<NA>","Offset<NA>","Residual<NA>")])
  correlation1 <- round(cor(fs_comp[,nms]), 3)
  

  plot_ly(x = nms, y = nms, z = correlation1,
          key = correlation1, type = "heatmap", source = "heatplot") %>%
    layout(xaxis = list(title = ""),
           yaxis = list(title = ""))

}
)


output$Phenotypic_Markers_plot <-renderUI({
  if(!is.null(input$Files$datapath)){
    fs<- read.FCS((input$Files$datapath)[1])
    col.names <- (colnames(fs)[colnames(fs)!=c("FSC-A","FSC-H","FSC-W","SSC-A","SSC-H","SSC-W","Time","Event_length","Center","Width","Offset","Residual")])
    return(selectInput('pheno_plot','Select Markers for Plot:',
                       choices =col.names,
                       multiple= FALSE,width="100%"))
  }
})

# output$heatmap <- renderD3heatmap({
#    fs <- read.FCS((input$Files$datapath)[1])
#    MAT <- round(cor(exprs(fs)), 3)
# 
#    d3heatmap(MAT)})
 
 
# output$heatmap <- renderPlotly({
#    fs <- read.FCS((input$Files$datapath)[1])
#    fs <- exprs(fs)
#    fs <- as.data.frame(fs)
#    plot_ly( fs, type = "scatter")})
output$markertest <- renderPlot({
  ff <- read.flowSet(input$Files$datapath)
  plot1 <- flowViz::densityplot(~ ., ff)
  plot1
})


output$markersplot <- renderPlot({
  
  #qcViolinOut()
  col.x = as.character(input$pheno_plot)
  col.x <- paste("`", col.x, sep = "")
  col.x <- paste(col.x, "`", sep = "")
  
  if (input$qualitycompensation == "TRUE"){
    col.y = as.character(input$pheno_plot)
    col.y <- paste("`", col.y, sep = "")
    col.y <- paste(col.y,"<NA>",sep ="")
    col.y <- paste(col.y, "`", sep = "")
  }
  
  if (input$qualitycompensation == "FALSE"){
    col.y = as.character(input$pheno_plot)
    col.y <- paste("`", col.y, sep = "")
    col.y <- paste(col.y, "`", sep = "")
  }
  

  ff<- premessa::concatenate_fcs_files(input$Files$datapath)
  # 
  # fs_comp <- cytofkit2::cytof_exprsMerge(input$Files$datapath,comp =input$qualitycompensation ,transformMethod =input$qualitytransformation )
  # fs_comp <- premessa::as_flowFrame(fs_comp)
  # fs_comp <- read.FCS(fs_comp)
  # p2 <- ggcyto(fs_comp, aes_string(x=col.y))
  # is(p, "ggplot")
  # p22 <- p2 + geom_density(fill = "yellow", alpha= 0.5) +
  #   ggtitle("Data transformed")
  # p1 <- as.ggplot(p1)
  # p22 <- as.ggplot(p22)
  # p3 <- JLutils::multiplot(p1,p22,cols=2)
  # p3

  # ff <- read.flowSet(input$Files$datapath)
  # sampleNames(ff)<- keyword(ff, "$FIL")
  # ff <- fsApply(ff,function(frame){
  #   fff <- keyword(frame)$FILENAME
  #   frame <- read.FCS(fff)
  #   frame
  # })
  #ff <- exprs(ff)
  #ff <- as.data.frame(ff)
  # ff@description$FILENAME <- input$Files$name
  p <- ggcyto(ff, aes_string(x=col.x))
  is(p, "ggplot")
  p1 <- p + geom_density(fill = "green", alpha= 0.5) +
    ggtitle("Data Non transformed")

  applyComp <- function(fcs, compMatrix) {
    
    comp_fcs <- compensate(fcs, compMatrix)
    
  }
  
  
  scaleData <- function(x, range = c(0, 4.5)) {
    
    (x - min(x))/(max(x) - min(x)) * (range[2] - range[1]) + range[1]
    
  }
  
  cytofAsinh <- function(value, cofactor = input$scale1) {
    
    value <- value  
    
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
  
  
  Merge_files <- function(fcsFiles, 
                               
                               comp = FALSE, 
                               
                               transformMethod = c("autoLgcl", "cytofAsinh", "logicle", "arcsinh", "none"), 
                               
                               scaleTo = NULL, 
                               
                               markers = NULL,
                               
                               mergeMethod = c("ceil", "all", "fixed", "min"), 
                               
                               fixedNum = 10000, 
                               
                               sampleSeed = 123, ...) {
    
    # browser()
    
    transformMethod <- match.arg(transformMethod)
    
    mergeMethod <- match.arg(mergeMethod)
    
    
    
    exprsL <- mapply(Transform_files, fcsFiles, 
                     
                     MoreArgs = list(comp = comp, 
                                     
                                     transformMethod = transformMethod, 
                                     
                                     scaleTo = scaleTo, ...), 
                     
                     SIMPLIFY = FALSE)
    
    if(!is.null(markers)){
      
      for (i in 1:length(exprsL)) {
        
        temp_name = colnames(exprsL[[i]])
        
        markers_id = gsub("(.*)<.*", "\\1", markers)
        
        temp_id = gsub("(.*)<.*", "\\1", temp_name)
        
        exprsL[[i]] = exprsL[[i]][, temp_id %in% markers_id, drop = F]
        
      }
      
    }
    
    
    
    if(is.numeric(sampleSeed))
      
      set.seed(sampleSeed)
    
    ## test if number of events in any fcs less than fixedNum
    
    if(mergeMethod == "fixed"){
      
      if(!is.null(fixedNum)){
        
        eventCountTest <- suppressWarnings(any(lapply(exprsL, function(x) if (nrow(x) < fixedNum) {1} else {0})))
        
        if(eventCountTest == TRUE){
          
          warning("One or more FCS files have less events than specified fixedNum; using lowest as fixedNum")
          
          fixedNum <- min(rapply(exprsL, nrow))
          
        }
        
      }
      
    }
    
    switch(mergeMethod,
           
           ceil = {
             
             mergeFunc <- function(x) {
               
               if (nrow(x) < fixedNum) {
                 
                 x
                 
               } else {
                 
                 x[sample(nrow(x), size = fixedNum, replace = FALSE), , drop = FALSE]
                 
               }
               
             }
             
             merged <- do.call(rbind, lapply(exprsL, mergeFunc))
             
           },
           
           all = {
             
             merged <- do.call(rbind, exprsL)
             
           },
           
           fixed = {
             
             mergeFunc <- function(x) {
               
               x[sample(nrow(x), size = fixedNum, replace = ifelse(nrow(x) < fixedNum, TRUE, FALSE)), , drop=FALSE]
               
             }
             
             merged <- do.call(rbind, lapply(exprsL, mergeFunc))
             
           },
           
           min = {
             
             minSize <- min(sapply(exprsL, nrow))
             
             mergeFunc <- function(x) {
               
               x[sample(nrow(x), size = minSize, replace = FALSE), , drop=FALSE]
               
             }
             
             merged <- do.call(rbind, lapply(exprsL, mergeFunc))
             
           })
    
    
    
    return(merged)
    
  }
  
  
  #fs_comp <- read.flowSet(input$Files$datapath)
  fs_comp<- premessa::concatenate_fcs_files(input$Files$datapath)
  #fs_comp <- keyword(fs_comp)$FILENAME
  print(keyword(fs_comp)$FILENAME)
  #sampleNames(fs_comp)<- keyword(fs_comp, "$FIL")
  # fs_comp <- fsApply(fs_comp,function(frame){
  #   ff <- keyword(frame)$FILENAME
  #   frame <- Transform_files(ff,comp =input$qualitycompensation ,transformMethod =input$qualitytransformation )
  #   frame <- as.data.frame(frame)
  #   frame <- flowAssist::DFtoFF(frame)
  #   frame
  # })
  fs_comp <- Merge_files(input$Files$datapath,comp =input$qualitycompensation ,transformMethod =input$qualitytransformation)
  fs_comp <- premessa::as_flowFrame(fs_comp)

  p2 <- ggcyto(fs_comp, aes_string(x=col.y))
  is(p, "ggplot")
  p22 <- p2 + geom_density(fill = "yellow", alpha= 0.5) +
    ggtitle("Data transformed")

  p1 <- as.ggplot(p1)
  p22 <- as.ggplot(p22)

  p3 <- JLutils::multiplot(p1,p22,cols=2)
  p3
  
}
)


