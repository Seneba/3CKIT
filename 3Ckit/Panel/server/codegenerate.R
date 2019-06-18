
output$generatequality <- downloadHandler(
  # For PDF output, change this to "report.pdf"
  filename = "QualityReport.html",
  content = function(file) {
    # Copy the report file to a temporary directory before processing it, in
    # case we don't have write permissions to the current working dir (which
    # can happen when deployed).
    tempReport <- system.file("man", "Report.Rmd",package = "Cytofkitapp")
    file.copy("Report.Rmd", tempReport, overwrite = TRUE)
    
    # Set up parameters to pass to Rmd document
    params <- list(transformation = input$transformation,
                   compensation= input$compensationMethods,
                   files =input$Files$datapath,
                   mergemethod= input$mergemethod,
                   fixedNum = input$fixedNum,
                   pheno_markers = input$pheno_selected,
                   funct_markers = input$funct_selected,
                   projectName =input$text,
                   fcsFiles =input$file_selected,
                   FCSversion =input$fcsversion,
                   Acquisition =input$acquisition,
                   Cytometer =input$cytometerversion,
                   Numberevents = input$events,
                   Markers_plot = input$pheno_plot,
                   hotable = input$hotable1,
                   beads = input$beads
    )
    
    # Knit the document, passing in the `params` list, and eval it in a
    # child of the global environment (this isolates the code in the document
    # from the code in this app).
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)



cytofkit <- reactive ({
  paste("```{r}","\n",
        "library(cytofkit)","\n",
        "library(flowCore)","\n",
        
        
       # #Configure,
       #  fcsDir = "O:/Equipes_PF_Services/PF_Bioinformatique_Integrative/OpenAccess/cyto_datasets/PBMC8_30min"
       #  prjName = "cytofkit_1000evts"
       #  # Get an example of marker names in order to define markers of interest
        'ff = ',read.FCS(input$Files$datapath[1]),"\n",
        'pd =' ,pData(parameters(ff)),"\n",
        # Configure the phenotypic markers
        'lineage_markers =',c(input$pheno_selected),"\n",
        'lineage_markers_idx =',match(lineage_markers, pd$desc),"\n",
        'if (any(is.na(lineage_markers_idx))) {
          message("Some marker names have not been found. Check carefully those names: ", lineage_markers[is.na(lineage_markers_idx)])
          show(ff)
          stop("Marker names not found.")
        }',"\n",
        
        # Alternatively you can give a vector of channel numbers
        # lineage_markers_idx = c(3,  4,  9, 11, 12, 14, 21, 29, 31, 33)
        'channels=',paste(pd$name, "<", pd$desc, ">", sep = "")[lineage_markers_idx],"\n",
        # To get an idea of the number of events per FCS
        'if (interactive()) do.call(rbind, 
                                  read.FCSheader(dir(path = fcsDir, pattern = "*.fcs", full.names = TRUE), keyword = "$TOT"))',"\n",
       # To get an idea of the number of events per FCS
        # Build result directory in the parent directory of the FCS
       ' resDir = paste(fcsDir, prjName, sep = "--")',"\n",
        "\n",
        "res = cytofkit(","\n",
        # '# Select one among "autoLgcl", "cytofAsinh", "logicle", "arcsinh", "none"',"\n",
        # "transformMethod=",input$transformation,"\n",
        # '# Select one among "ceil", "all", "min", "fixed"',"\n",
        # "mergeMethod=",input$mergemethod,"\n",
        # "fixedNum=",input$dynamic,"\n",
        # "scale=",input$scale,"\n",
        '# Set to TRUE for flow cytometry',"\n",
        "ifCompensation" = FALSE,"\n",
        'transformMethod' = "cytofAsinh","\n",
        '# Select one among "ceil", "all", "min", "fixed"',"\n",
        'mergeMethod=','"',input$mergemethod,'"',"\n",
        'fixedNum=','"',3000,"\n",
        '#Select one among "tSNE", "pca", "isoMAP"',"\n",
        "dimReductionMethod =",'"',input$methodchoose,'"',"\n",
        '# Select one or more among "Rphenograph", "ClusterX", "DensVM", "FlowSOM", "NULL"',"\n",
        "clusterMethods=",'"',input$methodclusterchoose,'"',"\n",
        '# Select one among "NULL", "diffusionmap", "isoMAP"',"\n",
        'progressionMethod = ','"',input$cellularprogression,'"',"\n",
        '# # Paramterization of clustering methods',"\n",
        "Flowsom_k=",'"',input$Flowsom_k,'"',"\n",
        "Flowsom_seed=",'"',input$Flowsom_seed,'"',"\n",
        "Flowsom_X=",'"',input$Flowsom_X,'"',"\n",
        "Flowsom_Y=",'"',input$Flowsom_Y,'"',"\n",
        "Rphenograph_k=",'"',input$Rphenograph_k,'"',"\n",
        "Rphenograph_seed=",'"',input$Rphenograph_seed,'"',"\n",
        "tSNE_max_iteration=",'"',input$tSNE_max_iteration,'"',"\n",
        "tSNE_perplexity =",'"',input$tSNE_perplexity,'"',"\n",
        "# Files, directories, markers...","\n",
        #"markers =",'"',input$pheno_selected,'"',"\n",
        "projectName =",'"',input$text,'"',"\n",
        "fcsFiles =",'"',input$file_selected,'"',"\n",
        "FCSversion=",'"',input$fcsversion,'"',"\n",
        "Acquisition=",'"',input$acquisition,'"',"\n",
        "Cytometer=",'"',input$cytometer,'"',"\n",
        "resultDir=" = '"',resDir,'"',"\n",
        # Files, directories, markers...
        "markers" = channels,"\n",
        
        "saveResults =",TRUE,"\n",
        "saveObject =",TRUE,"\n", 
        "openShinyAPP =",FALSE,"\n",
        ")","\n",
        '```',sep = ""
  )
})



output$generatecytofkit <- downloadHandler(
  # This function returns a string which tells the client
  # browser what name to use when saving the file.
  filename = function() {
    paste("report",".Rmd" ,sep = "")
  },
  # This function should write data to a file given to it by
  # the argument 'file'.
  content = function(file) {
    brew::brew(system.file("man","CytofkitReport.Rmd.brew",package="Cytofkitapp"),file)
    
  }
)
