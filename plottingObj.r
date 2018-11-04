initPlotting <- function(doCheckPlots=FALSE, checkPlotsAllRuns=FALSE){
  #initPlotting <- function(rasterRes=c(0.5,0.5), checkPlotsAllRuns=FALSE){
  
  # Initialise or reset the plotting object.
  
  # Create the plotting object.
  plotting <- list(#rasterRes=NULL,          # Two intensity plots use rasters which need a resolution.
                    #------------------------------
                    # Whether to plot Check input PLOTS
                    #------------------------------
                    doCheckPlots=FALSE,       # If TRUE, sets all check plots to TRUE, leaves them as set otherwise.
                    checkPlotsAllRuns=FALSE,  # Plot the check plots on every run (TRUE) or only the first (FALSE)
                    envirData=FALSE,          # Plot the raster of environmental data.
                    maskData=FALSE,           # Plot the mask raster created from the environmental data.
                    biasData=FALSE,           # Plot the raster of sample bias data.
                    domain=FALSE,             # Plot the domain of the simulation.
                    probObs=FALSE,            # Value of the probability of observation (b) function at the specified resolution.
                    intensity=FALSE,          # Value of intensity (lambda) function at the specified resolution.
                    biasIntensity=FALSE,      # Value of sample biased intensity (lambda*b) function at the specified resolution.
                    countPerCell=FALSE,       # Number of simulated individuals in each raster cell.
                    surveyAreas=FALSE,        # Number of survey locations per cell.
                    countData=FALSE,          # Number of simulated individuals detected in each survey area, per species.
                    PAData=FALSE,             # Presence or absence of individuals in each survey area, per species.
                    POData=FALSE,             # Simulated individuals and those that are retained in the PO data.
                    BGPoints=FALSE,           # Background points created for multispeciesPP method.
                    trueCoeffEstimates=FALSE, # plots for generation of 'true' coefficients
                    #------------------------------
                    # Whether to plot Result PLOTS
                    #------------------------------
                    estCoeffs=FALSE,          # The SDM methods estimates of the coefficients.
                    mseCompare=FALSE,         # A comparison of how well the SDMs perform against the true.
                    avgEstIntensity=FALSE,    # Average estimated intensity (across runs).
                    #------------------------------
                    # Settings for all plots ...
                    #------------------------------
                    #coastPoly=NULL,          # OBS: A coastline polygon in the right projection for use in plots.
                    #basePoints=NULL,         # OBS: A research bases points in the right projection for use in plots.
                    #plotToFile=FALSE,        # OBS: Whether or not to plot to a file.
                    device=NULL,              # The graphical device to plot to ("RStudioGD" for screen, "png", "pdf", etc. for file)
                    plotDir="",               # The directory to save the plot files to.
                    fileWidth=NULL,           # File plotting specification for width of plot.
                    fileHeight=NULL,          # File plotting specification for height of plot.
                    fileUnits=NULL,           # File plotting units.
                    fileRes=NULL,             # File plotting resolution.
                    #------------------------------
                    # Settings for a plot ...
                    #------------------------------
                    fileName="",              # File to plot to if device indicates file plotting.
                    mainTxt="",               # Text to use as a title on a plot.
                    subTxt="",                # Text to use as a sub title on a plot (FYI: under x-axis NOT under title!)
                    zlim=NULL,                # Limit on z-axis plotting (colour of points for image plots), e.g. zlim=c(0,20).
                    #------------------------------
                    # plot layers ...
                    #------------------------------
                    coastLayer=NULL,          # A plot layer that holds the coast line information
                    baseLayer=NULL,           # A plot layer that holds the base locations
                    baseNamesLayer=NULL,      # A plot layer that holds the base names and plotting position
                    isError=FALSE             # whether or not there has been an error within a function
  )
  
  # Set resolution value.
  #plotting$rasterRes <- rasterRes
  
  # Turn on all check plotting?
  plotting$doCheckPlots <- doCheckPlots
  
  # Set whether or not to plot the check plots on every run.
  plotting$checkPlotsAllRuns <- checkPlotsAllRuns
  
  # Return value.
  return(plotting)
}

#-----------------------------------------------------------------------------------------

setPlotsResults <- function(plotLst, estCoeffs=TRUE, mseCompare=TRUE, avgEstIntensity=TRUE) {
  
  # Set the values in the list from the arguments in the function call.
  plotLst$estCoeffs <- estCoeffs
  plotLst$mseCompare <- mseCompare
  plotLst$avgEstIntensity <- avgEstIntensity

  # Return value.
  return(plotLst)
  
}

#-----------------------------------------------------------------------------------------

setPlotsCheck <- function(plotLst, envirData=TRUE, maskData=FALSE, biasData=TRUE, domain=FALSE, 
                          intensity=FALSE, biasIntensity=FALSE, countPerCell=TRUE, 
                          surveyAreas=FALSE, countData=TRUE, 
                          PAData=FALSE, POData=TRUE, BGPoints=FALSE, probObs=FALSE,
                          trueCoeffEstimates=TRUE) {
  
  # Set the plot check values.  Remember that the "doCheckPlots" value can override these settings!
  
  if ( plotLst$doCheckPlots ) {
    plotLst$envirData <- TRUE
    plotLst$biasData <- TRUE
    plotLst$domain <- TRUE
    plotLst$intensity <- TRUE
    plotLst$biasIntensity <- TRUE
    plotLst$countPerCell <- TRUE
    plotLst$surveyAreas <- TRUE
    plotLst$countData <- TRUE
    plotLst$PAData <- TRUE
    plotLst$POData <- TRUE
    plotLst$BGPoints <- TRUE
    plotLst$probObs <- TRUE
    plotLst$trueCoeffEstimates <- TRUE
  } else {
    # Set the values in the list from the arguments in the function call.
    plotLst$envirData <- envirData
    plotLst$biasData <- biasData
    plotLst$domain <- domain 
    plotLst$intensity <- intensity
    plotLst$biasIntensity <- biasIntensity
    plotLst$countPerCell <- countPerCell
    plotLst$surveyAreas <- surveyAreas
    plotLst$countData <- countData
    plotLst$PAData <- PAData
    plotLst$POData <- POData
    plotLst$BGPoints <- BGPoints
    plotLst$probObs <- probObs
    plotLst$trueCoeffEstimates <- trueCoeffEstimates
  }
  
  # Return value.
  return(plotLst)
  
}

#-----------------------------------------------------------------------------------------

setPlotsLayers <- function(plotLst, simProj, coastPoly=NULL, basePoints=NULL, 
                           baseNamesCol="baseShort", basePlotPosCol="plotPos") {
  
  # Set the coastline SpatialPolygons object.  Project it first, if its projection is 
  # different.  Set the base locations SpatialPointsDataFrame object.  Project it first, 
  # if its projection is different.  If present, the base names and/or a plotting position
  # can be given by specifying the column/s in the basePoints data where this info will be
  # found.

  # Is there a coast line polygon?  
  if ( ! is.null(coastPoly) ) {
    # See if the coastline polygon needs to be projected.
    if ( ! compareCRS(coastPoly@proj4string, simProj) ) {
      # Project first, then set.
      coastPoly <- spTransform(coastPoly, simProj)
    }
    
    # Make plot layer for the coastline.
    plotLst$coastLayer <- setPlotLayer(coastPoly, asp=1)
  }

  # Is there research base information?
  if ( ! is.null(basePoints) ) {
    # See if the research base points need to be projected.
    if ( ! compareCRS(basePoints@proj4string, simProj) ) {
      # Project first, then set.
      basePoints <- spTransform(basePoints, simProj)
    }
    
    # Make plot layer for the research base location points.
    plotLst$baseLayer <- setPlotLayer(basePoints, pch="*", col="black")

    # Extra layer for names of bases (uses base name plotting position info, if available)
    if ( baseNamesCol %in% names(basePoints@data) ) {
      plotLabels <- basePoints@data[ ,baseNamesCol]
      
    } else {
      numBases <- dim(basePoints@coords)[1]
      plotLabels <- paste0("base", 1:numBases)
      
    }
    if ( basePlotPosCol %in% names(basePoints@data) ) {
      plotPos <- basePoints@data[ ,basePlotPosCol]
      
    } else {
      plotPos <- 1
      
    }
    
    # Make plot layer for the research base names.
    plotLst$baseNamesLayer <- setPlotLayer(basePoints, plotfunc="text", 
                                           labels=plotLabels, pos=plotPos, cex=0.5)
  }
  
  # Return value.
  return(plotLst)
  
}

#-----------------------------------------------------------------------------------------

setPlotsToFile <- function(plotLst, device=options()$device, plotDir=getwd(), 
                           fileName=makeFileName("myPlot%03d", plotDir, device), 
                           fileWidth=900, fileHeight=700, fileUnits = "px", fileRes = NA) {
  
  # What is the plot to the screen device called?
  screenDevice <- "RStudioGD"
  
  # Set values.
  plotLst$device <- device
  plotLst$plotDir <- plotDir
  plotLst$fileName <- fileName
  plotLst$fileWidth <- fileWidth
  plotLst$fileHeight <- fileHeight
  plotLst$fileUnits <- fileUnits
  plotLst$fileRes <- fileRes

  # Create directory, if necessary.
  if ( plotLst$device != screenDevice ) {
    if ( ! dir.exists(plotDir) ) {
      dir.create(plotDir, recursive = TRUE)
    }
  }
  
  # Return value.
  return(plotLst)
  
}

#-----------------------------------------------------------------------------------------

makeDomainLayers <- function(domain, withExt=TRUE, withPoly=TRUE, withOwin=FALSE) {
  
  # Put a few layers together ready for plotting.
  
  # Make the first layer where the plot area is initialised.
  firstLayer <- setPlotLayer(x=c(domain$ext@xmin, domain$ext@xmax), 
                           y=c(domain$ext@ymin, domain$ext@ymax), 
                           plotfunc="plot", type="n", asp=1, xlab="", ylab="")
  domainLayers <- setPlotLayers(firstLayer)
  
  # Add the owin.
  if ( withOwin ) domainLayers <- addPlotData(domainLayers, domain$owin, col="blue")
    #plot(domain$owin, add=TRUE, col="blue")
  
  # Add the extent.
  if ( withExt ) domainLayers <- addPlotData(domainLayers, domain$ext, plotfunc = "rect")
    #rect(domain$ext@xmin, domain$ext@ymin, domain$ext@xmax, domain$ext@ymax)
  
  # Add the polygon.
  if ( withPoly ) domainLayers <- addPlotData(domainLayers, domain$poly, border="red")
    #plot(domain$poly, border="red", add=TRUE)
  
#   # Add the titles.
#   if ( withTitles ) {
#     titleTxt = plotObj$mainTxt    #"Simulation domain"
#     subTxt = plotObj$subTxt     # "Blue is the included area, white the excluded area, black the extent, and red the polygon."
#   } else {
#     titleTxt = NULL
#     subTxt = NULL
#   }

  # Return these layers.
  return(domainLayers)

}

#-----------------------------------------------------------------------------------------

plotDomain <- function(plotObj, domain, withExt=TRUE, withPoly=TRUE, withOwin=FALSE){

  # Plot the domain using the given objects to the device specified in plotObj.

  # Set the file name and titles.
  fileName = makeFileName("CheckDomain", plotObj$plotDir, plotObj$device)   #paste("CheckDomain.", plotObj$device, sep="")
  titleTxt = "Simulation domain"
  subTxt = "Blue is the included area, white the excluded area, black the extent, and red the polygon."

  # Make the plotting layers for the domain.
  domainLayers <- makeDomainLayers(domain, withExt, withPoly, withOwin)

  # Plot the domain
  plot.plotLayers(domainLayers, plotObj$device, fileName, plotObj$fileHeight,
                  plotObj$fileWidth, titleTxt, subTxt)

}

#-----------------------------------------------------------------------------------------

makeIntensityLayers <- function(plotObj, rlIntensity, zlimIntensity=NULL, 
                                dataPoints=NULL, dataPointsPch="+", dataPointsCol="red") {
  
  # Put together an intensity with coastline and research base points, if available, ready
  # for plotting.  Returns a plotLayers object that can be plotted using plot.PlotLayers.
  #
  # Arguments ...
  # plotObj:       a plotting object that contains the plot settings for this plot.
  # rlIntensity:   a RasterLayer object that contains the intensity values.  
  # zlimIntensity: the zlim to use for the intensity plot layer.
  # dataPoints:    extra data points to add to plot, if available.  An object such that 
  #                dataPoints$x and dataPoints$y exist.
  # dataPointsPch: the pch to use for the data points plot layer (see graphical parameters)
  # dataPointsCol: the col to use for the data points plot layer (see graphical parameters)

  # User supplied intensity max for plot?
  if ( is.null(zlimIntensity) ) 
    zlimIntensity <- c(min(0, minValue(rlIntensity)), maxValue(rlIntensity))

  # Start the plot with the intensity layer ...
  lambdaLayer <- setPlotLayer(rlIntensity, asp=1, zlim=zlimIntensity)
  intensityLayers <- setPlotLayers(lambdaLayer)
  
  # Add the coastline, if available.
  if ( ! is.null(plotObj$coastLayer) ) 
    intensityLayers <- addPlotLayer(intensityLayers, plotObj$coastLayer)
  
  # Add the base locations, if available.
  if ( ! is.null(plotObj$baseLayer) )
    intensityLayers <- addPlotLayer(intensityLayers, plotObj$baseLayer)
  
  # Add base names, if available.
  if ( ! is.null(plotObj$baseNamesLayer) ) 
    intensityLayers <- addPlotLayer(intensityLayers, plotObj$baseNamesLayer)

  # Add other dataPoints, if given.
  if ( ! is.null(dataPoints) ) {
    # NB: using points makes no difference to the plotting issue of some of the individuals
    # looking like they are outside the domain!
    intensityLayers <- addPlotData(intensityLayers, dataPoints$x, dataPoints$y, "points", 
                                   pch=dataPointsPch, col=dataPointsCol)
  }
  
  # Return value
  return(intensityLayers)
}

#-----------------------------------------------------------------------------------------

plotCellValues <- function(plotObj, mask, cells, cellVals, thisRun=NULL, 
                                  fileNameStart="Intensity", titleTxts=NULL){

  # Plot each species' cell values (e.g. from cell object, cellVals = cellObj$truePrObs).
  #
  # Arguments ...
  # plotObj:   a plotting object.
  # mask:      a RasterLayer that contains the mask that creates the domain.
  # cells:     a vector that contains the cell numbers or mask for which there are values
  #            in cellVals.  NB: numCells <- length(cells)
  # cellVals:  a matrix that contains the values to be plotted for each species (numCells x numSpecies)
  # thisRun:   the number of the simulation run to be added to the plot as a sub-title, if
  #            available.  Also added to each plot's file name when plotting to file.
  # fileNameStart: start of the file name if plotting to a file.  File name will have the
  #            species number appended within the function (so they are unique) and then 
  #            the extension ".png".
  # titleTxts: title to give to each species plot (a vector of character strings).
  
  # Get the species stuff
  namesSpecies <- colnames(cellVals)
  numSpecies <- length(namesSpecies)
  numCells <- length(cells)
  if ( numCells != dim(cellVals)[1] ) {
    stop("Unable to plot cell values as arguments are of different length.")
  }
    
  # What should the file name be?
  device <- plotObj$device
  if ( is.null(thisRun) ) {
    fileNames <- paste0(fileNameStart, "-", namesSpecies)
  } else {
    fileNames <- paste0(fileNameStart, "-", namesSpecies, "-run", thisRun)
  }
  fileNames <- makeFileName(fileNames, plotObj$plotDir, plotObj$device)
  
  # What are the titles for the plots?
  if ( is.null(titleTxts) ) {
    titleTxts <- paste0("Cell values for ", namesSpecies, sep="")
  } else if ( length(titleTxts) == 1 ) {
    titleTxts <- paste0(titleTxts, namesSpecies, sep=" ")
  } else {
    # Use what was given.
  }
  
  # Should there be a sub title?
  if ( is.null(thisRun) ) {
    subTxt = ""
  } else {
    subTxt <- paste("Simulation run = ", thisRun, sep="")
  }
  
  # Set up plotting raster.
  rlValues <- mask
  
  # Make a plot for each species.
  for ( k in 1:numSpecies ) {
    # This species name is ...
    species <- namesSpecies[k]
    
    # Calculate this species intensity.
    rlValues[cells] <- cellVals[ ,species]

    # Get the plot layers necessary for an intensity plot.
    layers <- makeIntensityLayers(plotObj, rlValues)
    
    # Plot the layers
    plot.plotLayers(layers, device, fileNames[k], plotObj$fileHeight, 
                    plotObj$fileWidth, titleTxts[k], subTxt, fileUnits = plotObj$fileUnits,
                    fileRes = plotObj$fileRes)
  }
  
}

#-----------------------------------------------------------------------------------------

plotBGPoints <- function(plotObj, BG, domain, cells) {
  
  # Plot the background points (with the domain and extent) to the device specified in plotObj.
  
  # Set the file name and titles.
  fileName = makeFileName("CheckBGPoints", plotObj$plotDir, plotObj$device)  
  titleTxt = "Generated background points"
  subTxt = "Domain in red, background points in black"
  
  # Get the domain layers.
  BGLayers <- makeDomainLayers(domain)
  
  # Add the background points as a layer.
  BGLayers <- addPlotData(BGLayers, BG$x, BG$y, plotfunc="points", pch=".")
  #points(BG$x, BG$y, pch=".")

  # Plot the background points.
  plot.plotLayers(BGLayers, plotObj$device, fileName, plotObj$fileHeight, plotObj$fileWidth,
                  titleTxt, subTxt)
  
  # Cell plot.  TO DO: which plot do I need?  The one above or the one below this line?
  rlplot <- domain$mask
  rlplot[cells] <- 0
  numBGPerCell <- table(BG$cell)
  rlplot[as.integer(names(numBGPerCell))] <- numBGPerCell
  myZlim <- c(0, max(numBGPerCell))
  plot(rlplot, asp=1, main="Number of background points per cell", zlim=myZlim)
  
}

#-----------------------------------------------------------------------------------------
# 
# plotSpeciesIntensity <- function(domain, plotObj, myFormula, coeffs, covars, community=NULL, 
#                                  thisRun=NULL, maxLambda=NULL, fileNameStart="Intensity"){
#   
#   # Plot each species' intensity values.  Assumes there is a lambda function available.
#   # Can also plot the locations of the individuals in each species of the community.
#   #
#   # Arguments ...
#   # domain:    a domain object that contains extent and mask
#   # plotObj:   a plotting object.
#   # myFormula: a formula object that contains how to combine the information for an 
#   #            intensity function.
#   # coeffs:    the coefficients for each species (number of columns equals number of species)
#   # covars:    the covariates for the intensity function (same for all species).
#   # community: a community object whose species populations are to be added to the plot as
#   #            points, if available.  It contains all species locations.
#   # thisRun:   the number of the simulation run to be added to the plot as a sub-title, if
#   #            available.  Also added to each plot's file name when plotting to file.
#   # maxLambda: maximum value to use when plotting each intensity (same for all species).
#   # fileNameStart: start of the file name if plotting to a file.  File name will have the
#   #            species number appended (so they are unique) and then the extension ".png".
# 
#   # Get the number of species.
#   numSpecies <- dim(coeffs)[2]
#   
#   # What should the file name be?
#   device <- plotObj$device
#   if ( is.null(thisRun) ) {
#     fileNames <- paste(fileNameStart, "-sp", 1:numSpecies, ".", device, sep="")
#   } else {
#     fileNames <- paste(fileNameStart, "-sp", 1:numSpecies, "-run", thisRun, ".", device, sep="")
#   }
# 
#   # Get the title for the plot. If available, include simulated individuals.
#   if ( is.null(community) ) {
#     speciesNames <- colnames(coeffs)
#     titleTxts <- paste("True intensity for ", speciesNames, "\n", sep="")
#     speciesLocations <- vector("list", numSpecies)
#   } else {  
#     speciesNames <- community$speciesNames
#     titleTxts <- paste("True intensity with simulated individuals for ", speciesNames, "\n", sep="")
#     speciesLocations <- community$speciesPops
#   }
# 
#   # Should there be a sub title?
#   if ( is.null(thisRun) ) {
#     subTxt = ""
#   } else {
#     subTxt <- paste("Simulation run = ", thisRun, sep="")
#   }
#   
#   # User supplied intensity maximum for plots?
#   if ( is.null(maxLambda) ) {
#     myZlim <- NULL
#   } else {
#     myZlim <- c(0, maxLambda)
#   }
#   
#   # Make a plot for each species.
#   for ( k in 1:numSpecies ) {
#     # Calculate this species intensity.
#     rlIntensity <- rasterFromFunc(extent(covars), res(covars), domainObj$mask, 
#                                   domainObj$maskValue, lambda, myFormula, coeffs, covars)
# 
#     # Get the plot layers necessary for an intensity plot.
#     intensityLayers <- makeIntensityLayers(plotObj, rlIntensity, myZlim, speciesLocations[[k]])
#     
#     # Plot the layers
#     plot.plotLayers(intensityLayers, device, fileNames[k], plotObj$fileHeight, 
#                     plotObj$fileWidth, titleTxts[k], subTxt)
#   }
#   
# }
# 
#-----------------------------------------------------------------------------------------

# plotSpeciesCount <- function(plotObj, mask, cellObj, thisRun=NULL, 
#                              fileNameStart="CellCount"){
#                             
#    #(plotext, plotRes, community, thisRun=NULL){
#   
#   # Plot the number of individuals from a species in each cell.  Do a separate plot for 
#   # each species.  Assumes there is a lambda function available.
#   #
#   # Arguments ...
#   # domain:        a domain object that contains extent and mask
#   # plotObj:       a plotting object.
#   # community:     a community object whose species populations are to be added to the plot 
#   #                as points, if available.  It contains all species locations.
#   # thisRun:       the number of the simulation run to be added to the plot as a sub-title, 
#   #                if available.  Also added to each plot's file name when plotting to file.
#   # fileNameStart: start of the file name if plotting to a file.  File name will have the
#   #                species number appended (so they are unique) and then the extension ".png".
#   
#   # Get the number of species in the community.
#   namesSpecies <- cellObj$namesSpecies
#   numSpecies <- length(namesSpecies)
#   
#   # Make sure the number of individuals has been created for this run (if given).
#   if ( ! is.null(thisRun) && (cellObj$runNum != thisRun) ) {
#     stop("The simulated number of individuals is not from the given run number.")
#   }
# 
#   # Plot file name and sub title.  
#   if ( is.null(thisRun) ) {
#     subTxt = ""
#     fileNames <- paste0(fileNameStart, "-", namesSpecies)
#   } else {
#     subTxt <- paste("Simulation run = ", thisRun, sep="")
#     fileNames <- paste0(fileNameStart, "-", namesSpecies, "-run", thisRun)
#   }
#   fileNames <- makeFileName(fileNames, plotObj$plotDir, plotObj$device)
#   names(fileNames) <- namesSpecies
#   
#   # Initialise raster template (NB: cell values are all NA!)
#   rlplot <- mask
#   
#   # Make a plot for each species.
#   for ( species in namesSpecies ) {
#     # Get the number of individuals for this species.
#     rlplot[cellObj$cells] <- cellObj$N[ ,species]
# 
#     # Plot title.
#     titleTxt <- paste0("Count per cell with simulated individuals for ", species, "\n")
# 
#     # Get the intensity layers ready for plotting.
#     intensityLayers <- makeIntensityLayers(plotObj, rlplot) 
# 
#     # Plot.
#     plot.plotLayers(intensityLayers, plotObj$device, fileNames[species], plotObj$fileHeight,
#                     plotObj$fileWidth, titleTxt, subTxt)
#   }
# 
# }
# 
#-----------------------------------------------------------------------------------------

plotPOPoints <- function(plotObj, mask, cellObj, PO, thisRun, fileNameStart="") {
  
  # Plot the number of presence-only points per cell for each species.

  # fileNameStart: start of the file name if plotting to a file.  File name will have the
  #                species number appended within the function (so they are unique) and 
  #                then the extension ".png".
  
  # How many species?
  namesSpecies <- cellObj$namesSpecies
  numSpecies <- length(namesSpecies)
  
  # What are the file names?
  fileNames <- paste0(fileNameStart, "-", namesSpecies, "-run", thisRun)
  fileNames <- makeFileName(fileNames, plotObj$plotDir, plotObj$device)
  
  # What is the sub title?
  subTxt <- paste("Simulation run = ", thisRun, sep="")
  
  # Initialise plot raster.
  rlplot <- mask
  
  # Work out plot for each species.
  for ( k in 1:numSpecies ) {
    # Reset all cells in the domain.
    rlplot[cellObj$cells] <- 0
    
    # Species name.
    species <- namesSpecies[k]
    
    # Add titles.
    titleTxt <- paste0("Simulated number of PO observations for ", species, " per cell.")

    # Set the plot specific stuff
    #plotObj <- setPlotSingle(plotObj, titleTxt, subTxt, NULL, fileName)
    
    # Presence-only numbers per cell for this species.
    indSpeciesPO <- which(PO$species == species)
    POCellsSpecies <- PO$cell[indSpeciesPO]
    numPOPerCell <- table(POCellsSpecies)
    rlplot[as.integer(names(numPOPerCell))] <- numPOPerCell
  
    # Make the plot layers for this plot.
    POLayers <- makeIntensityLayers(plotObj, rlplot)
    
    # Plot the layers.
    plot.plotLayers(POLayers, plotObj$device, fileNames[k], plotObj$fileHeight,
                    plotObj$fileWidth, titleTxt, subTxt)
    # Make the species specific intensity plot.
 #   plotPoints(domain, plotObj, community$speciesPops[[k]], thisPO, "o", "red") 
    
  }

}

#-----------------------------------------------------------------------------------------
 
plotSurveyAreas <- function(plotObj, mask, cellObj, surveyObj, fileName="SurveyAreas",
                            plotTitle = "Simulated number of samples per cell") {
  
  # Plot the number of samples per cell.  The "patches" will show the survey areas.
  
  # Initialise plotting raster.
  rlplot <- mask
  rlplot[cellObj$cells] <- 0
  
  # Get the number of surveys per cell.
  numSurveyPerCell <- table(surveyObj$cells)
  
  # Set this in the plotting raster.
  rlplot[as.integer(names(numSurveyPerCell))] <- numSurveyPerCell

  # Set up the layers for plotting.
  intensityLayer <- makeIntensityLayers(plotObj, rlplot)
  
  # Plot the layers
  fileName <- makeFileName(fileName, plotObj$plotDir, plotObj$device)
  plot.plotLayers(intensityLayer, plotObj$device, fileName, plotObj$fileHeight, 
                  plotObj$fileWidth, main = plotTitle)

}  

#-----------------------------------------------------------------------------------------

plotSurveyCount <- function(domain, plotObj, count, namesSpecies, thisRun, 
                            fileNameStart="CountData") {
  
  # Plot the count of each species that has been surveyed (detected!).
  # NB: Each cell can contain more than one survey, so count per survey needs to be summed 
  #     to cell level for each species.
  
  # Numbers of things.
  numSurveys <- dim(count)[1]
  cellsWithSurveys <- unique(count$cell)
  numCellsWithSurveys <- length(cellsWithSurveys)
  
  # What are the file names?
  fileNames <- paste0(fileNameStart, "-", namesSpecies, "-run", thisRun)
  fileNames <- makeFileName(fileNames, plotObj$plotDir, plotObj$device)
  names(fileNames) <- namesSpecies
  
  # What is the sub title?
  subTxt <- paste("Simulation run = ", thisRun, sep="")

  # # Create colour palette.
  # maxCount <- max(count[ ,namesSpecies], na.rm=TRUE)
  # countColours <- rgb(0,0,0,1)                                    # Add black for count=0.  
  # if ( maxCount > 0 ) {
  #   countColours <- c(countColours, topo.colors(maxCount))       
  # }
  
  # Blank raster to use as template when plotting.
  rlplot <- domain$mask
  cells <- cellFromMask(domain$mask, domain$mask, domain$maskValue)
  rlplot[cells] <- 0
  
  for ( species in namesSpecies ) {
    # Reset the value of the cells.
    rlplot[cellsWithSurveys] <- NA  # 0
    
    # Title for this plot.
    titleTxt <- paste0("Simulated count data in survey areas for ", species)
    
    # Start the plot for this species.
    surveyLayers <- makeDomainLayers(domain)
    
    # Sum to cell level.
    countPerCell <- vector("integer", numCellsWithSurveys)
    for ( i in 1:numCellsWithSurveys ) {
      # Which surveys fall in this cell.
      cell <- cellsWithSurveys[i]
      indSurveysInCell <- which(count$cell == cell)
      
      # Sum these surveys' counts.
      countPerCell[i] <- sum(count[indSurveysInCell,species])
    }
    
    # Add the survey areas layer.
    rlplot[cellsWithSurveys] <- countPerCell
    surveyLayers <- addPlotData(surveyLayers, rlplot, plotfunc="plot")

    plot.plotLayers(surveyLayers, plotObj$device, fileNames[species], plotObj$fileHeight, 
                    plotObj$fileWidth, titleTxt, subTxt)

  }
  
}

#-----------------------------------------------------------------------------------------

plotSurveyPA <- function(domain, plotObj, surveys, PA, namesSpecies, thisRun, 
                            fileNameStart="PAData") {
  
  # Plot the presence or absence of each species that has been surveyed (detected!).
  
  # Numbers of things.
  numSurveys <- surveys$numSurveys
  
  # What are the file names?
  fileNames <- paste0(fileNameStart, "-", namesSpecies, "-run", thisRun)
  fileNames <- makeFileName(fileNames, plotObj$plotDir, plotObj$device)
  names(fileNames) <- namesSpecies
  
  # What is the sub title?
  subTxt <- paste("Simulation run = ", thisRun, sep="")
  
  # Blank raster to use as template when plotting.
  rlplot <- domain$mask
  cells <- cellFromMask(domain$mask, domain$mask, domain$maskValue)
  
  for ( species in namesSpecies ) {
    # Reset the value of the cells.
    rlplot[cells] <- NA  # 0
    
    # Title for this plot.
    titleTxt <- paste0("Simulated presence-absence data in survey areas for ", species)
    
    # Start the plot for this species.
    surveyLayers <- makeDomainLayers(domain)
    
    # Add the survey areas layer.
    rlplot[surveys$cells] <- PA[ ,species]
    surveyLayers <- addPlotData(surveyLayers, rlplot, plotfunc="plot")

    # Make the plot.
    plot.plotLayers(surveyLayers, plotObj$device, fileNames[species], plotObj$fileHeight, 
                    plotObj$fileWidth, titleTxt, subTxt)
    
  }
  
}

#-----------------------------------------------------------------------------------------

# plotSurveyPA <- function(domain, surveys, community, PA, thisRun, fileNameStart="PAData",
#                          isSymbols=TRUE) {
# 
#   # Plot the presence or absence of each species (that has been detected!) in each survey 
#   # area.  Will plot either black and white boxes that are the area of the survey OR 
#   # symbols "P" and "A" for present or absent at the survey location (centre of survey area).
#   
#   # Numbers of things.
#   numSpecies <- community$numSpecies
#   numSurveys <- surveys$numSurveys
# 
#   # What are the file names?
#   fileNames <- paste0(fileNameStart, "-sp", 1:numSpecies, "-run", thisRun)
#   fileNames <- makeFileName(fileNames, plotObj$plotDir, plotObj$device)
#   
#   # What is the sub title?
#   subTxt <- paste("Simulation run = ", thisRun, sep="")
#   
#   # Set up the plotting device (symbol or colour)
#   if ( isSymbols ) {
#     # A (absence) or P (presence)!
#     PASymbols <- c("A", "P")
#     PASymbolCols <- c("black", "blue")
#   } else {
#     # White (absence) or black (presence)!
#     PAColours <- c(rgb(1,1,1,1), rgb(0,0,0,1)) 
#   }
#   
#   for ( k in 1:numSpecies ) {
#     # Start the plot for this species.
#     #plotDomain(domain)
#     surveyLayers <- makeDomainLayers(domain)
# 
#     # Add the locations of the individuals from this species.
# #    plot(community$speciesPops[[k]], pch=".", add=TRUE)
#     
#     # Plot check of presence-absence for each species.
#     if ( isSymbols ) {
#       surveyLayers <- addPlotData(surveyLayers, surveys$locations$x, surveys$locations$y,
#                                   plotfunc="points", pch=PASymbols[PA[ ,k+3]+1], 
#                                   col=PASymbolCols[PA[ ,k+3]+1])
# #       points(surveys$locations$x, surveys$locations$y, pch=PASymbols[PA[ ,k+3]+1], 
# #              col=PASymbolCols[PA[ ,k+3]+1])
#     } else {
#       # Add the survey areas.
#       surveyLayers <- addPlotData(surveyLayers, surveys$extents, plotfunc="rect", 
#                                   col=PAColours[PA[ ,k+3] + 1])
# #       rect(surveys$extents$xmin, surveys$extents$ymin, surveys$extents$xmax, 
# #            surveys$extents$ymax, col=PAColours[PA[ ,k+3] + 1])    # + 1 for zero!
#     }
#     
#     # Add titles.
#     titleTxt <- paste("Presence-absence data in survey areas for ", community$speciesNames[k], ".", sep="")
# #    subTxt <- paste("Simulation run = ", thisRun, sep="")
# #    title(main=titleTxt, sub=subTxt)
#     
#     # Add a legend.
#     #uniqueCounts <- sort(unique(count[ ,k+3]))
#     if ( isSymbols ) {
#       surveyLayers <- addPlotData(surveyLayers, "bottomleft", plotfunc="legend", 
#                                   legend=c("Absent","Present"), pch=PASymbols, 
#                                   col=PASymbolCols, title="species")
# #       legend("bottomleft", legend=c("Absent","Present"), pch=PASymbols, col=PASymbolCols, 
# #              title="species")
#     } else {
#       surveyLayers <- addPlotData(surveyLayers, "bottomleft", plotfunc="legend", 
#                                   legend=c("Absent","Present"), fill=PAColours, title="species")
#       # legend("bottomleft", legend=c("Absent","Present"), fill=PAColours, title="species")
#     }
#   
#     # Plot.
#     plot.plotLayers(surveyLayers, plotObj$device, fileNames[k], plotObj$fileHeight, 
#                     plotObj$fileWidth, titleTxt, subTxt)
#   }
# 
# }

#-----------------------------------------------------------------------------------------

makePlotCoeffs <- function(resLst, trueCoeffs, namesSpecies, plotObj, 
                           fileNameStart="EstCoeffsSpecies", trueGamma=NULL) {
  
  # Plot to compare estimated coefficients to true coefficients, for each species.  
  # Plot different methods along x-axis and a separate plot per species.
  # Assumes this plot has been requested in the plot list object.
  #
  # Arguments ...
  # resLst:       the result object list
  # trueCoeffs:   a matrix with a column for each species and rows that are alpha, beta1, 
  #               ..., betaN, where N is the number of covariates.
  # namesSpecies: a vector of character strings that contains the species names
  # trueGamma:    a vector with a the true value of gamma (bias intercept) for each species
  
  # Numbers of things.
  numCoeffs <- resLst$numCoeffs
  numRuns <- resLst$numRuns
  numSpecies <- resLst$numSpecies
  numValidSDMs <- length(resLst$validSDM)
  
  # How many SDMs were tried?
  if ( numValidSDMs == 0 ) {
    #resLst$isError <- TRUE
    stop("Unable to plot estimated coefficients as no SDMs were tried.")
  }
  
  # Make sure there are the same number of coefficients.
  if ( dim(trueCoeffs)[1] != numCoeffs ) {
    #resLst$isError <- TRUE
    stop("There are not the same number of true coefficients as estimated coefficients.")
  }
  
  # Make sure there are the same number of species in the true coefficients.
  if ( dim(trueCoeffs)[2] != numSpecies ) {
    #resLst$isError <- TRUE
    stop("There are not the same number of species in the true coefficients as there are in the estimated coefficients.")
  }
  
  # Make sure there are the same number of species names.
  if ( length(namesSpecies) != numSpecies ) {
    #resLst$isError <- TRUE
    stop("There are not the same number of species names as there are in the results.")
  }
  
  # Set the x-axis stuff, same for all plots.
  xnames <- resLst$validSDM
  xlabel <- "SDMs"
  
  # Want a plot for each species and each coefficient.
  for ( species in namesSpecies )  {
    
    # For this species, plot coefficients for each SDM and simulation run.
    for ( b in 1:numCoeffs ) {
      nameCoeff <- rownames(trueCoeffs)[b]
      
      # Get the data required for the boxplot into a single matrix.
      plotData <- matrix(nrow=numRuns, ncol=numValidSDMs)
      for ( j in 1:numValidSDMs ) {
        nameSDM <- resLst$validSDM[j]
        plotData[ ,j] <- resLst[[nameSDM]]$coeffs[b,species, ]
      }
      
      # Are we plotting to a file?
      thisFileName <- paste0(fileNameStart, "-", species, "-", nameCoeff)  
      thisFileName <- makeFileName(thisFileName, plotObj$plotDir, plotObj$device)

      # Make layers for the plot.
      layers <- setPlotLayers()
      layers <- addPlotData(layers, plotData, plotfunc="boxplot", names=xnames)
      layers <- addPlotData(layers, list(h=trueCoeffs[b,species]), plotfunc="abline", lty=3)
      # if ( (b == 1) && (! is.null(trueGamma)) ) {
      #   # sample biased intensity intercept ...
      #   biasedIntercept <- trueCoeffs[1,species] + trueGamma[species]
      #   layers <- addPlotData(layers, list(h=biasedIntercept), plotfunc="abline", lty=2)
      #   layers <- addPlotData(layers, list(h=trueGamma[species]), plotfunc="abline", lty=4)
      # }
      
      # Plot this beta for this species ...
      titleTxt <- paste0("Estimated ", nameCoeff, " for ", species, " data.")
      
      # Plot the layers.
      plot.plotLayers(layers, plotObj$device, thisFileName, plotObj$fileHeight, 
                      plotObj$fileWidth, titleTxt, xlab=xlabel)
      
    }
  }
  
}

#-----------------------------------------------------------------------------------------

makePlotsErrorStats <- function(resLst, namesSpecies, plotObj, stat="mse", logYaxis=TRUE,
                                fileNameStart="MSESpecies", titleStart="Mean square error for ") {
  
  # Plot to compare estimated intensity function value to true value, for each species.  
  # Plot different SDM methods along x-axis and a separate plot for each species.
  # Assumes this plot has been requested in the plot list object.
  #
  # Arguments ...
  # resLst:       the result list object
  # namesSpecies: a vector of character strings that contains the species names
  # plotObj:      a plotting object 
  # stat:         statistic that is to be plotted (from results object)
  # logYaxis:     whether or not to plot the y axis on a log scale.
  
  # Numbers of things.
  numRuns <- resLst$numRuns
  numSpecies <- resLst$numSpecies
  numValidSDMs <- length(resLst$validSDM)
  
  # How many SDMs were tried?
  if ( numValidSDMs == 0 ) {
    #resLst$isError <- TRUE
    stop("Unable to plot error statistic as no SDMs were tried.")
  }
  
  # Make sure there are the same number of species names.
  if ( length(namesSpecies) != numSpecies ) {
    #resLst$isError <- TRUE
    stop("There are not the same number of species names as there are in the results.")
  }
  
  # Set the x-axis stuff, same for all plots.
  xnames <- resLst$validSDM
  xlabel <- "SDMs"
  if ( logYaxis ) {
    ylog <- "y"
    ylabel <- paste0("log10(", toupper(stat), ")")
  } else {
    ylog <- ""
    ylabel <- toupper(stat)
  }
  
  # Want a plot for each species.
  for ( species in namesSpecies )  {
    # Get the data required for the boxplot into a single matrix.
    plotData <- matrix(nrow=numRuns, ncol=numValidSDMs, 
                       dimnames=list(1:numRuns, resLst$validSDM))
    for ( sdm in resLst$validSDM ) {
      plotData[ ,sdm] <- resLst[[sdm]][[stat]][species, ]
    }
    
    # Are we plotting to a file?
    thisFileName <- paste0(fileNameStart, "-", species)  
    thisFileName <- makeFileName(thisFileName, plotObj$plotDir, plotObj$device)

    # Make layers for the plot.
    layers <- setPlotLayers()
    layers <- addPlotData(layers, plotData, plotfunc="boxplot", names=xnames, log=ylog)

    # Plot the data.
    titleTxt <- paste0(titleStart, species, " data.")

    # Plot the layers.
    plot.plotLayers(layers, plotObj$device, thisFileName, plotObj$fileHeight, 
                    plotObj$fileWidth, titleTxt, xlab=xlabel, ylab=ylabel)
     
  }
  
}

#-----------------------------------------------------------------------------------------

makePlotsAvgEstimate <- function(domainObj, plotObj, myFormula, resLst, cellObj,
                                fileNameStart="avgEstIntensity" ) {
  
  # Make a raster plot that is the average (in each cell of the covar data) of each 
  # simulation run's intensity estimation.  Do a separate plot for each SDM method used 
  # and each species.  
  #
  # Arguments ...
  # domainObj:    a domain object that contains the domain mask RasterLayer
  # plotObj:      a plotting object 
  # myFormula:    a formula object that contains how to combine the information for an 
  #               intensity function.
  # resLst:       the result list object (contains numbers of things and estimated coeffs)
  # cellObj:      a cell object that contains the valid cells in the domain and the 
  #               environmental covariate values in these cells.
  # fileNameStart: start of the file name if plotting to a file.  File name will have the
  #               species number appended (so they are unique) and then the extension ".png".
  
  
  # Numbers of things.
  numRuns <- resLst$numRuns
  numSpecies <- resLst$numSpecies
  numValidSDMs <- resLst$numSDMs
  namesSpecies <- resLst$namesSpecies
  
  # How many SDMs were tried?
  if ( numValidSDMs == 0 ) {
    stop("Unable to plot average estimated intensities as no SDMs were tried.")
  }
  
  # Set up results storage.
  avgIntensity <- vector("double", cellObj$numCells)
  rsAvgIntensity <- domainObj$mask
  
  # For each SDM method ...
  for ( sdm in resLst$validSDM ) {
    message(paste("Calculate average estimated intensities for SDM:", sdm))
    
    # For each species ...
    for ( species in namesSpecies ) {
      # Initialise average storage.
      avgIntensity[] <- 0
      
      # For each simulation run ...
      numErrors <- 0
      for ( i in 1:numRuns ) {
        if ( isSimError(resLst$errors,i,sdm,species) ) {
          # There was an error during execution, don't use this run.
          numErrors <- numErrors + 1
        } else {
          # Get the estimated coefficients for this run, this species and this SDM method.
          estCoeffs <- resLst[[sdm]]$coeffs[ ,species,i]
          
          # Calculate intensity and add it to the sum, for each point.
          thisIntensity <- lambda.cell(myFormula, estCoeffs, cellObj$covars)
          avgIntensity <- avgIntensity + thisIntensity
        }
      } # for runs
      
      # Finish calculating average
      if ( numErrors >= numRuns ) {
        # There are no results.
        #rsAvg <- setValues(rsAvg, values=NA)
        avgIntensity[] <- NA
      } else {
        # There have been some results.
        #rsAvg <- rsAvg / (numRuns - numErrors)
        avgIntensity <- avgIntensity / (numRuns - numErrors)
      }
      
      # Convert to raster to facilitate plotting.
      rsAvgIntensity[cellObj$cells] <- avgIntensity
      
      # Which file could we be plotting to?
      fileName <- paste0(fileNameStart, "-",species, "-", sdm)
      fileName <- makeFileName(fileName, plotObj$plotDir, plotObj$device)
      
      # What is the plot title?
      titleTxt <- paste("Average estimated intensity for", species, "and", sdm, sep=" ")

      # What is the intensity limit for plotting?
      myzlim <- c(0, max(avgIntensity))

      # Make layers for this plot.
      layers <- makeIntensityLayers(plotObj, rsAvgIntensity, myzlim)
      
      # Plot layers.
      plot.plotLayers(layers, plotObj$device, fileName, plotObj$fileHeight, plotObj$fileWidth,
                      titleTxt)
      
    } # for species
  } # for SDM method
  
}

#-----------------------------------------------------------------------------------------

makePointsLayers <- function(domainObj, plotObj, plotPoints, 
                             extraPoints=NULL, extraPointsPch="+", extraPointsCol="red") {
  
  # Plot locations of objects in the domain with coastline and research base points, if available.
  #
  # Arguments ...
  # domainObj:   a domain object that contains extent and mask
  # plotObj:     a plotting object that contains the plot settings for this plot.
  # extraPoints: extra data points to add to plot, if available.  An object such that 
  #              extraPoints$x and extraPoints$y exist.
  
  # Get the domain layers.
  pointsLayers <- makeDomainLayers(domainObj, withPoly = FALSE)

  # Add the plotPoints as a layer.
  pointsLayers <- addPlotData(pointsLayers, plotPoints, pch=".")
  #plot(plotPoints, pch=".", add=TRUE)
  
  # Add the coastline, if available.
  if ( ! is.null(plotObj$coastLayer) ) 
    pointsLayers <- addPlotLayer(pointsLayers, plotObj$coastLayer) 
  
  # Add the base locations and names, if available.
  if ( ! is.null(plotObj$baseLayer)) {
    pointsLayers <- addPlotLayer(pointsLayers, plotObj$baseLayer)
  }
  if ( ! is.null(plotObj$baseNamesLayer)) {
    pointsLayers <- addPlotLayer(pointsLayers, plotObj$baseNamesLayer)
  }
  
  # Add other extraPoints, if given.
  if ( ! is.null(extraPoints) ) {
    # NB: using points makes no difference to the plotting issue of some of the individuals
    # looking like they are outside the domain!
    pointsLayers <- addPlotData(pointsLayers, extraPoints$x, extraPoints$y, 
                                plotfunc="points", pch=extraPointsPch, col=extraPointsCol)
  }
  
  # Return value
  return(pointsLayers)
}
