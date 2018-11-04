#-----------------------------------------------------------------------------------------

setPlotLayer <- function(x=NULL, y=NULL, plotfunc="plot", ...) {
  
  # Initialise or reset the plotLayer object.  
  #
  # Arguments ...
  # x, y:     object/s to be plotted (assumes that the plotting function will take these)
  # plotfunc: a non-empty character string giving the plotting function to be used to plot 
  #           this layer
  # ...       arguments in tag = value form that will be used by "plotfunc" (e.g. col="red")
  #
  # Value ...
  # Assuming there is not an error, the return value is a list that has two items 
  # plotfunc: as above
  # plotargs: list of arguments and their values to be used in the plotting function.
  
  # Initialise return value.
  plotLayer <- list(plotfunc=NULL, plotargs=list())
  
  # Is it an empty plotLayer? Has the y been used instead of the x?
  if ( is.null(x) && is.null(y) ) {
    # Return empty plotlayer.
    return(plotLayer)
    
  } else if ( is.null(x) && ! is.null(y)) {
    # Swap them around so that x is not null.
    x <- y
    y <- NULL
    
  } else {
    # x is not null but y is.
  }
  
  # Work out what the different plotting function arguments should be.
  if ( plotfunc %in% c("plot", "text", "points", "lines", "boxplot", "legend") ) {
    if ( is.null(y) ) {
      plotargs=list(x=x, ...)
    } else {
      plotargs=list(x=x, y=y, ...)
    }
    
  } else if ( plotfunc == "hist" ) {
    # y has no meaning here.
    plotargs=list(x=x, ...)
    
  } else if ( plotfunc == "rect" ) {
    # Need four bits of information for rect plotting function.  Assumes data held in x!
    # Ways of getting this information from just x.
    if ( inherits(x, "Extent") ) {
      plotargs=list(xleft=x@xmin, ybottom=x@ymin, xright=x@xmax, ytop=x@ymax, ...)
      
    } else if ( inherits(x, c("data.frame", "matrix")) && dim(x)[2] == 4 ) {
      # Assumes has the named columns xmin, xmax, ymin and ymax.
      x <- as.data.frame(x)
      plotargs=list(xleft=x$xmin, ybottom=x$ymin, xright=x$xmax, ytop=x$ymax, ...)
      
    } else if ( inherits(x, "vector") && length(x) == 4 ) {
      # Assumes has the order required by rect function.
      plotargs=list(xleft=x[1], ybottom=x[2], xright=x[3], ytop=x[4], ...)
      
    } else {
      stop("Argument x cannot be converted for use in rect plotting function.")
    }

  } else if ( plotfunc == "abline" ) {
    if ( inherits(x, "list") ) {
      plotargs=c(x, list(...))
      
    } else {
      stop("Argument x cannot be converted for use in abline plotting function.")
    }
    
  } else if ( plotfunc == "axis" ) {
    # More specific info for axis drawing.  FYI: shouldn't be first layer!
    if ( is.null(y) ) {
      plotargs=list(side=x, ...)
    } else {
      plotargs=list(side=x, at=y, ...)
    }

  } else {
    msg <- paste("Object plotLayer not specifically coded for ", plotfunc, 
                 " function.  Take your chances!", sep="")
    warning(msg)
    plotargs <- list(x=x, y=y, ...)
    
  }
    
  # Set info in return value.
  plotLayer$plotfunc <- plotfunc
  plotLayer$plotargs <- plotargs
  
  # Return value.
  return(plotLayer)
}

#-----------------------------------------------------------------------------------------

setPlotLayers <- function(plotLayer=NULL, ...){
  
  # Initialise or reset the plotLayers object.
  
  # Create the empty object.
  plotLayers <- list(plotLayers=list(), nlayers=0, plotargs=list(...))
  
  # Add the plotLayer if it exists.
  if ( ! is.null(plotLayer) ) plotLayers <- addPlotLayer(plotLayers, plotLayer)
  
  # Return value.
  return(plotLayers)
}

#-----------------------------------------------------------------------------------------

addPlotLayer <- function(plotLayers, ...){  
  
  # Add a plotlayer or more to the plotLayers object.
  
  # Check the argument is the right type of objects.
  if ( ! is.plotLayers(plotLayers) ) {
    stop("plotLayers argument is not a valid plotLayers object.")
  }
  
  # How many ... arguments are there?
  dotArgs <- list(...)
  numDotArgs <- length(dotArgs)
  
  # Add each argument.
  for ( i in 1:numDotArgs ) {
    # Check argument is a plotLayer object.
    if ( ! is.plotLayer(dotArgs[[i]]) ) {
      errMsg <- paste("PlotLayer argument", i, "is not a valid plotLayer object.")
      stop(errMsg)
    }
    
    # Update the number of plotLayers.
    plotLayers$nlayers <- plotLayers$nlayers + 1
    
    # Add the plotLayer.
    plotLayers$plotLayers[[plotLayers$nlayers]] <- dotArgs[[i]]
  }
  
  # Return value.
  return(plotLayers)
  
}

#-----------------------------------------------------------------------------------------

addPlotArgs <- function(x, ...) {
  
  # Add plotting arguments to either a plotLayer or plotLayers object.
  
  if ( is.null(x$plotargs) ) {
    x$plotargs <- list(...)
  } else {
    x$plotargs <- c(x$plotargs, list(...))
  }
  
  # Return value
  return(x)
  
}

#-----------------------------------------------------------------------------------------

addPlotData <- function(plotLayers, x, y=NULL, plotfunc="plot", ...) {
  
  # Add the data to the plotLayers object as a plotLayer (can only add one plotLayer).
  # This a shortcut that uses both the setPlotLayer function and the addPlotLayer function.
  # All plotting arguments (i.e. ...) go into the plotLayer.  Use addPlotArgs to add 
  # plotting arguments to plotLayers!
  
  # Create a plot layer from the data.
  plotLayer <- setPlotLayer(x, y, plotfunc, ...)
  
  # Add it to the plotLayers object.
  plotLayers <- addPlotLayer(plotLayers, plotLayer)
  
  # Return value
  return(plotLayers)
  
}

#-----------------------------------------------------------------------------------------

dropPlotLayer <- function(plotLayers, whichLayer){

  # Drop the plotLayer/s specified in whichLayer from the plotLayers object. 
  # More than one layer can be specified in whichLayer (integer vector)
  
  
  # Check the arguments are the right type of objects.
  if ( ! is.plotLayers(plotLayers) ) {
    stop("plotLayers argument is not a valid plotLayers object.")
  }
  
  # Check plotLayers has the layers indexed in whichLayers.
  if ( ! all(whichLayer %in% 1:plotLayers$nlayers) ) 
    stop("Unable to find one or more of the plot layers indexed in argument 'whichLayers'")

  # Get those layers that are being kept.
  plotLayers$plotLayers <- plotLayers$plotLayers[-whichLayer]
  
  # Adjust the number of layers indicator.
  plotLayers$nlayers <- length(plotLayers$plotLayers)
    
  # Return value
  return(plotLayers)
  
}

#-----------------------------------------------------------------------------------------

is.plotLayer <- function(plotLayer) {
  
  # Test whether the argument is a valid plotLayer object.  Returns true if it is, false otherwise.
  # NB: only tests names of items at this stage, not classes of items!
  
  # Check argument is the right class (as far as we can!)
  if ( !is(plotLayer, "list") ) {
    # The argument is not even a list.  It is not a plotLayer object.
    return(FALSE)
  }
  
  # Get the expected names of the items for a plotLayer object.
  expectedItemNames <- names(setPlotLayer())
  
  # Get the names of the items in the plotLayer oject.
  objectItemNames <- names(plotLayer)
  
  # Check that they have the same items.
  if ( setequal(objectItemNames, expectedItemNames) ) {
    # The same item names, hence, a valid plotLayer object.
    return(TRUE)
    
  } else {
    # Not the same item names, hence, an invalid plotLayer object.
    return(FALSE)
    
  }
  
}

#-----------------------------------------------------------------------------------------

is.plotLayers <- function(plotLayers) {
  
  # Test whether the argument is a valid plotLayers object.  Returns true if it is, false otherwise.
  # NB: only tests names of items at this stage, not classes of items!
  
  # Check argument is the right class (as far as we can!)
  if ( !is(plotLayers, "list") ) {
    # The argument is not even a list.  It is not a plotLayers object.
    return(FALSE)
  }
  
  # Get the expected names of the items for a plotLayers object.
  expectedItemNames <- names(setPlotLayers())
  
  # Get the names of the items in the plotLayers oject.
  objectItemNames <- names(plotLayers)
  
  # Check that they have the same items.
  if ( setequal(objectItemNames, expectedItemNames) ) {
    # The same item names, hence, a valid plotLayers object.
    return(TRUE)
    
  } else {
    # Not the same item names, hence, an invalid plotLayers object.
    return(FALSE)
  }
  
}

#-----------------------------------------------------------------------------------------

plot.plotLayers <- function(x, 
                            device = options()$device, 
                            fileName = paste("myPlot%03d.", device, sep=""), 
                            fileHeight = 700, fileWidth = 900,
                            main = NULL, sub = NULL, xlab = NULL, ylab = NULL, outer=FALSE,
                            whichLayers = NULL, fileUnits = "px", fileRes = NA, ... ){
  
  # Plot the layers to the specified device.  Use the plot function given for each layer.
  # Any extra arguments (i.e. those in ...) are used as "par" arguments to set up plot.
  # NB: "add=TRUE" is added, if needed, so that layers are on the same plot.
  
  # What type of object is the first argument.
  if ( is.plotLayers(x) ) {
    # Cool, ready to go.
    plotLayers <- x
  } else if ( is.plotLayer(x) ) {
    # Need to set up a plotLayers object with one layer.
    plotLayers <- setPlotLayers(x)
  } else {
    # Treat as a call to the function plot.  Make a plotLayer and then a plotLayers object.
    plotLayer <- setPlotLayer(x)
    plotLayers <- setPlotLayers(plotLayer)
  }
  
  # Check plotLayers has the layers indexed in whichLayers.
  if ( is.null(whichLayers) ) {
    # No whichLayers given, set to all available layers.
    whichLayers <- 1:plotLayers$nlayers
  } else {
    # whichLayers given, check they are valid layer indices.
    if ( ! all(whichLayers %in% 1:plotLayers$nlayers) ) 
      stop("Unable to find one or more of the plot layers indexed in argument 'whichLayers'")
  }
  
  # Which device are we printing to?
  if ( device == "RStudioGD" ) {
    # plot to the R studio plot window.
    plotToFile <- FALSE
  } else {
    argList <- list(filename = fileName, width = fileWidth, height = fileHeight, 
                    units = fileUnits, res = fileRes)
    do.call(device, argList)
    plotToFile <- TRUE
  }
  
  # Implement "..." arguments using par (needs to be after device setting).
  opar <- par(...)
  
  # Get the plotting arguments to be used with every layer.
  everyPlotArgs <- plotLayers$plotargs
  
  # Plot each plotLayer using the given arguments (first layer is new plot, rest are added).
  firstPlot <- TRUE
  for ( i in whichLayers ) {
    # Get the layer (to simplify the code!)
    layer <- plotLayers$plotLayers[[i]]
    
    # Add the plotting arguments for every plot.
    plotargs <- c(layer$plotargs, everyPlotArgs)
    
    # Is this the first plot?  #### Add this to individual plot layer, not at this level!
    # if ( firstPlot ) {
    #   firstPlot <- FALSE
    # } else {
    #   # Do I need to have the add tag?  Other plot functions are predefined to be added.
    #   if ( layer$plotfunc %in% c("plot", "boxplot")) plotargs <- c(plotargs, list(add=TRUE))   
    # }
    
    # Get unique plotting arguments (order of precedence: plotLayer, plotLayers, "add").
    isDuplicated <- duplicated(names(plotargs))
    plotargs <- plotargs[!isDuplicated]
    
    # Plot the layer.
    do.call(layer$plotfunc, plotargs)
  }
  
  # Add the titles, if given.
  title(main, sub, xlab, ylab, outer=outer)
  
  # Reset par and turn off the plotting device, if it is a file.
  par(opar)
  if ( plotToFile ) dev.off()
  
}

#-----------------------------------------------------------------------------------------

makeFileName <- function(fnames, dir, ext, dirSep="/", extSep=".") {

  # Make a full file name/s from the given components.  If fnames is a vector, then a full
  # file name will be made for each element of the vector and a vector returned.
  
  fileNames <- fnames
  
  if ( ! missing(dir) ) {
    # Add a directory path to the file name.
    fileNames <- paste0(dir, dirSep, fileNames)
  }
  
  if ( ! missing(ext) ) {
    # Add an extension to the file name.
    fileNames <- paste0(fileNames, extSep, ext)
  }
  
  # Return file name.
  return(fileNames)
}

#-----------------------------------------------------------------------------------------
