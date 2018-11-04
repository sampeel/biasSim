initDataObj <- function(){
  
  # Initialise or reset the data object.
  
  dataObj <- list(directory=NULL,     # a string giving the directory where the data files are stored.
                  files=NULL,         # a vector of file names (each file contains a raster of data).
                  layerNames=NULL,    # a vector of names to be given to the layers of data.
                  data=NULL,          # a rasterStack or SpatialPointsDataFrame that contains the data values.
                  mask=NULL,          # a rasterLayer that indicates cells in data that are to be included/excluded.
                  maskValue=NULL,     # value to use for cells that are to be excluded from any further work.
                  nonMaskValue=NULL,  # value to use for cells that are to be included in any further work.
                  isProjected=FALSE,  # whether or not the data has been projected from longlat
                  isCropped=FALSE,    # Whether or not the data has been cropped
                  #
                  # The following values are worked out by the simulation, no need to set.
                  #
                  numLayers=NULL,     # the number of layers in the data (e.g. the number of data files)
                  isPoints=FALSE,     # Whether or not the data are points (TRUE) or stack (FALSE)
                  isError=FALSE       # whether or not there has been an error within a function
  )

  # Return value.
  return(dataObj)
  
}

#-----------------------------------------------------------------------------------------

readDataStack <- function(files, directory="", 
                          layerNames=paste("layer", 1:length(fileNames), sep=""), 
                          isProjected=FALSE, isCropped=FALSE) {
  
  # Get the data from raster files.  Assumes rasters have the same spatial extent and
  # resolution (this is checked in the "stack" function, assuming quick=FALSE is set).
  # Set the data item in the data object and return the data object.
  # NB: set directory="./" to get current working directory.
  
  # Initialise the return value.
  dataObj <- initDataObj()
  dataObj$isProjected <- isProjected
  dataObj$isCropped <- isCropped

  # Assign argument values to the list.
  dataObj$directory <- directory
  dataObj$files <- files
  dataObj$layerNames <- layerNames
  dataObj$numLayers <- length(files)
  
  # Check layerNames argument has same number of elements as fileNames argument.
  if ( length(layerNames) != dataObj$numLayers ) {
    dataObj$isError <- TRUE
    stop("The number of layer names does not equal the number of data files.")
  }

  # Make the full data file names (including the directory).
  fullFiles <- paste(dataObj$directory, dataObj$files, sep="")
  
  # Read data into a RasterStack object.
  dataObj$data <- stack(fullFiles)
  
  # Double check have the correct number of layers.
  if ( nlayers(dataObj$data) != dataObj$numLayers ) {
    dataObj$isError <- TRUE
    stop("The number of layers in the data stack is incorrect.")
  }
  
  # Set the layer names of the data stack.
  names(dataObj$data) <- dataObj$layerNames
  
  # Return value.
  return(dataObj)
  
}

#-----------------------------------------------------------------------------------------

readDataPoints <- function(file, directory="", xColName="Longitude", yColName="Latitude",
                           proj=CRS(proj4str.longlat()) ) {
  
  # Get the data from a "csv" formatted file or load it from an "RData" file.
  # Set the data item in the data object, convert to points and return the data object.
  # NB: set directory="./" to get current working directory.
  
  # Initialise the return value.
  dataObj <- initDataObj()
  
  # Assign argument values to the list.
  dataObj$directory <- directory
  dataObj$files <- file
  
  # Make the full data file names (including the directory).
  fullFiles <- paste0(dataObj$directory, dataObj$files)
  

  # Get the data.
  if ( toupper(extension(file)) == ".RDATA" ) {
    # Load data.  Assumes data is a data.frame object.
    objectLoaded <- load(fullFiles)
    dataObj$data <- get(objectLoaded)
    remove(objectLoaded)

  } else {
    # Read in data.
    dataObj$data <- read.table(file=fullFiles, header = TRUE, sep=",", quote="\"", 
                               stringsAsFactors = FALSE)
  }
  
  # Convert the data into points.
  dataObj <- as.DataPoints(dataObj, proj, xColName, yColName)
  
  # Return value.
  return(dataObj)
  
}

#-----------------------------------------------------------------------------------------

readDataTable <- function(file, directory="", delim=","){
  
  # Read the data into a data.frame from the file and directory given.
  # Assumes file is a csv formatted file with 'delim' as a delimiter.
  # Assumes paste(dir, file, sep="") makes a valid file name (i.e. include slash at end of
  # 'dir' if needed).
  # Assumes data has a header row.
  
  # Initialise the return value.
  dataObj <- initDataObj()
  
  # Assign argument values to the list.
  dataObj$directory <- directory
  dataObj$files <- file
  
  # Make the full data file names (including the directory).
  fullFiles <- paste(dataObj$directory, dataObj$files, sep="")
  
  # Read in data.
  dataObj$data <- read.table(file=fullFiles, header = TRUE, sep=delim, quote="\"", 
                             stringsAsFactors = FALSE)
  
  # Return value.
  return(dataObj)
  
}

#-----------------------------------------------------------------------------------------

setDataStack <- function(stack, layerNames=names(stack), isProjected=FALSE, isCropped=FALSE){
  
  # Set the data as the given RasterStack object.  Returns a data object.
  # Equivalent to dataObj <- stack, if this worked and assigned all the right parts!!!

  # Initialise the return value.
  dataObj <- initDataObj()
  
  # Check it is a RasterStack object.
  if ( !inherits(stack, "RasterStack") ) {
    dataObj$isError <- TRUE
    stop("Argument 'stack' is not a RasterStack object.")
  }
  
  # Set the data.
  dataObj$data <- stack
  
  # Set the number of layers.
  dataObj$numLayers <- nlayers(dataObj$data)
  
  # Set the layerNames
  if ( length(layerNames) == dataObj$numLayers ) {
    dataObj$layerNames <- layerNames
    names(dataObj$data) <- layerNames
  } else {
    dataObj$isError <- TRUE
    stop("The length of layerNames does not match the number of layers in the stack.")
  }
  
  # Set the projected and cropped indicators.
  dataObj$isProjected <- isProjected
  dataObj$isCropped <- isCropped
  
  # Return value.
  return(dataObj)

}

#-----------------------------------------------------------------------------------------

setDataPoints <- function(points, isProjected=FALSE, isCropped=FALSE) {
  
  # Set the data as the given SpatialPoints or SpatialPointsDataFrame object.  
  # Returns a data object.
  
  # Initialise the return value.
  dataObj <- initDataObj()
  
  # Check it is a SpatialPoints object.
  if ( !inherits(points, "SpatialPoints") && !inherits(points, "SpatialPointsDataFrame") ) {
    dataObj$isError <- TRUE
    stop("Argument 'points' is not a SpatialPoints or SpatialPointsDataFrame object.")
  }
  
  # Set the data.
  dataObj$data <- points
  
  # Set the projected and cropped indicators.
  dataObj$isProjected <- isProjected
  dataObj$isCropped <- isCropped
  
  # Set the data type indicator.
  dataObj$isPoints <- TRUE
  
  # Return value.
  return(dataObj)
  
}

#-----------------------------------------------------------------------------------------

setDataMask <- function(dataObj, mask, maskValue, nonMaskValue=TRUE) {
  
  # Sets the data mask info. Only relevant for RasterStack type data.

  # Check that there is some data.
  if ( is.null(dataObj$data) ) {
    dataObj$isError <- TRUE
    stop("There is no data.  Please set/read the data first.")
  }
  
  # Make sure the data is not points.
  if ( dataObj$isPoints ) {
    dataObj$isError <- TRUE
    stop("Data consists of point locations.  Use domain polygon to exclude points.")
  }
  
  # Set the other values related to the masking of data.
  dataObj$mask <- mask
  dataObj$maskValue <- maskValue        
  dataObj$nonMaskValue <- TRUE          # Default value in makeDataMask function
  
  # Return value.
  return(dataObj)
  
}

#-----------------------------------------------------------------------------------------

projectDataStack <- function(dataObj, projNew, extNew=NULL) {
  
  # Project the data stack to the given projection.  Crops to the given extent, if necessary.
  # Cropping is applied after projection so the new extent should be in the new projection's
  # coordinate reference system!
  
  # Check the data has not already been projected (safer to stick to only one projection.)
  if ( dataObj$isProjected ) {
    stop("Data is already projected.  Please re-read original data and then project.")
  }
  
  # Make sure the data is not points.
  if ( dataObj$isPoints ) {
    dataObj$isError <- TRUE
    stop("Data consists of point locations.  Use function 'projectDataPoints' instead.")
  }
  
  # Project data.
  dataObj$data <- projectRaster(dataObj$data, crs=projNew)

  # Set that the data has been projected.
  dataObj$isProjected <- TRUE
  
  # Crop the data, if required.
  if ( !is.null(extNew) ) {
    dataObj <- cropDataStack(dataObj, extNew)
  }
  
  # Return value.
  return(dataObj)
  
}

#-----------------------------------------------------------------------------------------

projectDataPoints <- function(dataObj, projNew, domainNew=NULL) {
  
  # Project the data points to the given projection.  Crops to the given domain, if necessary.
  # Cropping is applied after projection so the new domain should be in the new projection's
  # coordinate reference system (CRS)!
  #
  # Arguments ...
  # dataObj:   the data object whose data is to be projected to a new CRS.
  # projNew:   the string that describes the new CRS.
  # domainNew: a SpatialPolygon object that encloses the domain (can have holes!).  If
  #            NULL, no cropping of the projected data is performed.
  
  # Check the data has not already been projected (safer to stick to only one projection.)
  if ( dataObj$isProjected ) {
    stop("Data is already projected.  Please re-read original data and then project.")
  }
  
  # Make sure the data is not a RasterStack.
  if ( !dataObj$isPoints ) {
    dataObj$isError <- TRUE
    stop("Data is a RasterStack.  Use function 'projectDataStack' instead.")
  }
  
  # Project data.
  dataObj$data <- spTransform(dataObj$data, projNew)
  colnames(dataObj$data@coords) <- c("x","y")
  
  # Set that the data has been projected.
  dataObj$isProjected <- TRUE
  
  # Crop the data, if required.
  if ( !is.null(domainNew) ) {
    dataObj <- cropDataPoints(dataObj, domainNew)
  }
  
  # Return value.
  return(dataObj)
  
}

#-----------------------------------------------------------------------------------------

addDataLayer <- function(dataObj, dataLayer, 
                         layerName = paste("layer", dataObj$numLayers+1, sep="")) {
  
  # Add a layer of data to the data object's data stack.
  
  # Add layer.
  dataObj$data <- addLayer(dataObj$data, dataLayer)
  
  # Update number of layers.
  dataObj$numLayers <- dataObj$numLayers + 1
  
  # Add layer name.
  names(dataObj$data)[dataObj$numLayers] <- layerName
  
  # Return value.
  return(dataObj)
  
}

#-----------------------------------------------------------------------------------------

dropDataLayer <- function(dataObj, i) {
  
  # Remove the specified layer of data from the data objects data stack.  Uses dropLayer!
  
  # Remove layer.
  dataObj$data <- dropLayer(dataObj$data, i)
  
  # Update number of layers.
  dataObj$numLayers <- dataObj$numLayers - 1
  
  # Return value.
  return(dataObj)

}

#-----------------------------------------------------------------------------------------

cropDataStack <- function(dataObj, newExt) {
  
  # Crop the data to the new extent given.  Assumes that this is smaller than current extent!
  # NB: Rasters implement the "domain" using a mask during calculations!
  
  # Make sure the data is not points.
  if ( dataObj$isPoints ) {
    dataObj$isError <- TRUE
    stop("Data consists of point locations.  Use function 'cropDataPoints' instead.")
  }
  
  # Check the new extent is the right kind of object.
  if ( !inherits(newExt, "Extent")) {
    dataObj$isError <- TRUE
    stop("New extent argument needs to be an Extent object.")
  }
  
  # Crop data
  dataObj$data <- raster::crop(dataObj$data, newExt, snap="out")
  
  # Change back to stack as crop converts to RasterBrick
  dataObj$data <- stack(dataObj$data)
  
  # Set that the data has been cropped
  dataObj$isCropped <- TRUE
  
  # Return value.
  return(dataObj)
  
}

#-----------------------------------------------------------------------------------------

cropDataPoints <- function(dataObj, newDomain) {
  
  # Crop the data to the points in the new domain (SpatialPolygons or SpatialPolygonsDataFrame).
  
  # Make sure the data is not a RasterStack.
  if ( !dataObj$isPoints ) {
    dataObj$isError <- TRUE
    stop("Data is a RasterStack.  Use function 'cropDataStack' instead.")
  }
  
  # Check the new domain is the right kind of object.
  if ( !inherits(newDomain, c("SpatialPolygons", "SpatialPolygonsDataFrame")) ) {
    dataObj$isError <- TRUE
    stop("New domain argument needs to be a SpatialPolygons object.")
  }
  
  # Which points are in the domain?
  isPointInDomain <- drop(!is.na(over(dataObj$data, newDomain)))
  
  # Crop data.
  dataObj$data <- dataObj$data[isPointInDomain, ] 
  
  # Set that the data has been cropped.
  dataObj$isCropped <- TRUE
  
  # Return value.
  return(dataObj)
  
}

#-----------------------------------------------------------------------------------------

as.DataPoints <- function(dataObj, proj=CRS(proj4str.longlat()), xColName="x", yColName="y") {
  
  # Convert the data in the data object from a data.frame to a SpatialPoints or 
  # SpatialPointsDataFrame object.
  
  # Check it is a data.frame.
  if ( ! inherits(dataObj$data, "data.frame") ) {
    dataObj$isError <- TRUE
    stop("Data in data object is not recognised as a data.frame.")
  }
  
  # Figure out where the x and y columns are in the data.frame.
  namesCols <- names(dataObj$data)
  indXCol <- which(namesCols %in% xColName)
  if ( indXCol == 0 ) stop("Unable to find the given x column in the data.")
  indYCol <- which(namesCols %in% yColName)
  if ( indYCol == 0 ) stop("Unable to find the given y column in the data.")
  
  # Set the data.
  if ( length(namesCols) > 2 ) {
    dataObj$data <- SpatialPointsDataFrame(dataObj$data[ ,c(indXCol,indYCol)], dataObj$data, 
                                           coords.nrs = c(indXCol,indYCol), proj4string = proj)
  } else {
    dataObj$data <- SpatialPoints(dataObj$data, proj4string = proj)
  }
  
  # Set the data type indicator.
  dataObj$isPoints <- TRUE
  
  # Return value.
  return(dataObj)
  
}

#-----------------------------------------------------------------------------------------

makeDataMask <- function(data, outExt=NULL, heightLayer=NULL, heightLimit=NULL, isLandMask=TRUE, 
                         maskValue=FALSE, newMaskValue=maskValue, nonMaskValue=TRUE) {
  
  # Mask any cells that have 'maskValue' values in any layer of 'data' (a RasterStack).
  # Use the 'outExt' to crop the extent of the result.  Otherwise, the output has the same 
  # resolution and projection as data but is a single layer.
  #
  # If 'heightLayer' is a layer name in data, then this layer is used to either mask land
  # or ocean (depending on the value of 'isLandMask').  If 'isLandMask' is TRUE then the 
  # height layer of data will be used to mask the land (cells with height > 0 are set to 
  # 'newMaskValue'), otherwise, the height layer of data will be used to mask the ocean 
  # (cells with height <= 0 are set to 'newMaskValue').  
  #
  # The return value is a RasterLayer that has values of 'newMaskValue' for each cell that 
  # has a 'maskValue' value in any layer of the data OR is masked as indicated by the 
  # 'heightLayer' and 'isLandMask' values (as discussed in previous paragraph).  Other 
  # cell values (i.e. those cells that will be used) are set to the 'nonMaskValue' value.
  #
  # Arguments ...
  # data:         a RasterStack object that contains data values (NA for unknown values).
  # outExt:       the extent required in the result or output.  Assumes this is an area  
  #               within the extent of data.  Will cause the crop function to be envoked.
  # heightLayer:  a string that gives the layer in data that contains the height above sea 
  #               level values (negative values for depth below sea level).  If NULL
  #               don't worry about doing ocean or land mask (and ignore isLandMask).
  # heightLimit:  a value that gives the limit of acceptable values for the heightLayer.
  #               If isLandMask is FALSE then heightLimit will be positive and values greater
  #               than this will be masked, otherwise, heightLimit will be negative and 
  #               values less than this will be masked.  If null, no values will be masked
  #               (at least not because of this argument!).
  # isLandMask:   indicates whether to mask the land (TRUE) or the ocean (FALSE). 
  # maskValue:    value used in the data to indicate that a value is not available for a cell.
  # newMaskValue: value to use for cells that are to be excluded from any further work.
  # nonMaskValue: value to use for cells that are to be included in any further work.
  
  # Check or assign heightLimit, if necessary.
  if ( ! is.null(heightLayer) ) {
    # i.e. there is a height layer.
    if ( isLandMask ) {
      # i.e. use only the ocean.
      if ( is.null(heightLimit) ) {
        # Assign minimum value as limit.
        heightLimit <- minValue(data[[heightLayer]])
      } else if ( heightLimit > 0 ) {
        stop("Positive height limit value in mask function when need negative.")
      }
    } else {
      # i.e. use only the land.
      if ( is.null(heightLimit) ) {
        # Assign maximum value as limit.
        heightLimit <- maxValue(data[[heightLayer]])
      } else if ( heightLimit < 0 ) {
        stop("Negative height limit value in mask function when need positive.")
      }
    }
  } else {
    # Do nothing as no heightLayer!
  }
  
  # Do we need to crop the data?
  if ( is.null(outExt) ) {
    # No, use extent of data.
    rsData <- data
  } else {
    # Yes. Crop data
    rsData <- raster::crop(data, outExt, snap="out")    
  }
  
  # Initialise the return value.
  rlMask <- raster(rsData, layer=0)
  values(rlMask) <- nonMaskValue
  
  # Are there any cells in any layers that are maskValue?
  if ( is.na(maskValue) ) {
    isCellValMissing <- any(is.na(rsData))
  } else {
    isCellValMissing <- any(rsData == maskValue)
  }
  
  # Mask the cells that have at least one missing value.
  rlMask[isCellValMissing] <- newMaskValue
  
  # Is there a height layer in the data RasterStack?
  if ( !is.null(heightLayer) ) {
    # Yes there is.  Is the name a valid one?
    if ( heightLayer %in% names(rsData) ) {
      # Are the values in the heightLayer used to mask land or ocean?
      if ( isLandMask ) {
        # Mask the land: ocean = FALSE, land = TRUE
        isCellHeightMasked <- (rsData[[heightLayer]] > 0) | (rsData[[heightLayer]] < heightLimit) 
      } else {
        # Mask the ocean: ocean = TRUE, land = FALSE
        isCellHeightMasked <- (rsData[[heightLayer]] <= 0) | (rsData[[heightLayer]] > heightLimit)    
      }
      
      # Mask requested height values (either land or ocean).
      rlMask[isCellHeightMasked] <- newMaskValue 
      
    } else {
      stop("Unable to find given heightLayer in data.")
    }
  }
  
  # Return value.
  return(rlMask)
  
}

#-----------------------------------------------------------------------------------------

createTestBiasCovar <- function(extSim, resData, projSim, isSampleBias) {
  
  # Create the sample bias covariate raster stack.  If there is no sample bias a raster 
  # of close to ones is returned (NB: exactly one for every cell value doesn't work).
  #
  # This function is specific to this simulation and not really apart of the data object!
  #
  # This function has obsolete ideas but need it still for testing purposes!
  #
  # ARGUMENTS ...
  # extSim:       Domain of the simulation specified as a raster extent object.
  # resData:      Resolution or size of the cells in the raster stack returned (vector length=2).
  # projSim:      Projection of the simulation.
  # isSampleBias: Logical value indicating whether or not sample bias is present for the PO data.
  
  # Create the cells and their centres.
  rsSampBiasCovar <- raster(extSim, resolution=resData, crs=projSim )
  myCellCentres <- rasterToPoints(rsSampBiasCovar)
  dfCells <- data.frame(myCellCentres, stringsAsFactors = FALSE)
  
  # Is there sample bias?
  if ( isSampleBias ) {
    # Yes there is, set bias to be toward the higher longitudes.
    dfCells$zs <- dfCells$x     
    
  } else {
    # No there isn't, set bias to be one-ish.
    numCells <- dim(dfCells)[1]
    ones <- rep(1, times=numCells)
    dfCells$zs <- ones  #jitter(ones)
    warning("I have turned off the jitter for no sample bias.  Do I still need this?")
    # Think i have coded around this by putting in if (!isSampleBias) then ... 
  }
  rsSampBiasCovar <- setValues(rsSampBiasCovar, dfCells$zs)
  names(rsSampBiasCovar) <- "Bias1"
  
  # Return value.
  return(rsSampBiasCovar)
  
}

#-----------------------------------------------------------------------------------------

postProcessBaseData <- function(coords, x, y, yearEstablished=NULL, base, baseShort=NULL, 
                                notAfterYear=2010, plotCheck=FALSE, coastPoly=NULL) {
  
  
  # Fixing up ready for use.  Converts coordinate string into decimal latitude and longitude.
  # Removes bases that started after the year given in 'notAfterYear' argument.
  # Adds plotting position column.    Plots the research stations if asked.
  #
  # ARGUMENTS ...
  # coords:          a vector of degrees, minutes, and seconds for both NS and EW in a string.
  # x, y:            longitude and latitude vectors of decimal degrees
  # yearEstablished: a vector of integers giving the year of establishment of the base
  # base:            a vecotr of strings giving the name of the base (station or camp)
  # baseShort:       a vector of strings giving a unquie short name for each base.
  # notAfterYear:    an integer year value, excludes bases established after this year.
  # plotCheck:       logical value that either plots the data for checking, or not.
  # coastPoly:       coastline as a SpatialPolygonsDataFrame object.
  #
  # RETURN ...
  # bases:           a data frame containing the geographic position (given by decimal 
  #                  values of longitude and latitude in separate columns), the name and ?
  #                  of each research station.
  
  #   colNamesList=list(out.longitude = "x", 
  #                     out.latitude = "y", 
  #                     out.baseName = "base",
  #                     out.baseNameShort = "baseShort",
  #                     out.yearEstablished = "yearEst")
  
  # Number of research stations (i.e. number of rows in data frame).
  numStations <- length(baseShort)
  
  # Initialise return value.
  retBases <- as.data.frame(matrix(nrow=numStations, ncol=2), stringsAsFactors=FALSE)
  names(retBases) <- c("x", "y")
  
  # Which type of location data has been provided?
  if ( !(missing(x) || missing(y)) ) {
    # Have lats and longs.
    retBases$x <- x
    retBases$y <- y
    
  } else if ( ! missing(coords) ) {
    # Have strings of degrees, minutes and seconds.
    # For each row, get the decimal latitude and longitude value from the coordinates string.
    for ( station in 1:numStations ) {
      # What is the coordinate string for this station?
      coordStr <- researchStations[station, colNamesList$in.coordinate]
      
      # Get decimal latitude and longitude from coordinate string.
      coordList <- as.double.coordstring(coordStr)
      if ( coordList$isError ) stop(coordList$errMsg)
      
      # Store in data frame.
      retBases$x[station] <- coordList$longitude$decimalDegrees
      retBases$y[station] <- coordList$latitude$decimalDegrees
    }
    
  } else {
    # No location data!
    stop("Missing base location data (arguments coords or x and y).")
  }
  
  # Get the other column data that we are interested in.
  retBases$base <- base
  if ( missing(baseShort) ) {
    retBases$baseShort <- 1:numStations
  } else {
    retBases$baseShort <- baseShort
  }
  if ( is.null(yearEstablished) ) {
    retBases$yearEst <- NA
  } else {
    retBases$yearEst <- yearEstablished
  }
  retBases$plotPos <- rep(1, length.out=numStations)   # column to indicate plotting position of name.
  
  # Change plotting position of Mario and McMurdo so looks nicer.
  indPlotPos <- which(retBases$baseShort %in% c("Hallett ", "McMurdo ", "Browning Pass "))
  retBases$plotPos[indPlotPos] <- 2    # to the left of coordinate
  indPlotPos <- which(retBases$baseShort %in% c("Corner Camp"))
  retBases$plotPos[indPlotPos] <- 4    # to the right of coordinate
  indPlotPos <- which(retBases$baseShort %in% c("Mario Zucchelli "))
  retBases$plotPos[indPlotPos] <- 3    # to the top of coordinate
  
  # Get rid of bases that are relatively new.
  if ( !is.null(yearEstablished) ) {
    indUseBases <- which(retBases$yearEst <= notAfterYear)
    retBases <- retBases[indUseBases, ]
  }
  
  # Plot the research stations to check.
  if ( plotCheck ) {
    if ( is.null(coastPoly) ) {
      require("maptools")
      data("wrld_simpl", envir=environment())
      coastPoly <- wrld_simpl[wrld_simpl@data$NAME == "Antarctica", ]
    } 
    plot(retBases$x, retBases$y, asp=1, xlab="Longitude", ylab="Latitude",
         pch="+", xlim=c(-180,180))
    plot(coastPoly, border="grey", add=TRUE)  
    text(retBases$x, retBases$y, retBases$baseShort, cex=0.5, pos=retBases$plotPos)
    title("Check antarctic research bases.")
  }
  
  # Return value.
  return(retBases)
  
}

#-----------------------------------------------------------------------------------------
