#-----------------------------------------------------------------------------------------

as.owin.ext <- function(rasExt, mask=NULL, maskValue=NA, ...) {
  
  # Convert an Extent object from raster package to an owin object from spatstat package.
  # Can have a mask as a raster layer (where extent(mask) == rasExt).
  #
  # USES ...
  # spatstat library version 1.45-0
  # raster library version 2.3-24
  #
  # ARGUMENTS ...
  # rasExt:    an Extent object of a raster.
  # mask:      can be a raster layer or other mask objects as specified in function owin.
  # maskValue: the value that is used to mask other data in the argument 'mask'.  That is,
  #            any cell in mask, whose value equals maskValue, will not be included in the 
  #            observation window.
  
  # Check extent argument is valid class.
  if ( !inherits(rasExt, "Extent") ) {
    stop("Invalid class for argument 'rasExt' in function 'as.owin.ext'.")
  }
  
  # Check mask argument, if given.
  myMask <- NULL
  if ( !is.null(mask) ) {
    if ( inherits(mask, "RasterLayer") ) {
      # Is a raster layer, so extract the relevant values into right form for owin function.
      myMask <- as.data.frame(rasterToPoints(mask))
      if ( is.na(maskValue) ) {
        myMask[ ,3] <- !is.na(myMask[ ,3])
      } else {
        myMask[ ,3] <- myMask[ ,3] != maskValue   # Could be dodgy, test!
      }
      #myMask[,3] <- as.logical(myMask[,3])  # Converts third column back to TRUE or FALSE.
    } else {
      # Leave as is and let owin function deal with it.
      myMask <- mask
    }
  }
  
  # Get the observation window object.
  xrange <- c(rasExt@xmin, rasExt@xmax)
  yrange <- c(rasExt@ymin, rasExt@ymax)
  myOwin <- owin(xrange, yrange, mask=myMask, ...)
  
  # Return value.
  return(myOwin)
}

#-----------------------------------------------------------------------------------------

as.ext.owin <- function(myOwin, ...) {
  
  # Convert an owin object from spatstat package to an extent object from raster package.
  #
  # USES ...
  # spatstat library version 1.41-1
  # raster library version 2.3-24
  #
  # ARGUMENTS ...
  # myOwin: an object of type owin (see spatstat package).
  
  # Check extent argument is valid class.
  if ( !inherits(myOwin, "owin") ) {
    stop("Invalid class for argument 'myOwin' in function 'as.ext.owin'.")
  } 
  
  # Convert to a raster extent.
  myExtent <- extent(c(myOwin$xrange, myOwin$yrange), ...)
  
  # Return value.
  return(myExtent)
}

#-----------------------------------------------------------------------------------------

as.ext.bbox <- function(myBbox, mask=NULL, unitDivisor=1, ...) {
  
  # Convert a bbox object from sp package to an extent object from raster package.
  # Haven't coded mask in yet!
  #
  # USES ...
  # sp library version 1.1-1
  # raster library version 2.3-24
  #
  # ARGUMENTS ...
  # myBbox:      an object of type bbox (see sp package).
  # mask:       
  # unitDivisor: a real number that changes the values into the required units (e.g. to
  #              change from metres to kilometres, unitDivisor = 1000) 

  # Check extent argument is valid class.
  if ( !inherits(myBbox, "matrix") ) {
    stop("Invalid class for argument 'myBbox' in function 'as.ext.bbox'.")
  } 
  
  # Check is has the right dimensions.
  if ( ! all( dim(myBbox) == c(2,2) ) ) {
    stop("Invalid dimension sizes for argument 'myBbox' in function 'as.ext.bbox'.")
  }
  
  # Check mask argument, if given.
  if ( !is.null(mask) ) {
    stop("Haven't coded mask stuff in function as.ext.bbox yet!")
  }
  
  # Convert to a raster extent.
  myExtent <- extent(c(myBbox[1, ]/unitDivisor, myBbox[2, ]/unitDivisor))
  return(myExtent)
}

#-----------------------------------------------------------------------------------------

as.bbox.ext <- function(rasExt) {
  
  # Convert an extent object from raster package to a bbox object from sp package.
  #
  # ARGUMENTS ...
  # myExt:      an object of type extent (see raster package).

  # Check extent argument is valid class.
  if ( !inherits(rasExt, "Extent") ) {
    stop("Invalid class for argument 'rasExt' in function 'as.bbox.ext'.")
  } 
  
  # Convert to a bbox object from sp package.
  myBbox <- bbox(as.matrix(rasExt))
  return(myBbox)
}

#-----------------------------------------------------------------------------------------
as.owin.bbox <- function(myBbox, unitDivisor=1) {
  
  # Convert a bbox object from the sp package to a window object from the spatstat package.
  #
  # USES ...
  # sp library version 1.1-1
  # spatstat library version 1.45-0
  #
  # ARGUMENTS ...
  # myBbox: an object of type bbox (see sp package).
  # unitDivisor: a real number that changes the values into the required units (e.g. to
  #              change from metres to kilometres, unitDivisor = 1000) 
  
  # Check extent argument is valid class.
  if ( !inherits(myBbox, "matrix") ) {
    stop("Invalid class for argument 'myBbox' in function 'as.ext.bbox'.")
  } 
  
  # Check is has the right dimensions.
  if ( ! all( dim(myBbox) == c(2,2) ) ) {
    stop("Invalid dimension sizes for argument 'myBbox' in function 'as.ext.bbox'.")
  }
  
  # Convert to a spatstat window object.
  myOwin <- owin(xrange=myBbox[1, ]/unitDivisor, yrange=myBbox[2, ]/unitDivisor)
  return(myOwin)
}

#-----------------------------------------------------------------------------------------

as.ext.xy <- function(x,y, ...) {
  
  # Get the extent of the data in the vectors x and y (assumes they are geographical points
  # and thus x and y need to be the same length).  Could have the na.rm argument of the range 
  
  # Check that x and y are the same length.
  if ( length(x) != length(y) ) {
    stop("Arguments are not the same length.")
  }
  
  # Get the limits of the data.
  xRange <- range(x, ...)
  yRange <- range(y, ...)
  
  # Convert to extent.
  myExt <- extent(c(xRange, yRange))
  return(myExt)

}

#-----------------------------------------------------------------------------------------

as.polyxy.ext <- function(ext, isClosed=TRUE) {
  
  # Get the polygon that the given extent represents.  Return the polygon as a data.frame
  # with an x column and a y column.  The isClosed argument will add the starting point
  # at the end to form a "closed" polygon, otherwise, only the vertices are returned.
  
  # Initialise the return value.
  dfPoly <- data.frame(matrix(nrow=4, ncol=2), stringsAsFactors = FALSE)
  names(dfPoly) <- c("x", "y")
  
  # Form the four vertices of the extent.
  dfPoly[1, ] <- c(ext@xmin, ext@ymax)
  dfPoly[2, ] <- c(ext@xmax, ext@ymax)
  dfPoly[3, ] <- c(ext@xmax, ext@ymin)
  dfPoly[4, ] <- c(ext@xmin, ext@ymin)
  
  # Close the polygon?
  if ( isClosed ) dfPoly <- rbind(dfPoly, dfPoly[1, ])
  
  # Return value
  return(dfPoly)
  
}

#-----------------------------------------------------------------------------------------

rasterFromFunc <- function(ext, res, mask, maskValue, func, ... ) {
  
  # Evaluate function at the centroids of each cell of a raster.  The raster is defined by
  # the extent and resolution arguments.  Assumes the first two arguments of the function
  # are x and y (in vector form).  Other function arguments are allowed.
  # Returns this raster with the function result at each allowed centre point as its values.
  # Assumes raster functions will pick up any problems with extents that don't match!
  #
  # Arguments ...
  # ext:       extent of the raster on which to evaluate the function.
  # res:       resolution of the raster on which to evaluate the function.
  # func:      name of the function to be evaluated (assumes function is of the form 
  #            func(x,y, ...) and is vectorised).
  # mask:      a raster layer containing values that indicate which cells are to be 
  #            excluded from the calculations (same extent, resolution and CRS as other
  #            arguments sent to func, if they are raster objects).
  # maskValue: the value of the cells in mask that are to be excluded from the calculations.
  #            NB: will not work with a maskValue of NULL!
  # ...        other arguments to be sent to the function.
  
  # Create a blank raster (NB: crs not important?)
  rsFuncVals <- raster(ext, resolution=res)

  # Get the centre points of each cell in this raster.
  centres <- data.frame(xyFromCell(rsFuncVals, 1:ncell(rsFuncVals)), stringsAsFactors = FALSE)
  
  if ( !is.null(mask) ) { 
    # Use mask to determine points to be included or excluded.
    maskValues <- drop(davesExtract.v3(mask, centres))
    if ( is.na(maskValue) ) {
      indIncludePoints <- !is.na(maskValues)
    } else {
      indIncludePoints <- maskValues != maskValue
    }
    
  } else {
    # Include all points.
    indIncludePoints <- rep(TRUE, times=dim(centres)[1])
  }
  
  # Evaluate the function at the included centres.
  centres$z[indIncludePoints] <- func(centres$x[indIncludePoints], centres$y[indIncludePoints], ... )
  centres$z[!indIncludePoints] <- maskValue
  rsFuncVals <- setValues(rsFuncVals, centres$z)
  
  # Return raster containing function values at cell centres.
  return(rsFuncVals)
  
}

#-----------------------------------------------------------------------------------------

cropShiftData <- function(data, cropExt, shiftData=0) {
  
  # Crops the data to the extent specified.  Can shift the data if the longitude range of 
  # the data extent is different to that of the simulation extent (e.g. the data longitudes 
  # have a range of -180 to 180 but the simulation extent falls within the longitude range 
  # of 0 to 360).  Returns the cropped raster layer.
  #
  # ARGUMENTS...
  # data:      raster layer of data to be cropped (and possibly shifted if on different scale)
  # cropExt:   extent to crop the data to
  # shiftData: amount to shift *half* the data by, for example ...
  #               0   - no shift
  #               360 - shift -180:0 longitudes to 180:360 longitudes; 0:180 stays same.
  #              -360 - shift 180:360 longitudes to -180:0 longitudes; 0:180 stays same.
  
  # extent of data ...
  dataExt <- extent(data)
  
  # Set cropped extent:
  # To shift the data, need to split it into east and west halves as only the west half
  # needs to be shifted.  Get the extent of the two cropped halves on the current data range.
  cropExtEast <- cropExt            # Sets latitude extents.
  cropExtWest <- cropExt            # Sets latitude extents.
  cropExtEast@xmin <- cropExt@xmin
  cropExtEast@xmax <- dataExt@xmax           # approximately = 180
  cropExtWest@xmin <- dataExt@xmin           # approximately = -180
  cropExtWest@xmax <- cropExt@xmax - shiftData
  
  # Crop the data.
  # Get the values from the two parts of the extent.
  roCroppedEast <- raster::crop(data, cropExtEast)
  roCroppedWest <- raster::crop(data, cropExtWest)
  
  # Shift the western half (no need to shift the eastern half)
  # NB: shift(myRasterObj,x=360) by itself doesn't work as it makes the east values too big.
  if ( shiftData != 0 ) {
    roCroppedWest <- raster::shift(roCroppedWest, x=shiftData) 
  }
  
  # Merge them into one raster (takes care of overlap, if any, and sets new extent).
  roCropped <- merge(roCroppedEast, roCroppedWest)
  
  # Return value.
  return(roCropped)
}

#-----------------------------------------------------------------------------------------

proj4str.longlat <- function() {
  
  # Returns the string that gives the long-lat projection string.
  
  return("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
}

#-----------------------------------------------------------------------------------------

proj4str.laea <- function(units="m", lonOrigin="0", latOrigin="-90") {
  
  # Returns the string that gives the laea projection string.
  
  projStr <- paste("+proj=laea +lat_0=", latOrigin, " +lon_0=", lonOrigin, 
                   " +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0",
                   " +units=", units, sep="")
  return(projStr)
}

#-----------------------------------------------------------------------------------------

replaceVarName <- function(myFormula, varName, newVarName) {
  
  # Replaces the given variable name with the new variable name in the given formula.
  # Assumes myFormula is a formula object.  Assumes varName and newVarName are strings.
  # Converts to string then performs replacement then converts back to a formula object.  
  # Why is this necessary, substring matching functions will also match other variable 
  # names within the formula that contain "varName" (e.g. replace all "x" but have another
  # variable called "xray", then the "x" within "xray" would get replaced as well using
  # inbuilt functions.)

  # Convert formula to string.  
  strFormula <- deparse(myFormula)
  
  # Find where "words" that contain varName are within the formula string.
  strPattern <- paste("\\w*", varName, "\\w*", sep="")
  wordsWithVar <- gregexpr(strPattern, strFormula)        
  if ( wordsWithVar[[1]][1] == -1 ) return(myFormula)              # No matches
  
  # Get each word that contains varName from the formula string.
  wordStart <- wordsWithVar[[1]]
  wordLength <- attr(wordsWithVar[[1]],"match.length")
  wordEnd <- wordStart + wordLength - 1
  word <- substring(strFormula, wordStart, wordEnd)
  
  # Get only the words that are exactly varName.
  indMatchVarName <- word %in% varName
  wordStart <- wordStart[indMatchVarName]
  wordEnd <- wordEnd[indMatchVarName]
  
  # Replace these parts of the string; copy the bits before, inbetween and after.
  strReturn <- ""
  numMatches <- length(wordStart)
  numChars <- nchar(strFormula)
  if ( wordStart[1] != 1 ) strReturn <- substr(strFormula, 1, wordStart[1]-1)
  for ( i in 1:numMatches ) {
    # Replace varName with the new variable name.
    strReturn <- paste(strReturn, newVarName, sep="")

    # Add bit before next word, if there is a bit.
    if ( wordEnd[i] < numChars ) {
      # Still more to do.
      if ( i != numMatches ) {
        # Get bit inbetween matches. Will return "" if nothing inbetween.
        strAdd <- substr(strFormula, wordEnd[i]+1, wordStart[i+1]-1)
      } else {
        # Get last bit after last match.
        strAdd <- substr(strFormula, wordEnd[i]+1, numChars)
      }
      strReturn <- paste(strReturn, strAdd, sep="")
    } else {
      # Finished.
    }
  }
    
  # Convert string to formula and return.
  return(as.formula(strReturn))
  
}

#-----------------------------------------------------------------------------------------

numCoefficients <- function(myFormula, includeIntercept=TRUE) {
  
  # Return the number of coefficients necessary for the given formula.
  # NB: can do with or without an intercept!
  # NB: can do "poly" but no other functions!
  
  # Check it is a formula.
  if ( !inherits(myFormula, "formula") ) myFormula <- as.formula(myFormula)
  
  # Convert to a terms object and remove response (just less complicated!)
  myTerms <- terms(myFormula)
  myTerms <- delete.response(myTerms)

  # Does formula include poly?
  if ( "poly" %in% all.names(myFormula) ) {
    # Which term is the poly term?  Assumes "poly" is start of term!
    termLabels <- attr(myTerms,"term.labels")
    indPolyTerm <- which(unlist(gregexpr("poly", termLabels)) == 1)  
    
    # Work out how many coefficients there will be, without poly terms.
    numCoeffs <- length(termLabels) - length(indPolyTerm)
    
    # Work out how many coefficients there will be for each poly term.
    for ( term in termLabels[indPolyTerm]) {
      termAsExpression <- parse(text = term)
      vars <- all.vars(termAsExpression)
      vars.df <- data.frame(matrix(1:20,nrow=20,ncol=length(vars)))
      names(vars.df) <- vars
      thisNumCoeffs <- dim(eval(termAsExpression, vars.df))[2]
      numCoeffs <- numCoeffs + thisNumCoeffs
    }

  } else {
    # Get the number of terms.
    numCoeffs <- length(attr(myTerms,"term.labels"))
  }
  
  if ( includeIntercept ) {
    # Is there an intercept?
    if ( attr(myTerms,"intercept") ) numCoeffs <- numCoeffs + 1
  }
  
  # Return value.
  return(numCoeffs)
  
}

#-----------------------------------------------------------------------------------------

delete.response.formula <- function(myFormula) {
  
  # Return the formula without it's response variable.
  
  # Check it is a formula.
  if ( !inherits(myFormula, "formula") ) myFormula <- as.formula(myFormula)
  
  myTerms <- terms(myFormula)
  return(reformulate(attr(myTerms,"term.labels")))
  
}

#-----------------------------------------------------------------------------------------

davesExtract.v2 <- function(myRaster, xy){
 
  # Extract the values from myRaster at the points in xy.  Faster than raster extract function!
  
  # Get one layer of the raster 
  if ( ! inherits(myRaster, c("RasterStack", "RasterLayer")) ) {
    stop("Unrecognised class for first argument.  Expected RasterStack or RasterLayer.")
  }
  
  # Get the extent of the raster (range for both x and y)
  myExt <- extent(myRaster)
  
  # Get the cell size for the raster
  cellSize <- res(myRaster)
  
  # Get the cell index (one for x and one for y) for each xy point.
 ##dp is right  ix <- ceiling((xy[,1]-myExt@xmin)/cellSize[1])
  ix <- floor((xy[,1]-myExt@xmin)/cellSize[1])+1
  ix[ix > dim(myRaster)[2]] <- dim(myRaster)[2]
  ## end DP right
  
  ix[ix == 0] <- 1
  iy<-ceiling((myExt@ymax-xy[,2])/cellSize[2])
  iy[iy == 0] <- 1
  ixy <- cbind(iy,ix)
  
  # Extract values from these cells.
  numLayers <- nlayers(myRaster)
  vals <- NULL
  for ( i in 1:numLayers ) {
    myMat <- as.matrix(myRaster[[i]])
    vals <- cbind(vals, myMat[ixy])
  }
  
  # Add column names to the return values.
  colnames(vals) <- names(myRaster)
  
  # Return values.
  return(vals)
}  

#-----------------------------------------------------------------------------------------

davesExtract.v3 <- function(myRaster, xy){
  
  # Extract the values from myRaster at the points in xy.  Faster than raster extract function!
  
  # Get one layer of the raster 
  if ( ! inherits(myRaster, c("RasterStack", "RasterLayer")) ) {
    stop("Unrecognised class for first argument.  Expected RasterStack or RasterLayer.")
  }
  
  # Get the extent of the raster (range for both x and y)
  myExt <- extent(myRaster)
  
  # Get the cell size for the raster
  cellSize <- res(myRaster)
  numCols <- ncol(myRaster)
  numRows <- nrow(myRaster)
  
  # Get the cell indices (one for row and one for column) for each xy point.
  # Indices start at top left (as for a matrix). Cells contain top border and left border.
  # Right most border becomes part of cells to the left, and, bottom most border becomes 
  # part of cells above.
  icol <- floor((xy[,1] - myExt@xmin) / cellSize[1]) + 1
  icol[icol > numCols] <- numCols
  irow <- floor((myExt@ymax - xy[,2]) / cellSize[2]) + 1
  irow[irow > numRows] <- numRows
  ixy <- cbind(irow,icol)
  
  # Extract values from these cells.
  numLayers <- nlayers(myRaster)
  vals <- NULL
  for ( i in 1:numLayers ) {
    myMat <- as.matrix(myRaster[[i]])
    vals <- cbind(vals, myMat[ixy])
  }
  
  # Add column names to the return values.
  colnames(vals) <- names(myRaster)
  
  # Return values.
  return(vals)
}  

#-----------------------------------------------------------------------------------------

write.list <- function(myList, file=file.path(getwd(),"listData.txt"), append=FALSE){
  
  # Write the given list to the given file in a way that is easy to read.
  # Essentially opens a file and writes the console output into the file.
  # Using "sink" is necessary as "print" doesn't have a file option.
  
  # Open the file.
  if ( append ) {
    outConn <- file(file, open="at")
  } else {
    outConn <- file(file, open="wt")
  }
  
  # Divert the console output to the file.
  sink(outConn, type="output")

  # Print the list (this is the nicest format of all the functions that deal with output).
  print(myList)
  
  # Stop the diversion to the output file.
  sink()
  
  # Close the file.
  close(outConn)
}

#-----------------------------------------------------------------------------------------

getValuesMask <- function(x, mask, maskValue=NA) {
  
  # Get the values at the cells defined by mask to be included (i.e. mask cell != maskValue)
  # Do not get the values at the cells where mask cell == maskValue. 
  # Assumes x and mask have the same raster structure (i.e. number of rows and columns, 
  # and the same resolution).  
  # Returns a vector (or matrix where x is RasterStack) of the values at the included cells.

  # Get the cells that are to be included.
  centres <- data.frame(xyFromCell(x, 1:ncell(x)), stringsAsFactors = FALSE)

  # Get which of these centre points are to be included.  
  if ( !is.null(mask) ) { 
    # Use mask to determine points to be included or excluded.
    maskValues <- drop(davesExtract.v3(mask, centres))
    if ( is.na(maskValue) ) {
      indIncludePoints <- !is.na(maskValues)
    } else {
      indIncludePoints <- maskValues != maskValue
    }
  } else {
    # Include all points.
    indIncludePoints <- rep(TRUE, times=dim(centres)[1])
  }
  
  # Return the values of the raster object at these included cells.
  valsAtCentres <- davesExtract.v3(x, centres[indIncludePoints, ])
  return(valsAtCentres)
  
}

#-----------------------------------------------------------------------------------------

setValuesMask <- function(values, mask, maskValue=NA, otherCellsValue=maskValue) {
  
  # Set the values at the cells defined by mask to be included (i.e. mask[i,j] != maskValue)
  # to the values given.  Set the other cells to be the value given by otherCellsValue.
  # Returns a raster with the same raster structure as mask (i.e. number of rows and columns, 
  # and the same resolution).  Assumes that the length of values is the number of cells
  # to be included.
  
  # Get the cells that need to be included.
  indIncludeCells <- cellFromMask(mask, mask, maskValue, index=FALSE)

  # Return the raster object with these cells set to their new values.
  retRaster <- raster(mask)
  retRaster[indIncludeCells] <- values
  retRaster[!indIncludeCells] <- otherCellsValue
  return(retRaster)
  
}

#-----------------------------------------------------------------------------------------

xyFromCellMask <- function(x, mask, maskValue=NA) {
  
  # Get the coordinates of the centre of the x raster cells for the cells defined to be 
  # included by the mask (i.e. mask[i,j] != maskValue).
  # Unlike xyFromCell function in raster package, this returns a two column data.frame.
  
  # Get the cells that need to be included.
  whichCellsIncluded <- cellFromMask(x, mask, maskValue, index=TRUE)
  
  # Return the xy values from these cells.
  xy <- xyFromCell(x, cell=whichCellsIncluded)
  return(as.data.frame(xy, stringsAsFactors=FALSE))
  
}

#-----------------------------------------------------------------------------------------

cellFromMask <- function(x, mask, maskValue=NA, index=TRUE) {
  
  # Get the cell numbers of x that are defined to be included by the mask.  Note that x 
  # and mask can have different raster structures as long as mask extent is >= x extent.
  #
  # Arguments ...
  # x:         a raster from which the cell numbers are determined.
  # mask:      raster that defines included and excluded parts of the extent.
  # maskValue: value that is used in the cells for the excluded areas of the extent.
  # Index:     TRUE returns the cell number of the included cells, FALSE returns a logical
  #            vector with a TRUE for included cells and a FALSE for excluded cells.

  # Get the centre points of each cell of x.
  centres <- data.frame(xyFromCell(x, 1:ncell(x)), stringsAsFactors = FALSE)
  
  # Get the cells of x that need to be included.
  if ( !is.null(mask) ) { 
    # Use mask to determine centre points to be included or excluded.
    maskValues <- drop(davesExtract.v3(mask, centres))
    if ( is.na(maskValue) ) {
      indIncludeCells <- !is.na(maskValues)
    } else {
      indIncludeCells <- maskValues != maskValue
    }
  } else {
    # Include all points.
    indIncludeCells <- rep(TRUE, times=dim(centres)[1])
  }
  
  # Set return value.
  if ( index ) {
    # Index vector (i.e. vector of cell numbers that are to be included.)
    return(which(indIncludeCells))
  } else {
    # Logical vector of whether or not the cell is included (in raster cell order).
    return(indIncludeCells)
  }
  
}

#-----------------------------------------------------------------------------------------

domainAreaMask <- function(mask, maskValue=NA) {
  
  # Get the area of the domain (i.e. area of all the cells included according to mask).
  # NB: this is in what ever units squared the resolution of the mask is in.
  
  # Area of a cell.
  cellArea <- prod(res(mask))
  
  # The number of cells that are included in the domain.
  cellsInDomain <- cellFromMask(mask, mask, maskValue)
  numCellsInDomain <- length(cellsInDomain)
  
  # The area of the domain.
  areaDomain <- numCellsInDomain * cellArea
  return(areaDomain)
  
}

