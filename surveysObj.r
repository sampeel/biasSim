initSurveys <- function() {
  
  # Initialise or reset the survey object.
  
  # Create the object.
  surveys <- list(numSurveys=0,          # The number of samples
                  xy = NULL,             # A two column data.frame that contains the locations of the surveys (random point within cell???)
                  cells = NULL,          # A vector of length numSurveys that contains the survey cell number of the cells within the domain (cells of a raster).
                  rowsInCells = NULL,    # The row number of each survey's cell number in the cells object (vector of length numSurveys).
                  areas=NULL,            # The area of each survey (a vector with the same row order as cells in this list)
                  isError=FALSE          # Error indicator.
  )
  
  # Return value.
  return(surveys)
  
}
  
#-----------------------------------------------------------------------------------------

makeSurveyLocations <- function(domainMask, cellObj, numSurveys, minArea, maxArea, 
                                myFormula, zeta,
                                numClusters=0, widthCluster=3){
  
  # Simulates the survey locations using the given arguments.  The survey location is 
  # the cell of the domain in which the survey has happened.  Each survey has an area 
  # associated with it (random amount between the given limits) but no shape is assigned 
  # to this area.  Each survey area is assumed to occur wholely within its designated cell.  
  # Returns a survey object.
  #
  # Arguments ...
  # domainMask:   a raster layer that defines the projection and resolution of the domain 
  #               (as well as the cells that are included)
  # cellObj:      a cell object that contains info about each cell in the domain.
  # numSurveys:   number of survey locations to simulate
  # minArea:      minimum survey area allowed for simulated survey area
  # maxArea:      maximum survey area allowed for simulated survey area
  # myFormula:    bias formula (but only the sample bias part is used i.e. the intercept 
  #               is set to zero).
  # zeta:         true value of delta (from bias formula) without the deltaMultiplier.
  # numClusters:  number of clusters the survey locations are grouped into
  #               numClusters=0 gives random locations (regardless of widthCluster value???).
  # widthCluster: width, in number of cells, of the rectangular area of each cluster.
  #               eg. widthClust=3, each cluster will include the nine cells around and 
  #               including its cluster focal cell (cell where the centre of the cluster is).

  # Initialise the return value.
  surveys <- initSurveys()

  # Check that numSurveys has been set and is a positive number.
  if ( numSurveys < 1 ) {
    surveys$isError <- TRUE
    stop("The number of surveys is not a positive integer.") 
  } else {
    surveys$numSurveys <- numSurveys
  }
  
  # Check that the survey areas are non-zero and valid.
  if ( minArea <= 0.0 || maxArea <= 0.0 ) {
    surveys$isError <- TRUE
    stop("Min and/or max area for surveys is not a positive number.")
  }
  
  # Check that maximum survey area is not less than the minimum survey area.
  if ( minArea > maxArea ) {
    tmp <- minArea
    minArea <- maxArea
    maxArea <- tmp
    warning("Minimum and maximum survey areas have been reversed.")
  }
  
  # Select the cells that are included in the clusters.
  if ( numClusters > 0 ) {
    # Check that the widthCluster value is odd.
    if ( widthCluster %% 2 == 0 ) {
      warning("Width of a cluster needs to be odd, 1 added to given value.")
      widthCluster <- widthCluster + 1
    }
    
    # Get probability of observation and scale it so that it sums to 1. 
    # This could be different to what is used to create PO points so recreate using lambda.
    # NB: it is irrelevant what the intercept is as it cancels out with scaling!
    coeffs <- c(0, zeta)   
    prObsCell <- lambda.cell(myFormula, coeffs, cellObj$biases)
    sumPrObs <- sum(prObsCell)
    prObsCell <- prObsCell / sumPrObs
    
    # Get location of survey clusters (parents)
    parentCells <- sample(cellObj$cells, numClusters, replace=FALSE, prObsCell)
    
    # Get location of surveys (children)
    childCellPairs <- myAdjacent(domainMask, parentCells, widthCluster, idCells = TRUE, include = TRUE)
    childCells <- unique(childCellPairs$adjacent)    
    
  } else if ( numClusters == 0 ) {
    # Want whole domain and random sampling.  
    childCells <- cellObj$cells
    prObsCell <- rep(1, cellObj$numCells)
      
  } else {
    # Negative clusters are meaningless!
    surveys$isError <- TRUE
    stop("Unable to make a negative number of clusters for surveying locations, check settings.")
  }
    
  # Generate the cells that will contain the required number of cells surveys.
  surveys$cells <- sample(childCells, numSurveys, replace=TRUE)   # uniform sampling of available cells
  surveys$rowsInCells <- match(surveys$cells, cellObj$cells)  

  # Generate the random survey points.
  centreCells <- xyFromCell(domainMask, surveys$cells)
  cellRes <- res(domainMask)
  x <- centreCells[ ,1] + runif(numSurveys, -cellRes[1]/2, cellRes[1]/2)
  y <- centreCells[ ,2] + runif(numSurveys, -cellRes[2]/2, cellRes[2]/2)
  surveys$xy <- data.frame(x=x, y=y, stringsAsFactors = FALSE)
  
  # Temporary plot ...
  # rlPrObs <- domainMask
  # rlPrObs[cellObj$cells] <- prObsCell
  # plot(rlPrObs, asp=1, main="Survey locations")
  # points(x, y, pch=".", col="red")
  
  # Get the area of each survey
  surveys$areas <- runif(numSurveys, min=minArea, max=maxArea)
  
  # Return value.
  return(surveys)

}

#-----------------------------------------------------------------------------------------

is.surveys <- function(surveys) {
  
  # Test whether the argument is a valid surveys object.  Returns true if it is, false otherwise.
  # NB: only tests names of items at this stage, not classes of items!
  
  # Check argument is the right class (as far as we can!)
  if ( !is(surveys, "list") ) {
    # The argument is not even a list.  It is not a surveys object.
    return(FALSE)
  }
  
  # Get the expected names of the items for a surveys object.
  objectItemNames <- names(initsurveys())
  
  # Check the surveys argument has the same items.
  if ( all(names(surveys) %in% objectItemNames) ) {
    # The same item names, hence, a valid surveys object.
    return(TRUE)
    
  } else {
    # Not the same item names, hence, an invalid surveys object.
    return(FALSE)
  }
  
}

#-----------------------------------------------------------------------------------------

# as.data.frame.surveys <- function(surveys) {
#   
#   # Convert the information in the surveys object into a data.frame object.
#   
#   # Check it is a survey object first.
#   if ( !is.surveys(surveys) ) 
#     stop("Argument is not a recognised surveys object.  Cannot convert to a data.frame.")
#   
#   # Check there is data to convert.
#   if ( surveys$numSurveys <= 0 ) {
#     stop("There are no surveys to convert to a data.frame.")
#   }
#   
#   # Initialise return value with locations
#   dfSurveys <- data.frame(x = surveys$locations$x, y = surveys$locations$y, stringsAsFactors = FALSE)
#   
#   # Add areas
#   dfSurveys$area <- surveys$areas
#   
#   # Add extents
#   dfSurveys$xmin <- surveys$extents$xmin
#   dfSurveys$xmax <- surveys$extents$xmax
#   dfSurveys$ymin <- surveys$extents$ymin
#   dfSurveys$ymax <- surveys$extents$ymax
#     
#   # Return value.
#   return(dfSurveys)
# 
# }

#-----------------------------------------------------------------------------------------

# setSurveyLocations <- function(mask, xy, area=NULL, maskValue=NA){
#   
#   # Converts the given survey locations within the domain to cell numbers in a survey object.
#   # Uses mask to define where the domain is located (any cell = maskValue is outside domain).
#   # Returns a survey object.
#   #
#   # Arguments ...
#   # mask: a RasterLayer giving the valid and non valid cells in the domain.
#   # xy:   a two column data.frame containing the location of the surveys within the domain.
#   # area: a scalar or vector that contains the area of each survey (if a vector, must be
#   #       same length as xy).
# 
#   # Initialise the return value.
#   surveys <- initSurveys()
#   
#   # Get the number of surveys.
#   surveys$numSurveys <- dim(xy)[1]
#   
#   # Set the xy values.
#   surveys$xy <- as.data.frame(xy, stringsAsFactors=FALSE)
#   
#   # If there are survey areas then set them.
#   if ( ! is.null(surveyArea) ) surveys$areas <- surveyArea
#   
#   # Use the raster mask in the domain object to get the cell number of each survey.
#   surveys$cells <- cellFromXY(mask, surveyXY)
#   
#   # Check none of the surveys are outside the domain (given by mask)
#   surveyMaskVals <- mask[surveys$cells]
#   if ( is.na(maskValue) ) {
#     if ( any(is.na(surveyMaskVals)) ) {
#       stop("Some survey locations (as given by xy coordinates) are outside the domain.")
#     }
#   } else {
#     if ( any(surveyMaskVals == maskValue) ) {
#       stop("Some survey locations (as given by xy coordinates) are outside the domain.")
#     }
#   }
# 
#   # Return value.
#   return(surveys)
#   
# }

#-----------------------------------------------------------------------------------------

testPoissonCluster <- function(nPA, nClusts, childCellWidth=3,
                             nrows=64, ncols=96, simExt=extent(0,12,0,8), doPlots=TRUE) {
  
  # Test my own version of this spatstat function.  Need my own version so that I can
  # control the number of survey points.  Means I'll probably need a few more sim settings!
  #
  # nPA:            number of survey locations required
  # nClusts:        number of clusters of survey locations required (i.e. number of parent
  #                 points)
  # lambda.formula: string that specifies covariates and their relationship in intensity.
  # lambda.coeffs:  coefficients for above formula (including intercept).
  # childCellWidth: number of cells widths (around and including parent cell, i.e. odd nums) 
  #                 Really probably only works if cells are roughly square.  If, for example, 
  #                 they are long, thin rectangles then child points are not going to fall 
  #                 randomly around parent points but have a linear nature to their 
  #                 positioning.  But will do for test!
  # nrows:          number of rows in the raster that forms the domain (and cell structure).
  # ncols:          number of columns in the raster that forms the domain.
  # simExt:         extent of the domain.
  # doPlots:        whether or not to plot results.
  
  # # Numbers of things.
  # ncells <- nrows * ncols
  # 
  # # Set up domain.
  # rlBlank <- raster(simExt, nrows=nrows, ncols=ncols)
  # #rlMask <- setValues(rlBlank, TRUE)
  # #domainObj <- makeDomain(rlMask, NA)
  # 
  # # Set up the covariates
  # centreCells <- as.data.frame(xyFromCell(rlBlank, 1:ncells), stringsAsFactors=FALSE)
  # x <- centreCells$x
  # y <- centreCells$y
  # 
  # # Set up true intensity.
  # trueIntensity <- testLambda(x, y, lambda.formula, lambda.coeffs)
  # if (doPlots) {
  #   rlTrueIntensity <- setValues(rlBlank, trueIntensity)
  #   plot(rlTrueIntensity, asp=1)
  # }
  # 
  # # Make a probability of observation for each cell. (this could be dodgy! should be 1 - exp(-lambda)?)
  # sumTrueIntensity <- sum(trueIntensity)
  # probObsCell <- trueIntensity/sumTrueIntensity
  
  # Select the cells that are to be parent cells.
  parentCells <- sample(1:ncells, nClusts, replace=FALSE, probObsCell)
  if ( doPlots ) {
    # Add cells that contain parent points.
    points(x[parentCells], y[parentCells], pch=".")    
  }
  
  # Select the cells that can contain the children around and including the parent cells.
  if ( childCellWidth %% 2 == 0 ) {
    warning("childCellWidth needs to be odd, 1 added to given value.")
    childCellWidth <- childCellWidth + 1
  }
  neighbourMat <- matrix(1, nrow = childCellWidth, ncol = childCellWidth)
  neighbourMat[ceiling(childCellWidth/2), ceiling(childCellWidth/2)] <- 0    # parent cell!
  childCellPairs <- adjacent(rlBlank, parentCells, neighbourMat, pairs=TRUE, include=TRUE)
  childCellPairs <- as.data.frame(childCellPairs)
  childCells <- unique(childCellPairs$to)
  if ( doPlots ) {
    # Add cells that can contain child points.
    points(x[childCells], y[childCells], pch=".", col="red")
  }
  
  # Generate the cells that will contain the surveys.
  surveyCells <- sample(childCells, nPA, replace=TRUE)
  if ( doPlots ) {
    # Add plot of survey locations (assign x,y coordinates within each cell).
    cellRes <- res(rlBlank)
    xPA <- x[surveyCells] + runif(nPA, -cellRes[1]/2, cellRes[1]/2)
    yPA <- y[surveyCells] + runif(nPA, -cellRes[2]/2, cellRes[2]/2)
    points(xPA, yPA, pch="+")
  }
  
  # How many surveys in each cluster?
  clusters <- data.frame(cluster=1:nClusts, parentCell=parentCells, numChildCells=rep(0.0,nClusts),
                         numSurveys=rep(0.0,nClusts), stringsAsFactors = FALSE)
  for ( cell in surveyCells ) {
    # Which is this cell's parent?
    indCell <- which(childCellPairs$to == cell)
    parentsOfCell <- childCellPairs$from[indCell]
    
    # Add proportion of child to each parent.
    numParents <- length(parentsOfCell)
    indParents <- which(parentCells %in% parentsOfCell)
    clusters$numSurveys[indParents] <- clusters$numSurveys[indParents] + (1.0/numParents)
  }
  
  # How many child cells in each cluster?
  for ( cell in childCells ) {
    # Which is this cell's parent?
    indCell <- which(childCellPairs$to == cell)
    parentsOfCell <- childCellPairs$from[indCell]
    
    # Add proportion of child to each parent.
    numParents <- length(parentsOfCell)
    indParents <- which(parentCells %in% parentsOfCell)
    clusters$numChildCells[indParents] <- clusters$numChildCells[indParents] + (1.0/numParents)
  }
  
  # Return value.
  return(list(cells=surveyCells, x=xPA, y=yPA, clusters=clusters))
  
}

#-----------------------------------------------------------------------------------------

myAdjacent <- function(domain, cells, width, idCells=FALSE, include=TRUE) {
  
  # My version of raster::adjacent function (which is too slow in my sim to use). 
  # Cells must contain cell numbers that are within the domain.
  #
  # Arguments
  # domain:  a raster layer that givens the cells that are included in the domain (i.e. a mask).
  # cells:   a vector containing the cells whose adjacent cells are to be found (but only 
  #          those in the domain)
  # width:   the width (in number of cells) of the box of cells around the given cells that
  #          are considered to be adjacent.  For example, if width = 3, then the eight cells
  #          (to the left, right, top, bottom, and diagonally NW, NE, SE and SW of a given 
  #          cell from cells) are adjacent to "cell" and potentially part of the answer.
  # idCells: return the cell number (from cells) that each returned cell is adjacent to. 
  #          If this is TRUE, a matrix with "cell" and "adjacent" columns is returned.
  # include: include the cell number (from cells) in the result.
  
  # Numbers of things.
  numCols <- domain@ncols
  numCells <- domain@nrows * numCols
  
  # Check cell numbers within argument cells are valid domain cell numbers.
  #domainCells <- 1:numCells 
  domainCells <- cellFromMask(domain, domain)
  if ( length(setdiff(cells, domainCells)) ) {
    stop("One or more values in argument 'cells' are not valid domain cell numbers.")
  }
  
  # Width needs to be odd.
  if ( width %% 2 == 0 ) {
    warning("width needs to be odd, 1 added to given value.")
    width <- width + 1
  }
  
  # What are the potential cells?
  potentialCells <- NULL     #data.frame(cell=NULL, adjacent=NULL, stringsAsFactors = FALSE)
  neighbours <- (1:width) - ((width %/% 2) + 1)
  for ( i in neighbours ) {
    for ( j in neighbours ) {
      # Work out what the cell number of this neighbour is for each cell in cells.
      neighbourCells <- cells + j + (numCols * i)
      potentialCells <- rbind(potentialCells, cbind(cells, neighbourCells))
    }
  }
  
  # Include the original cells in the result?
  if ( ! include ) {
    # No, remove them.
    indCells <- match(potentialCells[ ,2], cells)
    potentialCells <- potentialCells[!indCells, ]
  }
  
  #Get rid of cells that are not in the domain (including cells outside extent!)
  indValidCells <- potentialCells[ ,2] %in% domainCells
  adjacentCells <- as.data.frame(potentialCells[indValidCells, ], stringsAsFactors = FALSE)
  names(adjacentCells) <- c("cell", "adjacent")
  
  # Return value.
  if ( idCells ) {
    # Return the a matrix that also contains a column of cells that the adjacent cells are adjacent to.
    return(adjacentCells)
  } else {
    # Return a vector of the adjacent cells (this may include the original cells as adjacent cells!)
    return(adjacentCells$adjacent)
  }
  
}

#-----------------------------------------------------------------------------------------

makeNSurveyed <- function(surveys, N, cellArea=1) {
  
  # Get the number of individuals counted in each sample.  Need to make sure that N is not
  # exceeded in any cell by considering all samples in a cell at the same time.  Returns 
  # the number of individuals counted in each sample for each species (numSurveys x numSpecies)
  #
  # Arguments ...
  # surveys:    a survey object that contains the cell, area and rowInCells values for each samples
  # N:          the number of individuals in each cell for each species (numCells x numSpecies).
  # cellArea:   a scalar containing the area of all cells in the domain (assumes all cells 
  #             have the same area).
  
  # Get the species.
  namesSpecies <- names(N)
  numSpecies <- length(namesSpecies)

  # Get the unique survey cells (those cells that contain one or more surveys)
  uniqueSurveyCells <- table(surveys$cells)
  numUniqueSurveyCells <- length(uniqueSurveyCells)
  
  # Initialise return value.
  NSurveyed <- as.data.frame(matrix(nrow = surveys$numSurveys, ncol = numSpecies), stringsAsFactors=FALSE)
  names(NSurveyed) <- namesSpecies
  
  # Fill each element of the list vector.
  for ( i in 1:numUniqueSurveyCells ) {
    # Initialise list of surveys in each cell.
    surveyLst <- list(cell=as.integer(names(uniqueSurveyCells)[i]), 
                      numSurveys=as.integer(uniqueSurveyCells[i]), 
                      rowInCells=NULL, 
                      rowsInSurveys=NULL,
                      areas=NULL)
    
    # What is the row in the survey object? (should be different for each survey in this cell)
    surveyLst$rowsInSurveys <- which(surveys$cells %in% surveyLst$cell)
    if ( length(surveyLst$rowsInSurveys) != surveyLst$numSurveys ) 
      stop("There should be a rowsInSurveys value for each of the surveys in this cell.")
    
    # What is the row in the cell object? (should be the same for all surveys in this cell)
    if ( length(unique(surveys$rowsInCells[surveyLst$rowsInSurveys])) != 1 ) 
      stop("Same survey cells should have same row in cells object!")
    surveyLst$rowInCells <- surveys$rowsInCells[surveyLst$rowsInSurveys[1]]

    # What is the area of each survey in the cell.
    surveyLst$areas <- surveys$area[surveyLst$rowsInSurveys]
    
    # Probability of being in each survey or outside the survey.
    prob <- surveyLst$areas/cellArea
    prob <- c(prob, 1 - sum(prob))
    
    for ( species in namesSpecies ) {
      # Split number of individuals in this cell into those in each survey and those not surveyed.
      tmp <- rmultinom(1, N[surveyLst$rowInCells,species], prob)
    
      # Save number surveyed.
      NSurveyed[surveyLst$rowsInSurveys,species] <- tmp[1:surveyLst$numSurveys,1]
    }
  }
  
  
  # Return value.
  return(NSurveyed)
  
}

#-----------------------------------------------------------------------------------------

