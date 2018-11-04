initLocations <- function(numLocs, numSpecies) {
  
  # Initialise the data collection locations object.
  
  # Create the object.
  locations <- list(numLocs = numLocs,       # Number of locations to create.
                    numSpecies = numSpecies, # Number of species to collect data on.
                    cells = NULL,            # a vector of length numLocs that will contain the cell number of each location
                    rowsInCells = NULL,      # The row number of each survey's cell number in the cells object (vector of length numSurveys).
                    xy = NULL,               # a data.frame with numLocs rows and 2 columns that will contain the (x,y) coordinate location of the centre of the data collection area
                    areas = NULL,            # a vector of length numLocs that will contain the area of each data collection location
                    Navailable = NULL,       # a data.frame with numLocs rows and numSpecies columns that will contain the number of individuals per species that are available within the data collection area
                    probDetection = 1.0,     # the probability of detecting an individual given that it is available within the data collection area
                    Ncollected = NULL,       # a data.frame with numLocs rows and numSpecies columns that will contain the number of individuals per species that were colleced within the data collection area  
                    isError = FALSE
                    )
  
  # Return the object.
  return(locations)
  
}

#-----------------------------------------------------------------------------------------

makeLocations <- function(locations, cellsObj, minArea, maxArea = minArea) {
  
  # Get the data collection locations, xy points and areas.
  
  # Get a random sample of the cells in the domain.  Repeats are allowed.
  locations$cells <- sample(cellsObj$cells, locations$numLocs, TRUE)
  locations$rowsInCells <- match(locations$cells, cellObj$cells)  
  
  # Get the xy coordinate for each location (a random point within the cell).
  xHalfWidth <- cellsObj$resCell[1] / 2.0
  yHalfWidth <- cellsObj$resCell[2] / 2.0
  locations$xy <- data.frame(matrix(nrow=locations$numLocs, ncol=2, 
                                    dimnames=list(1:locations$numLocs,c("x","y"))), 
                             stringsAsFactors = FALSE)
  cellCentres <- cellsObj$xy[locations$rowsInCells, ]
  locations$xy$x <- runif(locations$numLocs, -xHalfWidth, xHalfWidth) + cellCentres$x
  locations$xy$y <- runif(locations$numLocs, -yHalfWidth, yHalfWidth) + cellCentres$y
  
  # Get the area of each data collection location.
  locations$areas <- runif(locations$cells, minArea, maxArea)
  
  # Return value.
  return(locations)
  
}

#-----------------------------------------------------------------------------------------

makeIndividuals <- function(locations, cellsObj, probDetection = 1.0) {
  
  # Make the number of available and collected individuals within the data collection area.
  # trueLambda is only required for the cells in locations!
  
  # Get truelambda values at the data collection locations.
  trueLambda <- cellsObj$trueLambda[locations$rowsInCells, ]  # Up to here
  
  # Check trueLambda is the right length.
  if ( dim(trueLambda)[1] != locations$numLocs ) {
    locations$isError <- TRUE
    stop("Number of rows of trueLambda should be the number of data collection locations.")
  }
  if ( dim(trueLambda)[2] != locations$numSpecies ) {
    locations$isError <- trueLambda
    stop("Number of columns of trueLambda should be the number of species.")
  }
  
  # For each species ...
  namesSpecies <- colnames(trueLambda)
  locations$Navailable <- data.frame(matrix(nrow=locations$numLocs, ncol=locations$numSpecies,
                                            dimnames = list(1:locations$numLocs, namesSpecies),
                                     stringsAsFactors = FALSE))
  locations$Ncollected <- locations$Navailable
  for ( k in 1:locations$numSpecies ) {
    # Make the number of available individuals for this species.
    mu <- trueLambda[ ,k] * locations$areas
    locations$Navailable[ ,k] <- rpois(locations$numLocs, mu)
    
    # Make the number of collected individuals.
    for ( i in 1:locations$numLocs ) {
      locations$Ncollected[i,k] <- rbinom(1, locations$Navailable[i,k], probDetection)
    }
  }

  # Return value.
  return(locations)
  
}

#-----------------------------------------------------------------------------------------
