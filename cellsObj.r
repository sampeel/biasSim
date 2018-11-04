#-----------------------------------------------------------------------------------------

initPopulation <- function(){
  
  # Initialise or reset the population object.  
  
  # Create the object.
  pop <- list(numCells = 0,               # The number of cells in the domain.
              numRowsExt = 0,             # The number of rows in the extent of the domain (so can recover lost cell numbers).
              numColsExt = 0,             # The number of cols in the extent of the domain (ditto).   
              numSpecies = 0,             # The number of species populations.
              namesSpecies = NULL,        # A vector of the names of the species (used as column headings and/or identifiers).
              resCell = NULL,             # A vector of length two that contains the width and length of a cell (width is x-axis and length is y-axis)
              areaCell = 0,               # The area of each cell in the domain (assumes area is the same for all cells).
              cells = NULL,               # A vector of length numCells that contains the cell number of the cells within the domain (cells of a raster).
              xy = NULL,                  # A two column data.frame with numCells rows where each row contains the centrepoint (x,y) of each cell.
              covars = NULL,              # A data.frame (numCells x numCovars) containing the environmental covariate values for each cell in the domain.
              biases = NULL,              # A data.frame (numCells x numBiases) containing the bias covariate values for each cell in the domain.
              trueLambda=NULL,            # The intensity (value of lambda) per cell per species (data.frame numCells x numSpecies)
              truePrObs=NULL,             # The probability of observation (value of b in Fithian, et al.) per cell per species (data.frame numCells x numSpecies)
              N = NULL,                   # The number of individuals per cell per species for a single run (data.frame numCells x numSpecies)
              runNum = 0,                 # The run number that has created the values in N.
              isError = FALSE             # Error indicator
  )
  
  # Return value.
  return(pop)
}

#-----------------------------------------------------------------------------------------

is.population <- function(population) {
  
  # Test whether the argument is a valid population object.  Returns true if it is, false otherwise.
  # NB: only tests names of items at this stage, not classes of items!
  
  # Check argument is the right class (as far as we can!)
  if ( !is(Population, "list") ) {
    # The argument is not even a list.  It is not a population object.
    return(FALSE)
  }
  
  # Get the expected names of the items for a population object.
  objectItemNames <- names(initPopulation())
  
  # Check the population argument has the same items.
  if ( all(names(population) %in% objectItemNames) ) {
    # The same item names, hence, a valid population object.
    return(TRUE)
    
  } else {
    # Not the same item names, hence, an invalid population object.
    return(FALSE)
  }
  
}

#-----------------------------------------------------------------------------------------

makeCells <- function(mask, maskValue){
  
  # Make the valid cell numbers from the raster mask (assumes the raster structure of mask
  # is the same as the covariate rasters used elsewhere in the simulation).
  
  # Initialise the return value.
  pop <- initPopulation()
  
  # Get the valid cell numbers from the mask raster.
  pop$cells <- cellFromMask(mask, mask, maskValue)
  
  # Get the centre point for each of these cells.
  pop$xy <- as.data.frame(xyFromCell(mask, cell=pop$cells), stringsAsFactors=FALSE)
  
  
  # Set the number of valid cells.
  pop$numCells <- length(pop$cells)[]
  
  # Set the number of columns and rows in the extent of the domain.
  pop$numRowsExt <- mask@nrows
  pop$numColsExt <- mask@ncols
  
  # Set the area of each cell.
  pop$resCell <- res(mask)
  pop$areaCell <- prod(pop$resCell)
  
  # Return value.
  return(pop)

}

#-----------------------------------------------------------------------------------------

makeCovarData <- function(pop, covars) {
  
  # Makes a data.frame version of the covars from the raster data.
  #
  # Arguments ...
  # pop:       a population object that contains the valid cells of the domain.
  # coeffs:    is a matrix of coefficients for the intensity function lambda (with 
  #            nrows=numCoeffs and ncols=numSpecies)
  # covars:    is a raster stack with n layers that returns a valid value for all points 
  #            in the domain.
  # speciesNames: a vector of character strings containing the names (identifiers) of the species.
  
  # The number of species.
  if ( is.null(pop) || is.null(pop$cells) || pop$numCells == 0 ) {
    pop$isError <- TRUE
    stop("Please make cells of the domain before extracting the covariate data of these cells.")
  }
  
  # The covariate values for the given cells.
  pop$covars <- extract(covars, pop$xy)
  
  # Convert to data.frame with column names.
  pop$covars <- as.data.frame(pop$covars, stringsAsFactors=FALSE)
  names(pop$covars) <- names(covars)
  
  # Return value.
  return(pop)
  
}
  
#-----------------------------------------------------------------------------------------

makeIntensity <- function(pop, myFormula, coeffs, namesSpecies=names(coeffs)) {
  
  # Calculates the value of the true intensity using the given arguments. 
  # Assumes there is a function 'lambda.cell' that calculates the intensity argument.
  # Results are returned in the cell object "pop".
  #
  # Assumes that there is an intensity function called "lambda.cell" (cell version of lambda).  
  #
  # Arguments ...
  # pop:       a population object that contains the valid cells of the domain.
  # myFormula: a formula that specifies the form of the lambda function to be used.
  # coeffs:    is a matrix of coefficients for the intensity function lambda (with 
  #            nrows=numCoeffs and ncols=numSpecies)
  # speciesNames: a vector of character strings containing the names (identifiers) of the species.
  
  
  # Check cells have been made.
  if ( is.null(pop) || is.null(pop$cells) || pop$numCells == 0 ) {
    pop$isError <- TRUE
    stop("Please make cells of the domain before calculating the true species intensities.")
  }
  
  # Check covars have been extracted.
  if ( is.null(pop$covars) ) {
    pop$isError <- TRUE
    stop("Please extract covariate data before calculating the true species intensities.")
  }

  # Convert formula to formula class if it is a character.
  if ( inherits(myFormula, "character") ) myFormula <- as.formula(myFormula)
  
  # Get the number of species.
  pop$namesSpecies <- namesSpecies
  pop$numSpecies <- length(namesSpecies)
  
  # Check this matches species columns in coeffs.
  if ( is.vector(coeffs) ) coeffs <- as.matrix(coeffs) 
  coeffColNames <- colnames(coeffs)
  if ( ! setequal(coeffColNames,namesSpecies) ) {
    pop$isError <- TRUE
    stop("Species in coefficients does not match given species names.")
  }

  # Initialise the return values.
  pop$trueLambda <- as.data.frame(matrix(nrow = pop$numCells, ncol = pop$numSpecies), 
                                  stringsAsFactors=FALSE)
  names(pop$trueLambda) <- namesSpecies
  
  # Create each species' true intensity values in each cell.
  for ( species in namesSpecies) {
    pop$trueLambda[ ,species] <- lambda.cell(myFormula, coeffs[ ,species], pop$covars)
  }
  
  # Return value.
  return(pop)
  
}

#-----------------------------------------------------------------------------------------

makeProbObs <- function(pop, myFormula, gamma, delta, biases, gammaMultiplier=1.0, deltaMultiplier=1.0) {
  
  # Calculates the value of the true probability of observation (b) using the given arguments. 
  # Results are returned in the population object "pop".
  #
  # Assumes that there is an intensity function called "lambda.cell" (cell version of lambda).
  # Formula is for the regression relationship ...
  #                      ln(b(s)) = gamma + delta * covars(s)
  #
  # Arguments ...
  # pop:       a population object that contains the valid cells of the domain.
  # myFormula: a formula that specifies the form of the lambda function to be used.
  # gamma:     a vector containing the coefficients related to species probability of 
  #            observation (length of numSpecies)
  # delta:     a vector containing the coefficients related to the human probability of
  #            observation (length of nlayer(biases))
  # biases:    a rasterStack containing sample bias covariate data for the given formula 
  #            and coefficients.

  # Check cells have been made.
  if ( is.null(pop) || is.null(pop$cells) || pop$numCells == 0 ) {
    pop$isError <- TRUE
    stop("Please make cells of the domain before calculating the true species intensities.")
  }
  
  # Extract bias covars.
  pop$biases <- extract(biases, pop$xy)
  pop$biases <- as.data.frame(pop$biases, stringsAsFactors=FALSE)

  # Convert formula to formula class if it is a character.
  if ( inherits(myFormula, "character") ) myFormula <- as.formula(myFormula)
  
  # Check this matches species names in gamma.
  if ( ! setequal(names(gamma), pop$namesSpecies) ) {
    pop$isError <- TRUE
    stop("Species names in gamma do not match given species names.")
  }
  
  # Initialise the return values.
  pop$truePrObs <- as.data.frame(matrix(nrow = pop$numCells, ncol = pop$numSpecies), 
                                  stringsAsFactors=FALSE)
  names(pop$truePrObs) <- pop$namesSpecies
  
  # Create each species' true probability of observation values in each cell.
  for ( species in pop$namesSpecies) {
    thisSpeciesCoeffs <- c(gamma[species]*gammaMultiplier, delta*deltaMultiplier)
    pop$truePrObs[ ,species] <- lambda.cell(myFormula, thisSpeciesCoeffs, pop$biases)
  }
  
  # Return value.
  return(pop)
  
}

#-----------------------------------------------------------------------------------------

makeNumIndivids <- function(pop, runNum=0) {
  
  # Simulates the species populations using the given arguments.  Uses the Poisson
  # distribution to randomly generate the number of each species in each cell of the domain.
  # Returns a cells object with the simulated species populations added in the data.frame N.
  #
  # Arguments ...
  # pop:       a population object that contains the valid cells of the domain.
  # runNum:    an integer that indicates which run the numbers of individuals are from
  #            (gets overwritten for each run)

  # Check cells have been made.
  if ( is.null(pop) || is.null(pop$cells) || pop$numCells == 0 ) {
    pop$isError <- TRUE
    stop("Please make cells of the domain before making species populations.")
  }
  
  # Check the true intensity for each species has been calculated.
  if ( is.null(pop$trueLambda) ) {
    pop$isError <- TRUE
    stop("Please make each species' true intensity before making species populations.")
  }
  
  # Initialise the return values.
  pop$N <- as.data.frame(matrix(nrow = pop$numCells, ncol = pop$numSpecies), stringsAsFactors=FALSE)
  names(pop$N) <- pop$namesSpecies
  
  # Create each species' population.
  for ( species in pop$namesSpecies ) {
    # Calculate the expected number of individuals in each cell of the domain.
    # This assumes that the intensity (or value of lambda) is homogeneous within each cell.
    muN <- pop$trueLambda[ ,species] * pop$areaCell

    # Generate the number of individuals in each cell (NB: different for each run!)
    pop$N[ ,species] <- rpois(pop$numCells, muN)
  }
  
  # The run number.
  pop$runNum <- runNum 
  
  # Return value.
  return(pop)
  
}

#-----------------------------------------------------------------------------------------

getCellVals <- function(pop, cells, item="covars") {
  
  # Get the specified item's values at the given cell numbers.  'item' is the name of any
  # list item in pop that has a first dimension with length = numCells (e.g. "xy", "trueLambda")
  
  # Check values are available.
  if ( is.null(pop[[item]]) ) {
    pop$isError <- TRUE
    stop("These cell values have not yet been specified so can not be returned.")
  }

  # Initialise return value.
  numWantedCells <- length(cells)  
  if ( numWantedCells == 0 ) {
    pop$isError <- TRUE
    stop("There don't appear to be any cells for which to get cell values.")
  }  
  # numCovars <- dim(pop$covars)[2]
  # retVals <- as.data.frame(matrix(nrows=numWantedCells, ncols=numCovars), stringsAsFactors=FALSE)
  # names(retVals) <- names(pop$covars)
  
  # Which rows are the wanted cell numbers in the pop list of valid cell nums?
  indRows <- match(cells, pop$cells)
    #which(pop$cells %in% cellNums)
  
  # Get covariate values for these cells
  if ( inherits(pop[[item]], "matrix") ) {
    retVals <- as.matrix(pop[[item]][indRows, ])  # Must return as a matrix even if has one column.
    names(retVals) <- names(pop[[item]])
  } else if ( inherits(pop[[item]], "data.frame") ) {
    retVals <- as.data.frame(pop[[item]][indRows, ]) # Must return as a data.frame even if has one column.
    names(retVals) <- names(pop[[item]])
  } else if ( inherits(pop[[item]], "vector") ) { 
    retVals <- pop[[item]][indRows]
  }
  
  # Return value.
  return(retVals)

}

#-----------------------------------------------------------------------------------------
