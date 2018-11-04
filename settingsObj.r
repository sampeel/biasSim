initSimulation <- function(numRuns=0, numCoresUse=0) {
  
  # Initialise or reset the simulation object.
  # Contains the bits and pieces that specify what sort of simulation is to be performed.
  
  # Create the simulation list object.
  simObj <- list(ext=NULL,             # Extent, domain or observation window of the simulation.
                 proj=NULL,            # Projection that the simulation is to be performed in.
                 numRuns=0,            # Number of simulation runs/repeats to perform.
                 numSpecies=0,         # Number of species to be simulated.
                 namesSpecies=NULL,    # Vector of character strings (numSpecies x 1)
                 lambda.formula=NULL,  # String that is Intensity formula (without covariates and offset) see "formula" help in R
                 bias.formula=NULL,    # String that is Sample bias formula (without covariates and offset) see "formula" help in R
                 isCentred=FALSE,      # Whether or not to centre the model data (i.e. how the covariates are used in the formulae).
                 coeffs=NULL,          # Log intensity function coefficients (numCoeffs x numSpecies)
                 gamma=NULL,           # Log sample bias function coefficient (numSpecies x 1)
                 delta=NULL,           # Log sample bias function coefficient (numBiases x 1)
                 deltaMultiplier=1.0,  # Multiplier for delta (alters only the amount of anthropogenic sample bias)
                 gammaMultiplier=1.0,  # Multiplier for gamma (alters only the amount of species specific sample bias)
                 numSurveys=NULL,      # Number of presence-absence surveys to simulate
                 minSurveyArea=NULL,   # Minimum area for a survey (in square sim units, e.g. km^2)
                 maxSurveyArea=NULL,   # Maximum area for a survey (set same as minimum for equal areas)
                 numClusters=0,        # Number of clusters to group the surveys into, numClusters=0 gives random sampling.
                 widthClusters=NULL,   # Width of area that each cluster potentially occupies (in number of cells)
                 #numPOPoints=NULL,     # Number of presence-only points to generate for each species (numSpecies x 1)
                 numBGPoints=NULL,     # Number of background points to generate for multispeciesPP
                 minPrevalence=0,      # Minimum number of locations with evidence of species occupation (i.e. presences or non-zero abundances)
                 #optimRes=NULL,       # Resolution to use if the optimisation method is "raster" (vector of length two)
                 useSDM.Count=FALSE,   # Use a glm with count data as the SDM
                 useSDM.Cloglog=FALSE, # Use a glm with presence-absence data and a cloglog as the SDM
                 useSDM.PPM=FALSE,     # Use the PPM function with presence-only data as the SDM
                 useSDM.MsPP=FALSE,    # Use the MsPP function with presence-absence and presence-only data as the SDM
                 isOutputToFile=FALSE, # Whether or not to send messages to stdout and stderr, or, a file.
                 inputDir="",          # Input can come from this directory (for processed data that is used every time a scenario is tested)
                 outDir="",            # Output goes to this directory.
                 outFile="",           # Text output (eg. from functions message, error, warning, cat, print) goes to this file.
                 outWarnOption=0,      # The way warnings will be presented, 0 = in a group at end, 1 = immediately.
                 dataDumpDir="",       # The directory that will contain the saved data files if a warning or error occurs during an SDM run.
                 resultsDir="",        # The directory that will contain the saved results (in "RData" format)
                 numCoresUse=0,        # Number of cores to use in parallel processing (i.e. mclapply function).
                 #
                 # The following values are worked out by the code, no need to specifically set.
                 #
                 #domain=NULL,         # Replaced by domainObj!! valid points within the extent (i.e. owin object with boundary and mask)
                 numCoeffs=0,          # Saves number of coefficients (= number of rows in coeff)
                 numBiases=0,          # Saves number of sample bias covariates (from delta)
                 origWarnOption=NULL,  # Value of options("warn") when simulation starts, reset to this at end.
                 isError=FALSE         # Whether or not there has been an error within a function
  )
  
  # Set the number of simulations and number of cores to use.
  simObj$numRuns <- numRuns
  simObj$numCoresUse <- numCoresUse
  
  # Return value.
  return(simObj)
  
}  

#-----------------------------------------------------------------------------------------

setSimGeography <- function(simObj, extSim, projSim) {

  # Set the extent and projection of the simulation.
  simObj$ext <- extSim
  simObj$proj <- projSim

  # Return value.
  return(simObj)

}

#-----------------------------------------------------------------------------------------
  
setSimIntensity <- function(simObj, numSpecies, lambda.formula, bias.formula, beta, 
                            alpha=NULL, gamma=NULL, delta=NULL, 
                            namesSpecies=paste("species", 1:numSpecies, sep=""),
                            isCentred=TRUE) {
  
  # Sets these values in the settings object.
  # Works out how many environment covariates and how many sample bias covariates.
  
  # Set the number of species and check coefficients are consistent with this number.
  simObj$numSpecies <- numSpecies
  if ( !is.null(alpha) && (simObj$numSpecies != length(alpha)) ) {
    simObj$isError <- TRUE
    msg <- paste("Alpha coefficient has the wrong length, should be = ",
                 simObj$numSpecies, ".", sep="")
    stop(msg)
  }
  if ( simObj$numSpecies != dim(beta)[2] ) {
    simObj$isError <- TRUE
    msg <- paste("Beta coefficient has the wrong number of columns, should be = ",
                 simObj$numSpecies, ".", sep="")
    stop(msg)
  }
  if ( !is.null(gamma) && (simObj$numSpecies != length(gamma)) ) {
    simObj$isError <- TRUE
    msg <- paste("Gamma coefficient has the wrong length, should be = ",
                 simObj$numSpecies, ".", sep="")
    stop(msg)
  }
  
  # Set intensity coefficients.
  numBetas <- dim(beta)[1]
  if ( is.null(alpha) ) {
    simObj$coeffs <- beta
    rownames(simObj$coeffs) <- paste("beta", 1:numBetas, sep="")
  } else {
    simObj$coeffs <- rbind(alpha, beta)
    rownames(simObj$coeffs) <- c("alpha", paste("beta", 1:numBetas, sep=""))
  }
  colnames(simObj$coeffs) <- namesSpecies
  
  # Set the sample bias coefficients.
  simObj$gamma <- gamma
  simObj$delta <- delta
  numDeltas <- length(delta)
  
  # Set the number of intensity coefficients
  simObj$numCoeffs <- dim(simObj$coeffs)[1]
  
  # Set the number of sample bias covariates from delta.
  simObj$numBiases <- numDeltas
  
  # Set formulae.
  simObj$lambda.formula <- as.formula(lambda.formula)
  simObj$bias.formula <- as.formula(bias.formula)

  # Check consistency of intensity formula with coefficients.
  terms.attributes <- attributes(terms(simObj$lambda.formula))
  if ( numBetas != length(terms.attributes$term.labels) ) {
    simObj$isError <- TRUE
    msg <- paste("lambda.formula has the wrong number of covariate terms, should be = ",
                 numBetas, ".", sep="")
    stop(msg)
  }
  isIntercept <- terms.attributes$intercept
  if ( !is.null(alpha) == isIntercept ) {
    # All good.
    numCoeffs <- numBetas + isIntercept
  } else {
    simObj$isError <- TRUE
    stop("The presence or absence of an intercept is not consistent between arguments alpha and lambda.formula")
  }
  if ( simObj$numCoeffs != numCoeffs ) {
    simObj$isError <- TRUE
    msg <- paste("lambda.formula has the wrong number of terms, should be = ",
                 simObj$numCoeffs, ".", sep="")
    stop(msg)
  }
  
  # Check consistency of sample bias formula with coefficients.
  terms.attributes <- attributes(terms(simObj$bias.formula))
  if ( numDeltas != length(terms.attributes$term.labels) ) {
    simObj$isError <- TRUE
    msg <- paste("bias.formula has the wrong number of covariate terms, should be = ",
                 numDeltas, ".", sep="")
    stop(msg)
  }
  isIntercept <- terms.attributes$intercept
  if ( !is.null(gamma) == isIntercept ) {
    # All good.
    numCoeffs <- numDeltas + isIntercept
  } else {
    simObj$isError <- TRUE
    stop("The presence or absence of an intercept is not consistent between arguments gamma and bias.formula")
  }
  numCoeffs2 <- numDeltas + !is.null(gamma)
  if (  numCoeffs2 != numCoeffs ) {
    simObj$isError <- TRUE
    msg <- paste("bias.formula has the wrong number of terms, should be = ",
                 numCoeffs2, ".", sep="")
    stop(msg)
  }
  
  # Set species names and check there are the right number.
  if ( length(namesSpecies) != numSpecies ) {
    simObj$isError <- TRUE
    stop("The wrong number of species names has been given.  Check!")
  } else {
    simObj$namesSpecies <- namesSpecies
  }
  
  # Set whether or not to centre the model data.
  simObj$isCentred = isCentred
  
  # Return value.
  return(simObj)
  
}

#-----------------------------------------------------------------------------------------

setSimSurveyInfo <- function(simObj, numSurveys, minSurveyArea=1, maxSurveyArea=minSurveyArea, 
                             numClusters=0, widthClusters=Inf){
  
  # Sets the simulation list with these values.
  
  # Set survey information.
  simObj$numSurveys <- numSurveys
  simObj$minSurveyArea <- minSurveyArea
  simObj$maxSurveyArea <- maxSurveyArea
  simObj$numClusters <- numClusters
  simObj$widthClusters <- widthClusters
  
  # Check min and max survey area are in right order.
  if ( minSurveyArea > maxSurveyArea ) {
    simObj$isError <- TRUE
    stop("Minimum survey area is greater than maximum survey area, check settings.")
  }
  
  # Check that the number of clusters is positive.
  if ( numClusters < 0 ) {
    simObj$isError <- TRUE
    stop("Unable to make a negative number of clusters for surveying locations, check settings.")
  }
  
  # Return value.
  return(simObj)
  
}

#-----------------------------------------------------------------------------------------

setSimBGPoints <- function(simObj, numBGPoints=10000) {
  
  # Sets the simulation list with these values.
  
  # Set background points information.
  simObj$numBGPoints <- numBGPoints

  # Check number of points is positive.
  if ( numBGPoints < 1 ) {
    simObj$isError <- TRUE
    stop("The number of background points must be a positive integer.")
  }
  
  # Return value.
  return(simObj)
  
}

#-----------------------------------------------------------------------------------------

setSimPOPoints <- function(simObj, gammaMultiplier=1.0, deltaMultiplier=1.0){
  
  # Sets the simulation list with these values.
  
  simObj$gammaMultiplier <- gammaMultiplier
  simObj$deltaMultiplier <- deltaMultiplier
  
  # Return value.
  return(simObj)
  
}

#-----------------------------------------------------------------------------------------

setSimUseSDMs <- function(simObj, useCount=TRUE, useCloglog=TRUE, usePPM=TRUE, useMsPP=TRUE,
                          minPrevalence=10) {
  
  # Sets which SDMs to use in the simulation list object.
  
  # Set use SDM items in list.
  simObj$useSDM.Count <- useCount
  simObj$useSDM.Cloglog <- useCloglog
  simObj$useSDM.PPM <- usePPM
  simObj$useSDM.MsPP <- useMsPP
  
  # Check at least one of these is TRUE.
  if ( ! any(c(useCount, useCloglog, usePPM, useMsPP)) ) 
    stop("Please choose at least one SDM to try.")
  
  # Set minimum prevalence.
  simObj$minPrevalence <- minPrevalence
  
  # Return value.
  return(simObj)
  
}

#-----------------------------------------------------------------------------------------

setSimOutput <- function(simObj, inputDir=getwd(), outFileName="simOut.txt", outDir=getwd(), 
                         isOutputToFile=FALSE, dataDumpDir=getwd(), resultsDir=getwd(),
                         outWarnOption=getOption("warn"), dirSep="/") {
  
  # Sets the location of output for the simulation runs.  Also, options for simulations runs.
  # Creates directory, if necessary.  Opens connection to file, if necessary.
  
  # Set the values that control the output location.
  simObj$inputDir <- inputDir
  simObj$outDir <- outDir
  simObj$outFile <- paste0(outDir, dirSep, outFileName)
  simObj$isOutputToFile <- isOutputToFile
  simObj$dataDumpDir <- dataDumpDir
  simObj$resultsDir <- resultsDir
  simObj$outWarnOption <- outWarnOption
  #simObj$hugeRndObs <- hugeRndObs

  # Get the current warn option and then reset to the one specified for the simulation.
  simObj$origWarnOption <- getOption("warn")    
  options(warn=simObj$outWarnOption)
  
  # Get the current warning level for huge random observations and then reset.
  #simObj$origHugeRndObs <- spatstat.options("huge.npoints")
  #spatstat.options(huge.npoints=simObj$hugeRndObs)
  
  # If necessary, open file ready for diversion of output from stdout and stderr.
  if ( isOutputToFile ) {
    # Create directory, if necessary.
    if ( ! dir.exists(outDir) ) {
      dir.create(outDir, recursive = TRUE)
    }
    
    # Open file for output.
    # NB: using file automatically creates new file or overwrites/clears existing file.
    outConn <- file(simObj$outFile, open="wt")
    
    # Start diversion of output from stdout and stderror to a file.
    sink(file = outConn, type="output")
    sink(file = outConn, type="message")
    
    # File opened message.
    message(paste("Output file opened at", Sys.time()))
  } else {
    message(paste("Output started at", Sys.time()))
  }

  # Create the input directory for processed data, if necessary (NB: same across scenarios).
  if ( ! dir.exists(inputDir) ) {
    dir.create(inputDir, recursive = TRUE)
  }
  
  # Create data dump directory, if necessary.
  if ( ! dir.exists(dataDumpDir) ) {
    dir.create(dataDumpDir, recursive = TRUE)
  }
  
  # Create results directory, if necessary.
  if ( ! dir.exists(resultsDir) ) {
    dir.create(resultsDir, recursive = TRUE)
  }
  
  # Return value.
  return(simObj)

}

#-----------------------------------------------------------------------------------------

doSimCleanUp <- function(simObj) {
  
  # Turn off diversion of output to file, if necessary.
  if ( simObj$isOutputToFile ) {
    # File closed message.
    message(paste("File closed at", Sys.time()))
    
    # Turn off diversion.
    sink(type="message")
    sink(type="output")
  } else {
    message(paste("Finished at", Sys.time()))
  }
  
  # Change the warning option back to whatever it was before the simulation started.
  options(warn=simObj$origWarnOption)
  
  # Reset the spatstat options back to whatever it was before the simulation started.
  #spatstat.options(huge.npoints=simObj$origHugeRndObs)                                           

  # Garbage collection.
  gc()
  
  # Return value.
  invisible(NULL)
  
}

#-----------------------------------------------------------------------------------------

deSink <- function() {
  
  # Turn off diversion of output to file (usually lost ones caused by stopping the program).
  
  # Turn off message diversion.
  sink(type="message")

  # Turn off any other diversions.
  while ( sink.number() > 0) {
    sink()
    message("sink reversed")
  } 
  
}

#-----------------------------------------------------------------------------------------

numPOPointsPerSpecies <- function(numPOPoints, numSpecies, rateDexp, numDataSpecies, 
                                  speciesNames=paste0("species",1:numSpecies),
                                  minPrevalence=10, doPlots=FALSE) {
  
  # Assign numPOPoints to each species.  Use an exponential curve with specified rate to
  # assign points to each species such that they mimic the curve of real data.  
  
  # Scale species number to fit the number of species in the data.  
  speciesX <- 1:numSpecies
  binSize <- numDataSpecies/numSpecies
  binCentres <- seq(from=binSize/2, to=numDataSpecies, by=binSize)
  
  # Get probability of selecting as a particular species (area under curve).
  probSpeciesX <- dexp(binCentres, rateDexp) * binSize
  if ( doPlots ) plot(binCentres, probSpeciesX, type="l")
  
  # Take out the number of points required to meet the minimum prevalence threshold as
  # these will be automatically added at the end.
  numPoints <- numPOPoints - (minPrevalence * numSpecies)  
  if ( numPoints <= 0 ) stop("Not enough PO points being simulated for given minimum prevalence.")
  
  # Assign numPoints between species based on weightings from exponential curve
  newSpeciesPresences <- sample(speciesX, numPoints, replace=TRUE, probSpeciesX) 
  
  # Count the number for each species and sort most to least.
  numPointsPerSpecies <- sort(table(newSpeciesPresences), decreasing = TRUE)
  
  # Fill end with zeros for species that weren't sampled.
  numSpeciesNotSampled <- numSpecies - length(numPointsPerSpecies)
  if ( numSpeciesNotSampled > 0 ) {
    numPointsPerSpecies <- c(as.vector(numPointsPerSpecies), rep(0, numSpeciesNotSampled))
  }
  
  if ( doPlots ) points(binCentres, numPointsPerSpecies/numPoints, col="red")
  
  # Add back in the minimum prevalence.
  numPointsPerSpecies <- numPointsPerSpecies + minPrevalence
  if ( doPlots ) barplot(numPointsPerSpecies)
  
  # Sort the vector into highest first and add speciesNames.
  numPointsPerSpecies <- as.vector(sort(numPointsPerSpecies, decreasing = TRUE))
  names(numPointsPerSpecies) <- speciesNames
  
  # Return value.
  return(numPointsPerSpecies)
  
}

#-----------------------------------------------------------------------------------------
