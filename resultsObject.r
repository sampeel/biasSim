#-----------------------------------------------------------------------------------------

initResults <- function(numRuns, numCoeffs, numBiases, namesSpecies, 
                        numSpecies=length(namesSpecies), statsAtRun=c("mse")){
  
  # Initialise or reset the results list object.
  #
  # Arguments ...
  # numRuns:    the number of runs in the simulation
  # numSpecies: the number of species in the data
  # numCoeffs:  the number of coefficients in the intensity function (NB: including x and y.) 
  # numBiases:  the number of sample bias covariates (gives number of deltas)
  # numSpecies: the number of species in the data
  # statsAtRun: the statistics to calculate for each run (choice of "mse", "rmse1", "rmse2", 
  #             or "nrmse")

  # Dummy names for dimensions.
  coeffNames <- c("alpha", paste("beta", 1:(numCoeffs-1), sep=""))
  speciesNames <- namesSpecies
  runNames <- paste("run", 1:numRuns, sep="")
  biasNames <- paste("bias", 1:numBiases, sep="")
  
  # Check stats are valid.
  if ( ! all(statsAtRun %in% c("mse", "rmse1", "rmse2", "nrmse")) )
    stop("Invalid statistic requested in results.")

  # Create a skeleton list to store the results from each SDM.
  arrayEmpty <- array(data=NA, dim=c(numCoeffs, numSpecies, numRuns))
  dimnames(arrayEmpty) <- list(coeffNames, speciesNames, runNames)
  matEmpty <- matrix(nrow=numSpecies, ncol=numRuns, dimnames = list(speciesNames, runNames))
  mat2Empty <- matrix(nrow=numCoeffs, ncol=numSpecies, dimnames = list(coeffNames, speciesNames))
  lstEmpty <- list(tryMe=FALSE, fit=NA, coeffs=arrayEmpty, SE=arrayEmpty, bias=mat2Empty, 
                   var=mat2Empty, numCoeffsEst = mat2Empty)
  for ( stat in statsAtRun ) lstEmpty[[stat]] <- matEmpty

  # Initialise warning and error recording.
  errorsEmpty <- as.data.frame(matrix(nrow=0,ncol=4), stringsAsFactors=FALSE)
  colnames(errorsEmpty) <- c("species", "SDM", "run", "msg")

  # Create the results list object.
  resLst <- list(numRuns=numRuns,            # Number of runs
                 numCoeffs=numCoeffs,        # Number of coefficients
                 numBiases=numBiases,        # Number of sample bias covariates
                 numSpecies=numSpecies,      # Number of species
                 namesSpecies=namesSpecies,  # a vector (numSpecies x 1) of species names
                 numPresencesPA=matEmpty,    # a matrix (numSpecies x numRuns) of the number of presences per species per run in the simulated PA data
                 numPresencesPO=matEmpty,    # a matrix (numSpecies x numRuns) of the number of presences per species per run in the simulated PO data                
                 stats=statsAtRun,           # statistics to calculate for each species x run x SDM
                 Count=lstEmpty,             # SDM of count/survey/abundance data 
                 Cloglog=lstEmpty,           # SDM of presence-absense data using complementary-log-log
                 PPM=lstEmpty,               # SDM of presence-only data using PPM function (Poisson point process model)
                 MsPP=lstEmpty,              # SDM of presence absence and only data using MultispeciesPP funciton (Poisson point process)
                 errors=errorsEmpty,         # Errors: info about place of occurrence and error message.
                 warnings=errorsEmpty,       # Warnings: info about place of occurrence and warning message.
                 timeSetup = NULL,           # Time taken to run the setup part of the simulation (getting of data, creation of results storage, etc)
                 timeSim = NULL,             # Time taken to run the simulation for the given scenario
                 timeResPlots = NULL,        # Time taken to create the result plots for the given scenario (total time = timeSim + timeResPlots)
                 #
                 # The following values are worked out by the object's functions, no need to set.
                 #
                 validSDM=NULL,              # Vector of the SDM names from above items (not user set)
                 namesValidSDM=NULL,         # Vector of the SDM names for pretty plotting
                 numSDMs=0                   # Number of SDM to be used.
                )
  
  # Extra bits for MsPP SDM.
  resLst$MsPP$gammas <- matEmpty              # Estimated intercept coefficents for sample bias function
  matEmpty <- matrix(nrow=numBiases, ncol=numRuns, dimnames=list(biasNames, runNames))
  resLst$MsPP$deltas <- matEmpty              # Estimated covariate coefficients for sample bias function
  
  # Return value.
  return(resLst)
}

#-----------------------------------------------------------------------------------------

setResTryMe <- function(simLst, resLst) {
  
  # Sets the tryMe value for each SDM type from the appropriate vale in the sim list object.
  # Also sets the validSDM vector (names must be the same as SDM item names in result object list).
  
  # Reset this value to nothing.
  resLst$validSDM <- NULL
  
  # Set tryMe values using useSDM values in the simulation object list.
  resLst$Count$tryMe <- simLst$useSDM.Count
  if ( resLst$Count$tryMe ) {
    resLst$validSDM <- c(resLst$validSDM, "Count")
    resLst$namesValidSDM <- c(resLst$namesValidSDM, "AB")
  } else {
    resLst$Count <- NULL
  }
  
  resLst$Cloglog$tryMe <- simLst$useSDM.Cloglog
  if ( resLst$Cloglog$tryMe ) {
    resLst$validSDM <- c(resLst$validSDM, "Cloglog")
    resLst$namesValidSDM <- c(resLst$namesValidSDM, "PA")
  } else {
    resLst$Cloglog <- NULL
  }
  
  resLst$PPM$tryMe <- simLst$useSDM.PPM
  if ( resLst$PPM$tryMe ) {
    resLst$validSDM <- c(resLst$validSDM, "PPM")
    resLst$namesValidSDM <- c(resLst$namesValidSDM, "PO")
  } else {
    resLst$PPM <- NULL
  }
  
  resLst$MsPP$tryMe <- simLst$useSDM.MsPP
  if ( resLst$MsPP$tryMe ) {
    resLst$validSDM <- c(resLst$validSDM, "MsPP")
    resLst$namesValidSDM <- c(resLst$namesValidSDM, "MsPP")
  } else {
    resLst$MsPP <- NULL
  }
  
  # Number of SDMs to be used.
  resLst$numSDMs <- length(resLst$validSDM)
  
  # Return value.
  return(resLst)
  
}
  
#-----------------------------------------------------------------------------------------

setResEstCoeffs <- function(resLstSDM, estCoeffs, thisRun, stdErrors=NULL, 
                            estGammas=NULL, estDeltas=NULL) {
  
  # Sets the estimated coefficients for the given SDM for thisRun and all species.
  #
  # Arguments ...
  # resLstSDM: the SDM part of the result list (e.g. resLst$Count or resLst$PPM)
  # estCoeffs: a matrix with a column for each species and rows that are alpha, beta1, ..., 
  #            betaN, where N is the number of covariates (inculding x and y).
  # thisRun:   the run number under which these estimated coefficients are to be stored.
  # stdErrors: the standard errors associated with each coefficient estimate.
  # estGammas: a vector with numSpecies elements that are the estimates of the bias intercept.
  # estDeltas: a vector with numBiases elements that are the estiamtes of the sample bias 
  #            covariate coefficients.

  # Get the names of the species that have coefficient estimates.
  namesSpecies <- colnames(estCoeffs)
  
  # Set the estimated coefficients
  resLstSDM$coeffs[ ,namesSpecies,thisRun] <- estCoeffs
  resLstSDM$SE[ ,namesSpecies,thisRun] <- stdErrors
  
  # Is this the multispeciesPP SDM?
  if ( ("gammas" %in% names(resLstSDM)) && ("deltas" %in% names(resLstSDM)) ) {
    resLstSDM$gammas[namesSpecies,thisRun] <- estGammas
    resLstSDM$deltas[ ,thisRun] <- estDeltas
  }

  # Return value.
  return(resLstSDM)
  
}

#-----------------------------------------------------------------------------------------

setResStats <- function(resLstSDM, resStats, thisRun) {
  
  # Sets the result statistic for the given SDM for thisRun and all species.
  #
  # Arguments ...
  # resLstSDM: the SDM part of the result list (e.g. resLst$Count or resLst$PPM)
  # resStats:  a data.frame containing a result statistic for each species (numSpecies x numStats)
  # thisRun:   the run number under which these estimated coefficients are to be stored.

  # Get the names of the statistics to be saved.
  stats <- colnames(resStats)

  # Set the estimated coefficients
  for ( item in stats ) {
    resLstSDM[[item]][ ,thisRun] <- resStats[ ,item]
  }
  
  # Return value.
  return(resLstSDM)
  
}

#-----------------------------------------------------------------------------------------

setResError <- function(resLst, run, SDM=NA, species=NA, msg="", isWarning=FALSE) {

  # Sets the error stuff for the given run (and SDM and species, if given).
  # 
  # Arguments ...
  # resLst:  result object
  # run:     scalar integer giving the run that has had an error.
  # SDM:     character scalar or vector giving the method that has had an error (NA if error  
  #          has occurred for all methods).  If a vector, must have same length as errMsg.
  # species: integer scalar or vector giving the species that has had an error (NA if error
  #          has occurred for all species).  If a vector, must have same length as errMsg.
  # msg:     character scalar or vector that contains the error message for each error.
  # isWarning: if true, set the warning stuff instead.

  # How many errors are to be recorded?
  numErrors <- length(msg)
  
  # Make sure run is a scalar (and then make a vector of same value, if necessary).
  if ( length(run) > 1) {
    stop("A vector of run numbers is not accepted in function setResError.")
  } else {
    run <- rep(run, numErrors)
  }
  
  # Check species is the same size as errMsg.
  if ( length(species) != numErrors ) {
    stop("Error messages and species identifier are not the same length in error setting.")
  }
  
  # If SDM is a vector, check it is also the same size.
  if ( length(SDM) > 1 ) {
    if ( length(SDM) != numErrors ) {
      stop("Error messages and SDM identifier are not the same length in error setting.")
    }
  } else {
    SDM <- rep(SDM, numErrors)
  }
    
  # Add this error to the error list.
  newRow <- data.frame(species=species, SDM=SDM, run=run, msg=msg, stringsAsFactors = FALSE)
  if ( isWarning ) {
    resLst$warnings <- rbind(resLst$warnings, newRow)
  } else {
    resLst$errors <- rbind(resLst$errors, newRow)
  }
  
  # Return value.
  return(resLst)
  
}

#-----------------------------------------------------------------------------------------

isSimError <- function(errors, run, SDM, species) {
  
  # Did the given run, SDM and species have an error during the simulation?  Search the 
  # list of errors for a matching identification (combination of run, SDM and species).
  # Remember that "NA" is recorded for errors occurring in all SDMs and/or all species.
  # Thus, any value of SDM will "match" any NA, ditto for species.
  
  # If there are no errors.
  if ( length(errors$run) == 0 ) return(FALSE)
  
  # Search for the given run in the list of errors.
  indRun <- which(errors$run == run)
  if ( length(indRun) == 0 ) return(FALSE)
  
  # Cut to possibles and search for the given SDM.
  possErrors <- errors[indRun,]
  indSDM <- which((possErrors$SDM == SDM) || ( is.na(possErrors$SDM)))
  if ( length(indSDM) == 0 ) return(FALSE)
  
  # Cut to possibles and search for the given species.
  possErrors <- possErrors[indSDM]
  indSpecies <- which((possErrors$species == species) || ( is.na(possErrors$species)))
  if ( length(indSpecies) == 0 ) return(FALSE)
  
  # There was an error during the simulation.
  return(TRUE)
  
}

#-----------------------------------------------------------------------------------------

setResOneToMany <- function(resLst, res1, whichRun) {
  
  # Assigns the results from one simulation to the full result list in the whichRun position.
  
  # Error stuff.
  numErrors <- dim(res1$errors)[1] 
  if ( numErrors > 0 ) {
    resLst <- setResError(resLst, whichRun, res1$errors$SDM, 
                          res1$errors$species, res1$errors$msg)    
  }
  
  # Warning stuff.
  numWarnings <- dim(res1$warnings)[1]
  if ( numWarnings > 0 ) {
    resLst <- setResError(resLst, whichRun, res1$warnings$SDM, 
                          res1$warnings$species, res1$warnings$msg, TRUE)    
  }

  # Numbers of presences.
  resLst$numPresencesPA[ ,whichRun] <- res1$numPresencesPA[ ,1]
  resLst$numPresencesPO[ ,whichRun] <- res1$numPresencesPO[ ,1]
  
  # How many SDMs where used?
  #numValidSDMs <- length(resLst$validSDM)
  
  # Which SDMs where used?
  for ( nameSDM in resLst$validSDM ) {
    # Which SDM is this?
    #nameSDM <- resLst$validSDM[j]
    
    # Coefficients ...
    estCoeffs <- res1[[nameSDM]]$coeffs[ , ,1]
    stdErrors <- res1[[nameSDM]]$SE[ , ,1]
    if ( ("gammas" %in% names(res1[[nameSDM]])) && ("deltas" %in% names(res1[[nameSDM]])) ) {
      estGammas <- res1[[nameSDM]]$gammas[ ,1]
      estDeltas <- res1[[nameSDM]]$deltas[ ,1]
    } else {
      estGammas <- NULL
      estDeltas <- NULL
    }
    resLst[[nameSDM]] <- setResEstCoeffs(resLst[[nameSDM]], estCoeffs, whichRun, stdErrors, 
                                         estGammas, estDeltas)
    
    # Result statistics ...
    stats <- res1$stats
    statsVals <- as.data.frame(matrix(nrow=res1$numSpecies, ncol=length(stats), 
                                      dimnames=list(res1$namesSpecies, stats)), stringsAsFactors=FALSE)
    for ( stat in stats ) {
      statsVals[ ,stat] <- res1[[nameSDM]][[stat]][ ,1]
    }
    resLst[[nameSDM]] <- setResStats(resLst[[nameSDM]], statsVals, whichRun)

  }

  # Return value.
  return(resLst)

}

#-----------------------------------------------------------------------------------------

setResTimes <- function(resLst, startTime, endTime, whichPart=0) {
  
  # NB: whichPart = 1 for setup, 
  #               = 2 for sim timing, or 
  #               = 3 for result plots timing.
  
  # Calculate time taken to run the part of the sim for this scenario.
  timeTaken <- format(endTime - startTime)
  
  # Which part of the whole code is this time for?
  if ( whichPart == 1 ) {
    resLst$timeSetup <- timeTaken
  } else if ( whichPart == 2 ) {
    resLst$timeSim <- timeTaken
  } else if ( whichPart == 3) {
    resLst$timeResPlots <- timeTaken
  } else {
    # Do nothing?
    warning("Unrecognised value given in 'whichPart' argument.")
  }
  
  # Return value.
  return(resLst)
  
}

#-----------------------------------------------------------------------------------------

setResNumPresences <- function(resLst, PAsp, POsp, thisRun=1){

  # Sets the number of presences for thisRun and each species.
  # Note that the number of absences is the number of surveys minus the number of presences.
  # for each run.
  #
  # Arguments ...
  # resLst:  result object
  # PAsp:    just the species columns of a PA data.frame.
  # POsp:    just the species name column of a PO data.frame.
  # thisRun: the run number that applies to this data.
  
  # Calculate the number of things.
  numSurveys <- dim(PAsp)[1]
  numSpecies <- dim(PAsp)[2]
  if ( numSpecies != dim(resLst$numPresencesPA)[1] ) {
    stop("Result dimensions don't match the number of species given in PA data.")  
  }
  
  # Calculate the number of presences for each species from the PA data.
  resLst$numPresencesPA[ ,thisRun] <- apply(PAsp, 2, sum)
  
  # Calculate the number of presences for each species from the PO data.
  numPresencesPerSpecies <- table(POsp)
  speciesNames <- names(numPresencesPerSpecies) 
  resLst$numPresencesPO[speciesNames,thisRun] <- numPresencesPerSpecies
  
  # Return value.
  return(resLst)
  
}

#-----------------------------------------------------------------------------------------

makeResStats <- function(estCoeffs, myFormula, cellObj, stats=c("mse")) {
  
  # Compare the SDM results to the true results using a mean square error function.
  # Compare the intensity functions at the cells within the doamin (implicitly from covars).
  # Returns a vector containing the mean square error for each species at the cells with
  # in the domain.
  #
  # Arguments ...
  # estCoeffs:  a matrix with a column for each species and rows that are alpha, beta1, 
  #             ..., betaN, where N is the number of covariates.
  # cellObj:    a cell object that contains environmental covariate values for each cell 
  #             in the domain, the names of the species, etc.
  # stats:      which statistics to return, can be a vector (options: "mse", "rmse1", 
  #             "rmse2", "nrmse")
  
  # Get numbers of things.
  numSpecies <- dim(estCoeffs)[2]
  namesSpecies <- cellObj$namesSpecies
  numStats <- length(stats)

  # Check there are the correct number of species in the true coefficients (should be!).
  if ( length(namesSpecies) != numSpecies ) {
    stop("Estimated coefficients do not have the required number of species.")
  }
  
  # Initialise return vector.
  statRes <- as.data.frame(matrix(NA, numSpecies, numStats), stringsAsFactors=FALSE)
  colnames(statRes) <- stats
  rownames(statRes) <- namesSpecies

  # For each species ...
  for ( species in namesSpecies ) {
    # True value of intensity function at the data points.
    #trueLambdaAtPoints <- lambda(dataPoints$x, dataPoints$y, myformula, trueCoeffs[ ,k], covars)
    trueLambdaAtCells <- cellObj$trueLambda[ ,species]

    # Estimated value of intensity function at the data points.
    #estLambdaAtPoints <- lambda(dataPoints$x, dataPoints$y, myformula, estCoeffs[ ,k], covars)
    estLambdaAtCells <- lambda.cell(myFormula, estCoeffs[ ,species], cellObj$covars)
    
    # Result for this species from the required statistic function.
    if ( "mse" %in% stats) 
      statRes[species,"mse"] <- mse(estLambdaAtCells, trueLambdaAtCells)
    if ( "rmse1" %in% stats ) 
      statRes[species, "rmse1"] <- rmse(estLambdaAtCells, trueLambdaAtCells, version = 1)
    if ( "rmse2" %in% stats ) 
      statRes[species, "rmse2"] <- rmse(estLambdaAtCells, trueLambdaAtCells, version = 2)
    if ( "nrmse" %in% stats ) 
      statRes[species, "nrmse"] <- nrmse(estLambdaAtCells, trueLambdaAtCells, denom = "mean")

  }
  
  # Return value.
  return(statRes)
}

#-----------------------------------------------------------------------------------------

mse <- function(Zobs, Ztrue) {
  
  # Mean square error function.
  #
  #   mse = sum((Zobs - Ztrue)^2)/length(Zobs)
  #
  # Returns the mean square error (an overall error measure).
  #
  # ARGUMENTS ...
  # Zobs:  the observed or estimated values
  # Ztrue: the true values
  
  num <- length(Zobs)
  if ( length(Ztrue) != num ) stop("True and observed must be same length.")
  diffsq <- (Zobs - Ztrue)^2
  mse <- sum(diffsq)/num
}

#-----------------------------------------------------------------------------------------

rmse <- function(Zobs, Ztrue, version=1) {
  
  # Root mean square error function (either version). Assumes Zobs and Ztrue are same length.
  #
  # Version 1 (i.e. version=1)
  #
  #   rmse1 = sqrt(mse)
  #
  # Version 2 (i.e. version=2)
  #
  #   rmse2 = sum(abs(Zobs - Ztrue)/Ztrue)/length(Zobs)
  #
  # Returns either rmse1 or rmse2 (both are scalars).
  
  if ( version == 1 ) {
    retVal <- sqrt(mse(Zobs,Ztrue))
  
  } else if ( version == 2 ) {
    num <- length(Zobs)
    if ( length(Ztrue) != num ) stop("True and observed must be same length.")
    absDiff <- abs(Zobs - Ztrue)
    retVal <- sum(absDiff/Ztrue)/num
    
  } else {
    stop("Unrecognised version in function 'rmse'.")
  }
  
  return(retVal)
  
}

#-----------------------------------------------------------------------------------------

nrmse <- function(Zobs, Ztrue, denom="mean") {
  
  # Normalised root mean square error (either denominator).  Assumes Zobs and Ztrue are same length.
  #
  # Normalised by mean (i.e. denom="mean") which is the coefficient of variation of the RMSD.
  # 
  #   nrmse = rmse1/mean(Ztrue)
  #
  # Normalised by range (i.e. denom="range")
  #
  #   nrmse = rmse1/(max(Ztrue)-min(Ztrue))
  #
  # Returns a scalar.
  
  retVal <- rmse(Zobs, Ztrue, version=1)
  if ( denom == "mean") {
    retVal <- retVal / mean(Ztrue)
    
  } else if ( denom == "range") {
    retVal <- retVal / (max(Ztrue) - min(Ztrue))
    
  } else {
    stop("Unrecognised denom value in function 'nrmse'.")
  }
  
  return(retVal)
}

#-----------------------------------------------------------------------------------------

makeCoeffBiasVar <- function(resLstSDM, coeffEstimates, coeffs) {
  
  # Calculate the bias and variance of the estimated coefficients where 
  #                mse = variance + bias^2
  # (see wiki "Mean Square Error")
  #
  # Arguments ...
  # resLstSDM:      the SDM part of the result list (e.g. resLst$Count or resLst$PPM)
  # coeffEstimates: the estimated value of the coefficients (numCoeffs x numSpecies x numRuns)
  # coeffs:         the true value of the coefficients (numCoeffs x numSpecies)
  
  # Numbers and names of things.
  numCoeffs <- dim(coeffEstimates)[1]
  namesCoeffs <- dimnames(coeffEstimates)[[1]]
  numSpecies <- dim(coeffEstimates)[2]
  namesSpecies <- dimnames(coeffEstimates)[[2]]
  numRuns <- dim(coeffEstimates)[3]
  
  # Check dimensions are the same.
  if ( (dim(coeffs)[1] != numCoeffs) || (dim(coeffs)[2] != numSpecies) ) 
    stop("Dimensions of arguments don't match.")
  
  # Initialise return values.
  bias <- matrix(nrow=numCoeffs, ncol=numSpecies, dimnames=list(namesCoeffs, namesSpecies))
  bias <- as.data.frame(bias, stringsAsFactors=FALSE)
  var <- bias

  # Calculate bias for each coeff x species combination.
  for ( coeff in namesCoeffs ) {
    for ( species in namesSpecies ) {
      # Which runs were successful for this species and this coefficient?
      indSuccessfulRuns <- which(! is.na(coeffEstimates[coeff,species, ]))
      resLstSDM$numCoeffsEst[coeff,species] <- length(indSuccessfulRuns)
      
      if ( length(indSuccessfulRuns) > 0 ) {
        # Mean of the estimated values.
        meanEstimates <- mean(coeffEstimates[coeff,species, indSuccessfulRuns])
        
        # Calculate bias.
        bias[coeff,species] <- meanEstimates - coeffs[coeff,species,indSuccessfulRuns] 
        
        # Calculate variance.
        var[coeff,species] <- mean(((coeffEstimates[coeff,species,indSuccessfulRuns] - meanEstimates)^2))
      }
    }
  }

  # Return value.
  resLstSDM$bias <- bias
  resLstSDM$var <- var
  return(resLstSDM)
}  
  
#-----------------------------------------------------------------------------------------

