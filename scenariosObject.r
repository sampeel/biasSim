initialiseScenarios <- function(scenariosDir, numPA, gammaMultiplier=1.0, deltaMultiplier=1.0, 
                                numClusters=0, numRuns=10, sep="", prettyNames = NULL ){
  
  # Initialise or setup the scenarios object from the given arguments.  Arguments numPA, 
  # gammaMultiplier, deltaMultiplier, numClusters and useSDM can each be scalars or vectors.
  # Argument scenariosDir is the directory where this group of scenarios is to be located
  # The unique combinations of these arguments form the settings for each scenario.
  # Returns the total number of scenarios, settings and name for each scenario.
  
  
  # Initialise return value.
  scenObj <- list(scenariosDir=scenariosDir,
                  numPA = numPA, 
                  gammaMultiplier = gammaMultiplier, 
                  deltaMultipler = deltaMultiplier, 
                  numClusters = numClusters, 
                  numRuns = numRuns,
                  prettyNamesScenarios = prettyNames,  
                  #
                  # Things created within this function.
                  #
                  numSettings = 0,                # Number of scenario settings for each scenario (that can be vectors).
                  numSettingVals = NULL,          # a vector containing the number of values per setting to be tried.
                  numScenarios=0,                 # number of scenarios to be performed (calculated product of numSettingVals)
                  settings = NULL,                # a data.frame that contains the expanded settings with a row for each scenario and a column for each setting.
                  namesScenarios = NULL,          # a vector of strings containing a unique name for each scenario (can be used as directory name or on plots).
                  isError=FALSE
                  )
  
  # What is the length of each setting argument?
  # This is where it will change if the number of settings changes (as well as function line).
  settingsLst <- list(nPA = numPA, GMult = gammaMultiplier, DMult = deltaMultiplier, 
                      nClust = numClusters)
  namesSettings <- names(settingsLst)           # Prefix used to create unique directorys.
  scenObj$numSettings <- length(settingsLst)
  scenObj$numSettingVals <- sapply(settingsLst, length)

  # How many scenarios need to be performed?
  scenObj$numScenarios <- prod(scenObj$numSettingVals)
  
  # Create settings for each unique scenario.
  scenObj$settings <- as.data.frame(matrix(nrow=scenObj$numScenarios, ncol=scenObj$numSettings),
                                    stringsAsFactors = TRUE)
  names(scenObj$settings) <- namesSettings
  scenObj$namesScenarios <- rep("", scenObj$numScenarios) 
  firstSettingVec <- TRUE
  for ( i in 1:scenObj$numSettings ) {
    # Repeat settings to form rows of unique combinations (in whole data.frame)
    if ( i < scenObj$numSettings ) {
      numEach <- prod(scenObj$numSettingVals[(i+1):scenObj$numSettings])
    } else {
      numEach <- 1
    }
    if ( i > 1 ) {
      numTimes <- prod(scenObj$numSettingVals[1:(i-1)])
    } else {
      numTimes <- 1
    }
    scenObj$settings[ ,i] <- rep(rep(settingsLst[[i]],each=numEach), times=numTimes)
    
    # Concatenate descriptive strings to form unique directory names for each scenario.
    # Don't bother for settings that have only one value.
    if ( scenObj$numSettingVals[i] > 1) {
      # Are the values for this setting doubles, characters or something else?  
      #strVals <- format(settingsLst[[i]], trim=TRUE)
      if ( is.double(settingsLst[[i]]) ) {
        strVals <- format(settingsLst[[i]], trim=TRUE, nsmall=1)
      } else if ( is.character(settingsLst[[i]]) ) {
        strVals <- settingsLst[[i]]
      } else {
        # Integers, logical.
        strVals <- format(settingsLst[[i]], trim=TRUE, nsmall=0)
      }
      if ( firstSettingVec ) {
        firstSettingVec <- FALSE
        strSep <- ""
      } else {
        strSep <- sep
      }
      strAdd <- rep(rep(paste0(strSep, namesSettings[i], strVals), each=numEach, times=numTimes))
      scenObj$namesScenarios <- paste0(scenObj$namesScenarios, strAdd)
    }
  }
  
  # Are there pretty names (print or plot worthy) provided by the user for each scenarios?
  if ( length(prettyNames) == 1 && scenObj$numScenarios > 1) {
    # Same scalar value for each, append number of scenario so names not all the same!
    scenObj$prettyNamesScenarios <- paste0(rep(prettyNames, scenObj$numScenarios),
                                           1:(scenObj$numScenarios)) 

  } else if ( is.null(prettyNames) ) {
    # No pretty names, use names created within this function.
    scenObj$prettyNamesScenarios <- scenObj$namesScenarios

  } else if ( length(prettyNames) == scenObj$numScenarios ) {
    # All pretty names supplied.
    # Check they are unique (as they will be used as identifiers for statistics and plotting).
    if ( length(unique(prettyNames)) < scenObj$numScenarios ) {
      scenObj$isError <- TRUE
      stop("There are duplicated values in the 'prettyNames' argument.  Duplicates are not permitted.")
    } else {
      scenObj$prettyNamesScenarios <- prettyNames  
    }

  } else {
    # Possible error in number of pretty names supplied?
    scenObj$isError <- TRUE
    stop("The length of the 'prettyNames' argument does not match the calculated number of scenarios.")
  }
  
  # Add the pretty names for plotting.
  scenObj <- addPrettyNamesToScenarios(scenObj, prettyNames)
  
  # Return value
  return(scenObj)

}

#-----------------------------------------------------------------------------------------

addPrettyNamesToScenarios <- function(scenObj, prettyNames=NULL) {

  # Add pretty names to the scenarios object.
  
  # Are there pretty names (print or plot worthy) provided by the user for each scenarios?
  if ( length(prettyNames) == 1 && scenObj$numScenarios > 1) {
    # Same scalar value for each, append number of scenario so names not all the same!
    scenObj$prettyNamesScenarios <- paste0(rep(prettyNames, scenObj$numScenarios),
                                           1:(scenObj$numScenarios)) 
    
  } else if ( is.null(prettyNames) ) {
    # No pretty names, use names created for directories.
    scenObj$prettyNamesScenarios <- scenObj$namesScenarios
    
  } else if ( length(prettyNames) == scenObj$numScenarios ) {
    # All pretty names supplied.
    # Check they are unique (as they will be used as identifiers for statistics and plotting).
    if ( length(unique(prettyNames)) < scenObj$numScenarios ) {
      scenObj$isError <- TRUE
      stop("There are duplicated values in the 'prettyNames' argument.  Duplicates are not permitted.")
    } else {
      scenObj$prettyNamesScenarios <- prettyNames  
    }
    
  } else {
    # Possible error in number of pretty names supplied?
    scenObj$isError <- TRUE
    stop("The length of the 'prettyNames' argument does not match the calculated number of scenarios.")
  }

  # Return value
  return(scenObj)

}

#-----------------------------------------------------------------------------------------

addPrettyNamesToStats <- function(statsObj, prettyNames=NULL) {
  
  # Add pretty names to the statsObj object by replacing existing names on scenario dimension.  
  # Assume they have already been made or added to scenarios object, so do less error checking.
  
  # Are there pretty names (print or plot worthy) provided by the user for each scenario?
  numScenarios <- dim(statsObj$stats[[1]]$avg)[[1]]
  if ( length(prettyNames) != numScenarios ) {
    # Possible error in number of pretty names supplied?
    stop("The length of the 'prettyNames' argument does not match the scenarios dimension in stats.")
  }
  
  # Replace names in each stat.
  for ( i in 1:statsObj$numStats ) {
    # Replace names in avg.
    dimnames(statsObj$stats[[i]]$avg)[[1]] <- prettyNames
    
    # Replace names in sd.
    dimnames(statsObj$stats[[i]]$sd)[[1]] <- prettyNames
  }
  
  # Return value
  return(statsObj)
  
}

#-----------------------------------------------------------------------------------------

runScenarios <- function(scenariosObj, useSDM=c("AB","PA","PO","MsPP"), isOutputToFile=TRUE, 
                         useSavedData=TRUE, doResultPlots=FALSE, doCheckPlots=FALSE, 
                         widthClusters=5, randomSeed=NULL, covarsCentred=TRUE, ...) {
   
  # Runs the scenarios using the information contained within the scenarios object.

  # Start time.
  timeTotal <- Sys.time()
  
  # Initialise result storage.
  resLstAll <- list()
  timeStart <- rep(NA, scenariosObj$numScenarios)
  class(timeStart) <- class(timeTotal)
  
  # Run each scenario.
  for ( i in 1:scenariosObj$numScenarios ) {
    # Start time for scenario.
    timeStart[i] <- Sys.time()
    
    # Create a scenario directory.
    scenarioDir <- paste0(scenariosObj$scenariosDir, "/", scenariosObj$namesScenarios[i])
    
    # Run scenario
    rLst <- runScenario(scenariosObj$settings$nPA[i], scenariosObj$settings$GMult[i], 
                        scenariosObj$settings$DMult[i], scenariosObj$numRuns, 
                        scenariosObj$settings$nClust[i], widthClusters, useSDM, 
                        isOutputToFile, scenarioDir, doResultPlots, 
                        doCheckPlots, useSavedData, randomSeed, covarsCentred, ...) 

    # Save this scenario's results 
    resLstAll[[i]] <- rLst$resLst
    if ( dim(rLst$resLst$errors)[1] > 0 ) {
      message(paste0("Errors occurred in scenario ",i))
    } else {
      message(paste0("Completed scenario ",i))
    }
  }

  # Finish time for last scenario.
  timeEnd <- Sys.time()
  
  # Save data for future.
  saveDir <- paste0(scenariosObj$scenariosDir,"/Data")
  if ( ! dir.exists(saveDir) ) {
    dir.create(saveDir, recursive = TRUE)
  }
  saveFile <- makeFileName("DataDump", saveDir,  "RData")
  simObj <- rLst$simObj          # Mostly same for all scenarios (only differences are in the scenarios object)
  plotObj <- rLst$plotObj        # Same for all scenarios
  save(resLstAll, simObj, plotObj, scenariosObj, file=saveFile)
  
  # Return result.
  timeTotal <- format(Sys.time() - timeTotal)
  retLst <- list(plotObj=plotObj, simObj=simObj, resLstAll=resLstAll, 
                 timeStart=timeStart, timeEnd=timeEnd, timeTotal=timeTotal)
  return(retLst)
  
}

#-----------------------------------------------------------------------------------------

loadScenariosResults <- function(scenariosDir) {
  
  # Use the given scenarios directory to load the results for these scenarios.
  # Assumes same directory structure as created by scenarios (see initialiseScenarios).

  # Directory where the data was saved ...  
  savedDir <- paste0(scenariosDir,"/Data")
  if ( ! dir.exists(savedDir) ) 
    stop(paste0("Unable to load data as scenarios directory doesn't exist.\n", 
                "  Directory tried: '", scenariosDir,"'"))
  
  # File where the data was saved ...
  savedFile <- makeFileName("DataDump", savedDir,  "RData")
  
  # Load back into function's environment.
  currentEnvir <- environment()
  loadedObjectNames <- load(savedFile, envir = currentEnvir)
  
  # Reformat into a list for return from function.
  retLst <- mget(loadedObjectNames, envir = currentEnvir)
  # retLst <- list(plotObj=plotObj, simObj=simObj, resLstAll=resLstAll, scenariosObj=scenariosObj)
  
  # Return value.
  return(retLst)
  
}

#-----------------------------------------------------------------------------------------

saveScenariosResults <- function(scenariosDir, retLst) {
  
  # Use the given scenarios directory to save the results for these scenarios.
  # Assumes same directory structure as created by scenarios (see initialiseScenarios).
  
  # Directory where the data will be saved ...  
  saveDir <- paste0(scenariosDir,"/Data")

  # File where the data will be saved ...
  saveFile <- makeFileName("DataDump", saveDir,  "RData")
  if ( file.exists(saveFile) ) {
    userAns <- readline("Save results file exists.  Do you wish to overwrite?  Answer Y or N: ")
    if ( tolower(userAns) != "y" ) stop("Results not saved!")
  }
  
  # Assign return list objects as separate objects within the function's environment.
  saveObjects <- names(retLst)
  currentEnvir <- environment()
  for ( obj in saveObjects ) {
    assign(obj, retLst[[obj]], envir=currentEnvir)
  }
  
  # Save these objects to the file.
  save(list=saveObjects, file=saveFile, envir = currentEnvir)
  
}

#-----------------------------------------------------------------------------------------

numSuccessfulRunsScenarios <- function(scenariosObj, resLstAll, asPercentage=FALSE) {
  
  # Returns the number of successful runs (i.e. that produced coefficient estimates) per 
  # scenario x SDM for each species.
  #
  # Arguments ...
  # scenariosObj: A scenarios object (see initialiseScenarios)
  # resLstAll:    A vector of lists, one per scenario, that contains the results from the runs.
  # asPercentage: Whether or not to return the results as the number of successful runs or
  #               The percentage of successful runs (of the total number of runs)
  
  # Numbers and names of things.
  numScenarios <- scenariosObj$numScenarios
  namesScenarios <- scenariosObj$namesScenarios
  namesSpecies <- resLstAll[[1]]$namesSpecies
  numSpecies <- length(namesSpecies)
  numSDMs <- resLstAll[[1]]$numSDMs
  namesSDMs <- resLstAll[[1]]$namesValidSDM
  validSDMs <- resLstAll[[1]]$validSDM
  
  # Initialise return value.
  namesScenariosSDMs <- paste0(rep(namesSDMs, each=numScenarios), "-" ,
                               rep(namesScenarios, times=numSDMs))
  numSuccesses <- matrix(nrow=numScenarios*numSDMs, ncol=numSpecies, 
                         dimnames = list(namesScenariosSDMs, namesSpecies))
  
  # Calculate number of successful runs per scenario
  for ( i in 1:numScenarios ) {
    for ( s in 1:numSDMs ) {
      # Row in return value.
      thisRow <- i + (s-1)*numScenarios
      
      # Number of successful runs for this scenario x SDM combo for each species.
      numSuccesses[thisRow, ] <- resLstAll[[i]][[validSDMs[s]]]$numCoeffsEst[1,namesSpecies]
    }
  }
  
  # Return a percentage?
  if ( asPercentage ) {
    numRuns <- resLstAll[[1]]$numRuns
    numSuccesses <- numSuccesses / numRuns
  }
  
  # Return value.
  return(numSuccesses)
  
}

#-----------------------------------------------------------------------------------------

plotScenariosSummaries2 <- function(scenariosObj, trueCoeffs, resLstAll, 
                                    samePage=TRUE, ylimVals=NULL, plotSDMs=NULL, 
                                    absStats=TRUE, vioPlots=FALSE, diffPORanges=c(TRUE,TRUE),
                                    ...) {

  # Make the summary level plots.  Plots to assess unbiasedness (mean is near true solution) 
  # and efficiency (variance is not too great) per SDM, for all species x coefficients 
  # (scaled so that they make sense on the same plot).
  
  # Names and numbers of things.  FYI: assume all objects are required dimensions, no checking done!
  namesCoeffs <- rownames(trueCoeffs)
  numCoeffs <- length(namesCoeffs)
  numRuns <- scenariosObj$numRuns
  numScenarios <- scenariosObj$numScenarios
  namesScenarios <- scenariosObj$prettyNamesScenarios
  namesSpecies <- colnames(trueCoeffs)
  numSpecies <- length(namesSpecies)
  namesValidSDMs <- resLstAll[[1]]$namesValidSDM
  validSDMs <- resLstAll[[1]]$validSDM        # Assumes all scenarios use the same SDMs!
  numValidSDMs <- length(validSDMs)           # Assumes namesValidSDMs and validSDMs are same length!
  
  # Plot directory for scenario level plots.
  plotDir <- paste0(scenariosObj$scenariosDir, "/Plots")
  if ( ! dir.exists(plotDir) ) {
    dir.create(plotDir, recursive = TRUE)
  }
  
  # Initialise statistics to summarise the scenarios results (per SDM!).
  unbiasedness <- array(dim=c(numScenarios, numCoeffs, numSpecies, numValidSDMs), 
                        dimnames=list(namesScenarios, namesCoeffs, namesSpecies, namesValidSDMs))
  efficiencySD <- unbiasedness
  efficiencySE <- unbiasedness
  
  # Calculate the statistics ...
  for ( sdm in 1:numValidSDMs ) {
    nameSDM <- validSDMs[sdm]
    
    for ( species in namesSpecies ) {
      
      for ( j in 1:numCoeffs ) {
        nameCoeff <- namesCoeffs[j]
        
        # True coefficient.
        trueCoeff <- trueCoeffs[nameCoeff,species]
        
        # Will need the estimates of this coefficient for the given species.
        # coeffEsts <- matrix(nrow=numRuns, ncol=numScenarios, 
        #                     dimnames = list(1:numRuns, namesScenarios))
        
        # Get data from each scenario.
        for (sc in 1:numScenarios) {
          resLst <- resLstAll[[sc]]
          
          # All estimates of this species x this coeff for this scenario (i.e. estimates from each run)
          coeffEsts <- resLst[[nameSDM]]$coeffs[nameCoeff,species, ]
          
          # Which runs were successful for this species and this coefficient?
          indSuccessfulRuns <- which(! is.na(coeffEsts))

          # Statistics data.
          if ( length(indSuccessfulRuns) == 0 ) {
            msg <- paste0("No statistics available as all runs have failed: \n",
                          "    SDM      = ", namesValidSDMs[sdm], "\n",
                          "    species  = ", species, "\n",
                          "    coeff    = ", nameCoeff, "\n",
                          "    scenario = ", namesScenarios[sc])
            warning(msg)
          } else {
            # unbiasedness and efficiency using standard errors ...
            avgCoeffEst <- mean(coeffEsts[indSuccessfulRuns])
            unbiasedness[sc,nameCoeff,species,sdm] <- (avgCoeffEst - trueCoeff) / trueCoeff
            avgCoeffSE <-  mean(resLst[[nameSDM]]$SE[nameCoeff,species,indSuccessfulRuns])
            efficiencySE[sc,nameCoeff,species,sdm] <- avgCoeffSE / trueCoeff
            
            # Can form an efficiency from the standard deviation of estimates?
            if ( length(indSuccessfulRuns) > 1 ) {
              efficiencySD[sc,nameCoeff,species,sdm] <- sd(coeffEsts[indSuccessfulRuns]) / trueCoeff
            } else {
              msg <- paste0("Unable to form a standard deviation from only one successful run: \n",
                            "    SDM      = ", namesValidSDMs[sdm], "\n",
                            "    species  = ", species, "\n",
                            "    coeff    = ", nameCoeff, "\n",
                            "    scenario = ", namesScenarios[sc])
              warning(msg)
            }
          }
        }
      }
    }
  }
  
  ### Plotting ...
  
  # Which SDMs to plot.
  if ( is.null(plotSDMs) ) plotSDMs <- namesValidSDMs
  
  # Plot for unbiasedness
  internalPlotScenariosStat2(unbiasedness, "unbiasedness", samePage = samePage, plotSDMs = plotSDMs, 
                             ylimVals = ylimVals, diffPORanges = diffPORanges, absStats = absStats,
                             vioPlots = vioPlots, plotDir = plotDir, ...)
  
  # Plot for efficiency using SD of estimates.
  internalPlotScenariosStat2(efficiencySD, "efficiencySD", samePage = samePage, plotSDMs = plotSDMs, 
                             ylimVals = ylimVals, diffPORanges = c(FALSE,FALSE), absStats = absStats,
                             vioPlots = vioPlots, plotDir = plotDir, ...)
  
  # Plot for efficiency using SEs from results.
  internalPlotScenariosStat2(efficiencySE, "efficiencySE", samePage = samePage, plotSDMs = plotSDMs, 
                             ylimVals = ylimVals, diffPORanges = c(FALSE,FALSE), absStats = absStats,
                             vioPlots = vioPlots, plotDir = plotDir, ...)
  
# 
#   # Return statistics ...
#   return(list(accuracy=unbiasedness, efficiencySD=efficiencySD, efficiencySE=efficiencySE))
}

#-----------------------------------------------------------------------------------------

internalPlotScenariosStat <- function(stats, statName, plotObj, plotDir, samePage=FALSE, 
                                      ylimVals=NULL, redAlphas=FALSE, labels=FALSE,
                                      boxPlots=FALSE){
  
  # Internal plotting function to plot the summary statistics for scenarios.
  #
  # Arguments ...
  # stats:     A 4D array that contains the values to be plotted (numScenarios x numCoeffs 
  #            x numSpecies x numValidSDMs)
  # statName:  Name of statistic (used in file names and titles).
  # samePage:  Whether the plots are all on the same page (one file) or separate pages 
  #            (multiple files)  
  # ylimVals:  Indicates the range of the y-axis to be plotted.  Generally, when not NULL, 
  #            the y-axis is to be truncated to less than the actual values present.  When
  #            NULL, ylimVals is set to range(stats) so that all plots have same axis.
  # redAlphas: Colour only the points for the alpha coefficients red (otherwise use black)
  # labels:    Use labels (coefficient x species) instead of points for points outside
  #            y-axis range of [-1,1]
  # boxPlots:  
  
  # Numbers and names of things.
  dimStats <- dim(stats)
  numScenarios <- dimStats[1]
  numCoeffs <- dimStats[2]
  numSpecies <- dimStats[3]
  numSDMs <- dimStats[4]
  dimnamesStats <- dimnames(stats)
  namesScenarios <- dimnamesStats[[1]]
  namesCoeffs <- dimnamesStats[[2]]
  namesSDMs <- dimnamesStats[[4]]
  namesSpecies <- dimnamesStats[[3]]
  acceptableStatVal <- 1
  
  # Is the y-axis truncated (assumes ylimVals are less than actual range)
  if ( is.null(ylimVals) ) {
    fileTrunc <- ""
    titleTrunc <- ""
    ylimVals <- range(stats)   # Same y-axis for all plots.
  } else {
    fileTrunc <- "-trunc"
    titleTrunc <- " - truncated"
    ylimVals <- ylimVals
  }

  # There will be a plot per valid SDM.  Are they on the same page (in the same file)?
  if ( samePage ) {
    numPages <- 1
    numPlotsPerPage <- numSDMs
    fileNames <- makeFileName(paste0(statName, fileTrunc), plotDir, plotObj$device)
    titleTxt <- paste0(statName," of all estimates", titleTrunc)
  } else {
    numPages <- numSDMs
    numPlotsPerPage <- 1
    fileNames <- makeFileName(paste0(namesSDMs, statName, fileTrunc), 
                                     plotDir, plotObj$device)
    titleTxt <- paste0(statName, " of all estimates for ", namesSDMs, " SDM", titleTrunc)
  }
  
  # Labels for points, should they be needed.
  if ( labels ) {
    # Create combination of coefficient and species as labels.
    shortCoeffNames <- c("a", paste0("b", 1:(numCoeffs-1)))
    labelMat <- matrix(nrow=numCoeffs, ncol=numSpecies, 
                       dimnames=list(shortCoeffNames, namesSpecies))
    labelMat[] <- paste0(rep(shortCoeffNames, times=numSpecies), rep(namesSpecies, each=numCoeffs))
  }
  
  # Mean of the statistics for each scenario x SDM combination.
  meanStats <- apply(stats, c(1,4), mean)

  # Start plots.
  indSDM <- 0
  opar <- par(mfcol=c(1,numPlotsPerPage))
  for ( page in 1:numPages ) {
    # Start plot layers for this page ...
    layers <- setPlotLayers()
    xVals <- 1:(numScenarios * numPlotsPerPage)
    xlimVals <- c(0.5, max(xVals)+0.5)
    layers <- addPlotData(layers, xlimVals, ylimVals, plotfunc="plot", 
                          type="n", xaxs="i", xaxt="n", xlab="", ylab="", ylim=ylimVals)
    xLabs <- rep(namesScenarios, numPlotsPerPage)
    layers <- addPlotData(layers, 1, xVals, "axis", labels=xLabs, las=2, cex=0.5)
    layers <- addPlotData(layers, list(h=0), plotfunc="abline", lty=3)
    
    # Do layers that contain stats.
    for ( plot in 1:numPlotsPerPage ) {
      # Index for SDM to be used.
      indSDM <- indSDM + 1
      
      # X-axis values to be used for this plot.
      theseXVals <- (1:numScenarios) + ((plot - 1) * numScenarios)
      
      # Box plots or points and means?
      if ( boxPlots ) {
        # Add stats as box plots.
        for ( i in 1:numScenarios ) {
          # Add box plot layer for this scenario and this SDM.
          statsForBox <- as.vector(stats[i, , ,indSDM])
          layers <- addPlotData(layers, statsForBox, plotfunc="boxplot", at=theseXVals[i])
          
          # Highlight alpha coefficients by adding points and making them red?
          if ( redAlphas ) {
            layers <- addPlotData(layers, rep(theseXVals[i], times=numSpecies), 
                                  stats[i,1, ,indSDM], plotfunc="points", col="red")
          }
          
          # Do we need to plot labels?
          if ( labels ) {
            # Is this species x coefficients absolute value large enough 
            indLargeStat <- which(abs(stats[i, , ,indSDM]) > acceptableStatVal, arr.ind=TRUE)
            
            # Plot labels as well, if necessary.
            numLargeStats <- dim(indLargeStat)[1]
            if ( numLargeStats > 0) {
              # Get the approriate labels
              labelnames <- labelMat[indLargeStat]
              labelPos <- rep(3, numLargeStats)     # label above point (see pos in text help)
              statsMat <- stats[i, , ,indSDM]
              indNeg <- statsMat[indLargeStat] < (-acceptableStatVal)
              labelPos[indNeg] <- 1                 # label below point 
              layers <- addPlotData(layers, rep(theseXVals[i], times=numLargeStats), 
                                    statsMat[indLargeStat], plotfunc="text", labels=labelnames,
                                    pos=labelPos, cex=0.65)
            }
          } # if (labels )
        } # for ( i in 1:numScenarios )
        
      } else {
        # Add stats as points.
        for ( species in namesSpecies ) {
          for ( coeff in 1:numCoeffs ) {
            if ( redAlphas && coeff == 1 ) {
              # Separate out the alpha coefficients by making them red.
              layers <- addPlotData(layers, theseXVals, stats[ ,coeff,species,indSDM], 
                                    plotfunc="points", col="red")
            } else {
              layers <- addPlotData(layers, theseXVals, stats[ ,coeff,species,indSDM], 
                                    plotfunc="points", col="black")
            }
            
            # Do we need to plot labels?
            if ( labels ) {
              # What is this species x coefficient's label?
              indLargeStat <- which(abs(stats[ ,coeff,species,indSDM]) > acceptableStatVal)
              
              # Plot labels as well, if necessary.
              numLargeStats <- length(indLargeStat)
              if ( numLargeStats > 0) {
                label <- labelMat[coeff,species]
                labelPos <- rep(3, numLargeStats)       # label above point (see pos in text help)
                indNeg <- stats[indLargeStat,coeff,species,indSDM] < (-acceptableStatVal)
                labelPos[indNeg] <- 1                   # label below point 
                layers <- addPlotData(layers, theseXVals[indLargeStat], 
                                      stats[indLargeStat,coeff,species,indSDM], 
                                      plotfunc="text", labels=rep(label,times=numLargeStats),
                                      pos=labelPos, cex=0.65)
              }
            } # if (labels )
          } # for (coeff ...)
        } # for ( species ...)
        
        # Add mean line for each scenario.
        layers <- addPlotData(layers, theseXVals, meanStats[ ,indSDM], plotfunc="points", 
                              pch="_", col="green")
      }
      
      # Add division between SDM's if on same plot.
      if ( plot > 1 ) {
        xLine <- theseXVals[1] - 0.5
        layers <- addPlotData(layers, list(v=xLine), plotfunc="abline", lty="solid")
      }
    }
    
    # Do I need to add SDM titles to plots on same page?
    if ( numPlotsPerPage > 1 ) {
      x <- ((1:numSDMs) * numScenarios) + 0.5
      y <- rep(ylimVals[2], numSDMs)
      layers <- addPlotData(layers, x, y, plotfunc = "text", labels = namesSDMs, pos = 2)
    }
      
    # Plot layers ...
    plot.plotLayers(layers, plotObj$device, fileNames[page], plotObj$fileHeight, 
                    plotObj$fileWidth, titleTxt[page], xlab="")
    
  }

  # Return plotting to original settings.
  par(opar)
  
}

#-----------------------------------------------------------------------------------------

plotScenariosSummaries <- function(scenariosObj, plotObj, trueCoeffs, resLstAll, 
                                   truncPlots=FALSE, useBoxPlots=FALSE, redAlphas=FALSE, 
                                   SDMsOnSamePlot=FALSE) {

  # Make the summary level plots.  Plots to assess unbiasedness (mean is near true solution) 
  # and efficiency (variance is not too great) per SDM, for all species x coefficients 
  # (scaled so that they make sense on the same plot).
  
  # Names and numbers of things.  FYI: assume all objects are required dimensions, no checking done!
  namesCoeffs <- rownames(trueCoeffs)
  numCoeffs <- length(namesCoeffs)
  numRuns <- scenariosObj$numRuns
  numScenarios <- scenariosObj$numScenarios
  namesScenarios <- scenariosObj$namesScenarios
  namesSpecies <- colnames(trueCoeffs)
  numSpecies <- length(namesSpecies)
  namesValidSDMs <- resLstAll[[1]]$namesValidSDM
  validSDMs <- resLstAll[[1]]$validSDM        # Assumes all scenarios use the same SDMs!
  numValidSDMs <- length(validSDMs)           # Assumes namesValidSDMs and validSDMs are same length!
  
  # Plot directory for scenario level plots.
  plotDir <- paste0(scenariosObj$scenariosDir, "/Plots")
  if ( ! dir.exists(plotDir) ) {
    dir.create(plotDir, recursive = TRUE)
  }
  
  # Want a plot for each sdm x each statistic.
  for ( s in 1:numValidSDMs ) {
    nameSDM <- validSDMs[s]
    
    # Initialise statistics to summarise the scenarios results (per SDM!).
    unbiasedness <- array(dim=c(numScenarios, numCoeffs, numSpecies), 
                          dimnames=list(namesScenarios, namesCoeffs, namesSpecies))
    efficiencySD <- unbiasedness
    efficiencySE <- unbiasedness
    
    for ( species in namesSpecies ) {
      
      for ( j in 1:numCoeffs ) {
        nameCoeff <- namesCoeffs[j]
        
        # True coefficient.
        trueCoeff <- trueCoeffs[nameCoeff,species]
        
        # Will need the estimates of this coefficient for the given species.
        # coeffEsts <- matrix(nrow=numRuns, ncol=numScenarios, 
        #                     dimnames = list(1:numRuns, namesScenarios))

        # Get data from each scenario.
        for (i in 1:numScenarios) {
          resLst <- resLstAll[[i]]
          
          # All estimates of this species x this coeff for this scenario (i.e. estimates from each run)
          coeffEsts <- resLst[[nameSDM]]$coeffs[nameCoeff,species, ]

          # Which runs were successful for this species and this coefficient?
          indSuccessfulRuns <- which(! is.na(coeffEsts))
          
          # Statistics data.
          avgCoeffEst <- mean(coeffEsts[indSuccessfulRuns])
          unbiasedness[i,nameCoeff,species] <- (avgCoeffEst - trueCoeff) / trueCoeff
          efficiencySD[i,nameCoeff,species] <- sd(coeffEsts[indSuccessfulRuns]) / trueCoeff
          avgCoeffSE <-  mean(resLst[[nameSDM]]$SE[nameCoeff,species,indSuccessfulRuns])
          efficiencySE[i,nameCoeff,species] <- avgCoeffSE / trueCoeff
        }
      }
    }
  
    ### Plot for unbiasedness
    
    # Truncate plot?
    if ( truncPlots ) {
      thisFileName <- makeFileName(paste0(nameSDM,"unbiasedness-trunc"), plotDir, plotObj$device)
      titleTxt <- paste0("Unbiasedness of all estimates for ", nameSDM, " SDM - truncated")
      ylimVals <- c(-1.5,1.5)
    } else { 
      thisFileName <- makeFileName(paste0(nameSDM,"unbiasedness"), plotDir, plotObj$device)
      titleTxt <- paste0("Unbiasedness of all estimates for ", nameSDM, " SDM")
      ylimVals <- range(unbiasedness)
    }
    
    # Make layers for the plot.
    layers <- setPlotLayers()
    layers <- addPlotData(layers, c(1,numScenarios), range(unbiasedness), plotfunc="plot", 
                          type="n", xaxt="n", xlab="", ylab="", ylim=ylimVals)
    layers <- addPlotData(layers, 1, 1:numScenarios, "axis", labels=namesScenarios, las=2, cex=0.5)
    layers <- addPlotData(layers, list(h=0), plotfunc="abline", lty=3)
    layers <- makePlotLayersResStats(unbiasedness, layers, useBoxPlots, redAlphas)
    plot.plotLayers(layers, plotObj$device, thisFileName, plotObj$fileHeight, 
                    plotObj$fileWidth, titleTxt, xlab="")

    
    ### Plot for efficiency using SD of estimates.
    
    # Truncate plot?
    if ( truncPlots ) {
      thisFileName <- makeFileName(paste0(nameSDM,"efficiencySD-trunc"), plotDir, plotObj$device)
      titleTxt <- paste0("Efficiency of all estimates using SD for ", nameSDM, " SDM - truncated")
      ylimVals <- c(-10,10)
    } else {
      thisFileName <- makeFileName(paste0(nameSDM,"efficiencySD"), plotDir, plotObj$device)
      titleTxt <- paste0("Efficiency of all estimates using SD for ", nameSDM, " SDM")
      ylimVals <- range(efficiencySD)
    }
    
    # Make layers for the plot.
    layers <- setPlotLayers()
    layers <- addPlotData(layers, c(1,numScenarios), range(efficiencySD), plotfunc="plot", 
                          type="n", xaxt="n", xlab="", ylab="", ylim=ylimVals)
    layers <- addPlotData(layers, 1, 1:numScenarios, "axis", labels=namesScenarios, las=2, cex=0.5)
    layers <- addPlotData(layers, list(h=0), plotfunc="abline", lty=3)
    layers <- makePlotLayersResStats(efficiencySD, layers, useBoxPlots, redAlphas)
    plot.plotLayers(layers, plotObj$device, thisFileName, plotObj$fileHeight, 
                    plotObj$fileWidth, titleTxt, xlab="")
    
    ### Plot for efficiency using SEs from results.
    
    # Truncate plot?
    if ( truncPlots ) {
      thisFileName <- makeFileName(paste0(nameSDM,"efficiencySE-trunc"), plotDir, plotObj$device)
      titleTxt <- paste0("Efficiency of all estimates using SE for ", nameSDM, " SDM - truncated")
      ylimVals <- c(-10,10)
    } else {
      thisFileName <- makeFileName(paste0(nameSDM,"efficiencySE"), plotDir, plotObj$device)
      titleTxt <- paste0("Efficiency of all estimates using SE for ", nameSDM, " SDM")
      ylimVals <- range(efficiencySE)
    }
    
    # Make layers for the plot.
    layers <- setPlotLayers()
    layers <- addPlotData(layers, c(1,numScenarios), range(efficiencySE), plotfunc="plot", 
                          type="n", xaxt="n", xlab="", ylab="", ylim=ylimVals)
    layers <- addPlotData(layers, 1, 1:numScenarios, "axis", labels=namesScenarios, las=2, cex=0.5)
    layers <- addPlotData(layers, list(h=0), plotfunc="abline", lty=3)
    layers <- makePlotLayersResStats(efficiencySE, layers, useBoxPlots, redAlphas)
    plot.plotLayers(layers, plotObj$device, thisFileName, plotObj$fileHeight, 
                    plotObj$fileWidth, titleTxt, xlab="")
  }
  
}

#-----------------------------------------------------------------------------------------

plotScenarioCoeffEsts <- function(scenariosObj, plotObj, trueCoeffs, resLstAll, 
                                  plotSDMs = resLstAll[[1]]$namesValidSDM, 
                                  fileNameStart="EstCoeffsSpecies") {
  
  # Plot for each coefficient x species.  Each plot contains a boxplot for each sdm x scenario.
  
  # Names and numbers of things.  FYI: assume all objects are required dimensions, no checking done!
  namesCoeffs <- rownames(trueCoeffs)
  numCoeffs <- length(namesCoeffs)
  numRuns <- scenariosObj$numRuns
  numScenarios <- scenariosObj$numScenarios
  namesScenarios <- scenariosObj$namesScenarios
  namesSpecies <- colnames(trueCoeffs)
  numSpecies <- length(namesSpecies)
  namesValidSDMs <- resLstAll[[1]]$namesValidSDM
  validSDMs <- resLstAll[[1]]$validSDM
  numValidSDMs <- length(validSDMs)           # Assumes namesValidSDMs and validSDMs are same length!

  # Which SDMs to plot.
  # Figure out the number of SDMs to plot.
  numPlotSDMs <- length(plotSDMs)
  if ( ! all(plotSDMs %in% namesValidSDMs) ) {
    stop("Unrecognised SDM requested in plots.")
  }
  
  # Figure out the validSDMs needed from the plotSDMs requested. 
  # NB: validSDMs are the names used by the result object to store each SDM results.  This
  #     is different to the names used by the user to request various SDMs (clunky work 
  #     around as I changed the public names of the SDMs towards the end of the coding and
  #     it was too much trouble to change the internal names).
  indPlotSDMs <- match(plotSDMs, namesValidSDMs)
  plotValidSDMs <- validSDMs[indPlotSDMs] 
  
  # x-axis labels for the plots.
  namesSDMScenarios <- paste0(rep(plotSDMs, each=numScenarios),
                              rep(namesScenarios, times=numPlotSDMs))
  
  # Plot directory for scenario level plots.
  plotDir <- paste0(scenariosObj$scenariosDir, "/Plots")
  if ( ! dir.exists(plotDir) ) {
    dir.create(plotDir, recursive = TRUE)
  }
  
  # Want a plot for each species and each coefficient.
  for ( species in namesSpecies ) {
    
    for ( j in 1:numCoeffs ) {
      nameCoeff <- namesCoeffs[j]
      
      # True coefficient.
      trueCoeff <- trueCoeffs[nameCoeff,species]
      
      # Create storage for the data required for the boxplot into a single matrix.
      coeffEsts <- matrix(nrow=numRuns, ncol=numPlotSDMs*numScenarios, 
                          dimnames = list(1:numRuns, namesSDMScenarios))
      
      # Get data from each SDM x scenario combination.
      for (i in 1:numScenarios) {
        # This scenario's results.
        scenarioRes <- resLstAll[[i]]
        
        for ( s in 1:numPlotSDMs ) {
          nameSDM <- plotValidSDMs[s]
          
          # Index in storage matrix.
          indCol <- i + ((s-1)*numScenarios)
          
          # All estimates of this species x this coeff for this SDM x scenario (i.e. estimates from each run)
          coeffEsts[ ,indCol] <- scenarioRes[[nameSDM]]$coeffs[nameCoeff,species, ]
        }
      }

      # File name for the plot.
      thisFileName <- paste0(fileNameStart, "-", species, "-", nameCoeff)  
      thisFileName <- makeFileName(thisFileName, plotDir, plotObj$device)
      
      # Make layers for the plot.
      layers <- setPlotLayers()
      layers <- addPlotData(layers, coeffEsts, plotfunc="boxplot", las=2, cex=0.25, 
                            names=rep(namesScenarios,numPlotSDMs), 
                            xlim=c(1,(numPlotSDMs*numScenarios)) )
      layers <- addPlotData(layers, list(h=trueCoeff), plotfunc="abline", lty=3)
      
      # Do we need to add vertical line/s to separate SDMs into sub-plots?
      if ( numPlotSDMs > 1 ) {
        for ( i in 2:numPlotSDMs) {
          xVal <- ((i-1) * numScenarios ) + 0.5
          layers <- addPlotData(layers, list(v=xVal), plotfunc="abline", lty=1)
        }
      }
      
      # Add name of SDM to subplots.
      xVal <- (1:numPlotSDMs) * numScenarios
      yVal <- rep(max(coeffEsts, na.rm=TRUE), times=numPlotSDMs)
      layers <- addPlotData(layers, xVal, yVal, plotfunc="text", labels=plotSDMs)
      
      
      # Plot this coefficient for this species ...
      titleTxt <- paste0("True and estimated ", nameCoeff, " for ", species, " data.")
      
      # Plot the layers.
      plot.plotLayers(layers, plotObj$device, thisFileName, plotObj$fileHeight, 
                      plotObj$fileWidth, titleTxt, xlab="")
      
    }
  }

}

#-----------------------------------------------------------------------------------------

showAllScenariosErrors <- function(resLstAll) {
  
  # Prints to the console all the errors that are recorded in the results, for each scenario.
  
  # Number of scenarios
  numScenarios <- length(resLstAll)
  
  # Cycle through the results for each sceanrio.
  for ( i in 1:numScenarios ) {
    scenarioErrors <- resLstAll[[i]]$errors
    
    # Are there errors for this scenario?
    if ( dim(scenarioErrors)[1] > 0 ) {
      message("Errors that occurred in scenario ", i)
      print(scenarioErrors)
    } else {
      message("No errors recorded for scenario ", i)
    }
  }
  
}

#-----------------------------------------------------------------------------------------

showAllScenariosWarnings <- function(resLstAll) {
  
  # Prints to the console all the warnings that are recorded in the results, for each scenario.
  
  # Number of scenarios
  numScenarios <- length(resLstAll)
  
  # Cycle through the results for each sceanrio.
  for ( i in 1:numScenarios ) {
    scenarioWarnings <- resLstAll[[i]]$warnings
    
    # Are there warnings for this scenario?
    if ( dim(scenarioWarnings)[1] > 0 ) {
      message("Warnings that occurred in scenario ", i)
      print(scenarioWarnings)
    } else {
      message("No warnings recorded for scenario ", i)
    }
  }
  
}

#-----------------------------------------------------------------------------------------

makePlotLayersResStats <- function(resStats, layers, useBoxPlots=FALSE, redAlphas=FALSE){

  # Add the data layers for the given result statistic to the given layers that setup the plot.
  #
  # Arguments ...
  # resStats:    an array containing the result statistic (numScenarios x numCoeffs x numSpecies)
  # layers:      plot layers already setup to start plot (this function will add layers to this)
  # useBoxplots: by default, points of data are plotted but if this is true, boxplots will be used
  # redAlphas:   by default, no distinction is made between any of the coefficients.  
  #              However, if this is TRUE, then the alpha coefficient data points will be
  #              coloured red OR, if useBoxplots = TRUE, plotted as separate boxes.

  # Numbers and names of things.
  numCoeffs <- dim(resStats)[2]
  namesCoeffs <- dimnames(resStats)[[2]]
  numSpecies <- dim(resStats)[3]
  namesSpecies <- dimnames(resStats)[[3]]
  numScenarios <- dim(resStats)[1]
  namesScenarios <- dimnames(resStats)[[1]]

  if ( useBoxPlots ) {
    # Boxplots ... organise data into correct form.
    plotData <- as.data.frame(matrix(nrow = numSpecies*numCoeffs, ncol = numScenarios),
                              stringsAsFactors=FALSE)
    names(plotData) <- namesScenarios
    for ( j in 1:numCoeffs ) {
      for ( k in 1:numSpecies) {
        species <- namesSpecies[k]
        ind <- (j-1)*numSpecies + k
        plotData[ind, ] <- resStats[ ,j,species]
      }
    }
    
    # Do we need to separate out the alpha coefficients (i.e. make them red)?
    if ( redAlphas ) {
      # NB: alpha coefficients are the first numSpecies rows of data.
      layers <- addPlotData(layers, plotData[1:numSpecies, ], plotfunc="boxplot", 
                            names=rep("",numScenarios), border="red")
      layers <- addPlotData(layers, plotData[-(1:numSpecies), ], plotfunc="boxplot",
                            names=rep("",numScenarios))
    } else {
      layers <- addPlotData(layers, plotData, plotfunc="boxplot", names=rep("",numScenarios))
    }
    
  } else {
    # Points ... use data as is.
    for ( species in namesSpecies ) {
      for ( j in 1:numCoeffs ) {
        if ( redAlphas && j == 1 ) {
          # Separate out the alpha coefficients by making them red.
          layers <- addPlotData(layers, 1:numScenarios, resStats[ ,j,species], 
                                plotfunc="points", col="red")
        } else {
          layers <- addPlotData(layers, 1:numScenarios, resStats[ ,j,species], plotfunc="points")
        }
      }
    }
  }
  
  # Return plot layers.
  return(layers)
  
}

#-----------------------------------------------------------------------------------------

internalPlotScenariosStat2 <- function(stats, statName="statistics", samePage=TRUE,
                                       ylimVals=NULL, plotSDMs=dimnames(stats)[[4]], 
                                       diffPORanges=c(FALSE,FALSE), absStats=TRUE, vioPlots=FALSE,
                                       plotDevice="RStudioGD", plotDir=getwd(), 
                                       plotUnits="cm", plotHeight=9, plotWidth=12.25, 
                                       plotRes=600, doTitle=FALSE, outlierLabels=FALSE,
                                       xAxisTitle=NULL){
  
  # Internal plotting function to plot the summary statistics for scenarios.
  #
  # Arguments ...
  # stats:      A 4D array that contains the values to be plotted (numScenarios x numCoeffs 
  #             x numSpecies x numValidSDMs)
  # statName:   Name of statistic (used in file names and titles).
  # samePage:   Whether the plots are all on the same page (one file) or separate pages 
  #             (multiple files) NOT CODED YET!  TO DO !!!
  # ylimVals:   Indicates the range of the y-axis to be plotted.  Generally, when not NULL, 
  #             the y-axis is to be truncated to less than the actual values present.  When
  #             NULL, ylimVals is set to range(stats) so that all plots have same axis.
  # plotSDMs:   Which SDMs (and what order when on same page) to plot results from.
  # diffPORanges: if true, use different ranges for the y-axis in the PO SDM plot of 
  #             the alphas for diffPORanges[1] and/or betas for diffPORanges[2].
  # absStats:   if true, plot the absolute value of the statistics.
  # vioPlots:   if true, include violin plots behind the points.
  # plotDevice: What type of graphics device is to be used to plot.  "RStudioGD" is the 
  #             default and will display the plot in the RStudio plots tab.  See the 
  #             "grDevices" package for more help with devices but "png" is a good one!
  # plotDir:    Only when plotDevice not equal to "RStudioGD".  Directory where to save
  #             plot file.  File name is created as <statName>.<plotDevice>
  # plotUnits:  Only when plotDevice not equal to "RStudioGD".  Units in which height and
  #             width are given.
  # plotHeight: Only when plotDevice not equal to "RStudioGD".  Height of the plot in the 
  #             given units.
  # plotWidth:  Only when plotDevice not equal to "RStudioGD".  Width of the plot in the 
  #             given units.
  # plotRes:    Only when plotDevice not equal to "RStudioGD".  Resolution of the plot in
  #             pixels per inch (ppi).
  # doTitle:    Whether or not to add title to the plot.
  # outlierLabels: Whether or not to add outlier labels to plot.
  # xAxisTitle: Title to add to x-axis (at bottom of plot).  If NULL, nothing added.
  
  # Numbers and names of things.
  dimStats <- dim(stats)
  numScenarios <- dimStats[1]
  numCoeffs <- dimStats[2]
  numSpecies <- dimStats[3]
  numSDMs <- dimStats[4]
  dimnamesStats <- dimnames(stats)
  namesScenarios <- dimnamesStats[[1]]
  namesCoeffs <- dimnamesStats[[2]]
  namesSDMs <- dimnamesStats[[4]]
  namesSpecies <- dimnamesStats[[3]]

  # Colours for plots.
  pointsCol <- "black"
  violinCol <- "grey"
  pointsColPO <- rgb(0, 114, 178, maxColorValue = 255)  # blue
  violinColPO <- rgb(86, 180, 233, maxColorValue = 255) # sky blue
  
  # Figure out the number of SDMs to plot.
  numPlotSDMs <- length(plotSDMs)
  if ( ! all(plotSDMs %in% namesSDMs) ) {
    stop("Unrecognised SDM requested in plots.")
  }
  
  # Is the y-axis truncated (assumes ylimVals are less than actual range)
  if ( is.null(ylimVals) ) {
    fileTrunc <- ""
    titleTrunc <- ""
  } else {
    fileTrunc <- "-trunc"
    titleTrunc <- " - truncated"
  }
  
  # Which device are we printing to?
  if ( plotDevice == "RStudioGD" ) {
    # plot to the R studio plot window.
    plotToFile <- FALSE
  } else {
    fileName <- makeFileName(paste0(statName, fileTrunc), plotDir, plotDevice)
    argList <- list(filename = fileName, width = plotWidth, height = plotHeight,
                    units = plotUnits, res = plotRes)
    do.call(plotDevice, argList)
    plotToFile <- TRUE
  }
  
  # Divide plotting page into 2 rows (one for alphas and one for betas) and numSDMs columns.
  omaPar <- par("mar")         # use default single plot per page margins
  marPar <- c(0.0,0.0,1.3,0.0) # small margin on top to separate rows and allow for column titles.
  if ( is.null(xAxisTitle) ) {
    # No x-axis title, change margins to gain plotting area.
    omaPar[1] <- omaPar[1] - 1.0  # leave space for x-axis ticks and labels.  
  }
  if ( ! doTitle ) {
    # No main title, change margins to gain plotting area.
    omaPar[3] <- 0.0  # NB: there is already a bit at top due to mar settings.
  }
  opar <- par(mfrow=c(2,numPlotSDMs), oma = omaPar, mar = marPar, cex=0.66)
  
  # Convert stats to absolute values?
  if ( absStats ) stats <- abs(stats)
  
  # Plot the alpha coefficients in the first row.
  internalPlotRowScenarios(stats, plotSDMs, whichCoeffs=c(1), ylimVals = ylimVals, 
                           diffPORanges = diffPORanges[1], vioPlots = TRUE, 
                           rowTitle = "alphas", columnTitles = TRUE)
  
  # Plot the beta coefficients in the second row.
  internalPlotRowScenarios(stats, plotSDMs, whichCoeffs=2:numCoeffs, ylimVals = ylimVals, 
                           diffPORanges = diffPORanges[2], vioPlots = TRUE, rowTitle = "betas", 
                           xAxisLabels = namesScenarios)
  
  # Add a title to the plot.
  if ( doTitle ) 
    title(paste0("Coefficient ", statName, " for the given scenarios with the given SDMs", 
               titleTrunc), outer=TRUE)

  # Add an x-axis title to the plot.
  if ( ! is.null(xAxisTitle) ) title(xlab = xAxisTitle, outer=TRUE, line=min(4,omaPar[1]))
    
  # Turn off the plotting device, if it is a file.
  if ( plotToFile ) dev.off()
  
  # Return plotting to original settings.
  par(opar)
  
}

#-----------------------------------------------------------------------------------------

internalPlotRowScenarios <- function(stats, plotSDMs = dimnames(stats)[[4]], 
                                     whichCoeffs = 1:dim(stats)[2], ylimVals = NULL,
                                     diffPORanges = FALSE, vioPlots = FALSE, 
                                     rowTitle="stats", columnTitles=FALSE, 
                                     xAxisLabels=NULL, outlierLabels=FALSE) {
  
  # Plot a row (i.e. either alphas or betas) of the matrix of plots to compare the 
  # scenario results.

  # Numbers and names of things.
  dimStats <- dim(stats)
  numScenarios <- dimStats[1]
  numCoeffs <- dimStats[2]
  numSpecies <- dimStats[3]
  numSDMs <- dimStats[4]
  dimnamesStats <- dimnames(stats)
  namesScenarios <- dimnamesStats[[1]]
  namesCoeffs <- dimnamesStats[[2]]
  namesCoeffsShort <- c("a", paste0("b",1:(numCoeffs-1)))
  namesSDMs <- dimnamesStats[[4]]
  namesSpecies <- dimnamesStats[[3]]
  
  # Colours for plots.
  pointsCol <- "black"
  violinCol <- "grey"
  pointsColPO <- rgb(0, 114, 178, maxColorValue = 255)  # blue
  violinColPO <- rgb(86, 180, 233, maxColorValue = 255) # sky blue
  
  # Figure out the number of SDMs to plot.
  numPlotSDMs <- length(plotSDMs)
  if ( ! all(plotSDMs %in% namesSDMs) ) {
    stop("Unrecognised SDM requested in plots.")
  }
  
  # Y-axis range ... 
  yaxisRange <- matrix(nrow=2, ncol=numPlotSDMs, dimnames = list(1:2,plotSDMs))
  indPOSDM <- length(0)
  indOtherSDMs <- 1:numPlotSDMs
  if ( is.null(ylimVals) ) {
    if ( diffPORanges ) {
      # Different range for PO SDM.
      if ( "PO" %in% plotSDMs ) 
        yaxisRange[ ,"PO"] <- range(stats[ ,whichCoeffs, ,"PO"], na.rm = TRUE)
      
      # Same range for other SDMs.
      indPOSDM <- which(plotSDMs %in% "PO" )
      if ( length(indPOSDM) > 0 ) indOtherSDMs <- indOtherSDMs[-indPOSDM]
      if ( length(indOtherSDMs) > 0 ) {
        tmp <- range(stats[ ,whichCoeffs, ,plotSDMs[indOtherSDMs]], 0, na.rm = TRUE)  # include zero when stats are positive.
        yaxisRange[ ,plotSDMs[indOtherSDMs]] <- rep(tmp, length(indOtherSDMs))
      }
    } else {
      # Same range for all.
      tmp <- range(stats[ ,whichCoeffs, ,plotSDMs], 0, na.rm = TRUE)  # include zero when stats are positive.
      yaxisRange[ ,plotSDMs] <- rep(tmp, numPlotSDMs)
    }
  } else {
    # Same for all, whatever is specified.
    yaxisRange[1, ] <- rep(ylimVals[1], numPlotSDMs)
    yaxisRange[2, ] <- rep(ylimVals[2], numPlotSDMs)
  }
  
  # Calculate widths for violin plots in row.
  if ( vioPlots ) {
    # Get the maximum density for each SDM x scenario combination.
    maxDensity <- matrix(nrow=numScenarios, ncol=numPlotSDMs, dimnames=list(1:numScenarios, plotSDMs))
    for ( sdm in plotSDMs ) {
      
      for ( sc in 1:numScenarios ) {
        # What is the maximum of the density for each scenario.
        thisStats <- stats[sc,whichCoeffs, ,sdm]
        maxDensity[sc,sdm] <- max(density(thisStats)$y)
      }
    }
    
    # Scale maximum densities so that widths are between [0,1]
    widthsViolins <- maxDensity
    if ( ! is.null(ylimVals) || ! diffPORanges ) {
      # All sdm plots are on the same scale.
      maxMaxDensity <- max(maxDensity[])
      widthsViolins <- maxDensity/maxMaxDensity
      
    } else { # if ( is.null(ylimVals) && diffPOAlphaRanges )
      # PO sdm is on a different scale (NB: and ylimVals are not set by user).
      if ( length(indPOSDM) > 0 ) {
        maxMaxDensity <- max(maxDensity[ ,"PO"])
        widthsViolins[ ,"PO"] <- maxDensity[ ,"PO"] / maxMaxDensity
      }
      
      # Other SDMs are on the same scale (i.e. y-axis range).
      if ( length(indOtherSDMs) > 0 ) {
        maxMaxDensity <- max(maxDensity[ ,plotSDMs[indOtherSDMs]])
        widthsViolins[ ,plotSDMs[indOtherSDMs]] <- maxDensity[ ,plotSDMs[indOtherSDMs]] /
                                                     maxMaxDensity
      }
    }
  }
  
  # Work out what the labels would be for each species x coefficient combination.
  if ( outlierLabels ) {
    allLabels <- matrix(nrow=numCoeffs, ncol=numSpecies, dimnames=list(1:numCoeffs, namesSpecies))
    for ( i in 1:numCoeffs ) {
      allLabels[i, ] <- paste0(namesCoeffsShort[i], namesSpecies)
    }
  }
  
  # Plot row ...
  firstPlotInRow <- TRUE
  for ( sdm in plotSDMs ) {
    # Plot statistics for each SDM (i.e. column) for this row.
    plot(c(0.5,numScenarios+0.5),yaxisRange[ ,sdm], type="n", xaxs="i", xaxt="n", xlab="", 
         yaxt="n", ylab="")
    for ( sc in 1:numScenarios ) {
      # All species' coefficient stats at this scenario and SDM!
      thisStats <- as.vector(stats[sc,whichCoeffs, ,sdm])
      if ( outlierLabels ) thisLabels <- as.vector(allLabels[whichCoeffs, ])
      indNA <- which(is.na(thisStats))
      if ( length(indNA) > 0 ) {
        warning(paste0("One or more NA values in ", statName, " for SDM = ", sdm, 
                       " and scenario = ", namesScenarios[sc]))
        thisStats <- thisStats[-indNA]
        if ( outlierLabels ) thisLabels <- thisLabels[-indNA]
      }
      
      # If PO is to be a different range, do different y-axis for PO.
      if ( is.null(ylimVals) && diffPORanges && sdm == "PO" ) {
        if ( vioPlots ) vioplot(thisStats, col=violinColPO, border=violinColPO, add=TRUE,
                                drawRect = FALSE, at=sc, wex=widthsViolins[sc,sdm])
        axis(4, labels=TRUE, col=pointsColPO)
        stripchart(thisStats, at=sc, vertical=TRUE, pch="_", add=TRUE, col=pointsColPO)
      } else {
        if ( vioPlots ) vioplot(thisStats, col=violinCol, border=violinCol, add=TRUE,
                                drawRect = FALSE, at=sc, wex=widthsViolins[sc,sdm])
        stripchart(thisStats, at=sc, vertical=TRUE, pch="_", add=TRUE, col=pointsCol)
      }
      
      # Labels on outliers?
      if ( outlierLabels ) {
        # Use the interquartile range method to identify outliers.
        quartiles <- quantile(thisStats, probs=c(0.25,0.5,0.75), type = 8)
        intQuartRange <- quartiles[3] - quartiles[1]
        notOutlierRange <- c(quartiles[1] - 1.5*intQuartRange,
                             quartiles[3] + 1.5*intQuartRange)
        indOutliers <- which(thisStats < notOutlierRange[1] | thisStats > notOutlierRange[2])
        numOutliers <- length(indOutliers)
        if ( numOutliers > 0 ) {
          # FYI: cex=0.3 will only be readable if the plot resolution (i.e. pixels per inch) 
          #      is high enough (at least 600ppi).
          text(sc, thisStats[indOutliers], labels=thisLabels[indOutliers], pos=4, cex=0.3,
               offset=0.2)
        }
        
        # Median and boundaries between outliers and "normal" values.
        points(sc, quartiles[2], pch="_", col="green")
        points(sc, notOutlierRange[1], pch="_", col="blue")
        points(sc, notOutlierRange[2], pch="_", col="blue")
      }
    }
    abline(h=0, lty="dotted")
    
    
    # If this is the first column, do y-axis.
    if ( firstPlotInRow ) {
      axis(2, labels=TRUE)
      midyRange <- ((yaxisRange[2,sdm] - yaxisRange[1,sdm])/2.0) + yaxisRange[1,sdm]
      axis(2, at=midyRange, labels=rowTitle, tick=FALSE, padj=-2.0)
      firstPlotInRow <- FALSE
    }
    
    # If this is the PO column (this will cause all sorts of problems if PO is the first column!)
    if ( is.null(ylimVals) && diffPORanges && sdm == "PO" && !firstPlotInRow ) {
      axis(4, labels=TRUE, col=pointsColPO)
    } else if ( is.null(ylimVals) && diffPORanges && sdm == "PO" && firstPlotInRow) {
      warning("No code for correct axes when PO is the first SDM and a different range is required!")
    }
    
    # Do SDM labels at top x-axes?
    if ( columnTitles ) 
      axis(3, (numScenarios+1)/2.0, lwd.ticks = 0, labels=paste(sdm,"SDM"), padj=1.5)

    # Do x-axis for this row's column?
    if ( ! is.null(xAxisLabels) ) axis(1, 1:numScenarios, xAxisLabels, las=2)
  }

}  

#-----------------------------------------------------------------------------------------

plotWarningErrorSummary <- function(scenariosObj, retLst) {
  
  # Simple count of warnings and errors per species x scenario combination.  Plots to console!
  # Visual version of numSuccessfulRunsScenarios function output (I think?).
  
  # numbers of things
  numScenarios <- scenariosObj$numScenarios
  namesSpecies <- retLst$simObj$namesSpecies
  numSpecies <- length(namesSpecies)

  # Get number of warnings per species for each scenario.
  y <- matrix(0,nrow=numScenarios, ncol=numSpecies, dimnames=list(1:numScenarios,namesSpecies))
  for ( i in 1:numScenarios) {
    tmp <- table(retLst$resLstAll[[i]]$warnings$species)
    y[i,names(tmp)] <- tmp
    tmp <- table(retLst$resLstAll[[i]]$errors$species)
    if ( ! is.null(names(tmp)) ) y[i,names(tmp)] <- y[i,names(tmp)] + tmp
  }
  
  # Plot ...
  plotCol <- rainbow(numScenarios)
  plot(c(1,numSpecies), range(y, na.rm=TRUE), type="n", xaxt="n", xlab="", ylab="num warnings + errors")
  for ( i in 1:numScenarios ) {
    points(1:numSpecies, y[i,], col=plotCol[i], pch=i)
  }
  legend("topleft", scenariosObj$namesScenarios, col=plotCol, pch=1:numScenarios)
  axis(1, 1:numSpecies, namesSpecies, las=2)
  title("Number of warnings and errors per scenario for each species", 
        sub=paste0("numRuns=", scenariosObj$numRuns))
  abline(h=0, lty="dashed")

}

#-----------------------------------------------------------------------------------------

makeScenarioStats <- function(scenariosObj, resLstAll, whichStats=c(1,2,3), 
                              numCoresUse=14, whichSDMs=resLstAll[[1]]$namesValidSDM, 
                              whichScenarios=1:scenariosObj$numScenarios) {
  
  # Make the scenario statistics for the expected number of values per cell (i.e. look at 
  # the accuracy and precision of the estimated lambda in each cell).
  # 
  # Values of whichStats (per species x scenario x sdm)
  # 1 = difference betwwen true and estimated coefficients (extra coefficient dimension)
  # 2 = correlation between true and estimated lambda[cell_i]
  # 3 = difference between true and estimated for total expected number of species (i.e. 
  #     overall abundance for whole domain).  This is lambda[cell_i] * area[cell_i]!
  # 
  # Values of whichSDMs: AB, PA, PO, and/or MsPP.
  
  # Names and numbers of things.  
  numRuns <- scenariosObj$numRuns
  namesScenarios <- scenariosObj$namesScenarios
  prettyNamesScenarios <- scenariosObj$prettyNamesScenarios
  namesSpecies <- resLstAll[[1]]$namesSpecies
  numSpecies <- length(namesSpecies)
  namesValidSDMs <- resLstAll[[1]]$namesValidSDM
  validSDMs <- resLstAll[[1]]$validSDM        # Assumes all scenarios use the same SDMs!
#  numValidSDMs <- length(validSDMs)           # Assumes namesValidSDMs and validSDMs are same length!
  validStats <- c(1,2,3)
  
  # Which SDMs results to use.
  numRequestedSDMs <- length(whichSDMs)
  if ( ! all(whichSDMs %in% namesValidSDMs) ) {
    stop("Unrecognised SDM requested in argument 'whichSDMs'.")
  } else if ( numRequestedSDMs == 0 ) {
    stop("No SDM has been specified in the 'whichSDMs' argument.")
  }
  
  # Check statistics requested are valid.
  numStats <- length(whichStats)
  if ( numStats == 0 ) {
    stop("No statistics have been requested in the 'whichStats' argument.")
  } else if ( ! all(whichStats %in% validStats) ) {
    stop("Unrecognised statistic requested in argument 'whichStats'.")
  }
  
  # Which scenarios are we interested in?
  numRequestedScenarios <- length(whichScenarios)
  if ( ! all(whichScenarios %in% (1:(scenariosObj$numScenarios))) ) {
    stop("Unrecognised scenario index requested in 'whichScenarios' argument.")
  } else if ( numRequestedScenarios == 0 ) {
    stop("No scenarios have been requested in 'whichScenarios' argument.")
  }
  
  # Initialise the return value.
  tmp <- array(dim=c(numRequestedScenarios, numSpecies, numRequestedSDMs, 1), 
               dimnames=list(prettyNamesScenarios[whichScenarios], namesSpecies, whichSDMs, 1))
  namesCoeffs <- dimnames(resLstAll[[1]][[validSDMs[1]]]$coeffs)[[1]]
  numCoeffs <- length(namesCoeffs)
  tmpCoeff <- array(dim=c(numRequestedScenarios, numSpecies, numRequestedSDMs, numCoeffs), 
                    dimnames=list(prettyNamesScenarios[whichScenarios], namesSpecies, whichSDMs, namesCoeffs))
  stats <- vector("list", numStats)
  for ( istat in 1:numStats  ) {
    thisStat <- whichStats[istat]
    if ( thisStat == 1 ) {
      # Need extra dimension for coefficients.
      stats[[istat]] <- list(avg=tmpCoeff, sd=tmpCoeff)
    } else {
      stats[[istat]] <- list(avg=tmp, sd=tmp)
    }
  }
  
  # Calculate each statistic's mean and sd (across numRuns) for each sdm x species x scenario.
  for ( reqSc in 1:numRequestedScenarios ) {
    # What is the actual scenario number for the reqSc^th requested scenario?
    sc <- whichScenarios[reqSc]
    nameScenario <- namesScenarios[sc]
    
    # Start timing.
    message("Begin statistics calculation for scenario ", nameScenario)
    timeBegin <- Sys.time()
    message(paste0("    Begin: ", timeBegin))
    
    # Load required data (i.e. cellObj which is different for each scenario)
    load(paste0(scenariosObj$scenariosDir, "/", nameScenario,"/DataDumps/DataDump-AllRuns.RData"))
    
    # Run parallelised version of statistics calculations (sdm x run x stat loops inside here)
    # parRes <- internalParallelScenarioStats(namesSpecies, resLstAll[[sc]], whichSDMs,
    #                                         whichStats, simObj$coeffs,
    #                                         cellObj, simObj$lambda.formula)
    parRes <- mclapply(namesSpecies, internalParallelScenarioStats, resLstAll[[sc]],
                       whichSDMs, whichStats, simObj$coeffs, cellObj, simObj$lambda.formula,
                       mc.cores = numCoresUse)
    
    # Collect results into return value.
    for (k in 1:numSpecies ) {
      # Was there an error?
      if ( is.null(parRes[[k]]) ) {
        message(paste0("Error for scenario ", nameScenario, ":\n", 
                       "Results are not available as core returned NULL."))

      } else if ( parRes[[k]]$numErrors > 0 ) {
        message(paste0("Error for scenario ", nameScenario, ":"))
        for ( err in parRes[[k]]$numErrors ) {
          message(parRes[[k]]$errorMsgs[err])
        }
      } else {
        species <- parRes[[k]]$species
        for ( istat in 1:numStats) {
          thisStat <- whichStats[istat]
          if ( thisStat == 1 ) {
            for ( j in 1:numCoeffs ) {
              stats[[istat]]$avg[reqSc,species, ,j] <- parRes[[k]]$avg[istat, ,j]
              stats[[istat]]$sd[reqSc,species, ,j] <- parRes[[k]]$sd[istat, ,j]
            }
          } else {
            stats[[istat]]$avg[reqSc,species, ,1] <- parRes[[k]]$avg[istat, ,1]
            stats[[istat]]$sd[reqSc,species, ,1] <- parRes[[k]]$sd[istat, ,1]
          }
        }
      }
      
      # Were there warnings?
      if ( parRes[[k]]$numWarnings > 0 ) {
        message(paste0("Warning for scenario ", nameScenario,":"))
        for ( wrn in parRes[[k]]$numWarnings ) {
          message(parRes[[k]]$warningMsgs[wrn])
        }
      }
    }    
    timeEnd <- Sys.time()
    message(paste0("    End:   ", timeEnd))
  }
  
  # Return value.
  return(stats)
  
}

#-----------------------------------------------------------------------------------------

plotScenariosSummaryLambda <- function(stat, nameStat="", whichCoeffs=1:dim(stat$avg)[[4]],
                                       ylimVals=NULL, plotSDMs=dimnames(stat$avg)[[3]], 
                                       vioPlots=TRUE, absMeans=TRUE, plotDevice="RStudioGD", 
                                       plotDir=getwd(), plotUnits="cm", plotHeight=9, 
                                       plotWidth=12.25, plotRes=600, doTitle=FALSE,
                                       xAxisLabels=NULL, xAxisTitle=NULL, xAxisLabelStyle=2,
                                       bigNumThreshold = 10, horizontalLines=c(0,0),
                                       diffPORange=c(FALSE,FALSE)) {

  # Plot the scenarios' summary for a single overall statistic (i.e. look at the accuracy/avg
  # and precision/sd of the given statistic). Plotting function for "makeScenarioStats" but
  # call this function once for *each* statistic made in "makeScenarioStats".

  # Names and numbers of things.  
  dimnamesStat <- dimnames(stat$avg)
  namesScenarios <- dimnamesStat[[1]]
  numScenarios <- length(namesScenarios)
  namesSpecies <- dimnamesStat[[2]]
  numSpecies <- length(namesSpecies)
  namesValidSDMs <- dimnamesStat[[3]]
  numValidSDMs <- length(namesValidSDMs)
  namesForthDim <- dimnamesStat[[4]]            # May be coefficients, may not be!
  numForthDim <- length(namesForthDim)
  
  # Check whichCoeffs are within valid range.
  numReqCoeffs <- length(whichCoeffs)
  if ( numReqCoeffs == 0 && numForthDim > 1 ) {
    stop("No coefficients have been requested in the 'whichCoeffs' argument.")
  } else if ( ! all(whichCoeffs %in% 1:numForthDim) ) {
    stop("Unrecognised coefficient requested in argument 'whichCoeffs'.")
  }

  # Were x-axis labels provided (i.e. pretty names for scenarios)?
  if ( is.null(xAxisLabels) ) {
    xAxisLabels <- namesScenarios
  } else if ( length(xAxisLabels) == numScenarios && 
              length(unique(xAxisLabels)) == numScenarios ) {
    # use provided labels.
  } else {
    stop("Invalid x-axis labels given in argument 'xAxisLabels'.")
  }

  # Figure out the number of SDMs to plot.
  numPlotSDMs <- length(plotSDMs)
  if ( ! all(plotSDMs %in% namesValidSDMs) ) {
    stop("Unrecognised SDM requested in plots.")
  }
  
  # Plot directory for scenario level plots.
  if ( ! dir.exists(plotDir) ) {
    dir.create(plotDir, recursive = TRUE)
  }
  
  # Are the absolute value of the means to be used?
  if ( absMeans ) stat$avg <- abs(stat$avg)
  
  # Threshold setting for plotting extremely big numbers (or big negative numbers)!
  for ( sdm in plotSDMs ) {
    for ( sc in 1:numScenarios ) {
      for ( d4 in whichCoeffs) {
        # For this sdm and this scenario, the species accuracy statistics are:
        thisStat <- stat$avg[sc, ,sdm,d4]
        indBigNum <- which(abs(thisStat) > bigNumThreshold)
        if ( length(indBigNum) > 0 ) {
          signBigNum <- sign(thisStat[indBigNum])
          stat$avg[sc,indBigNum,sdm,d4] <- signBigNum * bigNumThreshold
        }
        
        # For this sdm and this scenario, the species precision statistics are:
        thisStat <- as.vector(stat$sd[sc, ,sdm,d4])
        indBigNum <- which((abs(thisStat) > bigNumThreshold))
        if ( length(indBigNum) > 0) {
          signBigNum <- sign(thisStat[indBigNum])
          stat$sd[sc,indBigNum,sdm,d4] <- signBigNum * bigNumThreshold
        }
        
        # For NaN which comes out of the sd function (eg. sd(c(1:5,Inf)) = NaN)
        indNaN <- which(is.nan(thisStat))
        if ( length(indNaN) > 0) stat$sd[sc,indNaN,sdm,d4] <- NA
      }
    }
  } 
  
  
  # Is the y-axis truncated (assumes ylimVals are less than actual range)
  if ( is.null(ylimVals) ) {
    fileTrunc <- ""
    titleTrunc <- ""
  } else {
    fileTrunc <- "-trunc"
    titleTrunc <- " - truncated"
  }
  
  # Which device are we printing to?
  if ( plotDevice == "RStudioGD" ) {
    # plot to the R studio plot window.
    plotToFile <- FALSE
  } else {
    fileName <- makeFileName(paste0(nameStat, fileTrunc), plotDir, plotDevice)
    argList <- list(filename = fileName, width = plotWidth, height = plotHeight,
                    units = plotUnits, res = plotRes)
    do.call(plotDevice, argList)
    plotToFile <- TRUE
  }
  
  # Divide plotting into the number of columns required.
  omaPar <- par("mar")         # use default single plot per page margins
  marPar <- c(0.0,0.0,1.3,0.0) # small margin on top to separate rows and allow for column titles.
  if ( is.null(xAxisTitle) ) {
    # No x-axis title, change margins to gain plotting area.
    omaPar[1] <- omaPar[1] - 1.0  # leave space for x-axis ticks and labels.  
  }
  if ( ! doTitle ) {
    # No main title, change margins to gain plotting area.
    omaPar[3] <- 0.0  # NB: there is already a bit at top due to mar settings.
  }
  opar <- par(mfrow=c(2,numPlotSDMs), oma = omaPar, mar = marPar, cex=0.66)
  
  # Colours for plotting.
  pointsCol <- "black"
  violinCol <- "grey"
  pointsColPO <- rgb(0, 114, 178, maxColorValue = 255)  # blue
  violinColPO <- rgb(86, 180, 233, maxColorValue = 255) # sky blue
  
  # Y-axis range ...
  yaxisRange <- array(dim=c(2,2,numPlotSDMs), 
                      dimnames = list(c("mean","sd"), c("min","max"), plotSDMs))  
  if ( is.null(ylimVals) ) {
    # What is the position of the "PO" SDM in the SDMs to be plotted?
    indPOSDM <- which(plotSDMs %in% "PO")
    indOtherSDMs <- 1:numPlotSDMs
    if ( length(indPOSDM) > 0) indOtherSDMs <- indOtherSDMs[-indPOSDM]
    
    # Plot the PO SDM with a different y-axis range for the first row?
    if ( diffPORange[1] && "PO" %in% plotSDMs ) {
      # Yes (and it is one of the plots!!!)
      yaxisRange[1,1,indPOSDM] <- min(stat$avg[ , ,plotSDMs[indPOSDM],whichCoeffs])
      yaxisRange[1,2,indPOSDM] <- max(stat$avg[ , ,plotSDMs[indPOSDM],whichCoeffs])
      if ( length(indOtherSDMs) > 0 ) {
        # There are other SDMs to be plotted.
        yaxisRange[1,1,indOtherSDMs] <- rep(min(stat$avg[ , ,plotSDMs[indOtherSDMs],whichCoeffs]), 
                                            times=length(indOtherSDMs))
        yaxisRange[1,2,indOtherSDMs] <- rep(max(stat$avg[ , ,plotSDMs[indOtherSDMs],whichCoeffs]), 
                                            times=length(indOtherSDMs))
      }
    } else {
      # No, use whatever the values suggest for the y axis range (same for all SDMs)
      yaxisRange[1,1, ] <- rep(min(stat$avg[ , ,plotSDMs,whichCoeffs]), times=numPlotSDMs)
      yaxisRange[1,2, ] <- rep(max(stat$avg[ , ,plotSDMs,whichCoeffs]), times=numPlotSDMs)
    }
    
    # Plot the PO SDM with a different y-axis range for the second row?
    yaxisRange[2,1, ] <- rep(0, times=numPlotSDMs)
    if ( diffPORange[2] && "PO" %in% plotSDMs ) {
      # Yes (and it is one of the plots!!!)
      yaxisRange[2,2,indPOSDM] <- max(stat$sd[ , ,indPOSDM,whichCoeffs])
      if ( length(indOtherSDMs) > 0 ) {
        # There are other SDMs to be plotted.
        yaxisRange[2,2,indOtherSDMs] <- rep(max(stat$avg[ , ,indOtherSDMs,whichCoeffs]), 
                                            times=length(indOtherSDMs))
      }
    } else {
      # No, use whatever the values suggest for the y axis range (same for all SDMs)
      yaxisRange[2,2, ] <- rep(max(stat$sd[ , ,plotSDMs,whichCoeffs], na.rm=TRUE), times=numPlotSDMs)
    }
    
  } else if ( length(ylimVals) == 4 ) {
    # A different range for mean and sd, eg. c(min mean, max mean, min sd, max sd).
    yaxisRange[1,1, ] <- ylimVals[1]
    yaxisRange[1,2, ] <- ylimVals[2]
    yaxisRange[2,1, ] <- ylimVals[3]
    yaxisRange[2,2, ] <- ylimVals[4]
  } else if ( length(ylimVals) == 2 ){
    # Same for mean and sd, eg. c(min, max)
    yaxisRange[1,1, ] <- ylimVals[1]
    yaxisRange[1,2, ] <- ylimVals[2]
    yaxisRange[2, , ] <- yaxisRange[1, , ]    
  } else {
    stop("Unrecognised version of 'ylimVals' argument.")
  }
  
  # Start plots.
  rowTitles <- c("mean", "sd")
  rowDataNames <- c("avg","sd")
  for ( row in 1:2 ) {
    # What is the row title.
    rowTitle <- rowTitles[row]
    
    # When are these things added?
    if ( row == 1 ) {
      columnTitles <- TRUE
      xAxisTicks <- FALSE
    } else {
      columnTitles <- FALSE
      xAxisTicks <- TRUE
    }
    firstPlotInRow <- TRUE
    
    # Calculate widths for violin plots in row.
    if ( vioPlots ) {
      # Get the maximum density for each SDM x scenario combination (i.e. each violin).
      maxDensity <- matrix(nrow=numScenarios, ncol=numPlotSDMs, dimnames=list(1:numScenarios, plotSDMs))
      for ( sdm in plotSDMs ) {
        
        for ( sc in 1:numScenarios ) {
          # For this sdm and this scenario, the species statistics are:
          thisStat <- as.vector(stat[[rowDataNames[row]]][sc, ,sdm, whichCoeffs])

          # Check for NA values.
          indNA <- which(is.na(thisStat))
          if ( length(indNA) > 0 ) {
            warning(paste0(length(indNA), " NA values in ", rowDataNames[row], 
                           " for SDM = ", sdm, 
                           " and scenario = ", namesScenarios[sc]))
            thisStat <- thisStat[-indNA]
            #if ( outlierLabels ) thisLabels <- thisLabels[-indNA]
          }
          
          # What is the maximum of the density for each scenario x sdm.
          maxDensity[sc,sdm] <- max(density(thisStat)$y)
        }
      }
      
      # Scale maximum densities so that widths are between [0,1]
      maxMaxDensity <- max(maxDensity[])
      widthsViolins <- maxDensity/maxMaxDensity
    }
    
    # Do a plot for each SDM.
    for ( sdm in plotSDMs ) {
      # Plot statistics for each SDM (i.e. column) for this row.
      plot(c(0.5,numScenarios+0.5), yaxisRange[row, ,sdm], type="n", xaxs="i", xaxt="n", xlab="", 
           yaxt="n", ylab="")
      for ( sc in 1:numScenarios ) {
        # All species' stats at this scenario and SDM!
        thisStats <- as.vector(stat[[rowDataNames[row]]][sc, ,sdm,whichCoeffs])
        #if ( outlierLabels ) thisLabels <- as.vector(allLabels[whichCoeffs, ])
        indNA <- which(is.na(thisStats))
        if ( length(indNA) > 0 ) {
          # warning(paste0(length(indNA), " NA values in ", rowDataNames[row], 
          #                " for SDM = ", sdm, 
          #                " and scenario = ", namesScenarios[sc]))
          thisStats <- thisStats[-indNA]
          #if ( outlierLabels ) thisLabels <- thisLabels[-indNA]
        }
        
        # Plot for this sdm x scenario.
        # If PO is to be a different range, do different y-axis for PO.
        if ( is.null(ylimVals) && diffPORange[row] && sdm == "PO" ) {
          if ( vioPlots ) vioplot(thisStats, col=violinColPO, border=violinColPO, add=TRUE,
                                  drawRect = FALSE, at=sc, wex=widthsViolins[sc,sdm])
          axis(4, labels=TRUE, col=pointsColPO)
          stripchart(thisStats, at=sc, vertical=TRUE, pch="_", add=TRUE, col=pointsColPO)
        } else {
          if ( vioPlots ) vioplot(thisStats, col=violinCol, border=violinCol, add=TRUE,
                                  drawRect = FALSE, at=sc, wex=widthsViolins[sc,sdm])
          stripchart(thisStats, at=sc, vertical=TRUE, pch="_", add=TRUE, col=pointsCol)
        }

        
        # # Labels on outliers?
        # if ( outlierLabels ) {
        #   # Use the interquartile range method to identify outliers.
        #   quartiles <- quantile(thisStats, probs=c(0.25,0.5,0.75), type = 8)
        #   intQuartRange <- quartiles[3] - quartiles[1]
        #   notOutlierRange <- c(quartiles[1] - 1.5*intQuartRange,
        #                        quartiles[3] + 1.5*intQuartRange)
        #   indOutliers <- which(thisStats < notOutlierRange[1] | thisStats > notOutlierRange[2])
        #   numOutliers <- length(indOutliers)
        #   if ( numOutliers > 0 ) {
        #     # FYI: cex=0.3 will only be readable if the plot resolution (i.e. pixels per inch) 
        #     #      is high enough (at least 600ppi).
        #     text(sc, thisStats[indOutliers], labels=thisLabels[indOutliers], pos=4, cex=0.3,
        #          offset=0.2)
        #   }
        #   
        #   # Median and boundaries between outliers and "normal" values.
        #   points(sc, quartiles[2], pch="_", col="green")
        #   points(sc, notOutlierRange[1], pch="_", col="blue")
        #   points(sc, notOutlierRange[2], pch="_", col="blue")
        # }
      }
      abline(h=horizontalLines[row], lty="dotted")
      #abline(h=0, lty="dotted")
      #if ( row == 1 ) abline(h=1, lty="dotted")
  
      # If this is the first column, do y-axis.
      if ( firstPlotInRow ) {
        axis(2, labels=TRUE)
        midyRange <- ((yaxisRange[row,2,sdm] - yaxisRange[row,1,sdm])/2.0) + yaxisRange[row,1,sdm]
        axis(2, at=midyRange, labels=rowTitle, tick=FALSE, padj=-2.0)
        firstPlotInRow <- FALSE
      }
      
      # If this is the PO column (this will cause all sorts of problems if PO is the first column!)
      # if ( is.null(ylimVals) && diffPORanges && sdm == "PO" && !firstPlotInRow ) {
      #   axis(4, labels=TRUE, col=pointsColPO)
      # } else if ( is.null(ylimVals) && diffPORanges && sdm == "PO" && firstPlotInRow) {
      #   warning("No code for correct axes when PO is the first SDM and a different range is required!")
      # }
      
      # Do SDM labels at top x-axes?
      if ( columnTitles ) 
        axis(3, (numScenarios+1)/2.0, lwd.ticks = 0, labels=paste(sdm,"SDM"), padj=1.0)
      
      # Do x-axis for this row's column?
      if ( xAxisTicks ) {
        axis(1, 1:numScenarios, xAxisLabels, las=xAxisLabelStyle)
      }
    }
  }

  # Add a title to the plot.
  if ( doTitle ) 
    title(paste0("Similarity of intensities (", nameStat, 
                 ") for the given scenarios with the given SDMs", titleTrunc), outer=TRUE)
  
  # Add an x-axis title to the plot.
  if ( ! is.null(xAxisTitle) ) title(xlab = xAxisTitle, outer=TRUE, line=min(4,omaPar[1]))
  
  # Turn off the plotting device, if it is a file.
  if ( plotToFile ) dev.off()
  
  # Return plotting to original settings.
  par(opar)

}

#-----------------------------------------------------------------------------------------

internalParallelScenarioStats <- function(species, resLstScenario, 
                                          whichSDMs=resLstScenario$namesValidSDM, 
                                          whichStats=c(1,2,3), 
                                          trueCoeffs=NULL, cellObj=NULL, 
                                          lambda.formula=NULL) {

  # Runs part of the scenario stats function in parallel to make it faster, hopefully.
  # No error checking as assume function that calls this has done error checking.
  #
  # Arguments ...
  # species:        the name of the species to run here.
  # resLstScenario: the results from the required scenario (i.e. all the coefficient estimates)
  # whichSDMs:      only work out statistics for these SDMs.
  # whichStats:     a vector giving the stats that are to be performed (by an index number)
  #                     1 = difference between true and estimated coefficients.
  #                     2 = correlation of true to estimated lambda value at each cell
  #                     3 = difference between true and estimated total abundance.
  # trueCoeffs:     a matrix of the true coefficients (alpha and betas on rows, species on columns).
  #                 Only needed if whichStats includes 1.
  # cellObj:        the cell object that corresponds to the required scenario.  Only needed
  #                 if whichStats includes 2 and/or 3.  Should contain trueLambda values,
  #                 and covariate values, for the cells in the domain.
  # lambda.formula: formula from which lambda values are created (as in glm formula).
  #                 Only needed if whichStats includes 2 and/or 3.
  
  # Names and numbers of things.  
  numRuns <- resLstScenario$numRuns
  numCoeffs <- resLstScenario$numCoeffs
  namesValidSDMs <- resLstScenario$namesValidSDM
  validSDMs <- resLstScenario$validSDM        # Assumes all scenarios use the same SDMs!
#  numValidSDMs <- length(validSDMs)           # Assumes namesValidSDMs and validSDMs are same length!
  numStats <- length(whichStats)
  areaCell <- cellObj$areaCell
  validStats <- c(1,2,3)
  numSDMs <- length(whichSDMs)
  
  # Figure out the validSDMs needed from the whichSDMs requested. 
  indWhichSDMs <- match(whichSDMs, namesValidSDMs)
  whichValidSDMs <- validSDMs[indWhichSDMs] 
  
  # Initialise return value (just uses 1st index in 3rd dimension if stat != 1)
  namesCoeffs <- dimnames(resLstScenario[whichValidSDMs[1]]$coeffs)[[1]]
  tmp <- array(dim=c(numStats,numSDMs,numCoeffs), dimnames=list(whichStats, whichSDMs, namesCoeffs))
  stats <- list(avg=tmp, sd=tmp, species=species, numErrors=0, errorMsgs=NULL, 
                numWarnings=0, warningMsgs=NULL)

  # True values.
  if ( 1 %in% whichStats ) speciesTrueCoeffs <- trueCoeffs[ ,species]
  if ( 2 %in% whichStats || 3 %in% whichStats ) speciesTrueLambda <- cellObj$trueLambda[ ,species]

  for ( isdm in 1:numSDMs ) {
    # Name of the SDM in the results list.
    nameSDM <- whichValidSDMs[isdm]

    # Calculate the statistics for each run.  
    statsRuns <- array(dim=c(numRuns,numStats,numCoeffs), 
                       dimnames=list(1:numRuns,whichStats,namesCoeffs))

    # Calculate stats
    for ( i in 1:numRuns ) {
      # Get the estimated coefficients for this run.
      speciesEstCoeffs <- resLstScenario[[nameSDM]]$coeffs[ ,species,i] 

      # Are these valid coefficients?  
      if ( all(! is.na(speciesEstCoeffs)) ) {
        # Get the estimated intensity for this run, if necessary.
        if ( 2 %in% whichStats || 3 %in% whichStats )
          speciesEstLambda <- lambda.cell(lambda.formula, speciesEstCoeffs, cellObj$covars)
          

        # Calculate the statistics for this run.
        for ( istat in 1:numStats ) {
          thisStat <- whichStats[istat]
          if ( thisStat == 1 ) {
            # Difference between true and estimated coefficients.
            statsRuns[i,istat, ] <- (speciesTrueCoeffs - speciesEstCoeffs)/speciesTrueCoeffs
            
          } else if ( thisStat == 2 ) {
            # Correlation between true and this run's estimated lambda.
            statsRuns[i,istat,1] <- cor(speciesTrueLambda, speciesEstLambda)

          } else if ( thisStat == 3 ) {
            # Difference in overall expected abundance between the true and estimated solution.
            # (Scale by true abundance to get percentage of difference per species)
            # NB: areaCell cancels out as it is in both the numerator and denominator!
            trueTotalAbundance <- sum(speciesTrueLambda)
            estTotalAbundance <- sum(speciesEstLambda)
            if ( is.infinite(estTotalAbundance) ) {
              msg <- paste0("  Infinite estimate for total abundance for run ", i, 
                               ", species ", species, ", and SDM ", nameSDM, ".")
              stats$numWarnings <- stats$numWarnings + 1
              stats$warningMsgs <- c(stats$warningMsgs, msg)
            } else {
              statsRuns[i,istat,1] <- (trueTotalAbundance - estTotalAbundance)/trueTotalAbundance
            }
          } else {
            stats$numErrors <- stats$numErrors + 1
            stats$errorMsgs <- c(stats$errorMsgs, 
                                 "Unrecognised statistic requested.  Check argument 'whichStats'.")
            return(stats)
          } 
        }
      } else {
        statsRuns[i, , ] <- rep(NA, numStats*numCoeffs)
      }
    }

    # Summarise across runs (i.e. what is the average performance, what is the variance of this).
    for ( istat in 1:numStats ) {
      thisStat <- whichStats[istat]
      if ( thisStat == 1 ) {
        num3Dim <- numCoeffs
      } else {
        num3Dim <- 1
      }
      for ( id3 in 1:num3Dim ) {  #statsRuns[i,stat,1]
        stats$avg[istat,isdm,id3] <- mean(statsRuns[ ,istat,id3], na.rm=TRUE)
        stats$sd[istat,isdm,id3] <- sd(statsRuns[ ,istat,id3], na.rm = TRUE)
        if ( is.infinite(stats$sd[istat,isdm,id3]) ) {
          msg <- paste0("  Infinite value for SD of statistic ", whichStats[istat], 
                        ", species ", species, ", and SDM ", nameSDM, ".")
          stats$numWarnings <- stats$numWarnings + 1
          stats$warningMsgs <- c(stats$warningMsgs, msg)
        }
        #print(id3)
      }
      #print(istat)
    }
    #print(isdm)
  }

  # Return value.
  #print("hello")
  return(stats)
  
}

#-----------------------------------------------------------------------------------------

saveScenariosSummaryStats <- function(stats, retLst, 
                                      namesStats=c("Coefficients","Correlation", "Percent Diff Total Abundance"),
                                      scenariosDir = retLst$scenariosObj$scenariosDir) {
  
  # Add the summary stats to the saved results.
  
  # Get the number of statistics
  numStats <- length(stats)
  if ( numStats != length(namesStats) ) stop("The 'namesStats' argument is incorrect length.")
  
  # Directory where the data was saved ...  
  savedDir <- paste0(scenariosDir,"/Data")
  if ( ! dir.exists(savedDir) ) stop("Unable to save data as scenarios directory doesn't exist.")
  
  # File where the data was saved ...
  savedFile <- makeFileName("DataDump", savedDir,  "RData")
  
  # Save objects.
  statsObj <- list(numStats=numStats, namesStats=namesStats, stats=stats)
  plotObj <- retLst$plotObj
  simObj <- retLst$simObj
  resLstAll <- retLst$resLstAll
  scenariosObj <- retLst$scenariosObj
  save(plotObj, simObj, resLstAll, scenariosObj, statsObj, file=savedFile)
  
  # Return value.
  retLst$statsObj <- statsObj
  return(retLst)
  
}

#-----------------------------------------------------------------------------------------

plotExperimentComparisons <- function(whichExperiments, whichStat=1, whichCoeffs=1, whichSpecies=NULL,
                                      plotSDMs=c("PA","MsPP","PO"), ylimVals=c(0,1,0,1), 
                                      vioPlots=TRUE, absMeans=TRUE, plotDevice="RStudioGD", 
                                      plotDir=getwd(), fileName="comparison", plotUnits="cm", 
                                      plotHeight=20, plotWidth=15.75, plotRes=600, plotTitle=NULL,
                                      xAxisLabels=NULL, xAxisTitle=NULL, xAxisLabelStyle=2,
                                      xAxisReverse=FALSE, 
                                      columnHeadings=paste("experiment",1:length(whichExperiments)),
                                      horizontalLines=c(0,0), diffPORange=NULL, medianLines=TRUE) {
  
  # TO DO: * truncated y-axis indicator (i.e. >= 1.0 same as stats comparison)  Up to here!

  # Plot a single statistic for a group of experiments (i.e. plot correlation for all bias 
  # change experiments). 
  #
  # Arguments ...
  # whichExperiments: a string vector containing directory names, one for each experiment.
  # whichStat:        an integer scalar that indicates the statistic within each experiment 
  #                   that we are interested in.  See 'makeScenarioStats' for valid values.
  # whichCoeffs:      an integer vector which indicates which of the coefficients should 
  #                   be included if whichStat = 1.
  # whichSpecies:     which species to plot.  A value of NULL will use all species as 
  #                   specified in the first experiment.
  # plotSDMs:         which SDMs from the experiment to plot (and the order).  
  # ylimVals:         a vector of length 4 that gives a y-axis range for mean and sd, eg. 
  #                   c(min mean, max mean, min sd, max sd).
  # xAxisTitle:       can be a single string (in which case it will be placed in the centre
  #                   of the outer plot margins) or a vector of strings, one for each experiment.
  # xAxisReverse:     reverse the positions of the scenarios along the x-axis for all experiments
  #                   ( = TRUE) or for specified experiments (e.g. = c(TRUE, FALSE, FALSE))
  # columnHeadings:   a vector of strings, one for each experiment, to use as column headings.
  #                   Set to "NULL" if no column headings are required.
  # horizontalLines:  a vector of length 2 that given the y values at which horizontal lines
  #                   will be drawn for each accuracy (mean) and each precision (sd) plot.
  # diffPORange:      a vector of length 2 that gives a different y-axis range for PO SDM 
  #                   mean plots.  (Set to NULL to use same y-axis range as other SDMs)
  # medianLines:      True to include a line to indicate the median value per scenario of
  #                   each plot, false to not include.
  
  # Get numbers of things to set up plotting layout.
  numExperiments <- length(whichExperiments)
  numPlotSDMs <- length(plotSDMs)
  substituteInfValue <- 1.0e+25
    
  # Colours for plotting.
  pointsCol <- "black"
  violinCol <- "grey"
  pointsColPO <- rgb(0, 114, 178, maxColorValue = 255)  # blue
  violinColPO <- rgb(86, 180, 233, maxColorValue = 255) # sky blue
  medianCol <- rgb(230, 159, 0, maxColorValue = 255)  # orange
  
  # Plot directory for experiment comparison level plots.
  if ( ! dir.exists(plotDir) ) {
    dir.create(plotDir, recursive = TRUE)
  }

  # Which device are we printing to?
  if ( plotDevice == "RStudioGD" ) {
    # plot to the R studio plot window.
    plotToFile <- FALSE
  } else {
    fileName <- makeFileName(fileName, plotDir, plotDevice)
    argList <- list(filename = fileName, width = plotWidth, height = plotHeight,
                    units = plotUnits, res = plotRes)
    do.call(plotDevice, argList)
    plotToFile <- TRUE
  }
  
  # Divide plotting into the number of rows and columns required.
  omaPar <- par("mar")         # use default single plot per page margins
  marPar <- c(0.0,0.5,1.3,0.0) # small margin on top to separate rows and allow for column headings.
  if ( is.null(xAxisTitle) ) {
    # No x-axis title, change margins to gain plotting area.
    omaPar[1] <- omaPar[1] - 1.0  # leave space for x-axis ticks and labels.  
  }
  if ( is.null(plotTitle) ) {
    # No main title, change margins to gain plotting area.
    omaPar[3] <- 0.0  # NB: there is already a bit at top due to mar settings.
  }
  opar <- par(mfcol=c(numPlotSDMs*2,numExperiments), oma = omaPar, mar = marPar, cex=0.66)
  
  if ( length(xAxisReverse) == 1 ) {
    xAxisReverseUse <- rep(xAxisReverse, numExperiments)
  } else if ( length(xAxisReverse) == numExperiments ) {
    xAxisReverseUse <- xAxisReverse
  } else {
    stop("Argument 'xAxisReverse' is an unexpected length.")
  }
  
  # Load experiments (FYI: necessary to load and store so that violin width can be calculated)
  allExperimentsInfo <- vector(mode="list", length = numExperiments)
  maxNumScenarios <- 0
  for ( ex in 1:numExperiments ) {
    # Load in this experiment's results
    exDir <- whichExperiments[ex]
    retLst <- loadScenariosResults(exDir)
    
    # Check requested SDMs are contained within this experiment.
    if ( ! all(plotSDMs %in% retLst$resLstAll[[1]]$namesValidSDM) ) {
      stop("Unrecognised SDM requested from experiment '", exDir, "'.")
    }
    
    # Check requested species are contained within this experiment.
    exSpecies <- retLst$simObj$namesSpecies
    if ( is.null(whichSpecies) ) {
      # Use the species in the first experiment, other experiments will be checked against these.
      whichSpecies <- exSpecies
    } else if ( ! all(whichSpecies %in% exSpecies) ) {
      stop("Unrecognised species requested from experiment '", exDir, "'.")
    }

    # Store this experiment's information.
    allExperimentsInfo[[ex]] <- list(numScenarios = retLst$scenariosObj$numScenarios,
                                     namesScenarios = retLst$scenariosObj$prettyNamesScenarios,
                                     exDir = exDir, whichStat = whichStat, whichSpecies = whichSpecies,
                                     thisStatAvg=retLst$statsObj$stats[[whichStat]]$avg[ ,whichSpecies, , ,drop=FALSE], 
                                     thisStatSD=retLst$statsObj$stats[[whichStat]]$sd[ ,whichSpecies, , ,drop=FALSE])
    if ( absMeans ) allExperimentsInfo[[ex]]$thisStatAvg <- abs(allExperimentsInfo[[ex]]$thisStatAvg)
    if ( xAxisReverseUse[ex] ) {
      # Reverse the scenarios on the x-axis for this experiment.
      allExperimentsInfo[[ex]]$namesScenarios <- rev(allExperimentsInfo[[ex]]$namesScenarios)
      numScenarios <- allExperimentsInfo[[ex]]$numScenarios
      allExperimentsInfo[[ex]]$thisStatAvg <- allExperimentsInfo[[ex]]$thisStatAvg[numScenarios:1, , , ,drop=FALSE]
      allExperimentsInfo[[ex]]$thisStatSD <- allExperimentsInfo[[ex]]$thisStatSD[numScenarios:1, , , ,drop=FALSE]
    }
    
    # What is the maximum number of scenarios, so far?
    maxNumScenarios <- max(maxNumScenarios, allExperimentsInfo[[ex]]$numScenarios)
    
  }  # experiments (or plot columns).

  # Calculate widths for violin plots for means and SDs, if necessary.
  if ( vioPlots ) {
    # Get the maximum density for each SDM x scenario combination (i.e. each violin).
    typeNames <- c("avg","sd")
    widthsViolins <- array(dim=c(numExperiments, numPlotSDMs, maxNumScenarios, length(typeNames)), 
                           dimnames=list(1:numExperiments, plotSDMs, 1:maxNumScenarios, typeNames))
    for ( type in typeNames ) { 
      for ( ex in 1:numExperiments ) {
        # Number of scenarios in this experiment (could be different?)
        namesScenarios <- allExperimentsInfo[[ex]]$namesScenarios
        numScenarios <- length(namesScenarios)
        
        # Which type of values "avg" or "sd"        
        if ( type =="avg" ) {
          theseValues <- allExperimentsInfo[[ex]]$thisStatAvg
        } else if ( type == "sd" ) {
          theseValues <- allExperimentsInfo[[ex]]$thisStatSD
        } else {
          stop("Unrecognised type of statistic.  Look at code!")
        }
        
        for ( sdm in plotSDMs ) {
          for ( sc in 1:numScenarios ) {
            # For this experiment, sdm and scenario, the species statistics are:
            thisStat <- as.vector(theseValues[sc, ,sdm,whichCoeffs])
            
            # Check for Inf values
            indInf <- which(is.infinite(thisStat))
            if ( length(indInf) > 0 ) {
              thisStat[indInf] <- substituteInfValue
            }
            
            # Check for NA, NaN, or Inf values
            indNA <- which(is.na(thisStat) || is.nan(thisStat))
            if ( length(indNA) > 0 ) {
              warning(paste0(length(indNA), " NA or NaN values in ", type,
                             " for experiment = ", ex,
                             " for SDM = ", sdm, 
                             " and scenario = ", namesScenarios[sc]))
              thisStat <- thisStat[-indNA]
            }
            
            # What is the maximum of the density for each scenario x sdm.
            if ( length(thisStat) < 2 ) {
              message(paste0("Not enough values to form a density in stat '", type,
                             "', experiment = ", ex,
                             ", SDM = ", sdm, 
                             " and scenario = ", namesScenarios[sc]))
            }
            widthsViolins[ex,sdm,sc,type] <- max(density(thisStat, na.rm=TRUE)$y)
          }
        }
      }
      
      # Scale maximum densities so that violin widths are between [0,1]
      maxMaxDensity <- max(widthsViolins[ , , ,type], na.rm=TRUE)
      widthsViolins[ , , ,type] <- widthsViolins[ , , ,type]/maxMaxDensity
    }    
  }
  
  # Cycle through information and add a mean and SD plot per SDM x experiment combination.
  for ( ex in 1:numExperiments ) {
    #if ( ex == 2) message("experiment: ", ex)
    # Number and names of scenarios for this experiment.
    numScenarios <- allExperimentsInfo[[ex]]$numScenarios
    namesScenarios <- allExperimentsInfo[[ex]]$namesScenarios
    
    # Were x-axis labels provided (i.e. pretty names for scenarios)?
    if ( is.null(xAxisLabels) ) {
      xAxisLabelsUse <- namesScenarios
    } else if ( length(xAxisLabels) == numScenarios && 
                length(unique(xAxisLabels)) == numScenarios ) {
      # use provided labels.
      xAxisLabelsUse <- xAxisLabels
    } else {
      stop("Invalid x-axis labels given in argument 'xAxisLabels'.")
    }
    
    # Loop to plot accuracy and precision of each SDM for this experiment.    
    isFirstRow <- TRUE
    for ( sdm in plotSDMs) {
      #if ( ex == 2 ) message("    SDM: ", sdm)
      # Plot accuracy statistics (a.k.a mean) for this SDM and this experiment.
      # Range of plot
      if ( sdm == "PO" && ! is.null(diffPORange) ) {
        yaxisRange <- diffPORange
      } else {
        yaxisRange <- ylimVals[1:2]
      }
      plot(c(0.5,numScenarios+0.5), yaxisRange, type="n", xaxs="i", xaxt="n", xlab="", 
           yaxt="n", ylab="")
      axis(1, tick=TRUE, labels=FALSE)
      abline(h=horizontalLines[1], lty="dotted")
      
      # First of row stuff ...
      if ( ex == 1) {
        # Y-axis tick marks, numbers and label.
        if ( all(yaxisRange == c(0.0,1.0))) {
          # Specifically for publication, really!
          yaxisTicks <- axTicks(2, axp=c(yaxisRange[1],yaxisRange[2],4))
          yaxisLabels <- vector(mode = "character", length = 5)
          yaxisLabels[c(1,3,5)] <- format(yaxisTicks[c(1,3,5)])
          yaxisLabels[c(2,4)] <- ""
          yaxisLabels[5] <- paste0("\u2265",yaxisLabels[5])
          axis(2, labels=yaxisLabels, at=yaxisTicks, las=1)
        } else {
          axis(2, labels=TRUE, las=1)
        }
        midyRange <- ((yaxisRange[2] - yaxisRange[1])/2.0) + yaxisRange[1]
        axis(2, at=midyRange, labels="accuracy", tick=FALSE, padj=-3.0)
      }
      
      # Column headings ...
      if ( isFirstRow  ) {
        isFirstRow <- FALSE
        if  ( ! is.null(columnHeadings) ) {
          if ( length(columnHeadings) == numExperiments )  {
            midxRange <- (numScenarios/2.0) + 0.5
            axis(3, at=midxRange, labels=columnHeadings[ex], tick=FALSE, padj=1.0)
          } else {
            stop("The number of columnHeadings needs to be the same as the number of experiments.")
          }
        }
      } else {
        # Do nothing, no column headings needed when not first row of plot matrix.
      }
      
      # Plot accuracy of species for each scenario.
      plotMedians <- vector(mode="double", length=numScenarios)
      for (sc in 1:numScenarios ) {
        # Statistic values for this scenario (will be plotted as a vertical bar).
        thisStat <- as.vector(allExperimentsInfo[[ex]]$thisStatAvg[sc, ,sdm,whichCoeffs])
        
        # Check for NA values.
        indNA <- c(which(is.na(thisStat)), which(is.nan(thisStat)))
        if ( length(indNA) > 0 ) {
          warning(paste0(length(indNA), " NA or NaN values in ", type,
                         " for experiment = ", ex,
                         " for SDM = ", sdm, 
                         " and scenario = ", namesScenarios[sc]))
          thisStat <- thisStat[-indNA]
        }
        
        # Plot ...
        if ( sdm == "PO" && ! is.null(diffPORange)) {
          violinColour <- violinColPO
          pointsColour <- pointsColPO
        } else {
          violinColour <- violinCol
          pointsColour <- pointsCol
        }
        if ( vioPlots ) {
          # Reset infinite values.
          indInf <- which(is.infinite(thisStat))
          if ( length(indInf) > 0 ) thisStat[indInf] <- substituteInfValue
          vioplot(thisStat, col=violinColour, border=violinColour, add=TRUE,
                  drawRect = FALSE, at=sc, wex=widthsViolins[ex,sdm,sc,"avg"])
        }
        stripchart(thisStat, at=sc, vertical=TRUE, pch="_", add=TRUE, col=pointsColour)
        
        # Median value for this scenario.
        plotMedians[sc] <- median(thisStat, na.rm=TRUE)
      }    
      
      # Add median value for all scenarios in this plot.
      if ( medianLines ) {
        #points(1:numScenarios, plotMedians, pch="_", col=medianCol)
        lines(1:numScenarios, plotMedians, lty="dashed", col=medianCol)
      }
      
      # Plot precision statistics (a.k.a sd) for this SDM and this experiment.
      # Range of plot
      yaxisRange <- ylimVals[3:4]
      plot(c(0.5,numScenarios+0.5), yaxisRange, type="n", xaxs="i", xaxt="n", xlab="", 
           yaxt="n", ylab="")
      axis(1, tick=TRUE, labels=FALSE)
      abline(h=horizontalLines[2], lty="dotted")
      
      # First of row stuff ...
      if ( ex == 1) {
        # Y-axis tick marks, numbers and label.
        if ( all(yaxisRange == c(0.0,1.0))) {
          # Specifically for publication, really!
          yaxisTicks <- axTicks(2, axp=c(yaxisRange[1],yaxisRange[2],4))
          yaxisLabels <- vector(mode = "character", length = 5)
          yaxisLabels[c(1,3,5)] <- format(yaxisTicks[c(1,3,5)])
          yaxisLabels[c(2,4)] <- ""
          yaxisLabels[5] <- paste0("\u2265",yaxisLabels[5])
          axis(2, labels=yaxisLabels, at=yaxisTicks, las=1)
        } else {
          axis(2, labels=TRUE, las=1)
        }
        midyRange <- ((yaxisRange[2] - yaxisRange[1])/2.0) + yaxisRange[1]
        axis(2, at=midyRange, labels="precision", tick=FALSE, padj=-3.0)

        # SDM title.
        axis(2, at=yaxisRange[2], labels=paste0("       ", sdm, " SDM"), tick=FALSE, padj=-4.5)
      }
      
      # Plot precision of species for each scenario.  
      for (sc in 1:numScenarios ) {
        # Statistic values for this scenario (will be plotted as a vertical bar).
        thisStat <- as.vector(allExperimentsInfo[[ex]]$thisStatSD[sc, ,sdm,whichCoeffs])

        # Check for NA values.
        indNA <- c(which(is.na(thisStat)), which(is.nan(thisStat)))
        if ( length(indNA) > 0 ) {
          warning(paste0(length(indNA), " NA or NaN values in standard deviation of ",
                         " stat = ", whichStat,
                         " for experiment = ", ex,
                         " for SDM = ", sdm, 
                         " and scenario = ", namesScenarios[sc]))
          thisStat <- thisStat[-indNA]
        }
        
        # Plot ...
        if ( vioPlots ) {
          # Reset infinite values.
          indInf <- which(is.infinite(thisStat))
          if ( length(indInf) > 0 ) thisStat[indInf] <- substituteInfValue
          vioplot(thisStat, col=violinCol, border=violinCol, add=TRUE,
                              drawRect = FALSE, at=sc, wex=widthsViolins[ex,sdm,sc,"sd"])
        }
        stripchart(thisStat, at=sc, vertical=TRUE, pch="_", add=TRUE, col=pointsCol)
        
        # Median value for this scenario.
        plotMedians[sc] <- median(thisStat, na.rm=TRUE)
      }
      
      # Add median value for all scenarios in this plot.
      if ( medianLines ) {
        #points(1:numScenarios, plotMedians, pch="_", col=medianCol)
        lines(1:numScenarios, plotMedians, lty="dashed", col=medianCol)
      }
    }  # sdm's (or plot rows).
    
    # Add x-axis for this column only on bottom row.
    axis(1, 1:numScenarios, xAxisLabelsUse, las=xAxisLabelStyle)
    if ( ! is.null(xAxisTitle) ) {
      if ( length(xAxisTitle) == numExperiments ) {
        midxRange <- (numScenarios/2.0) + 0.5
        axis(1, at=midxRange, labels=xAxisTitle[ex], tick=FALSE, padj=4.0)
      } else if ( length(xAxisTitle) == 1 ) {
        # Will add below.
      } else {
        # Will warn below.
      }
    }
    
  } # experiments (or plot columns).
  
  # Add a title to the plot.
  if ( ! is.null(plotTitle) ) 
    title(plotTitle, outer=TRUE)
  
  # Add an x-axis title to the plot.
  if ( ! is.null(xAxisTitle) ) {
    if ( length(xAxisTitle) == numExperiments ) {
      # Should have already added them above!      
    } else if ( length(xAxisTitle) == 1 ) {
      title(xlab = xAxisTitle, outer=TRUE, line=min(4,omaPar[1]))
    } else {
      warning("Argument 'xAxisTitle' is an unexpected length.  No titles added.")
    }
  }
  
  # Turn off the plotting device, if it is a file.
  if ( plotToFile ) dev.off()
  
  # Return plotting to original settings.
  par(opar)
}  

#-----------------------------------------------------------------------------------------

plotStatisticsComparisons <- function(whichExperiment, whichStats=c(1,1,2,3), 
                                      whichCoeffs=list(1,-1,NULL,NULL), whichSpecies=NULL,
                                      plotSDMs=c("PA","MsPP","PO"), ylimVals=c(0,1,0,1), 
                                      vioPlots=TRUE, absMeans=TRUE, plotDevice="RStudioGD", 
                                      plotDir=paste0(whichExperiment,"/Plots"), 
                                      fileName="comparison", plotTitle=NULL, plotUnits="cm", 
                                      plotHeight=20, plotWidth=15.75, plotRes=600, 
                                      xAxisLabels=NULL, xAxisTitle=NULL, xAxisLabelStyle=2,
                                      xAxisReverse=FALSE, 
                                      columnHeadings=paste("statistic",1:length(whichStats)),
                                      diffPOIntRange=NULL, medianLines=TRUE,
                                      doPlotIdentifiers=FALSE) {
  
  # Plot a multiple statistics for a single experiment (i.e. plot coeff difference, correlation, 
  # etc., for an experiment).  
  #
  # Arguments ...
  # whichExperiment:  a string vector containing a directory name for an experiment.
  # whichStats:       an integer vector that indicates the statistics to plot.  See 
  #                   'makeScenarioStats' for valid values.
  # whichCoeffs:      a list that has the same number of items as whichStats.  Each item
  #                   contains the coefficients to be plotted for its matching statistic. 
  #                   Use NULL per item to get all coeffs for that item.  Use negative 
  #                   values to remove the specified coefficient.
  # whichSpecies:     which species to plot.
  # plotSDMs:         which SDMs from the experiment to plot (and the order).  
  # ylimVals:         a vector of length 4 that gives a y-axis range for mean and sd, eg. 
  #                   c(min mean, max mean, min sd, max sd).
  # xAxisTitle:       can be a single string (in which case it will be placed in the centre
  #                   of the outer plot margins) or a vector of strings, one for each statistic.
  # xAxisReverse:     reverse the positions of the scenarios along the x-axis for all statistics.
  # columnHeadings:   a vector of strings, one for each statistic, to use as column headings.
  #                   Set to "NULL" if no column headings are required.
  # horizontalLines:  REPLACED BY HARDWIRED numbers based on statistics requested. = 0 unless
  #                   whichstats[i] == 2, then equal to 1 for accuracy only!
  #                   a numStats x 2 matrix that give the y values at which horizontal lines
  #                   will be drawn for each accuracy (mean) and each precision (sd) plot.
  # diffPOIntRange:   a vector of length 2 that gives a different y-axis range for PO SDM 
  #                   accuracy plots for intercept statistics (e.g. c(min mean, max mean)). 
  #                   Set to NULL to use same y-axis range as other SDMs
  # medianLines:      True to include a line to indicate the median value per scenario of
  #                   each plot, false to not include.
  # doPlotIdentifiers:If TRUE, add plot identifier letters (e.g. "(a)") to each sub-plot.
  
  # Get numbers of things to set up plotting layout.
  numPlotStats <- length(whichStats)
  numPlotSDMs <- length(plotSDMs)
  substituteInfValue <- 1.0e+25
  
  # Colours for plotting.
  pointsCol <- "black"
  violinCol <- "grey"
  pointsColPO <- rgb(0, 114, 178, maxColorValue = 255)  # blue
  violinColPO <- rgb(86, 180, 233, maxColorValue = 255) # sky blue
  medianCol <- rgb(230, 159, 0, maxColorValue = 255)  # orange

  # Check dimensions are valid.  
  if ( length(xAxisReverse) == 1 ) {
    xAxisReverseUse <- xAxisReverse
  } else {
    stop("Argument 'xAxisReverse' should be a scalar.  Please check using correct function!")
  }
  
  # Check dimensions are valid.  
  if ( length(whichExperiment) > 1 ) {
    stop("Argument 'whichExperiment' should be a scalar. Please check using correct function!")
  }
  
  # Check dimensions are valid.  
  if ( length(whichCoeffs) != numPlotStats || class(whichCoeffs) != "list") {
    stop("Argument 'whichCoeffs' should be a list of the same length as 'whichStats'.")
  }
  
  # Load in this experiment's results
  exDir <- whichExperiment
  retLst <- loadScenariosResults(exDir)
  
  # Check requested statistics are contained within this experiment.
  if ( ! all(whichStats %in% c(1:retLst$statsObj$numStats)) ) {
    stop("Unrecognised statistic requested from experiment '", exDir, "'.")
  }
  
  # Check requested coefficients, per requested statistic, are contained within this experiment.
  namesCoeffs <- dimnames(retLst$statsObj$stats[[1]]$avg)[[4]]
  numCoeffs <- length(namesCoeffs)
  doDiffPOYAxis <- FALSE
  for ( stat in 1:numPlotStats ) {
    # What are the requested coefficients for this statistic?
    theseCoeffs <- whichCoeffs[[stat]]

    # What are all the possible coefficients for this statistics.
    whichStat <- whichStats[stat]
    allCoeffs <- 1:dim(retLst$statsObj$stats[[whichStat]]$avg)[[4]]
    
    # Are they valid?
    if ( is.null(theseCoeffs) ) {
      whichCoeffs[[stat]] <- allCoeffs
    } else if ( ! all( abs(theseCoeffs) %in% allCoeffs) ) {
      # NB: absoulte value as negatives can be used to remove coefficients too.
      stop("Unrecognised coefficient requested for statistic at item ",stat," in 'whichCoeffs'.")
    }
    
    # Do a different axis for PO intercept accuracy?
    if ( ! is.null(diffPOIntRange) && whichStat == 1 && 1 %in% whichCoeffs[[stat]] ) {
      doDiffPOYAxis <- TRUE
    }
  }
  
  # Check requested SDMs are contained within this experiment.
  if ( ! all(plotSDMs %in% retLst$resLstAll[[1]]$namesValidSDM) ) {
    stop("Unrecognised SDM requested from experiment '", exDir, "'.")
  }
  
  # Check requested species are contained within this experiment.
  if ( is.null(whichSpecies) ) {
    # all good, going to take whatever species are within the experiment.
    whichSpecies <- dimnames(retLst$statsObj$stats[[1]]$avg)[[2]]
  } else if ( ! all(whichSpecies %in% dimnames(retLst$statsObj$stats[[1]]$avg)[[2]]) ) {
    stop("Unrecognised species requested from experiment '", exDir, "'.")
  }

  # Do we need absolute values for the accuracy?  
  if ( absMeans ) {
    for ( stat in 1:retLst$statsObj$numStats ) {
      retLst$statsObj$stats[[stat]]$avg <- abs(retLst$statsObj$stats[[stat]]$avg)
    }
  }
  
  # Do we need to reverse the scenario direction?
  numScenarios = retLst$scenariosObj$numScenarios
  namesScenarios = retLst$scenariosObj$prettyNamesScenarios
  if ( xAxisReverseUse ) {
    # Reverse the scenarios on the x-axis for this experiment.
    namesScenarios <- rev(namesScenarios)
    for ( stat in 1:retLst$statsObj$numStats ) {
      retLst$statsObj$stats[[stat]]$avg <- retLst$statsObj$stats[[stat]]$avg[numScenarios:1, , , ,drop=FALSE]
      retLst$statsObj$stats[[stat]]$sd <- retLst$statsObj$stats[[stat]]$sd[numScenarios:1, , , ,drop=FALSE]
    }
  }
  
  # Plot directory for statistics comparison level plots.
  if ( ! dir.exists(plotDir) ) {
    dir.create(plotDir, recursive = TRUE)
  }
  
  # Which device are we printing to?
  if ( plotDevice == "RStudioGD" ) {
    # plot to the R studio plot window.
    plotToFile <- FALSE
  } else {
    fileName <- makeFileName(fileName, plotDir, plotDevice)
    argList <- list(filename = fileName, width = plotWidth, height = plotHeight,
                    units = plotUnits, res = plotRes)
    do.call(plotDevice, argList)
    plotToFile <- TRUE
  }
  
  # Divide plotting into the number of rows and columns required.
  omaPar <- par("mar")         # use default single plot per page margins
  marPar <- c(0.0,0.5,1.3,0.0) # small margin on top to separate rows and allow for column headings.
  if ( is.null(xAxisTitle) ) {
    # No x-axis title, change margins to gain plotting area.
    omaPar[1] <- omaPar[1] - 1.0  # leave space for x-axis ticks and labels.  
  }
  if ( is.null(plotTitle) ) {
    # No main title, change margins to gain plotting area.
    omaPar[3] <- 0.0  # NB: there is already a bit at top due to mar settings.
  }
  if ( doDiffPOYAxis && "PO" %in% plotSDMs && ylimVals[1:2] == c(0.0,1.0)) {
    #  Probably only for publication version where using >= sign in axis tick mark values.
    omaPar[4] <- 3.1
  } else {
    omaPar[4] <- omaPar[4]+0.5
  }
  opar <- par(mfcol=c(numPlotSDMs*2,numPlotStats), oma = omaPar, mar = marPar, cex=0.66)
  plotIdentifier <- paste0("(",letters[1:(numPlotSDMs*2*numPlotStats)],")")

  # Calculate widths for violin plots for avg (accuracy) and SDs (precision), if necessary.
  if ( vioPlots ) {
    # Get the maximum density for each SDM x scenario combination (i.e. each violin).
    typeNames <- c("avg","sd")
    widthsViolins <- array(dim=c(numPlotStats, numPlotSDMs, numScenarios, length(typeNames)), 
                           dimnames=list(1:numPlotStats, plotSDMs, 1:numScenarios, typeNames))
    for ( type in typeNames ) { 
      for ( stat in 1:numPlotStats ) {
        # What is the requested statistic for this position in whichStats?
        whichStat <- whichStats[stat]
        whichCoeffsForStat <- whichCoeffs[[stat]]

        # Which type of values "avg" or "sd"        
        if ( type =="avg" ) {
          theseValues <- retLst$statsObj$stats[[whichStat]]$avg[ ,whichSpecies, ,whichCoeffsForStat,drop=FALSE]
          
        } else if ( type == "sd" ) {
          theseValues <- retLst$statsObj$stats[[whichStat]]$sd[ ,whichSpecies, ,whichCoeffsForStat,drop=FALSE]
        } else {
          stop("Unrecognised type of statistic.  Code may require update!")
        }
        
        for ( sdm in plotSDMs ) {
          for ( sc in 1:numScenarios ) {
            # For this experiment, sdm and scenario, the species statistics are:
            thisStat <- as.vector(theseValues[sc, ,sdm, ])
            
            # Check for Inf values
            indInf <- which(is.infinite(thisStat))
            if ( length(indInf) > 0 ) {
              thisStat[indInf] <- substituteInfValue
            }
            
            # Check for NA, NaN, or Inf values
            indNA <- which(is.na(thisStat) || is.nan(thisStat))
            if ( length(indNA) > 0 ) {
              warning(paste0(length(indNA), " NA or NaN values in ", type,
                             " for statistic = ", whichStat,
                             " for SDM = ", sdm, 
                             " and scenario = ", namesScenarios[sc]))
              thisStat <- thisStat[-indNA]
            }
            
            # What is the maximum of the density for each scenario x sdm.
            if ( length(thisStat) < 2) {
              message(paste0("Less than two values left in statistic to form density in ", type,
                             " for statistic = ", whichStat,
                             " for SDM = ", sdm, 
                             " and scenario = ", namesScenarios[sc]))
            }
            widthsViolins[stat,sdm,sc,type] <- max(density(thisStat, na.rm=TRUE)$y)
          }
        }
        
        # Scale maximum densities so that violin widths are between [0,1]
        maxMaxDensity <- max(widthsViolins[stat , , ,type], na.rm=TRUE)
        widthsViolins[stat , , ,type] <- widthsViolins[stat , , ,type]/maxMaxDensity
      }
    }    
  }
  
  # Were x-axis labels provided (i.e. pretty names for scenarios)?
  if ( is.null(xAxisLabels) ) {
    xAxisLabelsUse <- namesScenarios
  } else if ( length(xAxisLabels) == numScenarios && 
              length(unique(xAxisLabels)) == numScenarios ) {
    # use provided labels.
    xAxisLabelsUse <- xAxisLabels
  } else {
    stop("Invalid x-axis labels given in argument 'xAxisLabels'.")
  }
  
  # Cycle through information and add a mean and SD plot per SDM x statistic combination.
  doForthAxis <- FALSE
  for ( stat in 1:numPlotStats ) {
    whichStat <- whichStats[stat]
    whichCoeffsForStat <- whichCoeffs[[stat]]
    
    if ( whichStat == 2 ) {
      # Assumes that this is the correlation statistic!
      horizontalLine <- 1.0
    } else {
      horizontalLine <- 0.0
    }
    
    # Loop to plot accuracy and precision of each SDM for this statistic.    
    isFirstRow <- TRUE
    for ( isdm in 1:numPlotSDMs) {
      # Plot accuracy statistics (a.k.a 'avg') for this SDM and this statistic.
      # Range of plot
      sdm <- plotSDMs[isdm]
      if ( sdm == "PO" && ! is.null(diffPOIntRange) && whichStat == 1 && 1 %in% whichCoeffsForStat) {
        yaxisRange <- diffPOIntRange
      } else {
        yaxisRange <- ylimVals[1:2]
      }
      plot(c(0.5,numScenarios+0.5), yaxisRange, type="n", xaxs="i", xaxt="n", xlab="", 
           yaxt="n", ylab="")
      axis(1, tick=TRUE, labels=FALSE)
      abline(h=horizontalLine, lty="dotted")
      
      # First plot of row stuff ...
      if ( stat == 1) {
        # Y-axis tick marks, numbers and label.
        if ( sdm == "PO" && doDiffPOYAxis ) {
          axis(2, labels=TRUE, col=pointsColPO, col.axis=pointsColPO, las=1)
          doForthAxis <-TRUE
        } else {
          if ( all(yaxisRange == c(0.0,1.0))) {
            # Specifically only for publication, really!
            yaxisTicks <- axTicks(2, axp=c(yaxisRange[1],yaxisRange[2],4))
            yaxisLabels <- vector(mode = "character", length = 5)
            yaxisLabels[c(1,3,5)] <- format(yaxisTicks[c(1,3,5)])
            yaxisLabels[c(2,4)] <- ""
            yaxisLabels[5] <- paste0("\u2265",yaxisLabels[5])   
            axis(2, labels=yaxisLabels, at=yaxisTicks, las=1)
          } else {
            axis(2, labels=TRUE, las=1)
          }
        }
        midyRange <- ((yaxisRange[2] - yaxisRange[1])/2.0) + yaxisRange[1]
        axis(2, at=midyRange, labels="accuracy", tick=FALSE, padj=-3.0)
      }
      
      # Last plot of row, if necessary.
      if ( stat == numPlotStats && doForthAxis && sdm == "PO") {
        if ( all(yaxisRange == c(0.0,1.0))) {
          # Specifically only for publication, really!
          yaxisTicks <- axTicks(2, axp=c(yaxisRange[1],yaxisRange[2],4))
          yaxisLabels <- vector(mode = "character", length = 5)
          yaxisLabels[c(1,3,5)] <- format(yaxisTicks[c(1,3,5)])
          yaxisLabels[c(2,4)] <- ""
          yaxisLabels[5] <- paste0("\u2265",yaxisLabels[5])   
          axis(4, labels=yaxisLabels, at=yaxisTicks, las=1)
        } else {
          axis(4, labels=TRUE, las=1)
        }
        doForthAxis <- FALSE
      }
      
      # Column headings ...
      if ( isFirstRow  ) {
        isFirstRow <- FALSE
        if  ( ! is.null(columnHeadings) ) {
          if ( length(columnHeadings) == numPlotStats )  {
            midxRange <- (numScenarios/2.0) + 0.5
            axis(3, at=midxRange, labels=columnHeadings[stat], tick=FALSE, padj=1.0)
          } else {
            stop("The length of 'columnHeadings' needs to be the same as the number of requested statistics.")
          }
        }
      } else {
        # Do nothing, no column headings needed when not first row of plot matrix.
      }
      
      # Plot accuracy of species for each scenario.
      plotMedians <- vector(mode="double", length=numScenarios)
      for (sc in 1:numScenarios ) {
        # Statistic values for this scenario (will be plotted as a vertical bar).
        thisStat <- as.vector(retLst$statsObj$stats[[whichStat]]$avg[sc,whichSpecies,sdm,whichCoeffsForStat])

        # Check for NA values.
        indNA <- c(which(is.na(thisStat)), which(is.nan(thisStat)))
        if ( length(indNA) > 0 ) {
          warning(paste0(length(indNA), " NA or NaN values in ", type,
                         " for statistic = ", whichStat,
                         " for SDM = ", sdm, 
                         " and scenario = ", namesScenarios[sc]))
          thisStat <- thisStat[-indNA]
        }
        
        # Plot ...
        if ( sdm == "PO" && ! is.null(diffPOIntRange) && whichStat == 1 && 1 %in% whichCoeffsForStat) {
          violinColour <- violinColPO
          pointsColour <- pointsColPO
        } else {
          violinColour <- violinCol
          pointsColour <- pointsCol
        }
        if ( vioPlots ) {
          # Reset infinite values.
          indInf <- which(is.infinite(thisStat))
          if ( length(indInf) > 0 ) thisStat[indInf] <- substituteInfValue
          vioplot(thisStat, col=violinColour, border=violinColour, add=TRUE,
                  drawRect = FALSE, at=sc, wex=widthsViolins[stat,sdm,sc,"avg"])
        }
        stripchart(thisStat, at=sc, vertical=TRUE, pch="_", add=TRUE, col=pointsColour)
        
        # Median value for this scenario.
        plotMedians[sc] <- median(thisStat, na.rm=TRUE)
      }    
      
      # Add median value for all scenarios in this plot.
      if ( medianLines ) {
        #points(1:numScenarios, plotMedians, pch="_", col=medianCol)
        lines(1:numScenarios, plotMedians, lty="dashed", col=medianCol)
      }
      
      # Add plot identifier, if requested.
      if ( doPlotIdentifiers ) {
        if ( whichStat == 2 ) {
          # Correlation statistic, place identifier at bottom of plot, not top.
          identHere <- yaxisRange[1]
          adjy <- -0.5
        } else {
          identHere <- yaxisRange[2]
          adjy <- 1.5
        }
        whichIdent <- 1 + (isdm-1)*numPlotSDMs  + (stat-1)*numPlotSDMs*2 
        text(x=1, y=identHere, label=plotIdentifier[whichIdent], adj=c(0,adjy), cex=0.8)
      }
      
      # Plot precision statistics (a.k.a sd) for this SDM and this statistic.
      # Range of plot
      yaxisRange <- ylimVals[3:4]
      plot(c(0.5,numScenarios+0.5), yaxisRange, type="n", xaxs="i", xaxt="n", xlab="", 
           yaxt="n", ylab="")
      axis(1, tick=TRUE, labels=FALSE)
      abline(h=0.0, lty="dotted")         ## Hardwired.
      
      # First of row stuff ...
      if ( stat == 1) {
        # Y-axis tick marks, numbers and label.
        if ( all(yaxisRange == c(0.0,1.0))) {
          # Specifically for publication, really!
          yaxisTicks <- axTicks(2, axp=c(yaxisRange[1],yaxisRange[2],4))
          yaxisLabels <- vector(mode = "character", length = 5)
          yaxisLabels[c(1,3,5)] <- format(yaxisTicks[c(1,3,5)])
          yaxisLabels[c(2,4)] <- ""
          yaxisLabels[5] <- paste0("\u2265",yaxisLabels[5])
          axis(2, labels=yaxisLabels, at=yaxisTicks, las=1)
        } else {
          axis(2, labels=TRUE, las=1)
        }
        midyRange <- ((yaxisRange[2] - yaxisRange[1])/2.0) + yaxisRange[1]
        axis(2, at=midyRange, labels="precision", tick=FALSE, padj=-3.0)
        
        # SDM title.
        axis(2, at=yaxisRange[2], labels=paste0("       ", sdm, " SDM"), tick=FALSE, padj=-4.5)
      }
      
      # Plot precision of species for each scenario.  
      for (sc in 1:numScenarios ) {
        # Statistic values for this scenario (will be plotted as a vertical bar).
        thisStat <- as.vector(retLst$statsObj$stats[[whichStat]]$sd[sc,whichSpecies,sdm,whichCoeffsForStat])
        
        # Check for NA values.
        indNA <- c(which(is.na(thisStat)), which(is.nan(thisStat)))
        if ( length(indNA) > 0 ) {
          warning(paste0(length(indNA), " NA or NaN values in standard deviation of ",
                         " for statistic = ", whichStat,
                         " for SDM = ", sdm, 
                         " and scenario = ", namesScenarios[sc]))
          thisStat <- thisStat[-indNA]
        }
        
        # Plot ...
        if ( vioPlots ) {
          # Reset infinite values.
          indInf <- which(is.infinite(thisStat))
          if ( length(indInf) > 0 ) thisStat[indInf] <- substituteInfValue
          vioplot(thisStat, col=violinCol, border=violinCol, add=TRUE,
                  drawRect = FALSE, at=sc, wex=widthsViolins[stat,sdm,sc,"sd"])
        }
        stripchart(thisStat, at=sc, vertical=TRUE, pch="_", add=TRUE, col=pointsCol)
      
        # Median value for this scenario.
        plotMedians[sc] <- median(thisStat, na.rm=TRUE)
      }    
      
      # Add median value for all scenarios in this plot.
      if ( medianLines ) {
        #points(1:numScenarios, plotMedians, pch="_", col=medianCol)
        lines(1:numScenarios, plotMedians, lty="dashed", col=medianCol)
      }
    
      # Add plot identifier, if requested.
      if ( doPlotIdentifiers ) {
        identHere <- yaxisRange[2]
        whichIdent <- 2 + (isdm-1)*numPlotSDMs  + (stat-1)*numPlotSDMs*2 
        text(x=1, y=identHere, label=plotIdentifier[whichIdent], adj=c(0,1.5),cex=0.8)
      }
    }  # sdm's (or plot rows).
    
    # Add x-axis for this column only on bottom row.
    axis(1, 1:numScenarios, xAxisLabelsUse, las=xAxisLabelStyle)
    if ( ! is.null(xAxisTitle) ) {
      if ( length(xAxisTitle) == numPlotStats ) {
        midxRange <- (numScenarios/2.0) + 0.5
        axis(1, at=midxRange, labels=xAxisTitle[stat], tick=FALSE, padj=4.0)
      } else if ( length(xAxisTitle) == 1 ) {
        # Will add below.
      } else {
        # Will warn below.
      }
    }
    
  } # statistics (or plot columns).
  
  # Add a title to the plot.
  if ( ! is.null(plotTitle) ) 
    title(plotTitle, outer=TRUE)
  
  # Add an x-axis title to the plot.
  if ( ! is.null(xAxisTitle) ) {
    if ( length(xAxisTitle) == numPlotStats ) {
      # Should have already added them above!      
    } else if ( length(xAxisTitle) == 1 ) {
      title(xlab = xAxisTitle, outer=TRUE, line=min(4,omaPar[1]))
    } else {
      warning("Argument 'xAxisTitle' is an unexpected length.  No x-axis titles added.")
    }
  }
  
  # Turn off the plotting device, if it is a file.
  if ( plotToFile ) dev.off()
  
  # Return plotting to original settings.
  par(opar)
}  

#-----------------------------------------------------------------------------------------

makeDensityPlots <- function(envirVals, scenariosObj, 
                             whichScenarios=1:scenariosObj$numScenarios, 
                             whichRuns=c(1,scenariosObj$numRuns), 
                             legendTitle=NULL, xLabel="covariate value",
                             plotDevice = "RStudioGD", 
                             plotDir = paste0(scenariosObj$scenariosDir,"/Plots"), 
                             fileName="density", plotTitle=NULL, plotUnits="cm", 
                             plotHeight=12, plotWidth=15.75, plotRes=600) {
  
  # Plot the density function for all the environment values (within the domain) and then
  # each successive scenario's density function for the environment values at the survey
  # sample sites (specified as cells in the PA data for each scenario)
  
  # envirVals:      a vector of the selected environment variable's values for cells within
  #                 the domain.  It is assumed that envirVals and the results from the 
  #                 scenariosObj runs are based on the same domain (i.e. same cells).
  # scenariosObj:   a scenarios object that contains information about the scenarios run in 
  #                 an experiment.
  # whichScenarios: which scenarios to use in plotting
  # whichRuns:      which simulation run to use the survey sample locations from.  Usually
  #                 only have the option of 1 or 5000.
  
  # Check scenarios requested are valid.
  if ( all(whichScenarios %in% 1:scenariosObj$numScenarios) ) {
    numReqScenarios <- length(whichScenarios)
  } else {
    stop("Invalid scenarios requested.")
  }
  
  # Going to need the range of the y-axis in the plot to be based on all scenarios densities.
  # So, will need to load and store the sample sites for each scenario.
  experimentDir <- scenariosObj$scenariosDir
  numRuns <- length(whichRuns)
  densities <- vector(mode="list",length = numReqScenarios*numRuns)
  yRange <- NULL
  for ( sc in 1:numReqScenarios ) {
    # Get the directory for each scenario in this experiment.
    thisScenarioNum <- whichScenarios[sc]
    scenarioDir <- paste0(experimentDir,"/",scenariosObj$namesScenarios[thisScenarioNum],"/")
    
    for ( run in 1:numRuns ) {
      # Make the filename for requested run's survey sample sites.
      thisRunNum <- whichRuns[run]
      dataFile <- paste0(scenarioDir, "DataDumps/DataExample-run", thisRunNum,".RData")
      
      # Load the requested data (i.e. the survey object)
      load(dataFile)
    
      # Save the density for each of the scenarios.
      densityRes <- density(envirVals[surveyObj$rowsInCells])
      indList <- (sc-1)*numRuns + run
      densities[[indList]] <- list(x = densityRes$x, y=densityRes$y)
      
      # While here, get the y-axis range required by the scenario densities.
      yRange <- range(c(densityRes$y, yRange))
    }
  }
  
  # Get the density function for all the environment values.
  densityEnv <- density(envirVals)
  
  # Get the required range for all densities.
  xRange <- range(densityEnv$x)
  yRange <- range(c(densityEnv$y, yRange))
  
  # Set up line types, colours and symbols for plotting.
  ltyLines <- c("solid","dotdash","dashed","dotted","longdash","twodash")
  if ( numRuns > length(ltyLines) ) 
    stop("Unable to plot for this many runs due to limited line types.")
  # Recommended colour palette for colour blindness (with the black that I have already used!)
  scenarioCol <- c(rgb(240, 228, 66, maxColorValue = 255), # yellow
                   rgb(0, 114, 178, maxColorValue = 255),  # blue
                   rgb(213, 94, 0, maxColorValue = 255),   # vermillion (tomato red)
                   rgb(0, 158, 115, maxColorValue = 255),  # blue green
                   rgb(86, 180, 233, maxColorValue = 255), # sky blue
                   rgb(230, 159, 0, maxColorValue = 255),  # orange
                   rgb(204, 121, 167, maxColorValue = 255)) # reddish purple (orchid)
  if ( numReqScenarios > length(scenarioCol) )
    stop("Unable to plot for this many scenarios due to limited colours in current settings.")
  scenarioPch <- 1:numReqScenarios
  
  # Set up layers of densities.
  layers <- setPlotLayers()
  layers <- addPlotData(layers, densityEnv, plotfunc = "plot", ylim=yRange, main="", 
                        xlab=xLabel, ylab="density")
  layers <- addPlotData(layers, list(h=0.0), plotfunc = "abline", lty="dotted")
  for ( sc in 1:numReqScenarios ) {
    for ( run in 1:numRuns ) {
      # Get this scenario x run's densities
      indList <- (sc-1)*numRuns + run
      x <- densities[[indList]]$x
      y <- densities[[indList]]$y
      
      # Plot line
      layers <- addPlotData(layers, x, y, plotfunc="lines", col=scenarioCol[sc], lty=ltyLines[run])
      
      # Plot some points (not all as too messy)
      numX <- length(x)
      pts <- seq(from=1, to=numX, by=50) + sc*5    # offset so symbols aren't all in same place
      numPts <- length(pts)
      pts[numPts] <- min(pts[numPts], numX)     # make sure the last value isn't too large. 
      layers <- addPlotData(layers, x[pts], y[pts], plotfunc="points", col=scenarioCol[sc], 
                            pch=scenarioPch[sc])
    }
  }
  
  # Legend for scenarios
  layers <- addPlotData(layers, "topleft", plotfunc = "legend", 
                        legend=c("region", scenariosObj$prettyNamesScenarios[whichScenarios]), 
                        col = c("black", scenarioCol[1:numReqScenarios]), 
                        lty = c("solid",rep("solid",numReqScenarios)), 
                        pch = c(NA, scenarioPch), 
                        title = legendTitle)
  
  # Legend for runs
  if ( numRuns > 1 )
    layers <- addPlotData(layers, "topright", plotfunc = "legend",
                          legend = paste0("run ",whichRuns), lty = ltyLines[1:numRuns], 
                          title = "simulation")
  
  # Plot.
  if ( plotDevice != "RStudioGD" ) fileName <- makeFileName(fileName, plotDir, plotDevice)
  plot.plotLayers(layers, device = plotDevice, fileName = fileName, fileHeight = plotHeight,
                  fileWidth = plotWidth, fileUnits = plotUnits, fileRes = plotRes, 
                  main = plotTitle, mar=c(4,4,1,1))
  
}

#-----------------------------------------------------------------------------------------

plotScenarioMaps <- function(whichExperiment, whichSpecies=NULL, whichSDM=NULL, 
                             whichScenarios=NULL, plotTrue=TRUE, plotDevice="RStudioGD", 
                             plotDir=getwd(), fileName="mapCompare", plotUnits="cm", 
                             plotHeight=20, plotWidth=15.75, plotRes=600, plotNames=NULL,
                             numPlotCols=1, coastLine=NULL, 
                             usePalette=rev(terrain.colors(255))){
  
  
  # Plot scenario maps from a particular experiment for the given species.  Can also plot
  # the true map for this experiment and species, in which case, it occupies the first plot.
  #
  # Arguments ...
  # whichExperiment: a string vector containing a directory name of an experiment.
  # whichSpecies:    which species to plot.  
  # whichSDM:        which SDM's estimate from the scenario to plot.  If a scalar, all 
  #                  scenarios will use estiamtes from this SDM.  If a vector the same 
  #                  length as whichScenarios, the pair of SDM x scenario will be used.
  # whichScenarios:  a vector of scenario directory names from which to source results.
  # plotTrue:        if true, plots the true values for the given species.
  # plotNames:       a vector of character strings that will be plotted within each plot
  #                  to distinguish which plot is which. Include a name for the true plot,
  #                  in the first position, if plotTrue = TRUE.
  # numPlotCols:     number of columns of plots to put on the page (will figure out the 
  #                  number of rows automatically).
  # coastLine:       a spatial polygon giving the coast line for the map. 
  # usePalette:      colour palette to use when plotting maps.
  
  # Error check.
  if ( is.null(whichExperiment) || length(whichExperiment) > 1 )
    stop("Problem with experiment directory name, please check.")
  if ( is.null(whichSpecies) || length(whichSpecies) > 1 ) 
    stop("Problem with specified species.  Need one and only one!")
  if ( is.null(whichSDM) ) stop("No SDM given.  At least one is necessary.")
  if ( is.null(whichScenarios) ) stop("No scenarios given.")

  # Numbers of things.
  numScenarios <- length(whichScenarios)
  if ( length(whichSDM) == 1 && numScenarios > 1) whichSDM <- rep(whichSDM, numScenarios)
  numPlots <- numScenarios + as.integer(plotTrue)  # extra plot for true map.
  numPlotRows <- ceiling(numPlots/numPlotCols)
  
  # More error checking.
  if ( ! is.null(plotNames) ) {
    if ( length(plotNames) != numPlots )
      stop("Problem with number of plot names provided, please check.")
  }
  if ( length(whichSDM) != numScenarios ) 
    stop("One SDM only to be specified for each scenario (assuming not same for all).")

  
  # Plot directory 
  if ( ! dir.exists(plotDir) ) {
    dir.create(plotDir, recursive = TRUE)
  }
  
  # Which device are we printing to?
  if ( plotDevice == "RStudioGD" ) {
    # plot to the R studio plot window.
    plotToFile <- FALSE
  } else {
    fileName <- makeFileName(fileName, plotDir, plotDevice)
    argList <- list(filename = fileName, width = plotWidth, height = plotHeight,
                    units = plotUnits, res = plotRes)
    do.call(plotDevice, argList)
    plotToFile <- TRUE
  }
  
  # Divide plotting into the number of rows and columns required.
  omaPar <- par("mar")         # use default single plot per page margins
  marPar <- c(0.5,0.5,0.5,1.0) # small margin on top and bottom to separate rows and allow for tick marks.
  # margin on right side to allow for raster legend
  omaPar[1] <- omaPar[1] - 1.0 # leave space for x-axis ticks and labels but not x-axis title.  
  omaPar[3] <- 0.0             # No space at top (i.e. no plot title).
  opar <- par(mfrow=c(numPlotRows,numPlotCols), oma = omaPar, mar = marPar, cex=0.66)
  
  # Load cell object data and domain.
  savedDir <- paste0(whichExperiment, "/", whichScenarios[1], "/DataDumps")
  if ( ! dir.exists(savedDir) ) 
    stop(paste0("Unable to load data for map as directory doesn't exist.\n", 
                "  Directory tried: '", savedDir,"'"))
  tmp <- load(paste0(savedDir,"/DataDump-AllRuns.RData"))
  rlPlot <- domainObj$mask
  
  # If plot names are to be provided, get position.  HARDWIRED!
  xyName <- as.vector(simObj$ext)[c(1,3)]
  
  # Plot map of true values, if required.
  if ( plotTrue ) {
    # Plot raster of true species distribution values.  
    rlPlot[cellObj$cells] <- cellObj$trueLambda[ ,whichSpecies] * cellObj$areaCell
    plot(rlPlot, asp=1, xaxt="n", col=usePalette)
    axis(1, tick=TRUE, labels=FALSE)
    text(xyName, labels=plotNames[1])
    if ( ! is.null(coastLine) ) plot(coastLine, add=TRUE)
  }
  
  # Empty plots to test.
  for ( sc in 1:numScenarios ) {
    # Load saved data relevant to given scenario for this experiment.
    savedDir <- paste0(whichExperiment, "/", whichScenarios[sc], "/Results")
    if ( ! dir.exists(savedDir) ) 
      stop(paste0("Unable to load data for scenario map as directory doesn't exist.\n", 
                  "  Directory tried: '", savedDir,"'"))
    tmp <- load(paste0(savedDir,"/Results.RData"))
    
    # Plot map of this scenario's first estimate for the given species.
    indSDM <- which(resLst$namesValidSDM == whichSDM[sc])
    internalNameSDM <- resLst$validSDM[indSDM]
    estCoeffs <- resLst[[internalNameSDM]]$coeffs[ ,whichSpecies, ]
    cellValues <- as.matrix(lambda.cell(simObj$lambda.formula, estCoeffs[ ,1], cellObj$covars))
    rlPlot[cellObj$cells] <- cellValues * cellObj$areaCell
    plot(rlPlot, asp=1, xaxt="n", col=usePalette)
    if ( sc < numScenarios ) {
      axis(1, tick=TRUE, labels=FALSE)
    } else {
      axis(1, tick=TRUE, labels=TRUE)
    }
    if ( ! is.null(coastLine) ) plot(coastLine, add=TRUE)
  }
  
  # Turn off the plotting device, if it is a file.
  if ( plotToFile ) dev.off()
  
  # Return plotting to original settings.
  par(opar)
}  

#-----------------------------------------------------------------------------------------
