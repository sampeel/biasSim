runScenario <- function(numPA=2500, gammaMultiplier=1.0, deltaMultiplier=1.0, numRuns=100,
                        numClusters=0, widthClusters=5, useSDM = c("AB", "PA", "PO", "MsPP"),
                        isOutputToFile=TRUE, outDir=paste0(getwd(), "/", "Output"),
                        doResultPlots=TRUE, doCheckPlots=FALSE, useSavedData=TRUE,
                        randomSeed=NULL, covarsCentred = TRUE, numBGPoints = 10000,
                        numCoresUse = max(1, detectCores()-1),
                        minSurveyArea = 0.001, maxSurveyArea = minSurveyArea) {

  # Time at start.
  timeStartSetup <- Sys.time()
  if ( ! is.null(randomSeed) ) set.seed(randomSeed)

  #------------------------------------------------
  # Settings ...
  #------------------------------------------------

  # How many cores are there?  How many runs?
  if ( is.null(numCoresUse) || numCoresUse < 1 || numCoresUse >= detectCores() ) {
    numCoresUse <- max(1, detectCores() - 1)
  } 
  numRuns <- numRuns

  # Simulation space (for a laea projection)
  simUnits <- "km"
  simLonOrigin <- 180
  simLatOrigin <- -72
  simProj <- proj4str.laea(simUnits, simLonOrigin, simLatOrigin)
  #simExt <- extent(-900, 620, -780, 620)                                     # on laea projection scale!

  # True intensity functions stuff ...
  numSpecies <- 21
  namesSpecies <- NULL                                                        # set below.
  alpha <- NULL                                                               # estimated below.
  beta <- NULL                                                                # estimated below.
  gamma <- NULL                                                               # estimated below.
  delta <- NULL                                                               # estimated below.
  lambda.formula <- "z ~ bath + I(log(chl)) + seaice"                         # cloglog (PA) or log (PO) link assumed, area offset added if needed
  bias.formula <- "z ~ bath"                                                  # log link assumed, area offset added if needed
  isCentred <- covarsCentred                                                  # Centre the covariate data (x_i - mean(x_i))

  # Survey stuff ...
  numSurveys <- numPA                                                         # Number of survey locations to simulate.
  minSurveyArea <- minSurveyArea                                              # in simUnits squared
  maxSurveyArea <- maxSurveyArea                                              # in simUnits squared
  numClusters <- numClusters                                                  # Number of clusters in the survey locations, =1 gives random locations.
  widthClusters <- widthClusters                                              # Width of "square" each cluster potentially occupies, in number of cells.

  # Presence-only points ...
  gammaMultiplier <- gammaMultiplier                                          # Alter "intensity" of the probability of observation per species to see more or less PO points.
  deltaMultiplier <- deltaMultiplier                                          # Alter anthropogenic coefficient for probability of observation (between [0,1])

  # Background points ...
  numBGPoints <- numBGPoints

  # Minimum species location prevalence ...
  minPrevalence <- 5                                                          # Minimum number of locations with evidence of species occupation (i.e. presences or non-zero abundances) before glms will be performed (not sure what happens for PPM or MsPP!)

  # SDM's to use ...
  useCount <- "AB" %in% useSDM
  useCloglog <- "PA" %in% useSDM
  usePPM <- "PO" %in% useSDM
  useMsPP <- "MsPP" %in% useSDM

  # Environment and mask RasterLayers (laea projection, assumes same as specified above).
  # NB: assumes these cover the extent of the simulation (even if some of the values are NA).
  # NB: x and y are assumed to be covariates, these are additional environment covariates.
  envirDir <- "~/Data/Environmental Rasters/"
  envirFiles <- c("sim-rst_bathymetry.grd",
                  "sim-rst_chl_summer_climatology.grd",
                  "sim-rst_seaice_summer_variability.grd")
  envirNames <- c("bath", "chl", "seaice")                                    # use same names same as in lambda.formula above.
  maskFile <- paste0(envirDir, "sim-Mask.grd")
  maskValue <- NA

  # Sample bias info files.
  biasDir <-  envirDir
  biasFiles <-  c("sim-rst_bathymetry.grd")
  biasNames <- c("bath")                                                      # Use same names same as in bias.formula above.

  # Coastline data
  # NB: data is SpatialPolygon in longlat projection (will be converted to simulation projection)
  coastFile <-  "~/Data/Coastlines/southOceanPoly.RData"                      # Set to "" if this file of data is not available.

  # Research base data, csv formatted table data with long, lat and place name columns.
  baseFile <-  "~/Data/Research Stations/AntarcticBaseUniqueNames.csv"        # Set to "" if this file of data is not available.
  baseXcolName <- "LONGITUDE"
  baseYcolName <- "LATITUDE"
  baseNamesColName <- "PLACE_NAME"
  baseNamesShortColName <- "SHORT_NAME"

  # Coefficient estimation data. csv formatted table data with long, lat and species columns.
  coeffEstFilePO <-  "~/Data/Ross Sea/RossSeaMaskPO.RData"
  coeffEstFilePA <-  "~/Data/Ross Sea/RossSeaMaskPA.RData"
  coeffEstXcolName <- "x"
  coeffEstYcolName <- "y"
  coeffEstSpeciesColName <- "speciesID"
  coeffEstAreaColName <- NULL                                                 # Give this or an area val to be used for all records!
  coeffEstAreaVal <- 0.001                                                    # Give this or an area column name with data for all records!

  # Input directory (where previous runs have saved processed data that can be reused).
  inputDir <- paste0(getwd(), "/", "Input")
  useSavedData <- useSavedData                                                # Use data from a previous run that is in the Input directory.

  # Report progress and errors to this file, and all output (inc. plots) to this directory.
  isOutputToFile <- isOutputToFile                                            # TRUE for file output, FALSE for console output
  outDir <- outDir                                                            # Only active if isOutputToFile is TRUE
  outFileName <- "allmsg.txt"                                                 # Name of file where messages go when not going to console.
  outWarnOption <- 1                                                          # Option that sets the warning behaviour, see options("warn")
  dataDumpDir <- paste0(outDir, "/", "DataDumps")                             # Saves data in the case of an error or warning from the SDM calls.
  resultsDir <- paste0(outDir, "/", "Results")                                # Where the results are saved.

  # Where to plot to (NB: isOutputToFile=TRUE should override if set to screen plotting.)
  #plotToDevice <- "RStudioGD"                                                # "RStudioGD" for screen plotting in RStudio
  plotToDevice <- "png"                                                       # "png" for plotting to a file of type png.
  plotDir <- paste0(outDir, "/", "Plots")                                     # Only active if plotToDevice != "RStudio"
  fileHeight <- 700
  fileWidth <- 900

  # Plotting - which plots are to be produced ...
  doCheckPlots <- doCheckPlots                                                # Automatically sets all check plots to TRUE!  Helps with memory as plot layers aren't loaded until after sim is run!
  checkPlotsAllRuns <- FALSE                                                  # FALSE gives plots on first run ONLY! TRUE gives plots for all runs.
  envirData <- FALSE                                                          # Plot the environment covariate data (x in Fithian et al.)
  maskData <- FALSE                                                           # plot the domain mask
  biasData <- FALSE                                                           # plot the bias covariate data (z in Fithian et al.)
  trueCoeffEstimates <- FALSE                                                 # plots for generation of 'true' coefficients
  simDomain <- FALSE                                                          # called "domain" in plot object but can't be called that here!
  intensity <- FALSE                                                          # lambda(x,y,alpha,beta,envirCovars)
  probObs <- FALSE                                                            # bias(x,y,gamma,delta,biasCovars)
  biasIntensity <- FALSE                                                      # lambda(x,y) * bias(x,y) is the intensity function for the PO simulation.
  countPerCell <- FALSE                                                       # Number of simulated individuals per cell for each species
  surveyAreas <-  FALSE                                                       # Number of surveys located in each cell (same for each species!)
  countData <- FALSE                                                          # Number of species counted (i.e. abundunce) in each survey (well cell that the survey occurred in!)
  PAData <- FALSE                                                             # Whether a species has been present (=0 in cell) or absent (=1 in cell) in a survey
  POData <- FALSE                                                             # Number of observed individuals per cell for each species
  BGPoints <- FALSE                                                           # Plot of the location of the generated background points (same for all runs and all species)
  doResultPlots <- doResultPlots                                              # Automatically do result plots if this is TRUE.
  estCoeffs <- TRUE
  mseCompare <- TRUE
  avgEstIntensity <- TRUE

  #------------------------------------------------
  # End of Settings
  #------------------------------------------------


  # Load last sim settings.
  savedLastSimSettings <- makeFileName("LastSimSettings", inputDir, "RData")
  if ( file.exists(savedLastSimSettings) ) {
    # Load data from previously saved R data file.
    load(savedLastSimSettings)
  }



  #------------------------------------------------
  # Initialise settings/simulation object for use in this simulation ...
  #------------------------------------------------

  simObj <- initSimulation(numRuns, numCoresUse)
  simObj <- setSimOutput(simObj, inputDir, outFileName, outDir, isOutputToFile, dataDumpDir,
                         resultsDir, outWarnOption)


  #------------------------------------------------
  # Initialise plotting object for use in this simulation ...
  #------------------------------------------------

  # Initialise plotting ...
  plotObj <- initPlotting(doCheckPlots, checkPlotsAllRuns)
  plotObj <- setPlotsCheck(plotObj, envirData, maskData, biasData, simDomain, intensity,
                           biasIntensity, countPerCell, surveyAreas, countData,
                           PAData, POData, BGPoints, probObs, trueCoeffEstimates)
  plotObj <- setPlotsResults(plotObj, estCoeffs, mseCompare, avgEstIntensity)
  plotObj <- setPlotsToFile(plotObj, device = plotToDevice, fileWidth = fileWidth,
                            fileHeight = fileHeight, plotDir = plotDir)

  # Do we need to read in the plotting layers yet? (They use lots of memory!)
  if ( any(c(doCheckPlots, envirData, maskData, biasData, simDomain, intensity, biasIntensity,
             countPerCell, surveyAreas, countData, PAData, POData, BGPoints,
             probObs, trueCoeffEstimates)) ) {
    # Get the coastline SpatialPolygons.
    if ( file.exists(coastFile) ) {
      load(coastFile)
      coastLine.longlat <- cm1
      rm(cm1)
    } else {
      warning("Specified coast line file does not exist.  Coast line layer will not be plotted.")
      coastLine.longlat <- NULL
    }

    # Get the research base names.
    SavedBaseNames <- makeFileName("ResearchBaseNames", inputDir, "RData")
    if ( file.exists(SavedBaseNames) && useSavedData ) {
      # Load data from previously saved R data file.
      load(SavedBaseNames)

      # Check the data is from the same raw data file.
      if ( basesObj$files != baseFile ) {
        stop("Research base names file has changed. Remove saved data from input directory.")
      }

    } else {
      # Read in and process raw data.
      if ( file.exists(baseFile) ) {
        basesObj <- readDataTable(baseFile)
        basesObj$data <- postProcessBaseData(x=basesObj$data[ ,baseXcolName],
                                             y=basesObj$data[ ,baseYcolName],
                                             base=basesObj$data[ ,baseNamesColName],
                                             baseShort = basesObj$data[ ,baseNamesShortColName])
        basesObj <- as.DataPoints(basesObj, xColName = "x", yColName="y")
  
        # Save processed data for use in future.
        save(basesObj, file=SavedBaseNames)
      } else {
        warning("Specified base names file does not exist.  Place names layer will not be plotted.")
        basesObj <-  initDataObj()
      }
    }

    # Set in the plotting object.
    plotObj <- setPlotsLayers(plotObj, simProj, coastLine.longlat, basesObj$data)

    # Remove objects that aren't needed anymore.
    rm(coastLine.longlat, basesObj)
  }

  #------------------------------------------------
  # Get covariate data for use in this simulation ...
  #------------------------------------------------

  # Get the environmental covariates and mask.
  SavedEnvirCovariates <- makeFileName("EnvirCovariates", inputDir, "RData")
  if ( file.exists(SavedEnvirCovariates) && useSavedData && isCentred == simObj.last$isCentred) {
    # Load data from previously saved R data file.
    load(SavedEnvirCovariates)

    # Check the data is from the same raw data file.
    if ( ! all(envirObj$files == envirFiles) ) {
      stop("Environmental covariate files have changed. Remove saved data from input directory.")
    }

    # Check the data has the same centring setting.
    if ( envirObj$isCentred != isCentred ) {
      stop("Environmental covariates need to be changed. Remove saved data from input directory.")
    }

  } else {
    # Read in the environmental covariates from the given files.
    envirObj <- readDataStack(envirFiles, envirDir, envirNames, TRUE, TRUE)

    # Check projection.
    if ( ! compareCRS(crs(envirObj$data), simProj) ) {
      stop("Simulation projection is not the same as environment data projection.")
    }

    # Plot environment data.
    if ( plotObj$envirData ) {
      fileName <- makeFileName("CheckEnvirData", plotObj$plotDir, plotObj$device)   #paste("CheckEnvirData.", plotObj$device, sep="")
      plot.plotLayers(envirObj$data, asp=1, device=plotObj$device, fileName=fileName,
                      fileHeight = plotObj$fileHeight, fileWidth = plotObj$fileWidth,
                      main="Environmental covariate data")
    }

    # Read in the domain mask (assumes same projection and extent)
    rlMask <- raster(maskFile)
    envirObj <- setDataMask(envirObj, rlMask, maskValue)

    # Check projection, extent, etc.
    if ( ! compareRaster(envirObj$data[[1]], rlMask, extent=TRUE, rowcol=TRUE, crs=TRUE,
                         stopiffalse = FALSE) ) {
      stop("Environmental rasters and domain mask do not have the same raster structure.")
    }

    # Plot mask.
    if ( plotObj$maskData ) {
      fileName <- makeFileName("CheckMaskData", plotObj$plotDir, plotObj$device)   #paste("CheckMaskData.", plotObj$device, sep="")
      plot.plotLayers(envirObj$mask, asp=1, device=plotObj$device, fileName=fileName,
                      fileHeight = plotObj$fileHeight, fileWidth = plotObj$fileWidth,
                      main="Mask for the simulation", sub="white cells are not included")
    }

    # Check for NA values in environment covariate data within domain.
    maskVals <- rlMask[1:ncell(rlMask)]
    validCellNums <- which(!is.na(maskVals))
    for ( layer in names(envirObj$data) ) {
      envirVals <- envirObj$data[[layer]][validCellNums]
      if ( any(is.na(envirVals)) ) {
        stop(paste0("NA values found within domain in environmental covariate ", layer))
      }
    }

    # Centre data if necessary and overwrite existing data.
    envirObj$isCentred <- isCentred
    if ( isCentred ) {
      # Save current data.
      envirObj$dataUncentred <- envirObj$data

      # Get data as it is used by the model.
      envirCovars <- getValuesMask(envirObj$data, rlMask, maskValue)
      sdm.formula <- delete.response.formula(lambda.formula)
      modelCovars <- model.frame(sdm.formula, as.data.frame(envirCovars))

      # Centre the data and save over current data (uncentred version).
      modelMeans <- apply(modelCovars,2,mean)
      centredModelCovars <- modelCovars
      numData <- length(modelMeans)
      for ( i in 1:numData ) {
        # Centre model data.
        centredModelCovars[ ,i] <- centredModelCovars[ ,i] - modelMeans[i]

        # Save the centred data.  Up to here 25/06/2017
        rlSave <- setValuesMask(centredModelCovars[ ,i], rlMask, maskValue)
        if ( i == 1 ) {
          envirObj$data <- stack(rlSave)
        } else {
          envirObj$data <- addLayer(envirObj$data, rlSave)
        }
      }
      names(envirObj$data) <- paste0("data",1:numData)

      # Reset formula to reflect that data has already been processed.
      lambda.formula.old <- lambda.formula
      lambda.formula <- paste(names(envirObj$data), collapse = " + ")
      lambda.formula <- paste0("z ~ ", lambda.formula)

      # Save the centring values.
      envirObj$modelDataMeans <- modelMeans
    } else {
      envirObj$modelDataMeans <- NULL
    }

    # Save processed data for use in future.
    save(envirObj, lambda.formula, file=SavedEnvirCovariates)
  }

  # Reset the simulation extent to equal that produced by the projection and cropping of data.
  simExtReal <- extent(envirObj$data)

  # Create sample bias covariates for this scenario (making sure extent covers that of simulation, and no NAs!).
  SavedBiasCovariates <- makeFileName("BiasCovariates", inputDir, "RData")
  if ( file.exists(SavedBiasCovariates) && useSavedData && isCentred == simObj.last$isCentred) {
    # Load data from previously saved R data file.
    load(SavedBiasCovariates)

    # Check the data is from the same raw data file.
    if ( ! all(biasObj$files == biasFiles) ) {
      stop("Bias covariate data has changed. Remove saved data from input directory.")
    }

    # Check the data has the same centring setting.
    if ( biasObj$isCentred != isCentred ) {
      stop("Bias covariates need to be changed. Remove saved data from input directory.")
    }

  } else {
    # Read in the environmental covariates from the given files.
    # NB: x and y are also part of the sample bias covariates but not stored in stack (so that
    #     all values of x and y are possible, not just the centroids of the raster cells).
    biasObj <- readDataStack(biasFiles, biasDir, biasNames, TRUE, TRUE)

    # Check raster structure same as environment data (and by default, mask)
    if ( ! compareRaster(envirObj$data[[1]], biasObj$data[[1]], extent=TRUE, rowcol=TRUE, crs=TRUE,
                         stopiffalse = FALSE) ) {
      stop("Environmental rasters and sample bias info do not have the same raster structure.")
    }

    # Plot sample bias info.
    if ( plotObj$biasData ) {
      fileName <- makeFileName("CheckBiasData", plotObj$plotDir, plotObj$device)
      plot.plotLayers(biasObj$data, asp=1, device=plotObj$device, fileName=fileName,
                      fileHeight = plotObj$fileHeight, fileWidth = plotObj$fileWidth,
                      main="Sample bias covariate info")
    }

    # Read in the domain mask (assumes same projection and extent)
    rlMask <- raster(maskFile)
    biasObj <- setDataMask(biasObj, rlMask, maskValue)

    # Check projection, extent, etc.
    if ( ! compareRaster(biasObj$data[[1]], rlMask, extent=TRUE, rowcol=TRUE, crs=TRUE,
                         stopiffalse = FALSE) ) {
      stop("Sample bias info and domain mask do not have the same raster structure.")
    }

    # Plot mask.
    if ( plotObj$maskData ) {
      fileName <- makeFileName("CheckMaskData", plotObj$plotDir, plotObj$device)   #paste("CheckMaskData.", plotObj$device, sep="")
      plot.plotLayers(biasObj$mask, asp=1, device=plotObj$device, fileName=fileName,
                      fileHeight = plotObj$fileHeight, fileWidth = plotObj$fileWidth,
                      main="Mask for the simulation", sub="white cells are not included")
    }

    # Check for NA values in bias covariate data within domain.
    maskVals <- rlMask[1:ncell(rlMask)]
    validCellNums <- which(!is.na(maskVals))
    for ( layer in names(biasObj$data) ) {
      biasVals <- biasObj$data[[layer]][validCellNums]
      if ( any(is.na(biasVals)) ) {
        stop(paste0("NA values found within domain in sample bias info ", layer))
      }
    }

    # Centre data if necessary and overwrite existing data.
    biasObj$isCentred <- isCentred
    if ( isCentred ) {
      # Save current data.
      biasObj$dataUncentred <- biasObj$data

      # Get data as it is used by the model.
      biasCovars <- getValuesMask(biasObj$data, rlMask, maskValue)
      sdm.formula <- delete.response.formula(bias.formula)
      modelCovars <- model.frame(sdm.formula, as.data.frame(biasCovars))

      # Centre the data and save over current data (uncentred version).
      modelMeans <- apply(modelCovars,2,mean)
      centredModelCovars <- modelCovars
      numData <- length(modelMeans)
      for ( i in 1:numData ) {
        # Centre
        centredModelCovars[ ,i] <- centredModelCovars[ ,i] - modelMeans[i]

        # Save the centred data.  Up to here 25/06/2017
        rlSave <- setValuesMask(centredModelCovars[ ,i], rlMask, maskValue)
        if ( i == 1 ) {
          biasObj$data <- stack(rlSave)
        } else {
          biasObj$data <- addLayer(biasObj$data, rlSave)
        }

      }
      names(biasObj$data) <- paste0("bias",1:numData)

      # Reset formula to reflect that data has already been processed.
      bias.formula.old <- bias.formula
      bias.formula <- paste(names(biasObj$data), collapse = " + ")
      bias.formula <- paste0("z ~ ", bias.formula)

      # Save the centring values.
      biasObj$modelDataMeans <- modelMeans
    } else {
      biasObj$modelDataMeans <- NULL
    }

    # Save processed data for use in future.
    save(biasObj, bias.formula, file=SavedBiasCovariates)
  }


  #------------------------------------------------
  # Make "true" coefficients for use in this simulation ...
  #------------------------------------------------

  # Can we load the data?
  SavedTrueCoefficients <- makeFileName("TrueCoefficients", inputDir, "RData")
  if ( file.exists(SavedTrueCoefficients) && useSavedData && isCentred == simObj.last$isCentred) {
    # Load data from previously saved R data file.
    load(SavedTrueCoefficients)

    # Check the data is from a simulation with the same settings.  NB: Assumes intercept in formulae!!!
    if ( ( length(alpha) != numSpecies ) || ( length(gamma) != numSpecies) ||
         ( dim(beta)[1] != (numCoefficients(as.formula(lambda.formula)) - 1) ) ||
         ( length(delta) != (numCoefficients(as.formula(bias.formula)) - 1) ) ) {
      stop("True coefficients have changed. Remove saved data from input directory or change useSavedData.")
    }
  } else {
    # Make the domain, necessary for estimation of coefficients!
    domainObj <- makeDomain(envirObj$mask, envirObj$maskValue)

    # Get the PO and PA data (i.e. read actual data in from files)
    coeffEstData <- getPOPAData(domainObj, coeffEstFilePA, coeffEstFilePO, coeffEstXcolName,
                                coeffEstYcolName, coeffEstSpeciesColName, coeffEstAreaColName,
                                coeffEstAreaVal)

    # Check species number.
    if ( (dim(coeffEstData$PA)[2]-3 != numSpecies) ||
         (length(unique(coeffEstData$PO$species)) != numSpecies ) ) {
      stop("Specified number of species is not the same as in true PA and PO data.")
    }

    # Check the prevalence of each species.
    namesSpecies <- names(coeffEstData$PA)[c(-1,-2,-3)]
    prevLst <- checkPrevalence(namesSpecies, coeffEstData$PA, coeffEstData$PO,
                               minPrevalence, isPALocCell = FALSE)
    if ( length(prevLst$namesSpeciesPA) != length(namesSpecies) )
      stop("All species in actual PA data must reach minimum prevalence value.")
    if ( dim(coeffEstData$PO)[1] > length(prevLst$whichPORows) )
      stop("All species in actual PO data must reach minimum prevalence value.")

    # Make the species intensity coefficients.
    coeffLst <- makeSpeciesIntensityCoeffs(coeffEstData$PA, lambda.formula,
                                           envirObj$data, domainObj$mask )
    alpha <- coeffLst$alpha
    beta <- as.data.frame(coeffLst$beta)
    namesSpecies <- names(beta)
    rownames(beta) <- paste0("beta",1:dim(beta)[1])

    # Plot checks.
    coeffs <- as.data.frame(rbind(alpha, beta))
    if ( plotObj$trueCoeffEstimates ) {
      for ( species in namesSpecies) {
        # This species estimated true coefficients.
        thisSpeciesCoeffs <- coeffs[ ,species]

        # This species calculated true intensity
        rlIntensity <- rasterFromFunc(domainObj$ext, res(domainObj$mask), domainObj$mask,
                                      domainObj$maskValue, lambda.xy, as.formula(lambda.formula),
                                      thisSpeciesCoeffs, envirObj$data)


        # Make the plot layers (including the actual data points).
        indPresence <- which(coeffEstData$PA[ ,species] == 1)
        intensityLayers <- makeIntensityLayers(plotObj, rlIntensity,
                                               dataPoints = coeffEstData$PA[indPresence, c("x","y")],
                                               dataPointsPch = "+", dataPointsCol = "red")
        intensityLayers <- addPlotData(intensityLayers, coeffEstData$PA[-indPresence,c("x","y")],
                                       plotfunc = "points", pch=".", col="black")

        # Plot the plot layers
        fileName <- makeFileName(paste0("CheckTrueIntensity-",species),
                                 plotObj$plotDir, plotObj$device)
        plot.plotLayers(intensityLayers, asp=1, device=plotObj$device, fileName=fileName,
                        fileHeight = plotObj$fileHeight, fileWidth = plotObj$fileWidth,
                        main=paste0("Calculated true intensity for ", species))
      }
    }

    # Make the species probability of observation coefficients.
    coeffLst <- makeProbabilityObsCoeffs(coeffEstData$PO, bias.formula, biasObj$data,
                                         lambda.formula, envirObj$data, alpha, beta,
                                         domainObj, numBGPoints)
    gamma <- coeffLst$gamma
    delta <- coeffLst$delta

    # Plot check.
    if ( plotObj$trueCoeffEstimates ) {
      speciesNames <- unique(coeffEstData$PO$species)
      for ( species in speciesNames ) {
        # Calculate the probability of observation from this species coefficients.
        thisSpeciesCoeffs <- c(gamma[species], delta)
        rlProbObs <- rasterFromFunc(domainObj$ext, res(domainObj$mask), domainObj$mask,
                                    domainObj$maskValue, lambda.xy, as.formula(bias.formula),
                                    thisSpeciesCoeffs, biasObj$data)

        # Make the plot layers (including the actual data points).
        intensityLayers <- makeIntensityLayers(plotObj, rlProbObs,
                                               dataPoints = coeffEstData$PO[ , c("x","y")],
                                               dataPointsPch = ".", dataPointsCol = "black")
        indPresence <- which(coeffEstData$PO$species == species)
        intensityLayers <- addPlotData(intensityLayers, coeffEstData$PO[indPresence,c("x","y")],
                                       plotfunc = "points", pch="+", col="red")

        # Plot the plot layers
        fileName <- makeFileName(paste0("CheckTrueProbObs-",species),
                                 plotObj$plotDir, plotObj$device)
        plot.plotLayers(intensityLayers, asp=1, device=plotObj$device, fileName=fileName,
                        fileHeight = plotObj$fileHeight, fileWidth = plotObj$fileWidth,
                        main=paste0("Calculated probability of observation for ", species))

        # Recalculate species intensity.
        thisSpeciesCoeffs <- coeffs[ ,species]
        rlIntensity <- rasterFromFunc(domainObj$ext, res(domainObj$mask), domainObj$mask, domainObj$maskValue,
                                      lambda.xy, as.formula(lambda.formula), thisSpeciesCoeffs, envirObj$data)
        rlSampBias <- rlIntensity * rlProbObs

        # Make the plot layers (including the actual data points).
        intensityLayers <- makeIntensityLayers(plotObj, rlSampBias,
                                               dataPoints = coeffEstData$PO[indPresence, c("x","y")],
                                               dataPointsPch = "+", dataPointsCol = "red")

        # Plot the plot layers
        fileName <- makeFileName(paste0("CheckTrueBiasedIntensity-",species),
                                 plotObj$plotDir, plotObj$device)
        plot.plotLayers(intensityLayers, asp=1, device=plotObj$device, fileName=fileName,
                        fileHeight = plotObj$fileHeight, fileWidth = plotObj$fileWidth,
                        main=paste0("Calculated sample biased intensity for ", species))
      }
    }
    # Save processed data for use in future.
    save(alpha, beta, gamma, delta, namesSpecies, file=SavedTrueCoefficients)
  }


  #------------------------------------------------
  # Initialisation of simulation and results objects ...
  #------------------------------------------------

  # Set simulation object.
  simObj <- setSimGeography(simObj, simExtReal, simProj)
  simObj <- setSimIntensity(simObj, numSpecies, lambda.formula, bias.formula, beta, alpha,
                            gamma, delta, namesSpecies, isCentred)
  simObj <- setSimSurveyInfo(simObj, numSurveys, minSurveyArea, maxSurveyArea,
                             numClusters, widthClusters)
  simObj <- setSimPOPoints(simObj, gammaMultiplier, deltaMultiplier)
  simObj <- setSimBGPoints(simObj, numBGPoints)
  simObj <- setSimUseSDMs(simObj, useCount, useCloglog, usePPM, useMsPP, minPrevalence)

  # Save the settings for comparison next time.
  simObj.last <- simObj
  save(simObj.last, file=makeFileName("LastSimSettings", inputDir, "RData"))
  rm(simObj.last)

  # Get the time.
  timeEndSetup <- Sys.time()

  #------------------------------------------------
  # Run the simulation ...
  #------------------------------------------------

  # Get the time.
  timeStartSim <- Sys.time()

  # Make the domain.
  domainObj <- makeDomain(envirObj$mask, envirObj$maskValue)
  if ( plotObj$domain ) plotDomain(plotObj, domainObj, withOwin=TRUE)
  message("Domain object made.")

  # Make the cells in the domain.
  cellObj <- makeCells(domainObj$mask, domainObj$maskValue)
  cellObj <- makeCovarData(cellObj, envirObj$data)
  cellObj <- makeIntensity(cellObj, simObj$lambda.formula, simObj$coeffs, simObj$namesSpecies)
  if ( plotObj$intensity )
    plotCellValues(plotObj, domainObj$mask, cellObj$cells, cellObj$trueLambda,
                   fileNameStart = "CheckCellIntensity",  titleTxts = "True intensity for ")
  cellObj <- makeProbObs(cellObj, simObj$bias.formula, simObj$gamma, simObj$delta,
                         biasObj$data, simObj$gammaMultiplier, simObj$deltaMultiplier)
  if ( plotObj$probObs )
    plotCellValues(plotObj, domainObj$mask, cellObj$cells, cellObj$truePrObs,
                   fileNameStart = "CheckCellProbObs",  titleTxts = "True probability of observation for ")
  if ( plotObj$biasIntensity)
    plotCellValues(plotObj, domainObj$mask, cellObj$cells, cellObj$trueLambda * cellObj$truePrObs,
                   fileNameStart = "CheckCellBiasedIntensity",  titleTxts = "True biased intensity for ")
  message("Cell object made.")

  # # Make the surveys.
  # surveyObj <- makeSurveyLocations(domainObj$mask, cellObj, simObj$numSurveys, simObj$minSurveyArea,
  #                                  simObj$maxSurveyArea, simObj$bias.formula, simObj$delta,
  #                                  simObj$numClusters, simObj$widthCluster)
  # if ( plotObj$surveyAreas) plotSurveyAreas(plotObj, domainObj$mask, cellObj, surveyObj, "CheckSurveyAreas")
  # message("Survey object made.")

  # Make the background data.
  BG <- makeSimBG(simObj$numBGPoints, domainObj)
  BG[ ,names(cellObj$covars)] <- getCellVals(cellObj, BG$cell)
  if ( plotObj$BGPoints ) plotBGPoints(plotObj, BG, domainObj, cellObj$cells)
  message("Background points made.")

  # Dump data necessary for an SDM run (for analysis if there is an error).
  fileName <- paste0("DataDump-AllRuns")
  fileName <- makeFileName(fileName, simObj$dataDumpDir, "RData")
  save(simObj, domainObj, envirObj, biasObj, cellObj, file = fileName)

  # Garbage collection.
  gc()

  # # Turn warnings into errors for the duration of the simulation runs.
  # oldOptionsWarn <- getOption("warn")
  # options(warn=2)

  # Run repeat simulations using parallelisation.  (NB: use par1Res for debugging only!)
  #par1Res <- runSim(1, simObj, domainObj, cellObj, envirObj, biasObj, surveyObj, plotObj, BG)
  parRes <- mclapply(1:(simObj$numRuns), runSim, simObj, domainObj, cellObj, envirObj, biasObj,
                     plotObj, BG, mc.cores=simObj$numCoresUse)

  # # Turn warnings back to their old setting.
  #options(warn=oldOptionsWarn)


  #------------------------------------------------
  # Collect the results ...
  #------------------------------------------------

  message(paste("Begin organise results from runs", Sys.time()))

  # Initialise the result list object.
  resLst <- initResults(simObj$numRuns, simObj$numCoeffs, simObj$numBiases, cellObj$namesSpecies)
  resLst <- setResTryMe(simObj, resLst)

  # Decode the results.
  for ( i in 1:(simObj$numRuns) ) {
    # Collect results.
    if ( is.null(parRes[[i]]) ) {
      message(paste0("No results for run ",i))
      resLst <- setResError(resLst, i, msg = "No results returned from core for this run.")

    } else if ( inherits(parRes[[i]], "try-error")) {
      message(paste0("No results for run ",i))
      resLst <- setResError(resLst, i, msg = parRes[[i]]$errors[ ,4])

    } else {
      message(paste0("Results for run ", i))
      resLst <- setResOneToMany(resLst, parRes[[i]], i)
    }
  }

  # Calculate the statistics that are summaries of all runs.
  for ( sdm in resLst$validSDM ) {
    resLst[[sdm]] <- makeCoeffBiasVar(resLst[[sdm]], resLst[[sdm]]$coeffs, simObj$coeffs)
  }

  message(paste("End organise results from runs", Sys.time()))

  # Get the time.
  timeEndSim <- Sys.time()

  # Save times in results.
  resLst <- setResTimes(resLst, timeStartSetup, timeEndSetup, 1)
  resLst <- setResTimes(resLst, timeStartSim, timeEndSim, 2)

  # Save the results.
  SavedResults <- makeFileName("Results", resultsDir, "RData")
  save(resLst, simObj, cellObj, file=SavedResults)
  SavedSettings <- makeFileName("SimSettings", resultsDir, "txt")
  write.list(simObj, file = SavedSettings)
  SavedResults <- makeFileName("Results", resultsDir, "txt")
  strTextResults <- c("numRuns", "numSpecies", "namesSpecies", "errors", "timeSetup",
                      "timeSim", "namesValidSDM")
  write.list(resLst[strTextResults], file = SavedResults)

  if ( ! doResultPlots ) {
    # End the diversion of output to file, if necessary, and change back any options.
    message("Do clean up")
    doSimCleanUp(simObj)

    # Return value.
    return(list(resLst=resLst, simObj=simObj, plotObj=plotObj))
  }

  #------------------------------------------------
  # Analyse results ...
  #------------------------------------------------

  # Get the time.
  timeStartResPlots <- Sys.time()
  message(paste("Begin Plot results", timeStartResPlots))

  # Get the plot layers if they haven't already been got!
  if ( ! plotObj$doCheckPlots ) {
    # Get the coastline SpatialPolygons.
    load(coastFile)
    coastLine.longlat <- cm1
    rm(cm1)

    # Get the research base names.
    SavedBaseNames <- makeFileName("ResearchBaseNames", inputDir, "RData")
    if ( file.exists(SavedBaseNames) && useSavedData ) {
      # Load data from previously saved R data file.
      load(SavedBaseNames)

      # Check the data is from the same raw data file.
      if ( basesObj$files != baseFile ) {
        stop("Research base names file has changed. Remove saved data from input directory.")
      }

    } else {
      # Read in and process raw data.
      basesObj <- readDataTable(baseFile)
      basesObj$data <- postProcessBaseData(x=basesObj$data[ ,baseXcolName],
                                           y=basesObj$data[ ,baseYcolName],
                                           base=basesObj$data[ ,baseNamesColName],
                                           baseShort = basesObj$data[, baseNamesShortColName])
      basesObj <- as.DataPoints(basesObj, xColName = "x", yColName="y")

      # Save processed data for use in future.
      save(basesObj, file=SavedBaseNames)
    }

    # Set in the plotting object.
    plotObj <- setPlotsLayers(plotObj, simProj, coastLine.longlat, basesObj$data)

    # Remove objects that aren't needed anymore.
    rm(coastLine.longlat, basesObj)
  }


  # Boxplot of estimated coefficients (against true) for different SDMs
  if ( plotObj$estCoeffs ) {
    makePlotCoeffs(resLst, simObj$coeffs, simObj$namesSpecies, plotObj, "EstCoeffsSpecies",
                   simObj$gamma)
  }

  # Boxplot of error for each intensity estimate for different SDMs
  if ( plotObj$mseCompare ) {
    #  plotObj <- setPlotsToFile(plotObj, fileName="MSESpecies")
    #makePlotsMSE(resLst, simObj$namesSpecies, plotObj, "MSESpecies")
    makePlotsErrorStats(resLst, simObj$namesSpecies, plotObj, "mse", TRUE, "MSESpecies")
    # makePlotsErrorStats(resLst, simObj$namesSpecies, plotObj, "rmse1", FALSE, "RMSE1Species",
    #                     "Root mean square error 1 for ")
    # makePlotsErrorStats(resLst, simObj$namesSpecies, plotObj, "rmse2", FALSE, "RMSE2Species",
    #                     "Root mean square error 2 for ")
    # makePlotsErrorStats(resLst, simObj$namesSpecies, plotObj, "nrmse", FALSE, "NRMSESpecies",
    #                     "Normalised root mean square error for ")
  }

  # Boxplot of average estimated intensity surface for different species and SDMs
  if ( plotObj$avgEstIntensity) {
    # makePlotAvgEstimate(domainObj, plotObj, simObj$lambda.formula, resLst, envirObj$data,
    #                     maxLambda, simObj$namesSpecies, fileNameStart="AvgEstIntensity" )
    makePlotsAvgEstimate(domainObj, plotObj, simObj$lambda.formula, resLst, cellObj,
                         fileNameStart="AvgEstIntensity" )
    # Plot the true intensity.
    plotCellValues(plotObj, domainObj$mask, cellObj$cells, cellObj$trueLambda,
                   fileNameStart = "TrueIntensity", titleTxts = "True intensity for ")

    plotCellValues(plotObj, domainObj$mask, cellObj$cells, cellObj$trueLambda*cellObj$truePrObs,
                   fileNameStart = "TrueBiasedIntensity", titleTxts = "True sample biased intensity for ")

  }

  # Comparison of difference between the true intensity and the true sample biased intensity for species.
  mseVals <- vector(mode="double", length=cellObj$numSpecies)
  names(mseVals) <- cellObj$namesSpecies
  for ( species in cellObj$namesSpecies ) {
    # True intensity values.
    trueIntensity <- cellObj$trueLambda[ ,species]

    # True sample biased intensity values.
    trueSampleBiasedIntensity <- trueIntensity * cellObj$truePrObs[ ,species]

    # The mean square error of these two vectors.
    mseVals[species] <- mse(trueIntensity, trueSampleBiasedIntensity)
  }
  tmp <- sort(apply(resLst$numPresencesPO,1,mean), decreasing = TRUE)
  plot(1:cellObj$numSpecies, mseVals[names(tmp)], pch="o", log="y", xaxt="n",
       ylab="difference", xlab="species" )
  axis(1, 1:cellObj$numSpecies, labels = names(tmp), las=2)
  title("MSE of the true intensity and the true sample biased intensity")

  # Get the time.
  timeEndResPlots <- Sys.time()
  message(paste("End Plot results", timeEndResPlots))
  resLst <- setResTimes(resLst, timeStartResPlots, timeEndResPlots, 3)

  #------------------------------------------------
  # Finish ...
  #------------------------------------------------

  # End the diversion of output to file, if necessary, and change back any options.
  message("Do clean up")
  doSimCleanUp(simObj)

  # Return value.
  return(list(resLst=resLst, simObj=simObj, plotObj=plotObj))

}

#-----------------------------------------------------------------------------------------

getMaxLambda <- function(domain, res, myFormula, coeffs, covars) {

  # Get the maximum value of lambda (the intensity function) for each set of coefficients.
  # Uses rasters to calculate the value of lambda at the specified resolution.  The value
  # of the resolution will dictate how "accurate" the result is.  Assumes there is a
  # function called "lambda" that has the arguments (x,y,coeffs,covars) in the workspace.
  #
  # Arguments ...
  # domain:    a domain object on which the lambda values are to be calculated.
  # res:       the resolution at which the lambda values are to be calculated.
  # myFormula: a formula object that contains how to combine the information for an intensity function.
  # coeffs:    the vector of the coefficients of the lambda function.  If coeffs is a
  #            matrix, the coefficients of a lambda function are assumed to be in each
  #            column and the number of columns is the number of lambda functions whose
  #            maximum is to be found.
  # covars:    the RasterStack of covariates for the lambda function.  Must have values at
  #            all points in the domain (combination of extent and mask!)

  # The number of intensity function coefficients provided.
  numFuncs <- dim(coeffs)[2]

  # Initialise the return value.
  maxLambda <- drop(matrix(nrow=numFuncs, ncol=1))

  # Find maximum for each intensity function ...
  for (k in 1:numFuncs) {
    # This intensity function's coefficients.
    thisCoeffs <- coeffs[ ,k]

    # Make a RasterLayer of the values of lambda.
    rsLambda <- rasterFromFunc(domain$ext, res, domain$mask, domain$maskValue, lambda,
                               myFormula, thisCoeffs, covars)

    # The maximum intensity function value is ...
    maxLambda[k] <- maxValue(rsLambda)
  }

  # Return value.
  return(maxLambda)

}

#-----------------------------------------------------------------------------------------

getMaxLambda2 <- function(domain, res, myFormula, coeffs, covars,
                          numCoresUse=max(1, detectCores() - 1)) {

  # Get the maximum value of lambda (the intensity function) for each set of coefficients.
  # Uses rasters to calculate the value of lambda at the specified resolution.  The value
  # of the resolution will dictate how "accurate" the result is.  Assumes there is a
  # function called "lambda" that has the arguments (x,y,coeffs,covars) in the workspace.
  #
  # Arguments ...
  # domain:    a domain object on which the lambda values are to be calculated.
  # res:       the resolution at which the lambda values are to be calculated.
  # myFormula: a formula object that contains how to combine the information for an intensity function.
  # coeffs:    the vector of the coefficients of the lambda function.  If coeffs is a
  #            matrix, the coefficients of a lambda function are assumed to be in each
  #            column and the number of columns is the number of lambda functions whose
  #            maximum is to be found.
  # covars:    the RasterStack of covariates for the lambda function.  Must have values at
  #            all points in the domain (combination of extent and mask!)
  # numCoresUse: number of cores to use in the mclapply function that paralellises the code.

  # The number of intensity function coefficients provided.
  numFuncs <- dim(coeffs)[2]

  ## How many cores are there (don't use all of them)
  #numCoresUse <- max(1, detectCores() - 1)

  # Find maximum for each intensity function ...
  maxLambda <- mclapply(1:numFuncs, getOneMaxLambda, domain, res, myFormula, coeffs, covars,
                        mc.cores=numCoresUse)
  maxLambda <- unlist(maxLambda)

  # Return value.
  return(maxLambda)

}

#-----------------------------------------------------------------------------------------

getOneMaxLambda <- function(k, domain, res, myFormula, coeffs, covars){

  # This intensity function's coefficients.
  thisCoeffs <- coeffs[ ,k]

  # Make a RasterLayer of the values of lambda.
  rsLambda <- rasterFromFunc(domain$ext, res, domain$mask, domain$maskValue, lambda,
                             myFormula, thisCoeffs, covars)

  # The maximum intensity function value is ...
  maxLambda <- maxValue(rsLambda)

  # Return value.
  return(maxLambda)

}

#-----------------------------------------------------------------------------------------

lambda.xy <- function (x, y, myFormula, coeffs, covars, lnLambda=FALSE) {

  # Intensity function.  For inhomogeneous Poisson point distribution and model of this as
  #
  #        ln(lambda(x,y)) = formula with coeffs and covars(x,y)
  #
  # Returns a vector containing the value of lambda(x,y) for all points (x,y).
  # Assumes (x,y) will have valid values in covars (i.e. only use (x,y) that do!!!)
  #
  # ARGUMENTS ...
  # x:         a vector of longitude values of the points
  # y:         a vector of latitude values of the points
  # myFormula: a formula object that contains how to combine the information for an intensity function.
  # coeffs:    a vector containing the coefficients for the intensity formula.
  # covars:    a raster stack containing numEnvirs layers and values for all x and y.
  # lnLambda:  return ln(lambda(x,y)) for TRUE or lambda(x,y) for FALSE

  # Prepare the formula by removing any response variable.
  myTerms <- terms(myFormula, keep.order = TRUE)
  myTerms <- delete.response(myTerms)

  # Get the relevant data.
  xy <- cbind(x,y)
  vals <- davesExtract.v3(covars, xy)   # NB: one column each for each raster layer in stack
  vals <- as.data.frame(cbind(xy, vals), stringsAsFactors=FALSE)
  mat <- as.matrix(model.frame(myTerms, vals))

  # Is there an intercept term?
  if ( attr(myTerms, "intercept") ) {
    ones <- rep(1, length.out=length(x))
    mat <- cbind(ones, mat)
  }

  # Evaluate the function.
  lnRes <- mat %*% coeffs
  if ( lnLambda ) {
    return(drop(lnRes))
  } else {
    return(drop(exp(lnRes)))
  }

}

#-----------------------------------------------------------------------------------------

lambda.cell <- function (myFormula, coeffs, covars, lnLambda=FALSE) {

  # Intensity function.  For inhomogeneous Poisson point distribution and model of this as
  #
  #        ln(lambda(x,y)) = formula with coeffs and covars(x,y)
  #
  # Returns a vector containing the value of lambda(x,y) for all points (x,y).
  # Assumes (x,y) will have valid values in covars (i.e. only use (x,y) that do!!!)
  #
  # ARGUMENTS ...
  # myFormula: a formula object that contains how to combine the information for an intensity function.
  # coeffs:    a vector containing the coefficients for the intensity formula.
  # covars:    a data.frame containing columns of covariate data with the values in each
  #            cell forming the rows.  Only contains information for cells in the domain.
  #            NB: covariate columns need to be named the same as in the formula!
  # lnLambda:  return ln(lambda(x,y)) for TRUE or lambda(x,y) for FALSE

  # Prepare the formula by removing any response variable.
  myTerms <- terms(myFormula, keep.order = TRUE)
  myTerms <- delete.response(myTerms)

  # Get the relevant data.
  mat <- as.matrix(model.frame(myTerms, covars))

  # Is there an intercept term?
  if ( attr(myTerms, "intercept") ) {
    ones <- rep(1, length.out=nrow(covars))
    mat <- cbind(ones, mat)
  }

  # Evaluate the function.
  lnRes <- mat %*% coeffs
  if ( lnLambda ) {
    return(drop(lnRes))
  } else {
    return(drop(exp(lnRes)))
  }

}

#-----------------------------------------------------------------------------------------

makeSimBG <- function(numPoints, domainObj, numReps=5) {

  # Make the quasi-random background points for use in the multispeciesPP method.
  # Returns a data frame with three columns, called x, y and cell.
  #
  # Arguments ...
  # numPoints: the number of background points required.
  # domainObj: the domain or observation window of the simulation.
  # numReps:   the number of repeats of the number of the number of points.
  #            This combats the fact that rQuasi returns the same points each time by
  #            choosing a different set of numPoints from the numReps*numPoints.
  #            Set numReps = 1 if you do want the same points!

  # Check there has been a number specified.
  if ( numPoints < 1 ) {
    stop("Set the number of required background points before making background data.")
  }

  # Generate quasi-random points (lots of points as owin affects how many are made)
  numReps <- 5
  BG <- as.data.frame(rQuasi(numPoints*numReps, domainObj$owin, type="Halton"))

  # Choose a random starting point (as the same sequence of points every time).
  numQuasiPoints <- dim(BG)[1]
  indStart <- sample(1:(numQuasiPoints-numPoints+1), size=1)
  indEnd <- indStart + numPoints - 1
  BG <- BG[indStart:indEnd, ]

  # Extract cell numbers from points?
  BG$cell <- cellFromXY(domainObj$mask, BG)

  # Return value.
  return(BG)

}

#-----------------------------------------------------------------------------------------

makeSimPO <- function(cellObj, runNum) {

  # TO DO: make this right (see sim flow chart version 5)
  # Make the presence-only data points from the known population for each
  # species and the sample bias information.  That is, thin the given populations using
  # the sample bias probability function (assumed to be called probBias).
  #
  # Arguments ...
  # cellObj:     a cells object that contains the species populations.
  # runNum:      the current run number (as PO is created for each run)

  # Check there are simulated populations.
  if ( is.null(cellObj$N) || (runNum != cellObj$runNum) ) {
    stop("Must simulate species cell numbers before making presence-only data.")
  }

  # Check there are probability of observations
  if ( is.null(cellObj$truePrObs) || ! setequal(cellObj$namesSpecies, names(cellObj$truePrObs)) ) {
    stop("Please check that the probability of observing a species has been set correctly.")
  }

  # Initialise data storage (NB: don't know the number of rows yet!).
  PO <- as.data.frame(matrix(nrow=0, ncol=2), stringsAsFactors=FALSE)
  names(PO) <- c("cell", "species")

  # Work out PO data for each species' population.
  for ( species in cellObj$namesSpecies ) {
    # Calculate the number of individuals observed per cell.
    cellPOCount <- rbinom(cellObj$numCells, cellObj$N[ ,species], cellObj$truePrObs[ ,species])
    #rownames(cellPOCount) <- cellObj$cells

    # Check none of the numbers observed in a cell are actually bigger than the number in the cell.
    if ( any(cellPOCount > cellObj$N[ ,species]) )
      warning(paste("Some of the cells have observed numbers greater than the number present!"))

    # temporary plot check
    # rlplot[cellObj$cells] <- cellObj$truePrObs[ ,species] * cellObj$trueLambda[ ,species]
    # plot(rlplot, asp=1, main=paste0("Number of species ", species, " observed in each cell"))
    # indPOCells <- cellPOCount > 0
    # points(cellObj$xy[indPOCells,1], cellObj$xy[indPOCells,2], pch=as.character(cellPOCount[indPOCells]), col=cellPOCount[indPOCells])
    # title(sub="NB: sample biased intensity included")

    # Add points to return value.
    rowsToAdd <- data.frame(rep(cellObj$cells, times=cellPOCount), rep(species, sum(cellPOCount)),
                            stringsAsFactors = FALSE)
    names(rowsToAdd) <- names(PO)
    PO <- rbind(PO, rowsToAdd)
  }

  # Return value.
  return(PO)
}

#-----------------------------------------------------------------------------------------

# makeSimPO <- function(numPOPoints, cellObj, runNum) {
#
#   # Make the given number of presence-only data points from the known population for each
#   # species and the sample bias information.  That is, thin the given populations using
#   # the sample bias probability function (assumed to be called probBias).
#   #
#   # Arguments ...
#   # numPOPoints: a vector giving the number of required presence-only points per species.
#   # cellObj:     a cells object that contains the species populations.
#   # runNum:      the current run number (as PO is created for each run)
#
#   # Check there are simulated populations.
#   if ( is.null(cellObj$N) || (runNum != cellObj$runNum) ) {
#     stop("Must simulate species cell numbers before making presence-only data.")
#   }
#
#   # Check there are probability of observations
#   if ( is.null(cellObj$truePrObs) || ! setequal(cellObj$namesSpecies, names(cellObj$truePrObs)) ) {
#     stop("Please check that the probability of observing a species has been set correctly.")
#   }
#
#   # Initialise data storage (NB: don't know the number of rows yet!).
#   PO <- as.data.frame(matrix(nrow=0, ncol=2), stringsAsFactors=FALSE)
#   names(PO) <- c("cell", "species")
#
#   # Work out PO data for each species' population.
#   for ( species in cellObj$namesSpecies ) {
#     # Calculate the number of individuals observed per cell.
#     cellWeight <- cellObj$N[ ,species] * cellObj$truePrObs[ ,species]
#     cellProb <- cellWeight/sum(cellWeight)
#     cellPOCount <- rmultinom(1, numPOPoints[species], cellProb)
#     rownames(cellPOCount) <- cellObj$cells
#
#     # Check none of the numbers observed in a cell are actually bigger than the number in the cell.
#     if ( any(cellPOCount > cellObj$N[ ,species]) )
#       warning(paste("Some of the cells have observed numbers greater than the number present!"))
#
#     # # plot check
#     # rlplot[cellObj$cells] <- cellObj$truePrObs[ ,species] * cellObj$trueLambda[ ,species]
#     # plot(rlplot, asp=1, main=paste0("Number of species ", species, " observed in each cell"))
#     # indPOCells <- cellPOCount > 0
#     # points(cellObj$xy[indPOCells,1], cellObj$xy[indPOCells,2], pch=as.character(cellPOCount[indPOCells]), col=cellPOCount[indPOCells])
#     # title(sub="NB: sample biased intensity included")
#
#     # Add points to return value.
#     rowsToAdd <- data.frame(rep(cellObj$cells, times=cellPOCount), rep(species, sum(cellPOCount)),
#                             stringsAsFactors = FALSE)
#     names(rowsToAdd) <- names(PO)
#     PO <- rbind(PO, rowsToAdd)
#   }
#
#   # Return value.
#   return(PO)
# }

#-----------------------------------------------------------------------------------------

makeSimCount <- function(surveyObj, cellObj, runNum) {

  # Make the count data from the known populations and known survey areas.
  # Count the number of each species within each survey area (a number from 0, 1, 2, ... ).
  # Columns for the data frame that stores the made data are: long, lat, area, species1, ..., speciesn

  # Check survey locations exist.
  if ( is.null(surveyObj$cells) ) {
    dataLst$isError <- TRUE
    stop("Must make survey locations before making count data.")
  }

  # Check there are simulated populations.
  if ( is.null(cellObj$N) || (cellObj$runNum != runNum) ) {
    dataLst$isError <- TRUE
    stop("Must simulate number of individuals in each cell before making count data.")
  }

  # Initialise the return data frame.
  Count <- matrix(nrow=surveyObj$numSurveys, ncol=(cellObj$numSpecies + 2))
  Count <- as.data.frame(Count, stringsAsFactors=FALSE)
  names(Count) <- c("cell","area", cellObj$namesSpecies)

  # Get location of the surveys and their areas and store.
  Count$cell <- surveyObj$cells
  Count$area <- surveyObj$areas

  # # What percentage of the cell area does the survey cover?
  # prob <- surveyObj$areas/ cellObj$areaCell
  #
  # # TO DO: add in all that complicated stuff surveyed in cell (as there could be more than
  # #        one survey in a cell which could potentially be a problem with the surveys being
  # #        treated separately in rbinom as drawing from N when there would be less than N
  # #        left in the cell)
  # for ( species in cellObj$namesSpecies ) {
  #   # How many of this species is counted in each cell that is surveyed.
  #   NSurveyed <- rbinom(surveyObj$numSurveys, cellObj$N[surveyObj$rowsInCells,species], prob)
  #
  #   # Save this species count for each survey.
  #   Count[ ,species] <- NSurveyed
  # }

  # Number of individuals counted in each survey for each species.
  NSurveyed <- makeNSurveyed(surveyObj, cellObj$N, cellObj$areaCell)
  Count[ ,cellObj$namesSpecies] <- NSurveyed[ ,cellObj$namesSpecies]

  # Return value.
  return(Count)
}

#-----------------------------------------------------------------------------------------

makeSimPA <- function(count, namesSpecies) {

  # Make the presence-absence data from the known count data for each species.
  # Converts count data to a binary value (i.e. present = 1 or absent = 0) for each species.

  # Check count data has been done first.
  if ( is.null(count) ) {
    stop("Must make count data before making presence-absence data.")
  }

  # Check there are species count data.
  numSpecies <- length(namesSpecies)
  if ( length(intersect(namesSpecies, names(count))) != numSpecies ) {
    stop("Not all required species are present in simulated count data.")
  }

  # Initialise return value.
  PA <- count

  # Get index for cells where the number of surveyed individuals is greater than zero, for each species.
  for ( species in namesSpecies ) {
    # Get index for cells where the number of surveyed individuals is greater than zero.
    indWhichPresence <- which(count[ ,species] > 0)
    PA[indWhichPresence,species] <- 1
  }

  # Return value.
  return(PA)
}

#-----------------------------------------------------------------------------------------

runSDM.Count <- function(myFormula, cells, covars, counts, areas) {

  # Run a species distribution model for count/abundance data.  Use a Poisson glm with log link.
  # Returns the estimated coefficients for each species' intensity function (myFormula).
  #
  # ARGUMENTS ...
  # myFormula:  a formula object that contains how to combine the information for an intensity function.
  # cells:      a vector of cell numbers indicating where the data was collected within the domain
  # covars:     a data.frame with covariate values in these cells for each covariate
  # counts:     matrix of the number of individuals counted in each survey (same order as cells)
  #             rows are for each survey site and columns are for each species.
  # areas:      vector of the area of the survey defined by cell number.


  # Numbers of things.
  namesSpecies <- names(counts)
  numSpecies <- length(namesSpecies)
  numCoeffs <- numCoefficients(myFormula)

  # Initialise the return value.
  estCoeffs <- matrix(nrow=numCoeffs, ncol=numSpecies)
  colnames(estCoeffs) <- namesSpecies
  errors <- as.data.frame(matrix(nrow=0, ncol=2), stringsAsFactors=FALSE)
  colnames(errors) <- c("species", "msg")
  retVal <- list(estCoeffs=estCoeffs, stdErrors=estCoeffs, errors=errors, isErrors=FALSE,
                 glmFit=NULL)

  # Put the data together into one structure (except for species count)
  cellAndCovars <- cbind(cells, covars)
  dfData <- as.data.frame(cellAndCovars, stringsAsFactors = FALSE)
  names(dfData) <- c("cell", names(covars))
  dfData$area <- areas

  # Add an area offset to the formula.
  sdm.formula <- update.formula(myFormula, ". ~ . + offset(log(area))")

  # Estimate the coefficients for each species' intensity function.
  for ( species in namesSpecies) {
    # Get this species' count data.
    dfData$z <- counts[ ,species]

    # Estimate the coefficients for the model.
    glmFit <- glm(sdm.formula, family=poisson(), data=dfData, control=list(maxit=100))

    # Did the glm converge to an answer?
    if ( ! glmFit$converged ) {
      retVal$isErrors <- TRUE
      newError <- data.frame(species=species, msg="Count glm has not converged.", stringsAsFactors = FALSE)
      retVal$errors <- rbind(retVal$errors, newError)
    } else {
      # Save estimated coefficients
      retVal$estCoeffs[ ,species] <- glmFit$coefficients
      retVal$stdErrors[ ,species] <- summary(glmFit)$coefficients[ ,2]
      retVal$glmFit <- glmFit
    }
  }

  # Return results.
  return(retVal)

}

#-----------------------------------------------------------------------------------------

runSDM.Cloglog <- function(myFormula, cells, covars, PA, areas) {

  # Run a species distribution model for presence-absence data.  Use a binomial glm with
  # a complimentary log log link.  Returns the estimated coefficients from the glm of the
  # centred data (using centreCovars).  Assumes there is enough data (both presences and
  # absences; each species) for the glm to work.  Adds areas as on offset in the glm.
  #
  # ARGUMENTS ...
  # myFormula:    a formula object that contains how to combine the information for an
  #               intensity function.
  # cells:        a vector of cell numbers indicating where the data was collected within
  #               the domain (i.e. which cells contain the surveys)
  # covars:       a data.frame with covariate values in these cells for each covariate
  #               (numSurveys x numCovars)
  # PA:           matrix of whether or not individuals are present (1) or absent (0) in
  #               each survey (same order as cells) rows are for each survey site and
  #               columns are for each species.
  # areas:        vector of the area of the survey defined by cell number (numSurveys)

  # Numbers of things.
  if (is.numeric(PA) ) PA <- as.matrix(PA)
  namesSpecies <- names(PA)
  numSpecies <- length(namesSpecies)
  numCoeffs <- numCoefficients(myFormula)

  # Initialise the return value.
  estCoeffs <- matrix(nrow=numCoeffs, ncol=numSpecies)
  colnames(estCoeffs) <- namesSpecies
  errors <- as.data.frame(matrix(nrow=0, ncol=2), stringsAsFactors=FALSE)
  colnames(errors) <- c("species", "msg")
  retVal <- list(estCoeffs=estCoeffs, stdErrors=estCoeffs, errors=errors, isErrors=FALSE,
                 glmFit=NULL)

  # Put the data together into one structure (except for species PA)
  cellAndCovars <- cbind(cells, covars)
  dfData <- as.data.frame(cellAndCovars, stringsAsFactors = FALSE)
  names(dfData) <- c("cell", names(covars))
  dfData$area <- areas

  # Add an area offset to the formula and remove the response variable (if present)
  sdm.formula <- update.formula(myFormula, ". ~ . + offset(log(area))")

  # Estimate the coefficients for each species' intensity function.
  for ( species in namesSpecies) {
    # Get this species' PA data.
    dfData$z <- PA[ ,species]

    # Estimate the coefficients for the model.
    glmFit <- glm(sdm.formula, family=binomial(link=cloglog), data=dfData, control=list(maxit=100))

    # Did the glm converge to an answer?
    if ( ! glmFit$converged ) {
      retVal$isErrors <- TRUE
      newError <- data.frame(species=species, msg="Cloglog glm has not converged.", stringsAsFactors = FALSE)
      retVal$errors <- rbind(retVal$errors, newError)
    } else {
      # Save estimated coefficients
      retVal$estCoeffs[ ,species] <- glmFit$coefficients
      retVal$stdErrors[ ,species] <- summary(glmFit)$coefficients[ ,2]
      retVal$glmFit <- glmFit
    }
  }

  # Return results.
  return(retVal)

}

#-----------------------------------------------------------------------------------------

runSDM.PPM <- function(myFormula, PO, BG, domainObj, cellObj) {

  # GLM for presence-only data using PPM function.
  # NB: I think it might work out area itself! No, doesn't use area as they are points!!!!
  # NB: ONE SPECIES per run.
  # NB: need x and y as input for the PPM function even if they are not used in the formula.
  #
  # ARGUMENTS ...
  # myFormula: a formula object that contains how to combine the information for an
  #            intensity function.
  # PO:        the presence-only data points in a data frame (two columns: cell, species).
  # BG:        the quadrature or background data points in a data frame (columns: x, y, covar1, ...).
  # domainObj: a domain object that contains the mask and an owin object giving the region
  #            within the extent that is available.
  # cellObj:   a cell object that contains information at a cell level (see "cellsObj.r")

  # Numbers of things.
  namesSpecies <- cellObj$namesSpecies
  POSpecies <- sort(unique(PO$species))
  if ( ! setequal(POSpecies, intersect(POSpecies, namesSpecies)) ) {
    stop("Problem with species identification in PO data.")
  }
  numSpecies <- length(POSpecies)
  numCoeffs <- numCoefficients(myFormula)

  # Initialise the return value.
  estCoeffs <- matrix(nrow=numCoeffs, ncol=numSpecies)
  colnames(estCoeffs) <- POSpecies
  errors <- as.data.frame(matrix(nrow=0, ncol=2), stringsAsFactors=FALSE)
  colnames(errors) <- c("species", "msg")
  retVal <- list(estCoeffs=estCoeffs, stdErrors=estCoeffs, errors=errors, isErrors=FALSE,
                 glmFit=NULL)

  # Get background (BG) data in right format for PPM function.  Same for all species.
  pppBG <- ppp(BG$x, BG$y, window=domainObj$owin)

  # Get the environmental covariate data at the presence-only cells
  # (background points now done elsewhere as same no matter what run!).
  namesCovars <- names(cellObj$covars)
  PO[ ,namesCovars] <- getCellVals(cellObj, PO$cell, "covars")

  # Make xy points for the PO data (uniformly random location within given cell)
  numPOPoints <- dim(PO)[1]
  resCell <- res(domainObj$mask)
  cellxrange <- c(0, resCell[1]) - (resCell[1] / 2.0)  # cell x boundaries shifted to have zero at centre!
  cellyrange <- c(0, resCell[2]) - (resCell[2] / 2.0)  # cell y boundaries shifted to have zero at centre!
  PO[ ,c("x","y")] <- getCellVals(cellObj, PO$cell, "xy")  # cell centres!
  x <- runif(numPOPoints, min=cellxrange[1], max=cellxrange[2])
  y <- runif(numPOPoints, min=cellyrange[1], max=cellyrange[2])
  PO$x <- x + PO$x
  PO$y <- y + PO$y

  # Remove response and replace x and y as PPM has these reserved.
  # No need for an area offset as these are point observations, not area observations!
  sdm.formula <- delete.response.formula(myFormula)
  sdm.formula <- replaceVarName(sdm.formula, "x", "x1")  # This works for formula without x!
  sdm.formula <- replaceVarName(sdm.formula, "y", "y1")

  # Estimate coefficients but only for the species in PO data.
  for ( species in POSpecies) {
    # Get this species' PO data.
    indWhichSpecies <- which(PO$species == species)
    thisPO <- PO[indWhichSpecies, c("x","y", namesCovars)]

    # Get presence-only (PO) data in right format for PPM function.
    pppPO <- ppp(thisPO$x, thisPO$y, window=domainObj$owin)

    # Create a quadrature scheme that contains the presence-only points and the
    # quadrature (or background) points.
    qsData <- quadscheme(data = pppPO, dummy = pppBG, method = "dirichlet")

    # Put both data together (in the same columns, not extra columns!).
    dfPPMData <- rbind(thisPO, BG[ , names(thisPO)])
    colnames(dfPPMData)[1] <- "x1"      # Rename column x as x1
    colnames(dfPPMData)[2] <- "y1"      # Rename column y as y1

    # Fit the PPM (non-stationary Poisson point process model).
    glmFit <- ppm(qsData, trend=sdm.formula, interaction=Poisson(), covariates=dfPPMData,
                  gcontrol=list(maxit=100))

    # Did the glm converge to an answer?
    if ( ! glmFit$internal$glmfit$converged ) {
      # Create error message ...
      retVal$isErrors <- TRUE
      newError <- data.frame(species=species, msg="PPM glm has not converged.", stringsAsFactors = FALSE)
      retVal$errors <- rbind(retVal$errors, newError)

    } else {
      # Save estimates ...
      retVal$estCoeffs[ ,species] <- glmFit$coef
      retVal$stdErrors[ ,species] <- summary.ppm(glmFit)$coefs.SE.CI[ ,2]
      retVal$glmFit <- glmFit
    }
  }

  # Return results.
  return(retVal)

}

#-----------------------------------------------------------------------------------------

runSDM.MsPP <- function(lambda.formula, bias.formula, surveyCells, areaSurveys, PA, PO, BG,
                        cellObj) {

  # Run the MultispeciesPP species distribution model for PO + PA data.
  #
  # Arguments ...
  # lambda.formula: a formula object that contains how to combine the information for a
  #                 species intensity function.
  # bias.formula:   a formula object that contains how to combine the information for a
  #                 sample bias intensity function.
  # surveyCells:    the cell locations of the surveys (rows of PA data).
  # areaSurveys:    the areas of the surveys (in the same order as surveyCells and PA rows).
  # PA:             the presence-absence data in a data frame (numSurveys x numSpecies)
  # PO:             the presence-only data in a data frame (two columns, cell,species)
  # BG:             the background data in a data frame (one column, cell)
  # cellObj:        a cell object that contains the environmental and sample bias covariates
  #                 for each cell in the domain

  # Numbers and names of things.
  namesSpeciesAll <- cellObj$namesSpecies
  numSpeciesAll <- cellObj$numSpecies
  numEnvirs <- dim(cellObj$covars)[2]
  namesEnvirs <- names(cellObj$covars)
  numBiases <- dim(cellObj$biases)[2]
  namesBiases <- names(cellObj$biases)
  numCoeffs <- numCoefficients(lambda.formula)
  numDeltas <- numCoefficients(bias.formula, includeIntercept = FALSE)

  # Initialise a list to return the estimated coefficients.
  retLst <- list(estCoeffs=matrix(nrow=numCoeffs, ncol=numSpeciesAll, dimnames=list(1:numCoeffs, namesSpeciesAll)),
                 stdErrors=matrix(nrow=numCoeffs, ncol=numSpeciesAll, dimnames=list(1:numCoeffs, namesSpeciesAll)),
                 gamma=rep(NA, times=numSpeciesAll),
                 delta=rep(NA, times=numDeltas),
                 warnings=matrix(nrow=0, ncol=2, dimnames = list(NULL,c("species","msg"))),
                 errors=matrix(nrow=0, ncol=2, dimnames = list(NULL,c("species","msg"))),
                 isErrors=FALSE)
  names(retLst$gamma) <- namesSpeciesAll

  # Check survey cells and area surveys are the right length.
  numSurveys <- dim(PA)[1]
  if ( length(surveyCells) != numSurveys ) {
    retLst$isErrors = TRUE
    newError <- data.frame(species="NA",
                           msg="Argument surveyCells is not the correct length.",
                           stringsAsFactors = FALSE)
    retLst$errors <- rbind(retLst$errors, newError)
    return(retLst)
  }
  if ( length(areaSurveys) != numSurveys ) {
    retLst$isErrors = TRUE
    newError <- data.frame(species="NA",
                           msg="Argument areaSurveys is not the correct length.",
                           stringsAsFactors = FALSE)
    retLst$errors <- rbind(retLst$errors, newError)
    return(retLst)
  }
  
  # Set the number and name of the species to use.
  namesSpeciesPA <- names(PA)
  numSpeciesPA <- length(namesSpeciesPA)
  namesSpeciesPO <- sort(unique(PO$species))
  numSpeciesPO <- length(namesSpeciesPO)
  namesSpeciesInBoth <- intersect(namesSpeciesPA, namesSpeciesPO)
  numSpeciesInBoth <- length(namesSpeciesInBoth)
  namesSpeciesInEither <- unique(c(namesSpeciesPA,namesSpeciesPO))

  # Write an error message for those species that we lose because they aren't in both data sets.
  # This is an error that only MsPP needs (i.e. not Count, Cloglog or PPM).
  if ( numSpeciesInBoth <  length(namesSpeciesInEither) ) {
    namesSpeciesInOnlyOne <- namesSpeciesInEither[which(! namesSpeciesInEither %in% namesSpeciesInBoth)]
    for ( species in namesSpeciesInOnlyOne ) {
      # retLst$isErrors <- TRUE
      # newError <- data.frame(species=species,
      #                        msg="This species is available in only one of the data sets (PA or PO).",
      #                        stringsAsFactors = FALSE)
      # retLst$errors <- rbind(retLst$errors, newError)
      newWarning <- data.frame(species=species,
                               msg="This species is available in only one of the data sets (PA or PO).",
                               stringsAsFactors = FALSE)
      retLst$warnings <- rbind(retLst$warnings, newWarning)
    }
  }

  # Initialise temporary data frames for each type of data.
  dfPA <- data.frame(cell=surveyCells, stringsAsFactors = FALSE)
  dfPO <- PO
  dfBG <- BG[ ,c("cell",namesEnvirs)]

  # Add the environmental covariate data at each of the data points.  BG version done elsewhere now.
  dfPA[ ,namesEnvirs] <- getCellVals(cellObj, surveyCells, item="covars")
  dfPO[ ,namesEnvirs] <- getCellVals(cellObj, PO$cell, item="covars")

  # Add the species presence-absence indicator columns (assume it is necessary for these
  # to be after the covariate values, otherwise, they could be added with cell above.)
  dfPA[ ,namesSpeciesInBoth] <- PA[ ,namesSpeciesInBoth]

  # Add the bias covariate values at each of the presence-only points and background points.
  # Exclude bias covariates that have already been added by environment covariates!
  indNewBiasNames <- ! is.element(namesBiases, namesEnvirs)
  newBiasNames <- namesBiases[indNewBiasNames]
  if ( length(newBiasNames) > 0 ) {
    tmp <- getCellVals(cellObj, PO$cell, item="biases")
    dfPO[ ,newBiasNames] <- tmp[ ,newBiasNames]
    tmp <- getCellVals(cellObj, BG$cell, item="biases")
    dfBG[ ,newBiasNames] <- tmp[ ,newBiasNames]
  }

  # Re-arrange the presence-only data into a list as required by multispeciesPP.
  lstPO <- list()
  for ( species in namesSpeciesInBoth ) {
    # Which rows of the PO data are this species?
    indWhichSpecies <- which(dfPO$species == species)

    # Add covariates (inc. x and y).
    lstPO[[species]] <- dfPO[indWhichSpecies, c("cell", namesEnvirs, newBiasNames)] # Everything but the species indicator column.
  }

  # Make the model specifications.
  sdmLambda.formula <- delete.response.formula(lambda.formula)
  sdmBias.formula <- delete.response.formula(bias.formula)

  # Call the function.
  areaDomain <- cellObj$areaCell * cellObj$numCells
  glmFit <- multispeciesPP( sdmLambda.formula, sdmBias.formula, dfPA, lstPO, dfBG,
                            quadrat.size=areaSurveys, region.size=areaDomain, control=list(maxit=100))

  # Did the glm converge to an answer?
  if ( ! glmFit$converged ) {
    retLst$isErrors <- TRUE
    retLst$msg <- "MsPP has not converged."
  } else {
    # Store the estimated coefficients for the fit.
    retLst$estCoeffs[ ,namesSpeciesInBoth] <- glmFit$species.coef[1:numCoeffs, namesSpeciesInBoth]        # alphas and betas
    retLst$gamma[namesSpeciesInBoth] <- glmFit$species.coef[numCoeffs+1, namesSpeciesInBoth]            # gammas
    retLst$delta <- glmFit$bias.coef                                            # deltas
    tmp <- readStdErrors.MsPP(glmFit$std.errs, numCoeffs, namesSpeciesInBoth,
                              length(glmFit$bias.coef), rownames(glmFit$species.coef)[1:numCoeffs])
    retLst$stdErrors[ ,namesSpeciesInBoth] <- tmp$coeff.SE[ ,namesSpeciesInBoth]
  }

  # Return results.
  return(retLst)

}

#-----------------------------------------------------------------------------------------

readStdErrors.MsPP <- function(stdErrors, numCoeffs, namesSpecies, numDeltas,
                               coeffNames=1:numCoeffs){

  # Rearrange the stdErrors, given as a vector by MsPP, into the required format.

  # Species stuff.
  namesSpeciesSorted <- sort(namesSpecies)   # output from MsPP has species in alphanumeric order!
  numSpecies <- length(namesSpecies)

  # Initialise return value.
  retMat <- matrix(data=NA, nrow = numCoeffs, ncol = numSpecies)
  colnames(retMat) <- namesSpeciesSorted
  rownames(retMat) <- coeffNames
  retVal <- list(coeff.SE=retMat, gamma.SE=matrix(NA,numSpecies,1), delta.SE=NULL)
  rownames(retVal$gamma.SE) <- namesSpeciesSorted

  # Unpack vector.
  for ( k in 1:numSpecies ) {
      ind <- (k-1)*(numCoeffs+1)
      retVal$coeff.SE[ ,k] <- stdErrors[(ind+1):(ind+numCoeffs)]
      retVal$gamma.SE[k,1] <- stdErrors[ind+numCoeffs+1]
  }
  deltaStart <- ind + numCoeffs + 2
  deltaEnd <- deltaStart + numDeltas - 1
  retVal$delta.SE <- stdErrors[deltaStart:deltaEnd]

  # Change order back to order in namesSpecies
  retVal$coeff.SE <- retVal$coeff.SE[ , namesSpecies]
  retVal$gamma.SE <- retVal$gamma.SE[namesSpecies,1]

  # Return Value
  return(retVal)
}

#-----------------------------------------------------------------------------------------

runSim <- function(i, simObj, domainObj, cellObj, envirObj, biasObj, plotObj, BG) {

  # Run one simulation with the given objects. "i" is the number of this run.
  # NB: results are organised so that this function can be used in parallel.
  # TO DO: can I remove envirObj?

  # Catch if an error occurs in this run and report (rather than stopping everything!)
  #lstErrors <- list(msg=NULL, part=NULL, type=NULL)
  tryCatchOut <- tryCatch(
    {
      message(paste(i, "Starting sim run", Sys.time()))

      # Initialise data dump values.
      fileDump <- paste0("DataDump-run", i)
      fileDump <- makeFileName(fileDump, simObj$dataDumpDir, "RData")
      PO <- NULL
      Count <- NULL
      PA <- NULL
      namesSpecies <- NULL

      # Initialise the result list object for one run.
      resLst <- initResults(1, simObj$numCoeffs, simObj$numBiases, cellObj$namesSpecies)
      resLst <- setResTryMe(simObj, resLst)

      # Make the number of individuals per cell for each species.
      message(paste(i, "Begin makeNumIndivids"))
      cellObj <- makeNumIndivids(cellObj, i)
      message(paste(i, "End makeNumIndivids"))

      # Plots related to cell object.
      if ( plotObj$countPerCell && ( i == 1 || plotObj$checkPlotsAllRuns ) )
        plotCellValues(plotObj, domainObj$mask, cellObj$cells, cellObj$N, i, "CheckCellNumIndivids",
                       titleTxts = "Count per cell of simulated individuals for ")

      # Make the presence-only data (no need for mask as individuals are already in the domain!)
      message(paste(i, "Begin makeSimPO"))
      PO <- makeSimPO(cellObj, i)
      message(paste(i, "End makeSimPO"))

      # TO DO: alter plot to work with cell level data.
      if ( plotObj$POData  && ( i == 1 || plotObj$checkPlotsAllRuns ) )
        plotPOPoints(plotObj, domainObj$mask, cellObj, PO, i, "CheckPOPoints")

      # Make the survey locations.
      message(paste(i, "Begin makeSurveyLocations"))
      surveyObj <- makeSurveyLocations(domainObj$mask, cellObj, simObj$numSurveys, simObj$minSurveyArea,
                                       simObj$maxSurveyArea, simObj$bias.formula, simObj$delta,
                                       simObj$numClusters, simObj$widthCluster)
      message(paste(i, "End makeSurveyLocations"))
      if ( plotObj$surveyAreas && ( i == 1 || plotObj$checkPlotsAllRuns ))
        plotSurveyAreas(plotObj, domainObj$mask, cellObj, surveyObj, "CheckSurveyAreas")

      # Make the count data
      message(paste(i, "Begin makeSimCount"))
      Count <- makeSimCount(surveyObj, cellObj, i)
      message(paste(i, "End makeSimCount"))
      if ( plotObj$countData && ( i == 1 || plotObj$checkPlotsAllRuns ) )
        plotSurveyCount(domainObj, plotObj, Count, cellObj$namesSpecies, i, "CheckCountData")

      # Make the presence-absence data
      message(paste(i, "Begin makeSimPA"))
      PA <- makeSimPA(Count, cellObj$namesSpecies)
      resLst <- setResNumPresences(resLst, PA[ ,cellObj$namesSpecies], PO$species)
      message(paste(i, "End makeSimPA"))
      # TO DO: alter plot to work with cell level data.
      if ( plotObj$PAData && ( i == 1 || plotObj$checkPlotsAllRuns ) )
        plotSurveyPA(domainObj, plotObj, surveyObj, PA, cellObj$namesSpecies, i, "CheckPAData")

      # Restrict SDM usage to just those species with enough data.
      prevLst <- checkPrevalence(cellObj$namesSpecies, PA, PO, simObj$minPrevalence)
      if ( prevLst$isErrors ) {
        resLst <- setResError(resLst, 1, species = prevLst$errors$species,
                              msg = prevLst$errors$msg)
      }
      if ( prevLst$isWarnings ) {
        resLst <- setResError(resLst, 1, species = prevLst$warnings$species, 
                              msg = prevLst$warnings$msg, isWarning=TRUE)
      }

      # Run the SDMs to estimate the true intensity (and the MSE of the estimated coefficients).
      if ( simObj$useSDM.Count ) {
        # glm with count data.
        message(paste(i, "Begin count SDM"))
        tryRes <- try(runSDM.Count(simObj$lambda.formula, Count$cell,
                                   cellObj$covars[surveyObj$rowsInCells, ],
                                   Count[ ,prevLst$namesSpeciesPA], Count$area))
        if ( inherits(tryRes, "try-error") ) {
          # An error occured that stopped the function.
          #resLst <- setResError(resLst, 1:numSpecies, "Count", 1, tryRes[1])
          resLst <- setResError(resLst, 1, "AB", msg = tryRes[1])
        } else {
          # Some errors may have occured inside the function that did not require it to stop.
          if ( tryRes$isError ) {
            resLst <- setResError(resLst, 1, "AB", tryRes$errors$species, tryRes$errors$msg)
          }

          # Set the estimated coefficients.
          resLst$Count <- setResEstCoeffs(resLst$Count, tryRes$estCoeffs, 1, tryRes$stdErrors)

          # Calculate the mse for these estimated coefficients and the true coefficients.
          resStats <- makeResStats(resLst$Count$coeffs[ , ,1], simObj$lambda.formula, cellObj, resLst$stats)
          resLst$Count <- setResStats(resLst$Count, resStats, 1)
        }
        message(paste(i, "End count SDM"))
      }

      if ( simObj$useSDM.Cloglog ) {
        # glm with PA data.
        message(paste(i, "Begin cloglog SDM"))
        tryRes <- try(runSDM.Cloglog(simObj$lambda.formula, PA$cell,
                                     cellObj$covars[surveyObj$rowsInCells, ],
                                     PA[ ,prevLst$namesSpeciesPA], PA$area))
        if ( inherits(tryRes, "try-error") ) {
          # An error occured that stopped the function.
          resLst <- setResError(resLst, 1, "PA", msg = tryRes[1])
        } else {
          # Some errors may have occured inside the function that did not require it to stop.
          if ( tryRes$isError ) {
            resLst <- setResError(resLst, 1, "PA", tryRes$errors$species,
                                  tryRes$errors$msg)
          }

          # Set the estimated coefficients.
          resLst$Cloglog <- setResEstCoeffs(resLst$Cloglog, tryRes$estCoeffs, 1, tryRes$stdErrors)

          # Calculate the mse for these estimated coefficients and the true coefficients.
          resStats <- makeResStats(resLst$Cloglog$coeffs[ , ,1], simObj$lambda.formula, cellObj, resLst$stats)
          resLst$Cloglog <- setResStats(resLst$Cloglog, resStats, 1)
        }
        message(paste(i, "End cloglog SDM"))
      }

      if ( simObj$useSDM.PPM ) {
        # glm with PO and BG data.
        message(paste(i, "Begin ppm SDM"))
        tryRes <- try(runSDM.PPM(simObj$lambda.formula, PO[prevLst$whichPORows, ], BG,
                                 domainObj, cellObj))

        if ( inherits(tryRes, "try-error") ) {
          # An error occured that stopped the function.
          resLst <- setResError(resLst, 1, "PO", msg = tryRes[1])
        } else {
          # Some errors may have occured inside the function that did not require it to stop.
          if ( tryRes$isError ) {
            resLst <- setResError(resLst, 1, "PO", tryRes$errors$species,
                                  tryRes$errors$msg)
          }

          # Set the estimated coefficients.
          resLst$PPM <- setResEstCoeffs(resLst$PPM, tryRes$estCoeffs, 1, tryRes$stdErrors)

          # Calculate the mse for these estimated coefficients and the true coefficients.
          resStats <- makeResStats(resLst$PPM$coeffs[ , ,1], simObj$lambda.formula, cellObj, resLst$stats)
          resLst$PPM <- setResStats(resLst$PPM, resStats, 1)
        }
        message(paste(i, "End ppm SDM"))
      }

      if ( simObj$useSDM.MsPP ) {
        # glm with PA, PO and BG data.
        message(paste(i, "Begin MsPP SDM"))
        tryRes <- try(runSDM.MsPP(simObj$lambda.formula, simObj$bias.formula, PA$cell, PA$area,
                                  PA[ ,prevLst$namesSpeciesPA], PO[prevLst$whichPORows, ],
                                  BG, cellObj))

        if ( inherits(tryRes, "try-error") ) {
          # An error occured that stopped the function.
          resLst <- setResError(resLst, 1, "MsPP", msg = tryRes[1])

        } else {
          # Some errors may have occured inside the function that did not require it to stop.
          if ( tryRes$isError ) {
            resLst <- setResError(resLst, 1, "MsPP", species = tryRes$errors$species,
                                  msg = tryRes$errors$msg)
          }

          # Set the estimated coefficients.
          resLst$MsPP <- setResEstCoeffs(resLst$MsPP, tryRes$estCoeffs, 1, tryRes$stdErrors,
                                         tryRes$gamma, tryRes$delta)

          # Calculate the mse for these estimated coefficients and the true coefficients.
          resStats <- makeResStats(tryRes$estCoeffs, simObj$lambda.formula, cellObj, resLst$stats)
          resLst$MsPP <- setResStats(resLst$MsPP, resStats, 1)
        }
        message(paste(i, "End MsPP SDM"))
      }

      # Check if there has been an error in any of the methods.
      if ( dim(resLst$errors)[1] > 0 ) {
        # Dump data necessary for an SDM run (for analysis if there is an error).
        save(i, PO, Count, PA, BG, cellObj, surveyObj, file = fileDump)

      } else if ( i == 1 || i == simObj$numRuns) {
        # This ensures that there are at least two data dumps to be used for problem analysis.
        # Used a different file name but it is the same data as the error dump.
        # Different file name so that they are not mistaken as an error having occured!
        fileExamp <- paste0("DataExample-run", i)
        fileExamp <- makeFileName(fileExamp, simObj$dataDumpDir, "RData")
        save(i, PO, Count, PA, BG, cellObj, surveyObj, file = fileExamp)
      }

      # Return results.
      message(paste(i,"Finished sim run", Sys.time()))
      return(resLst)
    },
    error = function(cond) {
      # What to do if there was an error in other bits of code.
      msg <- conditionMessage(cond)
      msg <- paste0("ERROR on run ", i, ": ", msg)
      resLst <- setResError(resLst, 1, msg = msg)

      # Dump data necessary for an SDM run (for analysis if there is an error).
      save(i, PO, Count, PA, BG, cellObj, surveyObj, file = fileDump)

      # Return results.
      return(resLst)
    }
  )

  # Any uncaught errors?
  if ( inherits(tryCatchOut, "try-error")) {
    resLst <- setResError(resLst, 1, msg = tryRes$errors$msg)
    return(resLst)
  }

  # Return value.
  return(tryCatchOut)

}

#-----------------------------------------------------------------------------------------

checkPrevalence <- function(namesSpecies, PA, PO, minPrevalence, isPALocCell=TRUE){

  # Checks which species have enough data (using minPrevalence as the limit of acceptable).
  # Returns those species that have enough for each data set and for both data sets.
  # NB: Assumes that the AB (i.e. count) data is acceptable for the same species as the PA data!
  #
  # Arguments ...
  # namesSpecies:  the names of the species to be included in the simulation.
  # PA:            the presence-absence data points in a data frame (numSurveys x (numSpecies+2)).
  # PO:            the presence-only observation points in a data frame (two columns: cell, species).
  # minPrevalence: minimum number of oberservations per species that are required to run glm.
  # isPALocCell:   whether or not the PA locations are expressed as cell numbers (TRUE) or
  #                as geographic points (FALSE) using two columns (one for x and one for y).

  # Intialise return value.
  retVal <- list(namesSpeciesPA=namesSpecies, whichPORows=1:dim(PO)[1],
                 isErrors=FALSE, errors=NULL, isWarnings=FALSE, warnings)

  # Numbers and names of things ...
  numSurveys <- dim(PA)[1]
  if ( isPALocCell ) {
    PASpecies <- colnames(PA)[c(-1,-2)]     # exclude "cell" and "area" columns.
  } else {
    PASpecies <- colnames(PA)[c(-1,-2,-3)]  # excluse "x", "y" and "area" columns
  }
  POSpecies <- unique(PO$species)
  notThesePORows <- NULL

  # Check for species that are in data but not in names.
  if ( ! setequal(POSpecies, intersect(POSpecies, namesSpecies)) ) {
    retVal$isErrors <- TRUE
    newError <- data.frame(species=NA,
                           msg="Problem with species identification in PO data.",
                           stringsAsFactors = FALSE)
    retVal$errors <- rbind(retVal$errors, newError)
    return(retVal)
  }
  if ( ! setequal(PASpecies, intersect(PASpecies, namesSpecies)) ) {
    retVal$isErrors <- TRUE
    newError <- data.frame(species=NA,
                           msg="Problem with species identification in PA data.",
                           stringsAsFactors = FALSE)
    retVal$errors <- rbind(retVal$errors, newError)
    return(retVal)
  }

  for ( species in namesSpecies ) {
    # For PA data ...
    if ( is.na(match(species,PASpecies)) ) {
      # Missing species column in PA data?
      retVal$isWarnings <- TRUE
      newWarning <- data.frame(species=species,
                               msg="This species is missing from the AB/PA data columns.",
                               stringsAsFactors = FALSE)
      retVal$warnings <- rbind(retVal$warnings, newWarning)
      indWhich <- which(retVal$namesSpeciesPA == species)
      retVal$namesSpeciesPA <- retVal$namesSpeciesPA[-indWhich]

    } else if ( sum(PA[ ,species]) < minPrevalence ) {
      # Number of presences is less than the minimum prevalence required (including no presences!).
      retVal$isWarnings <- TRUE
      newWarning <- data.frame(species=species,
                               msg="This species does not have enough presence information in the AB/PA data.",
                               stringsAsFactors = FALSE)
      retVal$warnings <- rbind(retVal$warnings, newWarning)
      indWhich <- which(retVal$namesSpeciesPA == species)
      retVal$namesSpeciesPA <- retVal$namesSpeciesPA[-indWhich]

    } else if ( (numSurveys - sum(PA[ ,species])) < minPrevalence ) {
      # Number of absences is less than the minimum prevalence required (including no absences!).
      retVal$isWarnings <- TRUE
      newWarning <- data.frame(species=species,
                               msg="This species does not have enough absence information in the AB/PA data.",
                               stringsAsFactors = FALSE)
      retVal$warnings <- rbind(retVal$warnings, newWarning)
      indWhich <- which(retVal$namesSpeciesPA == species)
      retVal$namesSpeciesPA <- retVal$namesSpeciesPA[-indWhich]

    } else {
      # All good.
    }

    # For PO data ...
    indWhichRows <- which(PO$species == species)
    if ( length(indWhichRows) == 0 ) {
      # No observations for this species.
      retVal$isWarnings <- TRUE
      newWarning <- data.frame(species=species,
                               msg="This species is missing from the PO observations.",
                             stringsAsFactors = FALSE)
      retVal$warnings <- rbind(retVal$warnings, newWarning)
      
    } else if ( length(indWhichRows) < minPrevalence ) {
      # Number of presences is less than the minimum prevalence required.
      retVal$isWarnings <- TRUE
      newWarning <- data.frame(species=species,
                               msg="This species does not have enough presence information in the PO observations.",
                               stringsAsFactors = FALSE)
      retVal$warnings <- rbind(retVal$warnings, newWarning)
      notThesePORows <- c(notThesePORows, indWhichRows)

    } else {
      # Enough data for this species.
    }

  }

  # Reset acceptable rows for PO data, if necessary.
  if ( ! is.null(notThesePORows) ) retVal$whichPORows <- retVal$whichPORows[-notThesePORows]

  # Return value.
  return(retVal)

}

#-----------------------------------------------------------------------------------------
