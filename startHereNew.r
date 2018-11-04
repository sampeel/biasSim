# File to run experiments and plot results.

library(raster)
library(spatstat)
library(multispeciesPP)                         # Github: sampeel version (not wfithian)
library(sp)
library(rgeos)
library(parallel)
library(vioplot)
library(RColorBrewer)

source("utils.r")
source("cellsObj.r")
source("dataObj.r")
source("domainObj.r")
source("plottingObj.r")
source("resultsObject.r")
source("settingsObj.r")
source("simFuncs.r")
source("surveysObj.r")
source("estimateCoeffsNew.R")
source("plotLayers.r")
source("scenariosObject.r")
#source("tmpMsPPFuncs.r")

setwd("/perm_storage/home/sampeel/chap1/sim15")

#-----------------------------
# General Setup ...
#-----------------------------

highNumPA <- 3500L                       # Lots of survey locations
lowNumPA <- 500L                         # Not many survey locations
highGammaMultiplier <- 1.1               # More PO points per species
lowGammaMultiplier <- 1.3                # Less PO points per species
highDeltaMultiplier <- 2.0               # Lots of sample bias
lowDeltaMultiplier <- 0.0                # No sample bias
highNumClusters <- 500L                  # Slightly clustered survey locations
# (NB: no clustering or random survey locations is numClusters = 0)
lowNumClusters <- 100L                   # Highly clustered survey locations
tinyNumClusters <- 20L                   # Extremely clustered survey locations
testNumRuns <- 10L    
smallNumRuns <- 100L
lowNumRuns <- 1000L
highNumRuns <- 5000L
hugeNumRuns <- 10000L
truncRange <- c(-2,2)                    # Truncate the y-axis to this range on summary plots
randomSeed <- 10

############################
#                          #
#       Experiments        #
#                          #
############################

#--------------------------------
# SETUP - Find low number of PA
#--------------------------------

# Run a few different values of the number of survey locations 
deltaMultiplier <- 0.0
numClusters <- as.integer(0)
numPA <-  as.integer(seq(from=450, to=50, by=-50)) #as.integer(c(seq(from=500,to=5000,by=500),seq(from=6000,to=15000,by=1000)))
gammaMultiplier <- 1.0
numRuns <- highNumRuns 
scenariosDir <- paste0(getwd(), "/", "Output-SetupLowPA-runs", format(numRuns), "-centred")
scenariosPrettyNames <- as.character(numPA)
xAxisTitle <- "number of samples"

# Run scenarios ...
scenariosObj <- initialiseScenarios(scenariosDir, numPA, gammaMultiplier, deltaMultiplier, 
                                    numClusters, numRuns, prettyNames = scenariosPrettyNames)
retLst <- runScenarios(scenariosObj, useSDM=c("PA","MsPP"), randomSeed = randomSeed, 
                       covarsCentred = TRUE) #, numCoresUse = 14)  

# Save data.
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)

# Errors ...
deSink()
showAllScenariosErrors(retLst$resLstAll) 
showAllScenariosWarnings(retLst$resLstAll)
numSuccessfulRunsScenarios(scenariosObj, retLst$resLstAll)

# Plot response side summary statistics ...
namesStats <- c("Coefficients", "Correlation", "Percent Diff Total Abundance")
stats <- makeScenarioStats(scenariosObj, retLst$resLstAll, whichStats = c(1,2,3), 
                           whichSDMs=c("PA","MsPP"))
retLst <- saveScenariosSummaryStats(stats, retLst)

plotScenariosSummaryLambda(stats[[1]], "Intercept", whichCoeffs = 1, plotSDMs = c("PA","MsPP"), 
                           diffPORange = c(TRUE,FALSE), bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, ylimVals = c(0,1))
plotScenariosSummaryLambda(stats[[1]], "Coefficients", whichCoeffs = 2:4, plotSDMs = c("PA","MsPP"), 
                           diffPORange = c(TRUE,FALSE), bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, ylimVals = c(0,1))
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], retLst$statsObj$namesStats[1], 
                           plotSDMs = c("PA","MsPP"), vioPlots = TRUE, plotDevice="png",
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)
plotScenariosSummaryLambda(retLst$statsObj$stats[[2]], retLst$statsObj$namesStats[2], 
                           plotSDMs = c("PA","MsPP"), vioPlots = TRUE, plotDevice="png",
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)


#--------------------------------
# SETUP - Find high number of PA
#--------------------------------

# Run a few different values of the number of survey locations 
deltaMultiplier <- 0.0
numClusters <- as.integer(0)
numPA <-  as.integer(seq(from=3000, to=15000, by=500)) #as.integer(c(seq(from=500,to=5000,by=500),seq(from=6000,to=15000,by=1000)))
gammaMultiplier <- 1.0
numRuns <- highNumRuns 
scenariosDir <- paste0(getwd(), "/", "Output-SetupPA-runs", format(numRuns), "-centred")
scenariosPrettyNames <- as.character(numPA)
xAxisTitle <- "number of samples"

# Run scenarios ...
scenariosObj <- initialiseScenarios(scenariosDir, numPA, gammaMultiplier, deltaMultiplier, 
                                    numClusters, numRuns, prettyNames = scenariosPrettyNames)
retLst <- runScenarios(scenariosObj, useSDM=c("AB","PA"), randomSeed = randomSeed, 
                       covarsCentred = TRUE) #, numCoresUse = 14)  

# Save data.
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)

# Errors ...
deSink()
showAllScenariosErrors(retLst$resLstAll) 
showAllScenariosWarnings(retLst$resLstAll)
numSuccessfulRunsScenarios(scenariosObj, retLst$resLstAll)

# Plot response side summary statistics ...
namesStats <- c("Coefficients", "Correlation", "Percent Diff Total Abundance")
stats <- makeScenarioStats(scenariosObj, retLst$resLstAll, whichStats = c(1,2,3), 
                           whichSDMs=c("PA"))
retLst <- saveScenariosSummaryStats(stats, retLst)
plotScenariosSummaryLambda(stats[[1]], "Intercept", whichCoeffs = 1, plotSDMs = c("PA"), 
                           diffPORange = c(TRUE,FALSE), bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)
plotScenariosSummaryLambda(stats[[1]], "Coefficients", whichCoeffs = 2:4, plotSDMs = c("PA"), 
                           diffPORange = c(TRUE,FALSE), bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], retLst$statsObj$namesStats[1], 
                           plotSDMs = c("PA"), vioPlots = TRUE, plotDevice="png",
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)
plotScenariosSummaryLambda(retLst$statsObj$stats[[2]], retLst$statsObj$namesStats[2], 
                           plotSDMs = c("AB","PA"), vioPlots = TRUE, plotDevice="png",
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)

# Reload data.
scenariosDir <- paste0(getwd(), "/", "Output-SetupPA-runs", format(highNumRuns), "-centred")
retLst <- loadScenariosResults(scenariosDir)
scenariosObj <- retLst$scenariosObj
scenariosObj$scenariosDir <- scenariosDir
scenariosObj <- addPrettyNames(scenariosObj, scenariosPrettyNames)
retLst$statsObj <- addPrettyNamesToStats(retLst$statsObj, scenariosObj$prettyNamesScenarios)

# Save data (will include stats and prettynames if these have been added since last save).
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)

#--------------------------------
# 0.2 SETUP - Find high number of PO
#--------------------------------

# Run a few different values of the numbers of PO data
deltaMultiplier <- 0.0  # no bias!
numClusters <- 0        # random survey locations
numPA <- 3500           
gammaMultiplier <- seq(from=1.30, to=0.90, by=-0.05) 
numRuns <- highNumRuns
scenariosDir <- paste0(getwd(), "/", "Output-SetupPO2-runs", format(numRuns), "-centred")
scenariosPrettyNames <- c("600","1000","1700","2900","4800","8000","13500","22600","38000")  #c("XXSmall","XSmall", "Small","Medium","Large","XLarge","XXLarge","Huge","XHuge")
xAxisTitle <- "approximate number of PO observations"

# Run scenarios ...
scenariosObj <- initialiseScenarios(scenariosDir, numPA, gammaMultiplier, deltaMultiplier, 
                                    numClusters, numRuns, prettyNames = scenariosPrettyNames)
retLst <- runScenarios(scenariosObj, useSDM=c("PO", "MsPP"), randomSeed = randomSeed, 
                       covarsCentred = TRUE) # , numCoresUse = 14) 

# Save data.
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)

# Errors ...
deSink()
showAllScenariosErrors(retLst$resLstAll) 
showAllScenariosWarnings(retLst$resLstAll)
numSuccessfulRunsScenarios(scenariosObj, retLst$resLstAll)

# Plot response side summary statistics ...
namesStats <- c("Coefficients", "Correlation", "Percent Diff Total Abundance")
stats <- makeScenarioStats(retLst$scenariosObj, retLst$resLstAll, whichStats = c(1,2,3), 
                           whichSDMs=c("MsPP","PO"))
retLst <- saveScenariosSummaryStats(stats, retLst, namesStats)
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "Intercept", whichCoeffs = 1, 
                           plotSDMs = c("MsPP","PO"), diffPORange = c(TRUE,FALSE), 
                           bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "CovarCoeffs", whichCoeffs = 2:4, 
                           plotSDMs = c("MsPP","PO"), diffPORange = c(TRUE,FALSE), 
                           bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)
plotScenariosSummaryLambda(retLst$statsObj$stats[[2]], retLst$statsObj$namesStats[2], 
                           plotSDMs = c("MsPP","PO"), vioPlots = TRUE, plotDevice="png",
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, horizontalLines = c(1,0))
plotScenariosSummaryLambda(retLst$statsObj$stats[[3]], retLst$statsObj$namesStats[3], 
                           plotSDMs = c("MsPP"), vioPlots = TRUE, plotDevice="png",
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)

# Reload data.
scenariosDir <- paste0(getwd(), "/", "Output-SetupPO2-runs", format(highNumRuns), "-centred")
retLst <- loadScenariosResults(scenariosDir)
scenariosObj <- retLst$scenariosObj
scenariosObj$scenariosDir <- scenariosDir
scenariosObj <- addPrettyNamesToScenarios(scenariosObj, scenariosPrettyNames)
retLst$statsObj <- addPrettyNamesToStats(retLst$statsObj, scenariosObj$prettyNamesScenarios)


# Save data (will include stats and prettynames if these have been added since last save).
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)

#-----------------------------
# 0.3 BENCHMARK ...
#-----------------------------

# Run a few different values of the number of surveys 
deltaMultiplier <- lowDeltaMultiplier    # i.e. no sample bias
numClusters <- 0                         
strClusters <- "random"
numPA <- lowNumPA
gammaMultiplier <- lowGammaMultiplier
numRuns <- testNumRuns
scenariosDir <- paste0(getwd(), "/", "Output-Benchmark-runs", format(numRuns), "-centred")
scenariosPrettyNames <- NULL
xAxisTitle <- NULL

# Run scenarios ...
scenariosObj <- initialiseScenarios(scenariosDir, numPA, gammaMultiplier, deltaMultiplier, 
                                    numClusters, numRuns, prettyNames = scenariosPrettyNames)
retLst <- runScenarios(scenariosObj, randomSeed = randomSeed, covarsCentred = TRUE)    

# Save data.
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)

# Errors ...
deSink()
showAllScenariosErrors(retLst$resLstAll) 
showAllScenariosWarnings(retLst$resLstAll)
numSuccessfulRunsScenarios(scenariosObj, retLst$resLstAll)

# Plot response side summary statistics ...
namesStats <- c("Coefficients", "Correlation", "Percent Diff Total Abundance")
stats <- makeScenarioStats(scenariosObj, retLst$resLstAll, whichStats = c(1,2,3), 
                           whichSDMs=c("AB","PA","MsPP","PO"))
retLst <- saveScenariosSummaryStats(stats, retLst, namesStats)
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "Intercept", whichCoeffs = 1, 
                           plotSDMs = c("PA", "MsPP", "PO"), diffPORange = c(TRUE,FALSE), 
                           bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "CovarCoeffs", whichCoeffs = 2:4, 
                           plotSDMs = c("PA", "MsPP","PO"), diffPORange = c(FALSE,FALSE), 
                           bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)
plotScenariosSummaryLambda(retLst$statsObj$stats[[2]], retLst$statsObj$namesStats[2], 
                           plotSDMs = c("PA", "MsPP","PO"), vioPlots = TRUE, plotDevice="png",
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, horizontalLines = c(1,0))
plotScenariosSummaryLambda(retLst$statsObj$stats[[3]], retLst$statsObj$namesStats[3], 
                           plotSDMs = c("PA", "MsPP"), vioPlots = TRUE, plotDevice="png",
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)


# Reload data.
scenariosDir <- paste0(getwd(), "/", "Output-Benchmark-runs", format(highNumRuns), "-centred")
retLst <- loadScenariosResults(scenariosDir)
scenariosObj <- retLst$scenariosObj
scenariosObj$scenariosDir <- scenariosDir
scenariosObj <- addPrettyNamesToScenarios(scenariosObj, scenariosPrettyNames)
retLst$statsObj <- addPrettyNamesToStats(retLst$statsObj, scenariosObj$prettyNamesScenarios)

# Save data (will include stats and prettynames if these have been added since last save).
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)


#-----------------------------
# 1.1 REDUCE NUMBER OF PA
#-----------------------------

# Run a few different values of the number of surveys 
deltaMultiplier <- lowDeltaMultiplier                            # Unbiased sampling.
numClusters <- as.integer(0)                                     # Random survey locations
numPA <- as.integer(seq(from=lowNumPA, to=highNumPA, by=500))
gammaMultiplier <- highGammaMultiplier
numRuns <- highNumRuns 
scenariosDir <- paste0(getwd(), "/", "Output-DiffNumPA-runs", format(numRuns), "-centred")
scenariosPrettyNames <- as.character(numPA)
xAxisTitle <- "number of samples"

# Run scenarios ...
scenariosObj <- initialiseScenarios(scenariosDir, numPA, gammaMultiplier, deltaMultiplier, 
                                    numClusters, numRuns, prettyNames = scenariosPrettyNames)
retLst <- runScenarios(scenariosObj, useSDM = c("PA", "MsPP", "PO"), randomSeed = randomSeed, 
                       covarsCentred = TRUE)    

# Save data.
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)

# Errors ...
deSink()
showAllScenariosErrors(retLst$resLstAll) 
showAllScenariosWarnings(retLst$resLstAll)
numSuccessfulRunsScenarios(scenariosObj, retLst$resLstAll)

# Plot unbiasedness and efficiency for each SDM ...
plotScenariosSummaries2(scenariosObj, retLst$simObj$coeffs, retLst$resLstAll,
                        plotSDMs = c("PA", "MsPP", "PO"), plotDevice="png", vioPlots = TRUE, 
                        absStats = TRUE, diffPORanges = c(TRUE,FALSE), xlab=xAxisTitle)

# Plot response side summary statistics ...
namesStats <- c("Coefficients", "Correlation", "Percent Diff Total Abundance")
stats <- makeScenarioStats(retLst$scenariosObj, retLst$resLstAll, whichStats = c(1,2,3), 
                           whichSDMs=c("PA","MsPP","PO"))
retLst <- saveScenariosSummaryStats(stats, retLst, namesStats)
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "Intercept", whichCoeffs = 1, 
                           plotSDMs = c("PA", "MsPP", "PO"), diffPORange = c(TRUE,FALSE), 
                           bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "CovarCoeffs", whichCoeffs = 2:4, 
                           plotSDMs = c("PA", "MsPP","PO"), diffPORange = c(FALSE,FALSE), 
                           bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)
plotScenariosSummaryLambda(retLst$statsObj$stats[[2]], retLst$statsObj$namesStats[2], 
                           plotSDMs = c("PA", "MsPP","PO"), plotDevice="png",
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, horizontalLines = c(1,0))
plotScenariosSummaryLambda(retLst$statsObj$stats[[3]], retLst$statsObj$namesStats[3], 
                           plotSDMs = c("PA", "MsPP"), plotDevice="png",
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)
plotScenariosSummaryLambda(retLst$statsObj$stats[[3]], retLst$statsObj$namesStats[3], 
                           plotSDMs = c("PA","MsPP"), plotDevice="png",
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, ylimVals = c(0.0,0.1,0.0,1.0))


# Reload data.
scenariosDir <- paste0(getwd(), "/", "Output-DiffNumPA-runs", format(highNumRuns), "-centred")
retLst <- loadScenariosResults(scenariosDir)
scenariosObj <- retLst$scenariosObj
scenariosObj$scenariosDir <- scenariosDir
scenariosObj <- addPrettyNamesToScenarios(scenariosObj, scenariosPrettyNames)
retLst$statsObj <- addPrettyNamesToStats(retLst$statsObj, scenariosObj$prettyNamesScenarios)


# Save data (will include stats and prettynames if these have been added since last save).
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)


#-----------------------------
# 1.2 REDUCE NUMBER OF PO
#-----------------------------

# Run a few different values of the number of surveys 
deltaMultiplier <- lowDeltaMultiplier
numClusters <- as.integer(0)                                     # Random survey locations
numPA <- highNumPA
gammaMultiplier <- seq(from=lowGammaMultiplier, to=highGammaMultiplier, by=-0.05)
numRuns <- highNumRuns
scenariosDir <- paste0(getwd(), "/", "Output-DiffNumPO-runs", format(numRuns), "-centred")
scenariosPrettyNames <- c("600","1000","1700","2900","4800")
xAxisTitle <- "approximate number of PO observations"

# Run scenarios ...
scenariosObj <- initialiseScenarios(scenariosDir, numPA, gammaMultiplier, deltaMultiplier, 
                                    numClusters, numRuns, prettyNames = scenariosPrettyNames)
retLst <- runScenarios(scenariosObj, useSDM = c("PA","MsPP","PO"), randomSeed = randomSeed, 
                       covarsCentred = TRUE)    

# Save data.
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)

# Errors ...
deSink()
showAllScenariosErrors(retLst$resLstAll) 
showAllScenariosWarnings(retLst$resLstAll)
numSuccessfulRunsScenarios(scenariosObj, retLst$resLstAll)

# Plot response side summary statistics ...
namesStats <- c("Coefficients", "Correlation", "Percent Diff Total Abundance")
stats <- makeScenarioStats(retLst$scenariosObj, retLst$resLstAll, whichStats = c(1,2,3), 
                           whichSDMs=c("PA","MsPP","PO"))
retLst <- saveScenariosSummaryStats(stats, retLst, namesStats)
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "Intercept", whichCoeffs = 1, 
                           plotSDMs = c("PA", "MsPP", "PO"), diffPORange = c(TRUE,FALSE), 
                           bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "CovarCoeffs", whichCoeffs = 2:4, 
                           plotSDMs = c("PA", "MsPP","PO"), diffPORange = c(TRUE,FALSE), 
                           bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)
plotScenariosSummaryLambda(retLst$statsObj$stats[[2]], retLst$statsObj$namesStats[2], 
                           plotSDMs = c("PA", "MsPP","PO"), plotDevice="png",
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, horizontalLines = c(1,0))
plotScenariosSummaryLambda(retLst$statsObj$stats[[3]], retLst$statsObj$namesStats[3], 
                           plotSDMs = c("PA", "MsPP"), plotDevice="png",
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)

# Reload data.
scenariosDir <- paste0(getwd(), "/", "Output-DiffNumPO-runs", format(highNumRuns), "-centred")
retLst <- loadScenariosResults(scenariosDir)
scenariosObj <- retLst$scenariosObj
scenariosObj$scenariosDir <- scenariosDir
scenariosObj <- addPrettyNamesToScenarios(scenariosObj, scenariosPrettyNames)
retLst$statsObj <- addPrettyNamesToStats(retLst$statsObj, scenariosObj$prettyNamesScenarios)


# Save data (will include stats and prettynames if these have been added since last save).
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)


#-----------------------------
# 1.3 INCREASE SAMPLE BIAS
#-----------------------------

# Run a few different values of bias (i.e. deltaMultiplier)
deltaMultiplier <- seq(from=lowDeltaMultiplier, to=highDeltaMultiplier, by=0.5)
numClusters <- 0
numPA <- highNumPA
gammaMultiplier <- highGammaMultiplier
numRuns <- highNumRuns
scenariosDir <- paste0(getwd(), "/", "Output-SampBias-runs", format(numRuns), "-centred")
scenariosPrettyNames <- as.character(format(deltaMultiplier))
xAxisTitle <- "fraction of true sampling bias"
xAxisLabelStyle <- 0

# Run scenarios ...
scenariosObj <- initialiseScenarios(scenariosDir, numPA, gammaMultiplier, deltaMultiplier, 
                                    numClusters, numRuns, prettyNames = scenariosPrettyNames)
retLst <- runScenarios(scenariosObj, useSDM = c("PA","MsPP","PO"), randomSeed = randomSeed, 
                       covarsCentred = TRUE)    

# Save data.
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)

# Errors ...
deSink()
showAllScenariosErrors(retLst$resLstAll) 
showAllScenariosWarnings(retLst$resLstAll)
numSuccessfulRunsScenarios(scenariosObj, retLst$resLstAll)

# Plot response side summary statistics ...
namesStats <- c("Coefficients", "Correlation", "Percent Diff Total Abundance")
stats <- makeScenarioStats(retLst$scenariosObj, retLst$resLstAll, whichStats = c(1,2,3), 
                           whichSDMs=c("PA","MsPP","PO"))
retLst <- saveScenariosSummaryStats(stats, retLst, namesStats)
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "Intercept", whichCoeffs = 1, 
                           plotSDMs = c("PA", "MsPP", "PO"), diffPORange = c(TRUE,FALSE), 
                           bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "CovarCoeffs", whichCoeffs = 2:4, 
                           plotSDMs = c("PA", "MsPP","PO"), diffPORange = c(TRUE,FALSE), 
                           bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)
plotScenariosSummaryLambda(retLst$statsObj$stats[[2]], retLst$statsObj$namesStats[2], 
                           plotSDMs = c("PA", "MsPP","PO"), vioPlots = TRUE, plotDevice="png",
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, horizontalLines = c(1,0))
plotScenariosSummaryLambda(retLst$statsObj$stats[[3]], retLst$statsObj$namesStats[3], 
                           plotSDMs = c("PA", "MsPP"), vioPlots = TRUE, plotDevice="png",
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)


# Reload data.
scenariosDir <- paste0(getwd(), "/", "Output-SampBias-runs", format(highNumRuns), "-centred")
retLst <- loadScenariosResults(scenariosDir)
scenariosObj <- retLst$scenariosObj
scenariosObj$scenariosDir <- scenariosDir
scenariosObj <- addPrettyNamesToScenarios(scenariosObj, scenariosPrettyNames)
retLst$statsObj <- addPrettyNamesToStats(retLst$statsObj, scenariosObj$prettyNamesScenarios)


# Save data (will include stats and prettynames if these have been added since last save).
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)


#-----------------------------
# 1.4 REDUCE NUMBER OF SURVEYS (i.e. number of clusters)
#-----------------------------

# Run a random and increasingly clustered survey locations.
deltaMultiplier <- lowDeltaMultiplier
numPA <- highNumPA
numClusters <- c(0, 500, 100, 50, 20, 10, 5)   # NB: a cluster of zero is no clusters OR fully random surveying
gammaMultiplier <- highGammaMultiplier
numRuns <- highNumRuns
scenariosDir <- paste0(getwd(), "/", "Output-ClustSurvLocs2-runs", format(numRuns), "-centred")
scenariosPrettyNames <- c(paste0("r", numPA), numClusters[-1])
xAxisTitle <- "number of surveys"

# Run scenarios ...
scenariosObj <- initialiseScenarios(scenariosDir, numPA, gammaMultiplier, deltaMultiplier, 
                                    numClusters, numRuns, prettyNames = scenariosPrettyNames)
retLst <- runScenarios(scenariosObj, useSDM = c("AB","PA","MsPP","PO"), randomSeed = randomSeed, 
                       covarsCentred = TRUE) #, numCoresUse = 14)    

# Save data.
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)

# Errors ...
deSink()
showAllScenariosErrors(retLst$resLstAll) 
showAllScenariosWarnings(retLst$resLstAll)
numSuccessfulRunsScenarios(scenariosObj, retLst$resLstAll)

# Plot response side summary statistics ...
namesStats <- c("Coefficients", "Correlation", "Percent Diff Total Abundance")
stats <- makeScenarioStats(retLst$scenariosObj, retLst$resLstAll, whichStats = c(1,2,3), 
                           whichSDMs=c("PA","MsPP","PO"))
retLst <- saveScenariosSummaryStats(stats, retLst, namesStats)
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "Intercept", whichCoeffs = 1, 
                           plotSDMs = c("PA", "MsPP","PO"), diffPORange = c(TRUE,FALSE), 
                           bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(retLst$scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, ylimVals = c(0,0.2,0,1.0))
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "CovarCoeffs", whichCoeffs = 2:4, 
                           plotSDMs = c("PA", "MsPP","PO"), diffPORange = c(FALSE,FALSE), 
                           bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(retLst$scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, ylimVals = c(0,0.5,0,4.0))
plotScenariosSummaryLambda(retLst$statsObj$stats[[2]], retLst$statsObj$namesStats[2], 
                           plotSDMs = c("PA", "MsPP","PO"), plotDevice="png",
                           plotDir = paste0(retLst$scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, horizontalLines = c(1,0), ylimVals = c(0.5,1.0,0,0.5))
plotScenariosSummaryLambda(retLst$statsObj$stats[[3]], retLst$statsObj$namesStats[3], 
                           plotSDMs = c("PA", "MsPP","PO"), plotDevice="png",
                           plotDir = paste0(retLst$scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, ylimVals = c(0.0,1.0,0.0,1.0))


# Reload data.
scenariosDir <- paste0(getwd(), "/", "Output-ClustSurvLocs2-runs", format(highNumRuns), "-centred")
retLst <- loadScenariosResults(scenariosDir)
#scenariosObj <- retLst$scenariosObj
#scenariosObj$scenariosDir <- scenariosDir
#scenariosObj <- addPrettyNamesToScenarios(scenariosObj, scenariosPrettyNames)
#retLst$statsObj <- addPrettyNamesToStats(retLst$statsObj, scenariosObj$prettyNamesScenarios)


# Save data (will include stats and prettynames if these have been added since last save).
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)


#-----------------------------
# 1.5 REDUCE NUMBER OF PO - extended 
#-----------------------------

# Run a few different values of the number of surveys 
deltaMultiplier <- lowDeltaMultiplier
numClusters <- as.integer(0)                                     # Random survey locations
numPA <- highNumPA
gammaMultiplier <- seq(from=lowGammaMultiplier, to=0.90, by=-0.05)
numRuns <- highNumRuns
scenariosDir <- paste0(getwd(), "/", "Output-DiffNumPOExtended-runs", format(numRuns), "-centred")
scenariosPrettyNames <- c("600","1000","1700","2900","4800", "8100", "13500", "22600", "38100")
xAxisTitle <- "approximate number of PO observations"

# Run scenarios ...
scenariosObj <- initialiseScenarios(scenariosDir, numPA, gammaMultiplier, deltaMultiplier, 
                                    numClusters, numRuns, prettyNames = scenariosPrettyNames)
retLst <- runScenarios(scenariosObj, useSDM = c("PA","MsPP","PO"), randomSeed = randomSeed, 
                       covarsCentred = TRUE)    

# Save data.
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)

# Errors ...
deSink()
showAllScenariosErrors(retLst$resLstAll) 
showAllScenariosWarnings(retLst$resLstAll)
numSuccessfulRunsScenarios(scenariosObj, retLst$resLstAll)

# Plot response side summary statistics ...
namesStats <- c("Coefficients", "Correlation", "Percent Diff Total Abundance")
stats <- makeScenarioStats(retLst$scenariosObj, retLst$resLstAll, whichStats = c(1,2,3), 
                           whichSDMs=c("PA","MsPP","PO"))
retLst <- saveScenariosSummaryStats(stats, retLst, namesStats)
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "Intercept", whichCoeffs = 1, 
                           plotSDMs = c("PA", "MsPP", "PO"), diffPORange = c(TRUE,FALSE), 
                           bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "CovarCoeffs", whichCoeffs = 2:4, 
                           plotSDMs = c("PA", "MsPP","PO"), diffPORange = c(TRUE,FALSE), 
                           bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)
plotScenariosSummaryLambda(retLst$statsObj$stats[[2]], retLst$statsObj$namesStats[2], 
                           plotSDMs = c("PA", "MsPP","PO"), plotDevice="png",
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, horizontalLines = c(1,0))
plotScenariosSummaryLambda(retLst$statsObj$stats[[3]], retLst$statsObj$namesStats[3], 
                           plotSDMs = c("PA", "MsPP"), plotDevice="png",
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)

# Reload data.
scenariosDir <- paste0(getwd(), "/", "Output-DiffNumPOExtended-runs", format(highNumRuns), "-centred")
retLst <- loadScenariosResults(scenariosDir)
scenariosObj <- retLst$scenariosObj
scenariosObj$scenariosDir <- scenariosDir
scenariosObj <- addPrettyNamesToScenarios(scenariosObj, scenariosPrettyNames)
retLst$statsObj <- addPrettyNamesToStats(retLst$statsObj, scenariosObj$prettyNamesScenarios)


# Save data (will include stats and prettynames if these have been added since last save).
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)


#--------------------------------------------
# 2.1 REDUCE number of surveys with sample bias
#--------------------------------------------

# Run a random and increasingly clustered survey locations.
deltaMultiplier <- 2.0  # Should be 2.0, REDO
numPA <- highNumPA
numClusters <- c(0, 500, 100, 50, 20, 10, 5)
gammaMultiplier <- highGammaMultiplier
numRuns <- highNumRuns
scenariosDir <- paste0(getwd(), "/", "Output-ClustLocs2SampBias-runs", format(numRuns), "-centred")
scenariosPrettyNames <- c(paste0("r", numPA) ,numClusters[-1])
xAxisTitle <- "number of surveys"

# Run scenarios ...
scenariosObj <- initialiseScenarios(scenariosDir, numPA, gammaMultiplier, deltaMultiplier, 
                                    numClusters, numRuns, prettyNames = scenariosPrettyNames)
retLst <- runScenarios(scenariosObj, useSDM = c("AB","PA","MsPP","PO"), randomSeed = randomSeed, 
                       covarsCentred = TRUE) #, numCoresUse = 14)    

# Save data.
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)

# Errors ...
deSink()
showAllScenariosErrors(retLst$resLstAll) 
showAllScenariosWarnings(retLst$resLstAll)
numSuccessfulRunsScenarios(scenariosObj, retLst$resLstAll)
plotWarningErrorSummary(scenariosObj, retLst)

# Plot response side summary statistics ...
namesStats <- c("Coefficients", "Correlation", "Percent Diff Total Abundance")
stats <- makeScenarioStats(retLst$scenariosObj, retLst$resLstAll, whichStats = c(1,2,3), 
                           whichSDMs=c("PA","MsPP","PO"))
retLst <- saveScenariosSummaryStats(stats, retLst, namesStats)
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "Intercept", whichCoeffs = 1, 
                           plotSDMs = c("AB","PA", "MsPP", "PO"), diffPORange = c(TRUE,FALSE), 
                           bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "CovarCoeffs", whichCoeffs = 2:4, 
                           plotSDMs = c("AB","PA", "MsPP","PO"), diffPORange = c(TRUE,FALSE), 
                           bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)
plotScenariosSummaryLambda(retLst$statsObj$stats[[2]], retLst$statsObj$namesStats[2], 
                           plotSDMs = c("AB","PA", "MsPP","PO"), plotDevice="png",
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, horizontalLines = c(1,0))
plotScenariosSummaryLambda(retLst$statsObj$stats[[3]], retLst$statsObj$namesStats[3], 
                           plotSDMs = c("AB","PA", "MsPP"), plotDevice="png",
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)

# Reload data.
scenariosDir <- paste0(getwd(), "/", "Output-ClustLocs2SampBias-runs", format(highNumRuns), "-centred")
retLst <- loadScenariosResults(scenariosDir)
scenariosObj <- retLst$scenariosObj
scenariosObj$scenariosDir <- scenariosDir
scenariosObj <- addPrettyNamesToScenarios(scenariosObj, scenariosPrettyNames)
retLst$statsObj <- addPrettyNamesToStats(retLst$statsObj, scenariosObj$prettyNamesScenarios)


# Save data (will include stats and prettynames if these have been added since last save).
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)


#-----------------------------
# 2.2 INCREASE SAMPLE BIAS with low PA numbers
#-----------------------------

# Run a few different values of bias (i.e. deltaMultiplier)
deltaMultiplier <- seq(from=lowDeltaMultiplier, to=highDeltaMultiplier, by=0.5)
numClusters <- 0
numPA <- lowNumPA
gammaMultiplier <- highGammaMultiplier
numRuns <- highNumRuns
scenariosDir <- paste0(getwd(), "/", "Output-SampBiasLowPA-runs", format(numRuns), "-centred")
scenariosPrettyNames <- as.character(format(deltaMultiplier))
xAxisTitle <- "fraction of true sampling bias"
xAxisLabelStyle <- 0

# Run scenarios ...
scenariosObj <- initialiseScenarios(scenariosDir, numPA, gammaMultiplier, deltaMultiplier, 
                                    numClusters, numRuns, prettyNames = scenariosPrettyNames)
retLst <- runScenarios(scenariosObj, useSDM = c("PA","MsPP","PO"), randomSeed = randomSeed, 
                       covarsCentred = TRUE)    

# Save data.
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)

# Errors ...
deSink()
showAllScenariosErrors(retLst$resLstAll) 
showAllScenariosWarnings(retLst$resLstAll)
numSuccessfulRunsScenarios(scenariosObj, retLst$resLstAll)

# Plot response side summary statistics ...
namesStats <- c("Coefficients", "Correlation", "Percent Diff Total Abundance")
stats <- makeScenarioStats(retLst$scenariosObj, retLst$resLstAll, whichStats = c(1,2,3), 
                           whichSDMs=c("PA","MsPP","PO"))
retLst <- saveScenariosSummaryStats(stats, retLst, namesStats)
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "Intercept", whichCoeffs = 1, 
                           plotSDMs = c("PA", "MsPP", "PO"), diffPORange = c(TRUE,FALSE), 
                           bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "CovarCoeffs", whichCoeffs = 2:4, 
                           plotSDMs = c("PA", "MsPP","PO"), diffPORange = c(FALSE,FALSE), 
                           bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)
plotScenariosSummaryLambda(retLst$statsObj$stats[[2]], retLst$statsObj$namesStats[2], 
                           plotSDMs = c("PA", "MsPP","PO"), plotDevice="png",
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, horizontalLines = c(1,0))
plotScenariosSummaryLambda(retLst$statsObj$stats[[3]], retLst$statsObj$namesStats[3], 
                           plotSDMs = c("PA", "MsPP"), plotDevice="png",
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)

# Reload data.
scenariosDir <- paste0(getwd(), "/", "Output-SampBiasLowPA-runs", format(highNumRuns), "-centred")
retLst <- loadScenariosResults(scenariosDir)
scenariosObj <- retLst$scenariosObj
scenariosObj$scenariosDir <- scenariosDir
scenariosObj <- addPrettyNamesToScenarios(scenariosObj, scenariosPrettyNames)
retLst$statsObj <- addPrettyNamesToStats(retLst$statsObj, scenariosObj$prettyNamesScenarios)


# Save data (will include stats and prettynames if these have been added since last save).
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)


#-----------------------------
# 2.3 INCREASE SAMPLE BIAS with low PO numbers
#-----------------------------

# Run a few different values of bias (i.e. deltaMultiplier)
deltaMultiplier <- seq(from=lowDeltaMultiplier, to=highDeltaMultiplier, by=0.5)
numClusters <- 0
numPA <- highNumPA
gammaMultiplier <- lowGammaMultiplier
numRuns <- highNumRuns
scenariosDir <- paste0(getwd(), "/", "Output-SampBiasLowPO-runs", format(numRuns), "-centred")
scenariosPrettyNames <- as.character(format(deltaMultiplier))
xAxisTitle <- "fraction of true sampling bias"
xAxisLabelStyle <- 0

# Run scenarios ...
scenariosObj <- initialiseScenarios(scenariosDir, numPA, gammaMultiplier, deltaMultiplier, 
                                    numClusters, numRuns, prettyNames = scenariosPrettyNames)
retLst <- runScenarios(scenariosObj, useSDM = c("PA","MsPP","PO"), randomSeed = randomSeed, 
                       covarsCentred = TRUE)    

# Save data.
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)

# Errors ...
deSink()
showAllScenariosErrors(retLst$resLstAll) 
showAllScenariosWarnings(retLst$resLstAll)
numSuccessfulRunsScenarios(scenariosObj, retLst$resLstAll)

# Plot response side summary statistics ...
namesStats <- c("Coefficients", "Correlation", "Percent Diff Total Abundance")
stats <- makeScenarioStats(retLst$scenariosObj, retLst$resLstAll, whichStats = c(1,2,3), 
                           whichSDMs=c("PA","MsPP","PO"))
retLst <- saveScenariosSummaryStats(stats, retLst, namesStats)
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "Intercept", whichCoeffs = 1, 
                           plotSDMs = c("PA", "MsPP", "PO"), diffPORange = c(TRUE,FALSE), 
                           bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "CovarCoeffs", whichCoeffs = 2:4, 
                           plotSDMs = c("PA", "MsPP","PO"), diffPORange = c(FALSE,FALSE), 
                           bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)
plotScenariosSummaryLambda(retLst$statsObj$stats[[2]], retLst$statsObj$namesStats[2], 
                           plotSDMs = c("PA", "MsPP","PO"), plotDevice="png",
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, horizontalLines = c(1,0))
plotScenariosSummaryLambda(retLst$statsObj$stats[[3]], retLst$statsObj$namesStats[3], 
                           plotSDMs = c("PA", "MsPP"), plotDevice="png",
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)

# Reload data.
scenariosDir <- paste0(getwd(), "/", "Output-SampBiasLowPO-runs", format(highNumRuns), "-centred")
retLst <- loadScenariosResults(scenariosDir)
scenariosObj <- retLst$scenariosObj
scenariosObj$scenariosDir <- scenariosDir
scenariosObj <- addPrettyNamesToScenarios(scenariosObj, scenariosPrettyNames)
retLst$statsObj <- addPrettyNamesToStats(retLst$statsObj, scenariosObj$prettyNamesScenarios)


# Save data (will include stats and prettynames if these have been added since last save).
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)


#---------------------------------------
# 2.4 REDUCE number of surveys with low PA
#---------------------------------------

# Run a random and increasingly clustered survey locations.
deltaMultiplier <- lowDeltaMultiplier     # No sample bias!!
numPA <- lowNumPA
numClusters <- c(0, 500, 100, 50, 20, 10, 5)
gammaMultiplier <- highGammaMultiplier    # High num PO!
numRuns <- highNumRuns
scenariosDir <- paste0(getwd(), "/", "Output-ClustLocs2LowPA-runs", format(numRuns), "-centred")
scenariosPrettyNames <- c(paste0("r",numPA), numClusters[-1])
xAxisTitle <- "number of surveys"

# Run scenarios ...
scenariosObj <- initialiseScenarios(scenariosDir, numPA, gammaMultiplier, deltaMultiplier, 
                                    numClusters, numRuns, prettyNames = scenariosPrettyNames)
retLst <- runScenarios(scenariosObj, useSDM = c("AB","PA","MsPP","PO"), randomSeed = randomSeed, 
                       covarsCentred = TRUE) #, numCoresUse = 14)    

# Save data.
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)

# Errors ...
deSink()
showAllScenariosErrors(retLst$resLstAll) 
showAllScenariosWarnings(retLst$resLstAll)
numSuccessfulRunsScenarios(scenariosObj, retLst$resLstAll)
plotWarningErrorSummary(scenariosObj, retLst)

# Plot response side summary statistics ...
namesStats <- c("Coefficients", "Correlation", "Percent Diff Total Abundance")
stats <- makeScenarioStats(retLst$scenariosObj, retLst$resLstAll, whichStats = c(1,2,3), 
                           whichSDMs=c("AB","PA","MsPP","PO"))
retLst <- saveScenariosSummaryStats(stats, retLst, namesStats)
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "Intercept", whichCoeffs = 1, 
                           plotSDMs = c("PA", "MsPP","PO"), diffPORange = c(TRUE,FALSE), 
                           bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(retLst$scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, ylimVals = c(0,0.2,0,1.0))
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "CovarCoeffs", whichCoeffs = 2:4, 
                           plotSDMs = c("PA", "MsPP","PO"), diffPORange = c(FALSE,FALSE), 
                           bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(retLst$scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, ylimVals = c(0,0.5,0,4.0))
plotScenariosSummaryLambda(retLst$statsObj$stats[[2]], retLst$statsObj$namesStats[2], 
                           plotSDMs = c("PA", "MsPP","PO"), plotDevice="png",
                           plotDir = paste0(retLst$scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, horizontalLines = c(1,0), ylimVals = c(0.5,1.0,0,0.5))
plotScenariosSummaryLambda(retLst$statsObj$stats[[3]], retLst$statsObj$namesStats[3], 
                           plotSDMs = c("PA", "MsPP","PO"), plotDevice="png",
                           plotDir = paste0(retLst$scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, ylimVals = c(0.0,1.0,0.0,1.0))


# Reload data.
scenariosDir <- paste0(getwd(), "/", "Output-ClustLocs2LowPA-runs", format(highNumRuns), "-centred")
retLst <- loadScenariosResults(scenariosDir)
#scenariosObj <- retLst$scenariosObj
#scenariosObj$scenariosDir <- scenariosDir
#scenariosObj <- addPrettyNamesToScenarios(scenariosObj, scenariosPrettyNames)
#retLst$statsObj <- addPrettyNamesToStats(retLst$statsObj, scenariosObj$prettyNamesScenarios)


# Save data (will include stats and prettynames if these have been added since last save).
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)


#-----------------------------
# 2.5 REDUCE NUMBER OF PO for low PA
#-----------------------------

# Run a few different values of the number of surveys 
deltaMultiplier <- lowDeltaMultiplier
numClusters <- as.integer(0)                                     # Random survey locations
numPA <- lowNumPA
gammaMultiplier <- seq(from=lowGammaMultiplier, to=highGammaMultiplier, by=-0.05)
numRuns <- highNumRuns
scenariosDir <- paste0(getwd(), "/", "Output-DiffNumPOLowPA-runs", format(numRuns), "-centred")
scenariosPrettyNames <- c("600","1000","1700","2900","4800")
xAxisTitle <- "approximate number of PO observations"

# Run scenarios ...
scenariosObj <- initialiseScenarios(scenariosDir, numPA, gammaMultiplier, deltaMultiplier, 
                                    numClusters, numRuns, prettyNames = scenariosPrettyNames)
retLst <- runScenarios(scenariosObj, useSDM = c("PA","MsPP","PO"), randomSeed = randomSeed, 
                       covarsCentred = TRUE)   

# Save data.
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)


# Errors ...
deSink()
showAllScenariosErrors(retLst$resLstAll) 
showAllScenariosWarnings(retLst$resLstAll)
numSuccessfulRunsScenarios(scenariosObj, retLst$resLstAll)

# Plot response side summary statistics ...
namesStats <- c("Coefficients", "Correlation", "Percent Diff Total Abundance")
stats <- makeScenarioStats(retLst$scenariosObj, retLst$resLstAll, whichStats = c(1,2,3), 
                           whichSDMs=c("PA","MsPP","PO"))
retLst <- saveScenariosSummaryStats(stats, retLst, namesStats)
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "Intercept", whichCoeffs = 1, 
                           plotSDMs = c("PA", "MsPP", "PO"), diffPORange = c(TRUE,FALSE), 
                           bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "CovarCoeffs", whichCoeffs = 2:4, 
                           plotSDMs = c("PA", "MsPP","PO"), diffPORange = c(TRUE,FALSE), 
                           bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)
plotScenariosSummaryLambda(retLst$statsObj$stats[[2]], retLst$statsObj$namesStats[2], 
                           plotSDMs = c("PA", "MsPP","PO"), plotDevice="png",
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, horizontalLines = c(1,0))
plotScenariosSummaryLambda(retLst$statsObj$stats[[3]], retLst$statsObj$namesStats[3], 
                           plotSDMs = c("PA", "MsPP"), plotDevice="png",
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)

# Reload data.
scenariosDir <- paste0(getwd(), "/", "Output-DiffNumPOLowPA-runs", format(highNumRuns), "-centred")
retLst <- loadScenariosResults(scenariosDir)
scenariosObj <- retLst$scenariosObj
#scenariosObj$scenariosDir <- scenariosDir
scenariosObj <- addPrettyNamesToScenarios(scenariosObj, scenariosPrettyNames)
retLst$statsObj <- addPrettyNamesToStats(retLst$statsObj, scenariosObj$prettyNamesScenarios)


# Save data (will include stats and prettynames if these have been added since last save).
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)


#---------------------------------------
# 2.6 Reduce number of surveys with low PO numbers
#---------------------------------------

# Run a random and increasingly clustered survey locations.
deltaMultiplier <- lowDeltaMultiplier     # No sample bias!!
numPA <- highNumPA
numClusters <- c(0, 500, 100, 50, 20, 10, 5)
gammaMultiplier <- lowGammaMultiplier     # low num PO!
numRuns <- highNumRuns
scenariosDir <- paste0(getwd(), "/", "Output-ClustLocs2LowPO-runs", format(numRuns), "-centred")
scenariosPrettyNames <- c(paste0("r",numPA), numClusters[-1])
xAxisTitle <- "number of surveys"

# Run scenarios ...
scenariosObj <- initialiseScenarios(scenariosDir, numPA, gammaMultiplier, deltaMultiplier, 
                                    numClusters, numRuns, prettyNames = scenariosPrettyNames)
retLst <- runScenarios(scenariosObj, useSDM = c("AB","PA","MsPP","PO"), randomSeed = randomSeed, 
                       covarsCentred = TRUE)    

# Save data 
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)

# Errors ...
deSink()
showAllScenariosErrors(retLst$resLstAll) 
showAllScenariosWarnings(retLst$resLstAll)
numSuccessfulRunsScenarios(scenariosObj, retLst$resLstAll)
plotWarningErrorSummary(scenariosObj, retLst)

# Plot response side summary statistics ...
namesStats <- c("Coefficients", "Correlation", "Percent Diff Total Abundance")
stats <- makeScenarioStats(retLst$scenariosObj, retLst$resLstAll, whichStats = c(1,2,3), 
                           whichSDMs=c("AB","PA","MsPP","PO"))
retLst <- saveScenariosSummaryStats(stats, retLst, namesStats)
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "Intercept", whichCoeffs = 1, 
                           plotSDMs = c("PA", "MsPP", "PO"), diffPORange = c(TRUE,FALSE), 
                           bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, ylimVals = c(0,0.1,0,1.0))
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "CovarCoeffs", whichCoeffs = 2:4, 
                           plotSDMs = c("PA", "MsPP","PO"), diffPORange = c(FALSE,FALSE), 
                           bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, ylimVals = c(0,0.5,0,4.0))
plotScenariosSummaryLambda(retLst$statsObj$stats[[2]], retLst$statsObj$namesStats[2], 
                           plotSDMs = c("PA", "MsPP","PO"), plotDevice="png",
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, horizontalLines = c(1,0), ylimVals=c(0.5,1,0,0.5))
plotScenariosSummaryLambda(retLst$statsObj$stats[[3]], retLst$statsObj$namesStats[3], 
                           plotSDMs = c("PA", "MsPP"), plotDevice="png",
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, ylimVals=c(0,1,0,1))

# Reload data.
scenariosDir <- paste0(getwd(), "/", "Output-ClustLocs2LowPO-runs", format(highNumRuns), "-centred")
retLst <- loadScenariosResults(scenariosDir)
scenariosObj <- retLst$scenariosObj
scenariosObj$scenariosDir <- scenariosDir
scenariosObj <- addPrettyNamesToScenarios(scenariosObj, scenariosPrettyNames)
retLst$statsObj <- addPrettyNamesToStats(retLst$statsObj, scenariosObj$prettyNamesScenarios)

# Save data (will include stats and prettynames if these have been added since last save).
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)


#-----------------------------
# 2.7 REDUCE NUMBER OF PO (extended), with sample bias.
#-----------------------------

# Run a few different values of the number of surveys 
deltaMultiplier <- highDeltaMultiplier
numClusters <- as.integer(0)                                     # Random survey locations
numPA <- highNumPA
gammaMultiplier <- seq(from=lowGammaMultiplier, to=0.90, by=-0.05)
numRuns <- highNumRuns
scenariosDir <- paste0(getwd(), "/", "Output-DiffNumPOSampBias-runs", format(numRuns), "-centred")
scenariosPrettyNames <- c("830","1380","2300","3800","6300","10500","17600","29500","49500") #c("600","1000","1700","2900","4800", "8100", "13500", "22600", "38100")
xAxisTitle <- "approximate number of PO observations"

# Run scenarios ...
scenariosObj <- initialiseScenarios(scenariosDir, numPA, gammaMultiplier, deltaMultiplier, 
                                    numClusters, numRuns, prettyNames = scenariosPrettyNames)
retLst <- runScenarios(scenariosObj, useSDM = c("PA","MsPP","PO"), randomSeed = randomSeed, 
                       covarsCentred = TRUE, numCoresUse = 16)    

# Save data.
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)

# Errors ...
deSink()
showAllScenariosErrors(retLst$resLstAll) 
showAllScenariosWarnings(retLst$resLstAll)
numSuccessfulRunsScenarios(scenariosObj, retLst$resLstAll)

# Plot response side summary statistics ...
namesStats <- c("Coefficients", "Correlation", "Percent Diff Total Abundance")
stats <- makeScenarioStats(retLst$scenariosObj, retLst$resLstAll, whichStats = c(1,2,3), 
                           whichSDMs=c("PA","MsPP","PO"))
retLst <- saveScenariosSummaryStats(stats, retLst, namesStats)
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "Intercept", whichCoeffs = 1, 
                           plotSDMs = c("PA", "MsPP", "PO"), diffPORange = c(TRUE,FALSE), 
                           bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "CovarCoeffs", whichCoeffs = 2:4, 
                           plotSDMs = c("PA", "MsPP","PO"), diffPORange = c(TRUE,FALSE), 
                           bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)
plotScenariosSummaryLambda(retLst$statsObj$stats[[2]], retLst$statsObj$namesStats[2], 
                           plotSDMs = c("PA", "MsPP","PO"), plotDevice="png",
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, horizontalLines = c(1,0))
plotScenariosSummaryLambda(retLst$statsObj$stats[[3]], retLst$statsObj$namesStats[3], 
                           plotSDMs = c("PA", "MsPP"), plotDevice="png",
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)

# Reload data.
scenariosDir <- paste0(getwd(), "/", "Output-DiffNumPOSampBias-runs", format(highNumRuns), "-centred")
retLst <- loadScenariosResults(scenariosDir)
scenariosObj <- retLst$scenariosObj
scenariosObj$scenariosDir <- scenariosDir
scenariosObj <- addPrettyNamesToScenarios(scenariosObj, scenariosPrettyNames)
retLst$statsObj <- addPrettyNamesToStats(retLst$statsObj, scenariosObj$prettyNamesScenarios)


# Save data (will include stats and prettynames if these have been added since last save).
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)


#-----------------------------
# 3.1 INCREASE SAMPLE BIAS with low PA and PO numbers
#-----------------------------

# Run a few different values of bias (i.e. deltaMultiplier)
deltaMultiplier <- seq(from=lowDeltaMultiplier, to=highDeltaMultiplier, by=0.5)
numClusters <- 0
numPA <- lowNumPA
gammaMultiplier <- lowGammaMultiplier
numRuns <- highNumRuns
scenariosDir <- paste0(getwd(), "/", "Output-SampBiasLowPAPO-runs", format(numRuns), "-centred")
scenariosPrettyNames <- as.character(format(deltaMultiplier))
xAxisTitle <- "fraction of true sampling bias"
xAxisLabelStyle <- 0

# Run scenarios ...
scenariosObj <- initialiseScenarios(scenariosDir, numPA, gammaMultiplier, deltaMultiplier, 
                                    numClusters, numRuns, prettyNames = scenariosPrettyNames)
retLst <- runScenarios(scenariosObj, useSDM = c("AB","PA","MsPP","PO"), randomSeed = randomSeed, 
                       covarsCentred = TRUE) #, numCoresUse = 14)    

# Save data.
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)

# Errors ...
deSink()
showAllScenariosErrors(retLst$resLstAll) 
showAllScenariosWarnings(retLst$resLstAll)
numSuccessfulRunsScenarios(scenariosObj, retLst$resLstAll)
plotWarningErrorSummary(scenariosObj, retLst)

# Plot response side summary statistics ...
namesStats <- c("Coefficients", "Correlation", "Percent Diff Total Abundance")
stats <- makeScenarioStats(scenariosObj, retLst$resLstAll, whichStats = c(1,2,3), 
                           whichSDMs=c("AB","PA","MsPP","PO"))
retLst <- saveScenariosSummaryStats(stats, retLst, namesStats)
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "Intercept", whichCoeffs = 1, 
                           plotSDMs = c("AB","PA", "MsPP", "PO"), diffPORange = c(TRUE,FALSE), 
                           bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "CovarCoeffs", whichCoeffs = 2:4, 
                           plotSDMs = c("AB","PA", "MsPP","PO"), diffPORange = c(FALSE,FALSE), 
                           bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)
plotScenariosSummaryLambda(retLst$statsObj$stats[[2]], retLst$statsObj$namesStats[2], 
                           plotSDMs = c("AB","PA", "MsPP","PO"), plotDevice="png",
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, horizontalLines = c(1,0))
plotScenariosSummaryLambda(retLst$statsObj$stats[[3]], retLst$statsObj$namesStats[3], 
                           plotSDMs = c("AB","PA", "MsPP"), plotDevice="png",
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)

# Reload data.
scenariosDir <- paste0(getwd(), "/", "Output-SampBiasLowPAPO-runs", format(highNumRuns), "-centred")
retLst <- loadScenariosResults(scenariosDir)
scenariosObj <- retLst$scenariosObj
scenariosObj$scenariosDir <- scenariosDir
scenariosObj <- addPrettyNamesToScenarios(scenariosObj, scenariosPrettyNames)
retLst$statsObj <- addPrettyNamesToStats(retLst$statsObj, scenariosObj$prettyNamesScenarios)


# Save data (will include stats and prettynames if these have been added since last save).
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)



#---------------------------------------------
# 3.2 REDUCE number of surveys with low PA and low PO
#---------------------------------------------

# Run a random and increasingly clustered survey locations.
deltaMultiplier <- lowDeltaMultiplier     # No sample bias!!
numPA <- lowNumPA
numClusters <- c(0, 500, 100, 50, 20, 10, 5)
gammaMultiplier <- lowGammaMultiplier     # low num PO!
numRuns <- highNumRuns
scenariosDir <- paste0(getwd(), "/", "Output-ClustLocs2LowPAPO-runs", format(numRuns), "-centred")
scenariosPrettyNames <- c(paste0("r",numPA), numClusters[-1])
xAxisTitle <- "number of surveys"

# Run scenarios ...
scenariosObj <- initialiseScenarios(scenariosDir, numPA, gammaMultiplier, deltaMultiplier, 
                                    numClusters, numRuns, prettyNames = scenariosPrettyNames)
retLst <- runScenarios(scenariosObj, useSDM = c("AB","PA","MsPP","PO"), randomSeed = randomSeed, 
                       covarsCentred = TRUE) #, numCoresUse = 14)    

# Save data.
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)

# Errors ...
deSink()
showAllScenariosErrors(retLst$resLstAll) 
showAllScenariosWarnings(retLst$resLstAll)
numSuccessfulRunsScenarios(scenariosObj, retLst$resLstAll)
plotWarningErrorSummary(scenariosObj, retLst)

# Plot response side summary statistics ...
namesStats <- c("Coefficients", "Correlation", "Percent Diff Total Abundance")
stats <- makeScenarioStats(retLst$scenariosObj, retLst$resLstAll, whichStats = c(1,2,3), 
                           whichSDMs=c("AB","PA","MsPP","PO"))
retLst <- saveScenariosSummaryStats(stats, retLst, namesStats)
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "Intercept", whichCoeffs = 1, 
                           plotSDMs = c("PA", "MsPP","PO"), diffPORange = c(TRUE,FALSE), 
                           bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(retLst$scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, ylimVals = c(0,0.2,0,1.0))
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "CovarCoeffs", whichCoeffs = 2:4, 
                           plotSDMs = c("PA", "MsPP","PO"), diffPORange = c(FALSE,FALSE), 
                           bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(retLst$scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, ylimVals = c(0,0.5,0,4.0))
plotScenariosSummaryLambda(retLst$statsObj$stats[[2]], retLst$statsObj$namesStats[2], 
                           plotSDMs = c("PA", "MsPP","PO"), plotDevice="png",
                           plotDir = paste0(retLst$scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, horizontalLines = c(1,0), ylimVals = c(0.5,1.0,0,0.5))
plotScenariosSummaryLambda(retLst$statsObj$stats[[3]], retLst$statsObj$namesStats[3], 
                           plotSDMs = c("PA", "MsPP","PO"), plotDevice="png",
                           plotDir = paste0(retLst$scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, ylimVals = c(0.0,1.0,0.0,1.0))

# Reload data.
scenariosDir <- paste0(getwd(), "/", "Output-ClustLocs2LowPAPO-runs", format(highNumRuns), "-centred")
retLst <- loadScenariosResults(scenariosDir)
scenariosObj <- retLst$scenariosObj
scenariosObj$scenariosDir <- scenariosDir
scenariosObj <- addPrettyNamesToScenarios(scenariosObj, scenariosPrettyNames)
retLst$statsObj <- addPrettyNamesToStats(retLst$statsObj, scenariosObj$prettyNamesScenarios)


# Save data (will include stats and prettynames if these have been added since last save).
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)


#-----------------------------------------------------
# 3.3 REDUCE NUMBER OF PO for low PA and high sample bias
#-----------------------------------------------------

# Run a few different values of the number of surveys 
deltaMultiplier <- highDeltaMultiplier
numClusters <- as.integer(0)                                     # Random survey locations
numPA <- lowNumPA
gammaMultiplier <- seq(from=lowGammaMultiplier, to=0.90, by=-0.05)
numRuns <- highNumRuns
scenariosDir <- paste0(getwd(), "/", "Output-DiffNumPOLowPASampBias-runs", format(numRuns), "-centred")
scenariosPrettyNames <- c("830","1380","2300","3800","6300","10500","17600","29500","49500")
xAxisTitle <- "approximate number of PO observations"

# Run scenarios ...
scenariosObj <- initialiseScenarios(scenariosDir, numPA, gammaMultiplier, deltaMultiplier, 
                                    numClusters, numRuns, prettyNames = scenariosPrettyNames)
retLst <- runScenarios(scenariosObj, useSDM = c("PA","MsPP","PO"), randomSeed = randomSeed, 
                       covarsCentred = TRUE)   

# Save data.
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)

# Errors ...
deSink()
showAllScenariosErrors(retLst$resLstAll) 
showAllScenariosWarnings(retLst$resLstAll)
numSuccessfulRunsScenarios(scenariosObj, retLst$resLstAll)

# Plot response side summary statistics ...
namesStats <- c("Coefficients", "Correlation", "Percent Diff Total Abundance")
stats <- makeScenarioStats(retLst$scenariosObj, retLst$resLstAll, whichStats = c(1,2,3), 
                           whichSDMs=c("PA","MsPP","PO"))
retLst <- saveScenariosSummaryStats(stats, retLst, namesStats)
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "Intercept", whichCoeffs = 1, 
                           plotSDMs = c("PA", "MsPP", "PO"), diffPORange = c(TRUE,FALSE), 
                           bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "CovarCoeffs", whichCoeffs = 2:4, 
                           plotSDMs = c("PA", "MsPP","PO"), diffPORange = c(TRUE,FALSE), 
                           bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)
plotScenariosSummaryLambda(retLst$statsObj$stats[[2]], retLst$statsObj$namesStats[2], 
                           plotSDMs = c("PA", "MsPP","PO"), plotDevice="png",
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, horizontalLines = c(1,0))
plotScenariosSummaryLambda(retLst$statsObj$stats[[3]], retLst$statsObj$namesStats[3], 
                           plotSDMs = c("PA", "MsPP"), plotDevice="png",
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)

plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "Intercept", whichCoeffs = 1, 
                           plotSDMs = c("PA", "MsPP","PO"), diffPORange = c(TRUE,FALSE), 
                           bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(retLst$scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, ylimVals = c(0,0.2,0,1.0))
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "CovarCoeffs", whichCoeffs = 2:4, 
                           plotSDMs = c("PA", "MsPP","PO"), diffPORange = c(FALSE,FALSE), 
                           bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(retLst$scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, ylimVals = c(0,0.5,0,4.0))
plotScenariosSummaryLambda(retLst$statsObj$stats[[2]], retLst$statsObj$namesStats[2], 
                           plotSDMs = c("PA", "MsPP","PO"), plotDevice="png",
                           plotDir = paste0(retLst$scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, horizontalLines = c(1,0), ylimVals = c(0.5,1.0,0,0.5))
plotScenariosSummaryLambda(retLst$statsObj$stats[[3]], retLst$statsObj$namesStats[3], 
                           plotSDMs = c("PA", "MsPP","PO"), plotDevice="png",
                           plotDir = paste0(retLst$scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, ylimVals = c(0.0,1.0,0.0,1.0))

# Reload data.
scenariosDir <- paste0(getwd(), "/", "Output-DiffNumPOLowPASampBias-runs", format(highNumRuns), "-centred")
retLst <- loadScenariosResults(scenariosDir)
scenariosObj <- retLst$scenariosObj
scenariosObj$scenariosDir <- scenariosDir
scenariosObj <- addPrettyNamesToScenarios(scenariosObj, scenariosPrettyNames)
retLst$statsObj <- addPrettyNamesToStats(retLst$statsObj, scenariosObj$prettyNamesScenarios)


# Save data (will include stats and prettynames if these have been added since last save).
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)

#------------------------------------------------------
# 3.4 REDUCE number of surveys with low PA and sample bias
#------------------------------------------------------

# Run a random and increasingly clustered survey locations.
deltaMultiplier <- highDeltaMultiplier     # 2 x sample bias!!
numPA <- lowNumPA
numClusters <- c(0, 500, 100, 50, 20, 10, 5)
gammaMultiplier <- highGammaMultiplier     # high num PO!
numRuns <- highNumRuns
scenariosDir <- paste0(getwd(), "/", "Output-ClustLocs2LowPASampBias-runs", format(numRuns), "-centred")
scenariosPrettyNames <- c(paste0("r",numPA), numClusters[-1])
xAxisTitle <- "number of surveys"

# Run scenarios ...
scenariosObj <- initialiseScenarios(scenariosDir, numPA, gammaMultiplier, deltaMultiplier, 
                                    numClusters, numRuns, prettyNames = scenariosPrettyNames)
retLst <- runScenarios(scenariosObj, useSDM = c("PA","MsPP","PO"), randomSeed = randomSeed, 
                       covarsCentred = TRUE) #, numCoresUse = 14)    

# Save data.
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)

# Errors ...
deSink()
showAllScenariosErrors(retLst$resLstAll) 
showAllScenariosWarnings(retLst$resLstAll)
numSuccessfulRunsScenarios(scenariosObj, retLst$resLstAll)
plotWarningErrorSummary(scenariosObj, retLst)

# Plot response side summary statistics ...
namesStats <- c("Coefficients", "Correlation", "Percent Diff Total Abundance")
stats <- makeScenarioStats(retLst$scenariosObj, retLst$resLstAll, whichStats = c(1,2,3), 
                           whichSDMs=c("PA","MsPP","PO"))
retLst <- saveScenariosSummaryStats(stats, retLst, namesStats)
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "Intercept", whichCoeffs = 1, 
                           plotSDMs = c("PA", "MsPP", "PO"), diffPORange = c(TRUE,FALSE), 
                           bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(retLst$scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "CovarCoeffs", whichCoeffs = 2:4, 
                           plotSDMs = c("PA", "MsPP","PO"), diffPORange = c(FALSE,FALSE), 
                           bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(retLst$scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)
plotScenariosSummaryLambda(retLst$statsObj$stats[[2]], retLst$statsObj$namesStats[2], 
                           plotSDMs = c("PA", "MsPP","PO"), plotDevice="png",
                           plotDir = paste0(retLst$scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, horizontalLines = c(1,0))
plotScenariosSummaryLambda(retLst$statsObj$stats[[3]], retLst$statsObj$namesStats[3], 
                           plotSDMs = c("PA", "MsPP"), plotDevice="png",
                           plotDir = paste0(retLst$scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)

# Reload data.
scenariosDir <- paste0(getwd(), "/", "Output-ClustLocs2LowPASampBias-runs", format(highNumRuns), "-centred")
retLst <- loadScenariosResults(scenariosDir)
scenariosObj <- retLst$scenariosObj
scenariosObj$scenariosDir <- scenariosDir
scenariosObj <- addPrettyNamesToScenarios(scenariosObj, scenariosPrettyNames)
retLst$statsObj <- addPrettyNamesToStats(retLst$statsObj, scenariosObj$prettyNamesScenarios)


# Save data (will include stats and prettynames if these have been added since last save).
#retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)

#-----------------------------------------------------
# 3.5 Increase sample bias for low PO and low surveys
#-----------------------------------------------------

# Run a few different values of the sample bias multiplier 
deltaMultiplier <- seq(from=lowDeltaMultiplier, to=highDeltaMultiplier, by=0.5)
numClusters <- as.integer(5)
numPA <- highNumPA
gammaMultiplier <- lowGammaMultiplier
numRuns <- highNumRuns
scenariosDir <- paste0(getwd(), "/", "Output-SampBiasLowPOLowClust-runs", format(numRuns), "-centred")
scenariosPrettyNames <- as.character(format(deltaMultiplier))
xAxisTitle <- "fraction of true sampling bias"
xAxisLabelStyle <- 0

# Run scenarios ...
scenariosObj <- initialiseScenarios(scenariosDir, numPA, gammaMultiplier, deltaMultiplier, 
                                    numClusters, numRuns, prettyNames = scenariosPrettyNames)
retLst <- runScenarios(scenariosObj, useSDM = c("AB","PA","MsPP","PO"), randomSeed = randomSeed, 
                       covarsCentred = TRUE)   

# Save data.
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)

# Errors ...
deSink()
showAllScenariosErrors(retLst$resLstAll) 
showAllScenariosWarnings(retLst$resLstAll)
numSuccessfulRunsScenarios(scenariosObj, retLst$resLstAll)
plotWarningErrorSummary(scenariosObj, retLst)

# Plot response side summary statistics ...
namesStats <- c("Coefficients", "Correlation", "Percent Diff Total Abundance")
stats <- makeScenarioStats(retLst$scenariosObj, retLst$resLstAll, whichStats = c(1,2,3), 
                           whichSDMs=c("PA","MsPP","PO"))
retLst <- saveScenariosSummaryStats(stats, retLst, namesStats)
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "Intercept", whichCoeffs = 1, 
                           plotSDMs = c("PA", "MsPP", "PO"), diffPORange = c(TRUE,FALSE), 
                           bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(retLst$scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "CovarCoeffs", whichCoeffs = 2:4, 
                           plotSDMs = c("PA", "MsPP","PO"), diffPORange = c(TRUE,FALSE), 
                           bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(retLst$scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)
plotScenariosSummaryLambda(retLst$statsObj$stats[[2]], retLst$statsObj$namesStats[2], 
                           plotSDMs = c("PA", "MsPP","PO"), plotDevice="png",
                           plotDir = paste0(retLst$scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, horizontalLines = c(1,0))
plotScenariosSummaryLambda(retLst$statsObj$stats[[3]], retLst$statsObj$namesStats[3], 
                           plotSDMs = c("PA", "MsPP"), plotDevice="png",
                           plotDir = paste0(retLst$scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)

plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "Intercept", whichCoeffs = 1, 
                           plotSDMs = c("PA", "MsPP","PO"), diffPORange = c(TRUE,FALSE), 
                           bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(retLst$scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, ylimVals = c(0,0.2,0,1.0))
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "CovarCoeffs", whichCoeffs = 2:4, 
                           plotSDMs = c("PA", "MsPP","PO"), diffPORange = c(FALSE,FALSE), 
                           bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(retLst$scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, ylimVals = c(0,0.5,0,4.0))
plotScenariosSummaryLambda(retLst$statsObj$stats[[2]], retLst$statsObj$namesStats[2], 
                           plotSDMs = c("PA", "MsPP","PO"), plotDevice="png",
                           plotDir = paste0(retLst$scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, horizontalLines = c(1,0), ylimVals = c(0.5,1.0,0,0.5))
plotScenariosSummaryLambda(retLst$statsObj$stats[[3]], retLst$statsObj$namesStats[3], 
                           plotSDMs = c("PA", "MsPP","PO"), plotDevice="png",
                           plotDir = paste0(retLst$scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, ylimVals = c(0.0,1.0,0.0,1.0))

# Reload data.
scenariosDir <- paste0(getwd(), "/", "Output-SampBiasLowPOLowClust-runs", format(highNumRuns), "-centred")
retLst <- loadScenariosResults(scenariosDir)
scenariosObj <- retLst$scenariosObj
scenariosObj$scenariosDir <- scenariosDir
scenariosObj <- addPrettyNamesToScenarios(scenariosObj, scenariosPrettyNames)
retLst$statsObj <- addPrettyNamesToStats(retLst$statsObj, scenariosObj$prettyNamesScenarios)


# Save data (will include stats and prettynames if these have been added since last save).
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)



#-----------------------------
# 4.1 Real with increasing sample bias
#-----------------------------

# Run a random and increasingly clustered survey locations.
deltaMultiplier <- seq(from=lowDeltaMultiplier, to=highDeltaMultiplier, by=0.5)
numPA <- lowNumPA
numClusters <- 5
gammaMultiplier <- lowGammaMultiplier
numRuns <- highNumRuns
scenariosDir <- paste0(getwd(), "/", "Output-RealSampBias5-runs", format(numRuns), "-centred")
scenariosPrettyNames <- as.character(format(deltaMultiplier))
xAxisTitle <- "fraction of true sampling bias"
xAxisLabelStyle <- 0

# Run scenarios ...
scenariosObj <- initialiseScenarios(scenariosDir, numPA, gammaMultiplier, deltaMultiplier, 
                                    numClusters, numRuns, prettyNames = scenariosPrettyNames)
retLst <- runScenarios(scenariosObj, useSDM = c("AB","PA","MsPP","PO"), randomSeed = randomSeed, 
                       covarsCentred = TRUE) #, numCoresUse = 12)    

# Save data.
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)

# Errors ...
deSink()
showAllScenariosErrors(retLst$resLstAll) 
showAllScenariosWarnings(retLst$resLstAll)
numSuccessfulRunsScenarios(scenariosObj, retLst$resLstAll)
plotWarningErrorSummary(scenariosObj, retLst)

# Plot response side summary statistics ...
namesStats <- c("Coefficients", "Correlation", "Percent Diff Total Abundance")
stats <- makeScenarioStats(retLst$scenariosObj, retLst$resLstAll, whichStats = c(1,2,3), 
                           whichSDMs=c("PA","MsPP","PO"))
retLst <- saveScenariosSummaryStats(stats, retLst, namesStats)
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "Intercept", whichCoeffs = 1, 
                           plotSDMs = c("PA", "MsPP", "PO"), diffPORange = c(TRUE,FALSE), 
                           bigNumThreshold = 20, plotDevice = "png", 
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "CovarCoeffs", whichCoeffs = 2:4, 
                           plotSDMs = c("PA", "MsPP","PO"), diffPORange = c(FALSE,FALSE), 
                           bigNumThreshold = 20, plotDevice = "png", 
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)
plotScenariosSummaryLambda(retLst$statsObj$stats[[2]], retLst$statsObj$namesStats[2], 
                           plotSDMs = c("PA", "MsPP","PO"), plotDevice="png",
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, horizontalLines = c(1,0))
plotScenariosSummaryLambda(retLst$statsObj$stats[[3]], retLst$statsObj$namesStats[3], 
                           plotSDMs = c("PA", "MsPP"), plotDevice="png",
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)

# Reload data.
scenariosDir <- paste0(getwd(), "/", "Output-RealSampBias5-runs", format(highNumRuns), "-centred")
retLst <- loadScenariosResults(scenariosDir)
scenariosObj <- retLst$scenariosObj
scenariosObj$scenariosDir <- scenariosDir
scenariosObj <- addPrettyNamesToScenarios(scenariosObj, scenariosPrettyNames)
retLst$statsObj <- addPrettyNamesToStats(retLst$statsObj, scenariosObj$prettyNamesScenarios)


# Save data (will include stats and prettynames if these have been added since last save).
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)


#-----------------------------
# 4.2 Real with increasing clustering
#-----------------------------

# Run a random and increasingly clustered survey locations.
deltaMultiplier <- 2.0
numPA <- lowNumPA
numClusters <- c(0, 500, 100, 50, 20, 10, 5)
gammaMultiplier <- lowGammaMultiplier
numRuns <- highNumRuns
scenariosDir <- paste0(getwd(), "/", "Output-RealClustLocs-runs", format(numRuns), "-centred")
scenariosPrettyNames <- c(paste0("r",numPA), numClusters[-1])
xAxisTitle <- "number of surveys"

# Run scenarios ...
scenariosObj <- initialiseScenarios(scenariosDir, numPA, gammaMultiplier, deltaMultiplier, 
                                    numClusters, numRuns, prettyNames = scenariosPrettyNames)
retLst <- runScenarios(scenariosObj, useSDM = c("AB","PA","MsPP","PO"), randomSeed = randomSeed, 
                       covarsCentred = TRUE) #, numCoresUse = 15)    

# Save data.
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)

# Errors ...
deSink()
showAllScenariosErrors(retLst$resLstAll) 
showAllScenariosWarnings(retLst$resLstAll)
numSuccessfulRunsScenarios(scenariosObj, retLst$resLstAll)
plotWarningErrorSummary(scenariosObj, retLst)

# Plot response side summary statistics ...
namesStats <- c("Coefficients", "Correlation", "Percent Diff Total Abundance")
stats <- makeScenarioStats(retLst$scenariosObj, retLst$resLstAll, whichStats = c(1,2,3), 
                           whichSDMs=c("AB","PA","MsPP","PO"))
retLst <- saveScenariosSummaryStats(stats, retLst, namesStats)
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "Intercept", whichCoeffs = 1, 
                           plotSDMs = c("PA", "MsPP","PO"), diffPORange = c(TRUE,FALSE), 
                           bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(retLst$scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle) #, ylimVals = c(0,0.2,0,1.0))
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "CovarCoeffs", whichCoeffs = 2:4, 
                           plotSDMs = c("PA", "MsPP","PO"), diffPORange = c(FALSE,FALSE), 
                           bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(retLst$scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle) #, ylimVals = c(0,0.5,0,4.0))
plotScenariosSummaryLambda(retLst$statsObj$stats[[2]], retLst$statsObj$namesStats[2], 
                           plotSDMs = c("PA", "MsPP","PO"), plotDevice="png",
                           plotDir = paste0(retLst$scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, horizontalLines = c(1,0)) #, ylimVals = c(0.5,1.0,0,0.5))
plotScenariosSummaryLambda(retLst$statsObj$stats[[3]], retLst$statsObj$namesStats[3], 
                           plotSDMs = c("PA", "MsPP","PO"), plotDevice="png",
                           plotDir = paste0(retLst$scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle) #, ylimVals = c(0.0,1.0,0.0,1.0))

# Reload data.
scenariosDir <- paste0(getwd(), "/", "Output-RealClustLocs-runs", format(highNumRuns), "-centred")
retLst <- loadScenariosResults(scenariosDir)
#retscenariosObj <- retLst$scenariosObj
#scenariosObj$scenariosDir <- scenariosDir
retLst$scenariosObj <- addPrettyNamesToScenarios(retLst$scenariosObj, scenariosPrettyNames)
retLst$statsObj <- addPrettyNamesToStats(retLst$statsObj, scenariosObj$prettyNamesScenarios)


# Save data (will include stats and prettynames if these have been added since last save).
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)


#-----------------------------
# 4.3 Real with increasing clustering
#-----------------------------

# Run a random and increasingly clustered survey locations.
deltaMultiplier <- 2.0
numPA <- lowNumPA
numClusters <- 5
gammaMultiplier <- seq(from=lowGammaMultiplier, to=0.90, by=-0.05)
numRuns <- highNumRuns
scenariosDir <- paste0(getwd(), "/", "Output-RealDiffNumPO-runs", format(numRuns), "-centred")
scenariosPrettyNames <- c("830","1380","2300","3800","6300","10500","17600","29500","49500")
xAxisTitle <- "approximate number of PO observations"

# Run scenarios ...
scenariosObj <- initialiseScenarios(scenariosDir, numPA, gammaMultiplier, deltaMultiplier, 
                                    numClusters, numRuns, prettyNames = scenariosPrettyNames)
retLst <- runScenarios(scenariosObj, useSDM = c("PA","MsPP","PO"), randomSeed = randomSeed, 
                       covarsCentred = TRUE) #, numCoresUse = 15)    

# Save data.
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)

# Errors ...
deSink()
showAllScenariosErrors(retLst$resLstAll) 
showAllScenariosWarnings(retLst$resLstAll)
numSuccessfulRunsScenarios(scenariosObj, retLst$resLstAll)
plotWarningErrorSummary(scenariosObj, retLst)

# Plot response side summary statistics ...
namesStats <- c("Coefficients", "Correlation", "Percent Diff Total Abundance")
stats <- makeScenarioStats(retLst$scenariosObj, retLst$resLstAll, whichStats = c(1,2,3), 
                           whichSDMs=c("PA","MsPP","PO"))
retLst <- saveScenariosSummaryStats(stats, retLst, namesStats)
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "Intercept", whichCoeffs = 1, 
                           plotSDMs = c("PA", "MsPP","PO"), diffPORange = c(TRUE,FALSE), 
                           bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(retLst$scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle) #, ylimVals = c(0,0.2,0,1.0))
plotScenariosSummaryLambda(retLst$statsObj$stats[[1]], "CovarCoeffs", whichCoeffs = 2:4, 
                           plotSDMs = c("PA", "MsPP","PO"), diffPORange = c(FALSE,FALSE), 
                           bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(retLst$scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle) #, ylimVals = c(0,0.5,0,4.0))
plotScenariosSummaryLambda(retLst$statsObj$stats[[2]], retLst$statsObj$namesStats[2], 
                           plotSDMs = c("PA", "MsPP","PO"), plotDevice="png",
                           plotDir = paste0(retLst$scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, horizontalLines = c(1,0)) #, ylimVals = c(0.5,1.0,0,0.5))
plotScenariosSummaryLambda(retLst$statsObj$stats[[3]], retLst$statsObj$namesStats[3], 
                           plotSDMs = c("PA", "MsPP","PO"), plotDevice="png",
                           plotDir = paste0(retLst$scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle) #, ylimVals = c(0.0,1.0,0.0,1.0))

# Reload data.
scenariosDir <- paste0(getwd(), "/", "Output-RealDiffNumPO-runs", format(highNumRuns), "-centred")
retLst <- loadScenariosResults(scenariosDir)
#retscenariosObj <- retLst$scenariosObj
#scenariosObj$scenariosDir <- scenariosDir
retLst$scenariosObj <- addPrettyNamesToScenarios(retLst$scenariosObj, scenariosPrettyNames)
retLst$statsObj <- addPrettyNamesToStats(retLst$statsObj, scenariosObj$prettyNamesScenarios)


# Save data (will include stats and prettynames if these have been added since last save).
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)


############################
#                          #
#   Comparison Plotting    #
#                          #
############################


#-------------------------------------------
# Single change comparison of experiments plot.
#-------------------------------------------

experimentDirs <- c("Output-DiffNumPA-runs5000-centred",
                    "Output-DiffNumPO-runs5000-centred",
                    "Output-SampBias-runs5000-centred",
                    "Output-ClustSurvLocs2-runs5000-centred")
experimentNames <- c("Experiment 1.1","Experiment 1.2 ","Experiment 1.3","Experiment 1.4")
plotDir <- paste0(getwd(),"/Output-Combined/Plots/SingleChangeComparison")
xAxisTitle <- c("num samples", "~ num of PO obs", "fraction sampling bias","num surveys")
xAxisReverse <- c(TRUE,TRUE,FALSE,FALSE)

# Statistic 1a: difference between true and estimated intercept coefficients.
plotExperimentComparisons(experimentDirs, 1, whichCoeffs = 1, plotSDMs = c("PA","MsPP","PO"),
                          plotDevice = "png", plotDir = plotDir, ylimVals = c(0,0.5,0,1.2), 
                          fileName = "intercept", xAxisTitle = xAxisTitle, diffPORange = c(0,20),
                          xAxisReverse = xAxisReverse, vioPlots=TRUE,
                          columnHeadings = experimentNames)

# Statistic 1b: difference between true and estimated covariate coefficients.
plotExperimentComparisons(experimentDirs, 1, whichCoeffs = c(2:4), plotSDMs = c("PA","MsPP","PO"),
                          plotDevice = "png", plotDir = plotDir, ylimVals = c(0,1,0,10), 
                          fileName = "covarCoeffs", xAxisTitle = xAxisTitle, 
                          xAxisReverse = xAxisReverse, vioPlots=TRUE,  #diffPORange = c(0,3),
                          columnHeadings = experimentNames)

# Statistic 2: similarity of intensities at each cell.  
plotExperimentComparisons(experimentDirs, 2, plotSDMs = c("PA","MsPP","PO"),
                          plotDevice = "png", plotDir = plotDir, ylimVals = c(0.5,1.0,0,0.5),
                          fileName = "correlation", xAxisTitle = xAxisTitle, horizontalLines = c(1,0), 
                          xAxisReverse = xAxisReverse, vioPlots = TRUE,
                          columnHeadings = experimentNames)


# Statistic 3: difference between true and estimated total abundance 
# (i.e. sum(lambda(cell)) for all cells in the domain).
plotExperimentComparisons(experimentDirs, 3, plotSDMs = c("PA","MsPP","PO"),   
                          plotDevice = "png", plotDir = plotDir, #ylimVals = c(0,1.0,0,2.0),
                          fileName = "totalAbundance", xAxisTitle = xAxisTitle, 
                          xAxisReverse = xAxisReverse, vioPlots = TRUE, 
                          columnHeadings = experimentNames)

#-------------------------------------------
# Clustering comparison of experiments plot.
#-------------------------------------------

experimentDirs <- c("Output-ClustSurvLocs2-runs5000-centred",
                    "Output-ClustLocs2LowPA-runs5000-centred",
                    "Output-ClustLocs2LowPASampBias-runs5000-centred",
                    "Output-RealClustLocs-runs5000-centred")
experimentNames <- c("Experiment 1.4","Experiment 2.4","Experiment 3.4","Experiment 4.2")
columnHeadings <- c("*", "-nSamp", "-nSamp +bias", "-nSamp +bias -nObs")
plotDir <- paste0(getwd(),"/Output-Combined/Plots/ClusteredLocsComparison")
xAxisTitle <- "number of surveys"
plotTheseSDMs = c("PA","MsPP","PO")

# Statistic 1a: difference between true and estimated intercept coefficients.
plotExperimentComparisons(experimentDirs, 1, whichCoeffs = 1, plotSDMs = plotTheseSDMs,
                          plotDevice = "png", plotDir = plotDir,  #ylimVals = c(0,0.5,0,1), 
                          fileName = "intercept", xAxisTitle = xAxisTitle, diffPORange = c(0,20),
                          columnHeadings = columnHeadings)

# Statistic 1b: difference between true and estimated covariate coefficients.
plotExperimentComparisons(experimentDirs, 1, whichCoeffs = c(2:4), plotSDMs = plotTheseSDMs,
                          plotDevice = "png", plotDir = plotDir, #ylimVals = c(0,0.5,0,5), 
                          fileName = "covarCoeffs", xAxisTitle = xAxisTitle,  
                          columnHeadings = columnHeadings)

# Statistic 2: similarity of intensities at each cell.  
plotQMSSDMs <- c("PA","MsPP")
plotExperimentComparisons(experimentDirs, 2, plotSDMs = plotQMSSDMs, plotHeight = 15,  
                          plotDevice = "png", plotDir = plotDir, ylimVals = c(0.4,1.0,0,0.5),
                          fileName = "correlationQMS", xAxisTitle = xAxisTitle,
                          columnHeadings = columnHeadings, horizontalLines = c(1,0))



# Statistic 3: difference between true and estimated total abundance 
# (i.e. sum(lambda(cell)) for all cells in the domain).
plotExperimentComparisons(experimentDirs, 3, plotSDMs = plotTheseSDMs,
                          plotDevice = "png", plotDir = plotDir, #ylimVals = c(0,1.0,0,1.0),
                          fileName = "totalAbundance", xAxisTitle = xAxisTitle,
                          columnHeadings = columnHeadings)


#-------------------------------------------
# Sample bias comparison of experiments plot.
#-------------------------------------------

experimentDirs <- c("Output-SampBias-runs5000-centred",
                    "Output-SampBiasLowPO-runs5000-centred",
                    "Output-SampBiasLowPOLowClust-runs5000-centred",
                    "Output-RealSampBias5-runs5000-centred")
experimentNames <- c("Experiment 1.3","Experiment 2.3","Experiment 3.5","Experiment 4.1")
columnHeadings <- c("*", "-nPO", "-nPO -nSurv", "-nPO -nSurv -nSamp")
plotDir <- paste0(getwd(),"/Output-Combined/Plots/SampleBiasComparison")
xAxisTitle <- "fraction of true sampling bias"

# Statistic 1a: difference between true and estimated intercept coefficients.
plotExperimentComparisons(experimentDirs, 1, whichCoeffs = 1, plotSDMs = c("PA","MsPP","PO"),
                          plotDevice = "png", plotDir = plotDir, #ylimVals = c(0,0.5,0,1), 
                          fileName = "intercept", xAxisTitle = xAxisTitle, plotHeight = 20,
                          columnHeadings = columnHeadings, diffPORange = c(0,20))

# Statistic 1b: difference between true and estimated covariate coefficients.
plotExperimentComparisons(experimentDirs, 1, whichCoeffs = c(2:4), plotSDMs = c("PA","MsPP","PO"),
                          plotDevice = "png", plotDir = plotDir, #ylimVals = c(0,0.5,0,5), 
                          fileName = "covarCoeffs", xAxisTitle = xAxisTitle, plotHeight = 20,
                          columnHeadings = columnHeadings)

# Statistic 2: similarity of intensities at each cell.  
plotExperimentComparisons(experimentDirs, 2, plotSDMs = c("PA","MsPP","PO"),
                          plotDevice = "png", plotDir = plotDir, ylimVals = c(0.4,1.0,0,0.5),
                          fileName = "correlation", xAxisTitle = xAxisTitle,  plotHeight = 20,
                          columnHeadings = columnHeadings, horizontalLines = c(1,0)) 
                          #, whichSpecies = c("sp188","sp107","sp399"))


# Statistic 3: difference between true and estimated total abundance 
# (i.e. sum(lambda(cell)) for all cells in the domain).
plotExperimentComparisons(experimentDirs, 3, plotSDMs = c("PA","MsPP","PO"), plotHeight = 20,  
                          plotDevice = "png", plotDir = plotDir, #ylimVals = c(0,1.0,0,2.0),
                          fileName = "totalAbundance", xAxisTitle = xAxisTitle,
                          columnHeadings = columnHeadings)



#-------------------------------------------
# Numbers of PO comparison of experiments plot.
#-------------------------------------------

experimentDirs <- c("Output-DiffNumPOExtended-runs5000-centred",
                    "Output-DiffNumPOSampBias-runs5000-centred",
                    "Output-DiffNumPOLowPASampBias-runs5000-centred",
                    "Output-RealDiffNumPO-runs5000-centred")
experimentNames <- c("Experiment 1.5","Experiment 2.7", "Experiment 3.3","Experiment 4.3")
columnHeadings <- c("*", "+bias","+bias -nSamp", "+bias -nSamp -nSurv")
plotDir <- paste0(getwd(),"/Output-Combined/Plots/NumPOComparison")
xAxisTitle <- "approximate number of PO observations"
xAxisReverse = TRUE   # NB: same for all experiments!

# Statistic 1a: difference between true and estimated intercept coefficients.
plotExperimentComparisons(experimentDirs, 1, whichCoeffs = 1, plotSDMs = c("PA","MsPP","PO"),
                          plotDevice = "png", plotDir = plotDir, #ylimVals = c(0,0.5,0,1), 
                          fileName = "intercept", xAxisTitle = xAxisTitle, diffPORange = c(0,20), 
                          columnHeadings = columnHeadings,  
                          xAxisReverse = xAxisReverse)

# Statistic 1b: difference between true and estimated covariate coefficients.
plotExperimentComparisons(experimentDirs, 1, whichCoeffs = c(2:4), plotSDMs = c("PA","MsPP","PO"),
                          plotDevice = "png", plotDir = plotDir, #ylimVals = c(0,0.5,0,5), 
                          fileName = "covarCoeffs", xAxisTitle = xAxisTitle,  
                          columnHeadings = columnHeadings, xAxisReverse = xAxisReverse)

# Statistic 2: similarity of intensities at each cell.  
plotExperimentComparisons(experimentDirs, 2, plotSDMs = c("PA","MsPP","PO"), 
                          plotDevice = "png", plotDir = plotDir, #ylimVals = c(0.5,1.0,0,0.5),
                          fileName = "correlation", xAxisTitle = xAxisTitle, horizontalLines = c(1,0),
                          columnHeadings = columnHeadings, xAxisReverse = xAxisReverse)


# Statistic 3: difference between true and estimated total abundance 
# (i.e. sum(lambda(cell)) for all cells in the domain).
plotExperimentComparisons(experimentDirs, 3, plotSDMs = c("PA","MsPP","PO"), 
                          plotDevice = "png", plotDir = plotDir, # ylimVals = c(0,1.0,0,2.0),
                          fileName = "totalAbundance", xAxisTitle = xAxisTitle,
                          columnHeadings = columnHeadings, xAxisReverse = xAxisReverse)



#-------------------------------------------
# Single change experiments: comparison of statistics plot.
#-------------------------------------------

# Experiment 1.1: decrease numbers of samples (i.e. numbers of PA rows).
experimentDir <- "Output-DiffNumPA-runs5000-centred"
columnHeadings <- c(expression(alpha[k]),
                    expression(list(beta[1*k],beta[2*k],beta[3*k])),
                    expression(lambda[ik]),
                    expression(Sigma[i]*lambda[ik]))
xAxisTitle <- "number samples"
xAxisReverse <- TRUE
plotStatisticsComparisons(experimentDir, plotSDMs = c("PA","MsPP"),
                          plotDevice = "png", plotHeight = 15, 
                          fileName = "statsCompare", xAxisTitle = xAxisTitle, 
                          columnHeadings = columnHeadings, xAxisReverse = xAxisReverse)


# Experiment 1.4: decrease numbers of surveys.
experimentDir <- "Output-ClustSurvLocs2-runs5000-centred"
columnHeadings <- c(expression(alpha[k]),
                    expression(list(beta[1*k],beta[2*k],beta[3*k])),
                    expression(lambda[ik]),
                    expression(Sigma[i]*lambda[ik]))
xAxisTitle <- "number surveys"
xAxisReverse <- FALSE
plotStatisticsComparisons(experimentDir, plotSDMs = c("PA","MsPP"),
                          plotDevice = "png", plotHeight = 15, 
                          fileName = "statsCompare", xAxisTitle = xAxisTitle, 
                          columnHeadings = columnHeadings, xAxisReverse = xAxisReverse)


# Experiment 1.2: decrease numbers of PO observations.
experimentDir <- "Output-DiffNumPO-runs5000-centred"
columnHeadings <- c(expression(alpha[k]),
                    expression(list(beta[1*k],beta[2*k],beta[3*k])),
                    expression(lambda[ik]),
                    expression(Sigma[i]*lambda[ik]))
xAxisTitle <- "approximate num of PO observations"
xAxisReverse <- TRUE
plotStatisticsComparisons(experimentDir, plotSDMs = c("PO","MsPP"), diffPOIntRange = c(0,20),
                          plotDevice = "png", plotHeight = 15, 
                          fileName = "statsCompare", xAxisTitle = xAxisTitle, 
                          columnHeadings = columnHeadings, xAxisReverse = xAxisReverse)

# Experiment 1.3: increase in level of sample bias.
experimentDir <- "Output-SampBias-runs5000-centred"
columnHeadings <- c(expression(alpha[k]),
                    expression(beta[1*k]),
                    expression(list(beta[2*k], beta[3*k])),
                    expression(lambda[ik]))
xAxisTitle <- "fraction of sampling bias"
xAxisReverse <- FALSE
plotStatisticsComparisons(experimentDir, whichStats = c(1,1,1,2), whichCoeffs = list(1,2,c(3,4),NULL), 
                          plotSDMs = c("PO","MsPP"), 
                          plotDevice = "png", plotHeight = 15,  diffPOIntRange = c(0,20),
                          fileName = "statsCompare", xAxisTitle = xAxisTitle, 
                          columnHeadings = columnHeadings, xAxisReverse = xAxisReverse)

plotStatisticsComparisons(experimentDir, whichStats = c(1,1,1,2,3), whichCoeffs = list(1,2,c(3,4),NULL,NULL), 
                          plotSDMs = c("PO","MsPP"), plotWidth = 18.75,
                          plotDevice = "png", plotHeight = 15,  diffPOIntRange = c(0,20),
                          fileName = "statsCompareSec3", xAxisTitle = xAxisTitle, 
                          columnHeadings = c(columnHeadings,expression(Sigma[i] * lambda[ik])), xAxisReverse = xAxisReverse)

plotStatisticsComparisons(experimentDir, whichStats = c(1,1,1,2,3), whichCoeffs = list(1,2,c(3,4),NULL,NULL), 
                          plotSDMs = c("PO"), plotWidth = 18.75,
                          plotDevice = "png", plotHeight = 9,  diffPOIntRange = c(0,20),
                          fileName = "statsPOSDM", xAxisTitle = xAxisTitle, 
                          columnHeadings = c(columnHeadings,expression(Sigma[i]*lambda[ik])), xAxisReverse = xAxisReverse)

#-------------------------------------------
# Multiple change experiments: comparison of statistics plot.
#-------------------------------------------

# Experiment 4.1: increase sample bias
experimentDir <- "Output-RealSampBias5-runs5000-centred"
columnHeadings <- c(expression(alpha[k]),
                    expression(list(beta[1*k],beta[2*k],beta[3*k])),
                    expression(lambda[ik]),
                    expression(Sigma[i]*lambda[ik]))
xAxisTitle <- "fraction of sampling bias"
xAxisReverse <- FALSE
plotStatisticsComparisons(experimentDir, plotSDMs = c("PA","MsPP"),
                          plotDevice = "png", plotHeight = 15, 
                          fileName = "statsCompare", xAxisTitle = xAxisTitle, 
                          columnHeadings = columnHeadings, xAxisReverse = xAxisReverse)

# Experiment 4.1: increase sample bias
experimentDir <- "Output-RealSampBias5-runs5000-centred"
columnHeadings <- c(expression(alpha[k]),
                    expression(beta[1*k]),
                    expression(list(beta[2*k],beta[3*k])),
                    expression(lambda[ik]))
xAxisTitle <- "fraction of sampling bias"
xAxisReverse <- FALSE
plotStatisticsComparisons(experimentDir, plotSDMs = c("PA","MsPP","PO"), whichStats = c(1,1,1,2),
                          plotDevice = "png", plotHeight = 20,  whichCoeffs = list(1,2,c(3,4),NULL), 
                          fileName = "statsCompare2", xAxisTitle = xAxisTitle, 
                          columnHeadings = columnHeadings, xAxisReverse = xAxisReverse)

# Experiment 4.2: decrease numbers of surveys 
experimentDir <- "Output-RealClustLocs-runs5000-centred"
columnHeadings <- c(expression(alpha[k]),
                    expression(list(beta[1*k],beta[2*k],beta[3*k])),
                    expression(lambda[ik]),
                    expression(Sigma[i]*lambda[ik]))
xAxisTitle <- "number surveys"
xAxisReverse <- FALSE
plotStatisticsComparisons(experimentDir, plotSDMs = c("PA","MsPP"),
                          plotDevice = "png", plotHeight = 15, 
                          fileName = "statsCompare", xAxisTitle = xAxisTitle, 
                          columnHeadings = columnHeadings, xAxisReverse = xAxisReverse)


# Experiment 4.3: decrease numbers of PO observations 
experimentDir <- "Output-RealDiffNumPO-runs5000-centred"
columnHeadings <- c(expression(alpha[k]),
                    expression(list(beta[1*k],beta[2*k],beta[3*k])),
                    expression(lambda[ik]),
                    expression(Sigma[i]*lambda[ik]))
xAxisTitle <- "approximate num of PO observations"
xAxisReverse <- TRUE
plotStatisticsComparisons(experimentDir, plotSDMs = c("PA","MsPP"),
                          plotDevice = "png", plotHeight = 15, 
                          fileName = "statsCompare", xAxisTitle = xAxisTitle, 
                          columnHeadings = columnHeadings, xAxisReverse = xAxisReverse)



#--------------------------------
# EXTRA for review - Find low number of PA
#--------------------------------

# Run a few different values of the number of survey locations 
deltaMultiplier <- 0.0
numClusters <- as.integer(0)
numPA <-  as.integer(seq(from=450, to=50, by=-50)) #as.integer(c(seq(from=500,to=5000,by=500),seq(from=6000,to=15000,by=1000)))
gammaMultiplier <- 1.0
numRuns <- highNumRuns 
scenariosDir <- paste0(getwd(), "/", "Output-SetupLowPA-runs", format(numRuns), "-centred")
scenariosPrettyNames <- as.character(numPA)
xAxisTitle <- "number of samples"

# Run scenarios ...
scenariosObj <- initialiseScenarios(scenariosDir, numPA, gammaMultiplier, deltaMultiplier, 
                                    numClusters, numRuns, prettyNames = scenariosPrettyNames)
retLst <- runScenarios(scenariosObj, useSDM=c("PA","MsPP"), randomSeed = randomSeed, 
                       covarsCentred = TRUE) #, numCoresUse = 14)  

# Save data.
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)

# Errors ...
deSink()
showAllScenariosErrors(retLst$resLstAll) 
showAllScenariosWarnings(retLst$resLstAll)
numSuccessfulRunsScenarios(scenariosObj, retLst$resLstAll)

# Plot response side summary statistics ...
namesStats <- c("Coefficients", "Correlation", "Percent Diff Total Abundance")
stats <- makeScenarioStats(scenariosObj, retLst$resLstAll, whichStats = c(1,2,3), 
                           whichSDMs=c("PO","PA","MsPP"))
retLst <- saveScenariosSummaryStats(stats, retLst)

plotScenariosSummaryLambda(stats[[1]], "Intercept", whichCoeffs = 1, plotSDMs = c("PO","PA","MsPP"), 
                           diffPORange = c(TRUE,FALSE), bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, ylimVals = c(0,1))
plotScenariosSummaryLambda(stats[[1]], "Coefficients", whichCoeffs = 2:4, plotSDMs = c("PO","PA","MsPP"), 
                           diffPORange = c(TRUE,FALSE), bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, ylimVals = c(0,1))
plotScenariosSummaryLambda(retLst$statsObj$stats[[2]], retLst$statsObj$namesStats[2], 
                           plotSDMs = c("PO","PA","MsPP"), vioPlots = TRUE, plotDevice="png",
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)
plotScenariosSummaryLambda(retLst$statsObj$stats[[3]], retLst$statsObj$namesStats[3], 
                           plotSDMs = c("PO","PA","MsPP"), vioPlots = TRUE, plotDevice="png",
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)


#--------------------------------
# Extra for review - Real: decrease to low PA
#--------------------------------

# Run a few different values of the number of survey locations 
deltaMultiplier <- 2.0
numClusters <- as.integer(5)
numPA <-  as.integer(seq(from=450, to=50, by=-50)) #as.integer(c(seq(from=500,to=5000,by=500),seq(from=6000,to=15000,by=1000)))
gammaMultiplier <- 1.3
numRuns <- highNumRuns 
scenariosDir <- paste0(getwd(), "/", "Output-RealVeryLowPA-runs", format(numRuns), "-centred")
scenariosPrettyNames <- as.character(numPA)
xAxisTitle <- "number of samples"

# Run scenarios ...
scenariosObj <- initialiseScenarios(scenariosDir, numPA, gammaMultiplier, deltaMultiplier, 
                                    numClusters, numRuns, prettyNames = scenariosPrettyNames)
retLst <- runScenarios(scenariosObj, useSDM=c("PO","PA","MsPP"), randomSeed = randomSeed, 
                       covarsCentred = TRUE) #, numCoresUse = 14)  

# Save data.
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)

# Errors ...
deSink()
showAllScenariosErrors(retLst$resLstAll) 
showAllScenariosWarnings(retLst$resLstAll)
numSuccessfulRunsScenarios(scenariosObj, retLst$resLstAll)

# Plot response side summary statistics ...
namesStats <- c("Coefficients", "Correlation", "Percent Diff Total Abundance")
stats <- makeScenarioStats(scenariosObj, retLst$resLstAll, whichStats = c(1,2,3), 
                           whichSDMs=c("PA","MsPP"))
retLst <- saveScenariosSummaryStats(stats, retLst)

plotScenariosSummaryLambda(stats[[1]], "Intercept", whichCoeffs = 1, plotSDMs = c("PA","MsPP"), 
                           diffPORange = c(TRUE,FALSE), bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, ylimVals = c(0,1))
plotScenariosSummaryLambda(stats[[1]], "Coefficients", whichCoeffs = 2:4, plotSDMs = c("PA","MsPP"), 
                           diffPORange = c(TRUE,FALSE), bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, ylimVals = c(0,1))
plotScenariosSummaryLambda(retLst$statsObj$stats[[2]], retLst$statsObj$namesStats[2], 
                           plotSDMs = c("PA","MsPP"), vioPlots = TRUE, plotDevice="png",
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)
plotScenariosSummaryLambda(retLst$statsObj$stats[[3]], retLst$statsObj$namesStats[3], 
                           plotSDMs = c("PA","MsPP"), vioPlots = TRUE, plotDevice="png",
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)



#--------------------------------
# Extra for review - does small amount of PA mater when have loads of PO?
#--------------------------------

# Run a few different values of the number of survey locations 
deltaMultiplier <- seq(from=0.0, to=2.0, by=0.5)
numClusters <- as.integer(0)
numPA <- 250  
gammaMultiplier <- 0.9
numRuns <- highNumRuns 
scenariosDir <- paste0(getwd(), "/", "Output-HighPOVeryLowPA-runs", format(numRuns), "-centred")
scenariosPrettyNames <- as.character(deltaMultiplier)
xAxisTitle <- "fraction of true sampling bias"

# Run scenarios ...
scenariosObj <- initialiseScenarios(scenariosDir, numPA, gammaMultiplier, deltaMultiplier, 
                                    numClusters, numRuns, prettyNames = scenariosPrettyNames)
retLst <- runScenarios(scenariosObj, useSDM=c("PO","MsPP"), randomSeed = randomSeed, 
                       covarsCentred = TRUE) #, numCoresUse = 14)  

# Save data.
retLst$scenariosObj <- scenariosObj
saveScenariosResults(scenariosObj$scenariosDir, retLst)

# Errors ...
deSink()
showAllScenariosErrors(retLst$resLstAll) 
showAllScenariosWarnings(retLst$resLstAll)
numSuccessfulRunsScenarios(scenariosObj, retLst$resLstAll)

# Plot response side summary statistics ...
namesStats <- c("Coefficients", "Correlation", "Percent Diff Total Abundance")
stats <- makeScenarioStats(scenariosObj, retLst$resLstAll, whichStats = c(1,2,3), 
                           whichSDMs=c("PO","MsPP"))
retLst <- saveScenariosSummaryStats(stats, retLst)

plotScenariosSummaryLambda(stats[[1]], "Intercept", whichCoeffs = 1, plotSDMs = c("PO","MsPP"), 
                           diffPORange = c(TRUE,FALSE), bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, ylimVals = c(0,1))
plotScenariosSummaryLambda(stats[[1]], "beta1", whichCoeffs = 2, plotSDMs = c("PO","MsPP"), 
                           diffPORange = c(TRUE,FALSE), bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, ylimVals = c(0,1))
plotScenariosSummaryLambda(stats[[1]], "other betas", whichCoeffs = 3:4, plotSDMs = c("PO","MsPP"), 
                           diffPORange = c(TRUE,FALSE), bigNumThreshold = 100, plotDevice = "png", 
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle, ylimVals = c(0,1))
plotScenariosSummaryLambda(retLst$statsObj$stats[[2]], retLst$statsObj$namesStats[2], 
                           plotSDMs = c("PO","MsPP"), vioPlots = TRUE, plotDevice="png",
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)
plotScenariosSummaryLambda(retLst$statsObj$stats[[3]], retLst$statsObj$namesStats[3], 
                           plotSDMs = c("PO","MsPP"), vioPlots = TRUE, plotDevice="png",
                           plotDir = paste0(scenariosObj$scenariosDir, "/Plots"),
                           xAxisTitle = xAxisTitle)



#############################
# diagnostics 
#############################

# Average numPO generated
tmp <- matrix(nrow=retLst$scenariosObj$numScenarios,ncol=2)
for ( i in 1:retLst$scenariosObj$numScenarios) {
  tmp[i,1] <- sum(apply(retLst$resLstAll[[i]]$numPresencesPO,1,mean,na.rm=TRUE))
  tmp[i,2] <- mean(apply(retLst$resLstAll[[i]]$numPresencesPO,2,sum,na.rm=TRUE))
}

# Plot of range of numPO generated across runs (where 1 is the scenario).
hist(apply(retLst$resLstAll[[5]]$numPresencesPO,2,sum,na.rm=TRUE))    


# Average number of each species in PO data across change.
numScenarios <- retLst$scenariosObj$numScenarios
namesSpecies <- retLst$simObj$namesSpecies
numSpecies <- length(namesSpecies)
nPOSpecies <- matrix(ncol=numScenarios, nrow=numSpecies, 
                     dimnames = list(namesSpecies, retLst$scenariosObj$prettyNamesScenarios))
for ( sp in 1:numSpecies) {
  species <- namesSpecies[sp]
  for ( sc in 1:numScenarios ) {
    nPOSpecies[species, sc] <- mean(retLst$resLstAll[[sc]]$numPresencesPO[species, ], na.rm=TRUE)
  }
}

# Plot to show average number of species in PO data across scenarios.
symbol <- list(pch=rep(c("+","x","o"),times=7),
               col=rep(c("black","red","cyan","green","blue","magenta","gold"),each=3))
plot(c(1,numScenarios), range(nPOSpecies-nPOSpecies[ ,1]), type="n", xaxt="n", xlab=xAxisTitle)
for ( sp in 1:numSpecies ) {
  species <- namesSpecies[sp]
  points(1:numScenarios, nPOSpecies[species, ]-nPOSpecies[species,1], pch=symbol$pch[sp], 
         col=symbol$col[sp])
  lines(1:numScenarios, nPOSpecies[species, ]-nPOSpecies[species,1], lty="dotted", 
        col=symbol$col[sp])
}
abline(h=0)
legend("topleft", namesSpecies, col=symbol$col, pch=symbol$pch, lty="dotted")
axis(1, 1:numScenarios, retLst$scenariosObj$prettyNamesScenarios)



# Plot to show relationship between nPO and nPA presences for each species.
scenarioNum <- 1
numPresencesPA <- retLst$resLstAll[[scenarioNum]]$numPresencesPA
numPresencesPO <- retLst$resLstAll[[scenarioNum]]$numPresencesPO
namesSpecies <- dimnames(numPresencesPA)[[1]]
numSpecies <- length(namesSpecies)
numRuns <- dim(numPresencesPA)[[2]]
plot(c(1,numSpecies), range(numPresencesPA, numPresencesPO), type="n", xlab="", xaxt="n", 
     ylab="num presences")
for ( k in 1:numSpecies ) {
  species <- namesSpecies[k]
  points(rep(k,numRuns), numPresencesPA[species, ], pch="_", col="black")
  points(rep(k,numRuns), numPresencesPO[species, ], pch="_", col="red")
}
axis(1, labels=namesSpecies, at=1:numSpecies, las=2)
title(paste0("nPA = ",retLst$scenariosObj$prettyNamesScenarios[scenarioNum]))

# Plot to show relationship between environmental covariate and sample sites.
load(paste0(getwd(),"/Input/EnvirCovariates.RData"))      # Contains envirObj
envirMat <- getValuesMask(envirObj$data, envirObj$mask)
scenariosDir <- paste0(getwd(), "/", "Output-RealClustLocs-runs5000-centred")
load(paste0(scenariosDir,"/Data/DataDump.RData"))        # Contains scenariosObj for this experiment.
scenariosObj$scenariosDir <- scenariosDir                # There has been a change in directory names since some of these were created.
makeDensityPlots(envirMat[ ,1], scenariosObj, whichScenarios = c(1,3,5,6,7), whichRuns = 5000,
                 legendTitle="num surveys", xLabel="centred bathymetry", plotDevice = "png")


##### Plot to show difference between true species dist and PO SDM estimated when lots of bias.

# Look at just sample bias experiment (with no other degradations).
sampBiasExpDir <- paste0(getwd(), "/Output-SampBias-runs5000-centred")

# Load results and plotting object (just simpler to use functions already written for this object)
tmp <- load(paste0(sampBiasExpDir,"/Data/DataDump.RData"))

# Worst species based on correlation statistic for PO SDM and high sample bias.
worstSpecies <- names(sort(statsObj$stats[[2]]$avg["2.0", ,"PO",1]))[1]

# Get coastline polygon.
load(paste0("../Data/Coastlines/southOceanPoly.RData"))
coastPoly <- cm1
rm(cm1)
if ( ! compareCRS(coastPoly@proj4string, simObj$proj) ) {
  # Project first, then set.
  coastPoly <- spTransform(coastPoly, simObj$proj)
}

plotScenarioMaps(sampBiasExpDir, worstSpecies, "PO", c("DMult0.0","DMult2.0"), 
                 plotDevice = "png", plotDir = paste0(sampBiasExpDir,"/Plots"), 
                 plotWidth = 10.7, coastLine = coastPoly) #, usePalette = rev(heat.colors(255)))


#### Investigate use of Pearson's correlation.

# Get data.
expDir <- paste0(getwd(), "/Output-RealClustLocs-runs5000-centred")
tmp <- load(paste0(expDir, "/Data/DataDump.RData"))                    # will give resLstAll, simObj, scenariosObj, plotObj, statsObj
whichScenario <- scenariosObj$numScenarios
scenarioDir <- paste0(expDir, "/", scenariosObj$namesScenarios[whichScenario])
tmp <- load(paste0(scenarioDir, "/DataDumps/DataDump-AllRuns.RData"))  # will give simObj, domainObj, envirObj, biasObj, cellObj
tmp <- load(paste0(expDir, "/Data/DataDump.RData"))                    # will give resLstAll, simObj, scenariosObj, plotObj, statsObj

# Plot comparison of true and estimated intensities
numSpecies <- cellObj$numSpecies
namesSpecies <- cellObj$namesSpecies
cells <- cellObj$cells
whichRun <- 1000
whichSDM <- "MsPP"
densitylayers <- setPlotLayers()
versuslayers <- setPlotLayers()
speciesCor <- matrix(NA, nrow=numSpecies, ncol=2)
for ( k in 1:numSpecies ) {
  # True and estimated values of the intensity for this species
  species <- namesSpecies[k]
  speciesTrueLambda <- cellObj$trueLambda[ ,species]
  speciesEstCoeffs <- resLstAll[[whichScenario]][[whichSDM]]$coeffs[ ,species,whichRun]
  if ( any(is.na(speciesEstCoeffs)) ) {
    # Add empty sub-plot to plot.
    versuslayers <- addPlotData(versuslayers, c(0,1), c(0,1), plotfunc="plot", type="n",
                                xlab="", ylab="")
    versuslayers <- addPlotData(versuslayers, "bottomright", plotfunc="legend", bty = "n", 
                                legend = paste0("cor = NA"), title = species, text.col="red")
    densitylayers <- addPlotData(densitylayers, c(0,1), c(0,1), plotfunc = "plot", type="n",
                                 xlab="", ylab="")
    densitylayers <- addPlotData(densitylayers, "topright", plotfunc="legend", bty = "n", 
                                 legend = paste0("cor = NA"), title = species)
  } else {
    speciesEstLambda <- lambda.cell(simObj$lambda.formula, speciesEstCoeffs, cellObj$covars)
    speciesCor[k,1] <- cor(speciesTrueLambda, speciesEstLambda, method="pearson")
    speciesCor[k,2] <- cor(log(speciesTrueLambda), log(speciesEstLambda), method="pearson")
    speciesTrueDensity <- density(speciesTrueLambda)
    speciesEstDensity <- density(speciesEstLambda)
    
    # 2D density plot for "sp17" only.
    if ( species == "sp17") {
      rlTrue <- domainObj$mask
      rlTrue[cellObj$cells] <- speciesTrueLambda
      rlEst <- domainObj$mask
      rlEst[cellObj$cells] <- speciesEstLambda
      maxLambda <- max(maxValue(rlTrue), maxValue(rlEst))
      opar <- par(mfrow=c(2,1))
      plot(rlTrue, asp=1, ylab="TRUE",zlim=c(0,maxLambda))
      plot(rlEst,asp=1,ylab="EST",zlim=c(0,maxLambda))
      par(opar)
      #message("do ggplot")
      #x <- speciesTrueLambda
      #y <- speciesEstLambda
      #df <- data.frame(x=x,y=y)
      #save(x,y,df,file="ForDave.rda")
      #ggplot(df,aes(x=x,y=y)) + stat_binhex() + theme_bw()
    }
    
    # Add to true versus estimate plot.
    versuslayers <- addPlotData(versuslayers, speciesTrueLambda, speciesEstLambda, 
                                plotfunc="plot", xlab="", ylab="", pch=20)
    versuslayers <- addPlotData(versuslayers, "bottomright", plotfunc="legend", bty = "n", 
                                 legend = c(paste0("pears=",formatC(speciesCor[k,1], digits=3, format="f")),
                                            paste0("  log=",formatC(speciesCor[k,2], digits=3, format="f"))),
                                 title = species, text.col="red")
    
    # Add to density plot.
    xlimDensity <- range(c(speciesTrueDensity$x, speciesEstDensity$x))
    ylimDensity <- range(c(speciesTrueDensity$y, speciesEstDensity$y))
    densitylayers <- addPlotData(densitylayers, xlimDensity, ylimDensity, plotfunc = "plot", type="n",
                          xlab="", ylab="")
    densitylayers <- addPlotData(densitylayers, speciesTrueDensity$x, speciesTrueDensity$y, plotfunc = "lines", 
                          col="black", lty="solid")
    densitylayers <- addPlotData(densitylayers, speciesEstDensity$x, speciesEstDensity$y, plotfunc = "lines", 
                          col="red", lty="solid")
    densitylayers <- addPlotData(densitylayers, "topright", plotfunc="legend", bty = "n", 
                                 legend = c(paste0("pears=",formatC(speciesCor[k,1], digits=3, format="f")),
                                            paste0("  log=",formatC(speciesCor[k,2], digits=3, format="f"))),
                                 title = species)
  }
}
densitylayers <- addPlotData(densitylayers, c(0,1), c(0,1), plotfunc="plot", type="n")
densitylayers <- addPlotData(densitylayers,"center", plotfunc="legend", 
                             legend=c("True lambda","Est lambda"), bty="n",
                             col=c("black","red"), lty="solid")

# Plot layers ...
opar <- par()
newmar <- opar$mar
plot.plotLayers(densitylayers, mfrow=c(6,4), mar=c(0,0,0,0), oma=newmar, xaxt="n", yaxt="n",
                main=paste0("Comparison of true and estimated densities"),  
                xlab="Lambda", ylab="Density", outer=TRUE)
plot.plotLayers(versuslayers, mfrow=c(6,4), mar=c(0,0,0,0), oma=newmar, xaxt="n", yaxt="n",
                main=paste0("Comparison of true and estimated lambda cell values"),  
                xlab="true lambda", ylab="estimated lambda", outer=TRUE)
par(opar)



#-----------------------------------------------------------------------------------------

loadCoastBaseLayers <- function(plotObj, coastFile="~/Data/Coastlines/southOceanPoly.RData", 
                                baseFile="~/Input/ResearchBaseNames.RData",
                                simProj=proj4str.laea("km","180","-72") ) {

  # Load the coast polygon file.
  if ( file.exists(coastFile) ) {
    load(coastFile)
    coastLine.longlat <- cm1
    rm(cm1)
  } else {
    stop("Unable to load coast polygon from given file.")
  }  
  
  # Load the research bases names and locations.
  if ( file.exists(baseFile) ) {
    # Load data from previously saved R data file.
    load(baseFile)
  } else {
    stop("Unable to load research base names from given file.")
  }
  
  # Set in the plotting object.
  plotObj <- setPlotsLayers(plotObj, simProj, coastLine.longlat, basesObj$data)
  
  # Remove objects that aren't needed anymore.
  rm(coastLine.longlat, basesObj)
  
  # Return plotting object.
  return(plotObj)
  
}

