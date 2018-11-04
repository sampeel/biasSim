#------------------------------------------
# Plots for MEE chap1 paper
#------------------------------------------

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


#-------------------------------------------
# Section 3.1 - Sample bias effect - single change
#-------------------------------------------

# Experiment 1.3: increase in level of sample bias.
experimentDir <- "Output-SampBias-runs5000-centred"
columnHeadings <- c(expression(alpha[k]),
                    expression(beta[1*k]),
                    expression(list(beta[2*k], beta[3*k])),
                    expression(lambda[ik]),
                    expression(Sigma[i] * lambda[ik]))
xAxisTitle <- "fraction of sampling bias"
xAxisReverse <- FALSE
plotStatisticsComparisons(experimentDir, whichStats = c(1,1,1,2,3), whichCoeffs = list(1,2,c(3,4),NULL,NULL), 
                          plotSDMs = c("PO","MsPP"), plotWidth = 18.75,
                          plotDevice = "png", plotHeight = 15,  diffPOIntRange = c(0,20),
                          fileName = "statsCompareSec3", xAxisTitle = xAxisTitle, 
                          columnHeadings = columnHeadings, xAxisReverse = xAxisReverse)

#-------------------------------------------
# Section 3.2 - Multiple change experiments: comparison of statistics plot.
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

# # Experiment 4.1: increase sample bias
# experimentDir <- "Output-RealSampBias5-runs5000-centred"
# columnHeadings <- c(expression(alpha[k]),
#                     expression(beta[1*k]),
#                     expression(list(beta[2*k],beta[3*k])),
#                     expression(lambda[ik]))
# xAxisTitle <- "fraction of sampling bias"
# xAxisReverse <- FALSE
# plotStatisticsComparisons(experimentDir, plotSDMs = c("PA","MsPP","PO"), whichStats = c(1,1,1,2),
#                           plotDevice = "png", plotHeight = 20,  whichCoeffs = list(1,2,c(3,4),NULL), 
#                           fileName = "statsCompare2", xAxisTitle = xAxisTitle, 
#                           columnHeadings = columnHeadings, xAxisReverse = xAxisReverse)

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


#-------------------------------------------
# Appendix 3 - Single change comparison of experiments plot.
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
# Appendix 4 - Clustering comparison of experiments plot.
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
plotExperimentComparisons(experimentDirs, 2, plotSDMs = plotTheseSDMs, 
                          plotDevice = "png", plotDir = plotDir, ylimVals = c(0.4,1.0,0,0.5),
                          fileName = "correlation", xAxisTitle = xAxisTitle,
                          columnHeadings = columnHeadings, horizontalLines = c(1,0))

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
# Appendix 4 - Sample bias comparison of experiments plot.
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
# Appendix 4 - Numbers of PO comparison of experiments plot.
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
                          plotDevice = "png", plotDir = plotDir, ylimVals = c(0.4,1.0,0,0.5),
                          fileName = "correlation", xAxisTitle = xAxisTitle, horizontalLines = c(1,0),
                          columnHeadings = columnHeadings, xAxisReverse = xAxisReverse)


# Statistic 3: difference between true and estimated total abundance 
# (i.e. sum(lambda(cell)) for all cells in the domain).
plotExperimentComparisons(experimentDirs, 3, plotSDMs = c("PA","MsPP","PO"), 
                          plotDevice = "png", plotDir = plotDir, # ylimVals = c(0,1.0,0,2.0),
                          fileName = "totalAbundance", xAxisTitle = xAxisTitle,
                          columnHeadings = columnHeadings, xAxisReverse = xAxisReverse)



#-------------------------------
# Appendix 5
#-------------------------------

### Plot to show difference between true species dist and PO SDM estimated when zero and lots of bias.

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


#-------------------------------
# Appendix 6 - density
#-------------------------------

# Plot to show relationship between environmental covariate and sample sites.
load(paste0(getwd(),"/Input/EnvirCovariates.RData"))     # Contains envirObj
envirMat <- getValuesMask(envirObj$data, envirObj$mask)
scenariosDir <- paste0(getwd(), "/", "Output-RealClustLocs-runs5000-centred")
load(paste0(scenariosDir,"/Data/DataDump.RData"))        # Contains scenariosObj for this experiment.
scenariosObj$scenariosDir <- scenariosDir                # There has been a change in directory names since some of these were created.
makeDensityPlots(envirMat[ ,1], scenariosObj, whichScenarios = c(1,3,5,6,7), whichRuns = 5000,
                 legendTitle="num surveys", xLabel="centred bathymetry", plotDevice = "png")


# Plot to show change in "patchiness" of samples due to change in number of surveys.
# Get data.
expDir <- paste0(getwd(), "/Output-ClustLocs2LowPA-runs5000-centred")
tmp <- load(paste0(expDir, "/Data/DataDump.RData"))                    # will give resLstAll, simObj, scenariosObj, plotObj, statsObj
tmp <- load(paste0(expDir,"/", scenariosObj$namesScenarios[1],"/DataDumps/DataDump-AllRuns.RData"))  # will give simObj, domainObj, envirObj, biasObj, cellObj
rlplot <- domainObj$mask

# Get coastline polygon.
load(paste0("../Data/Coastlines/southOceanPoly.RData"))
coastPoly <- cm1
rm(cm1)
if ( ! compareCRS(coastPoly@proj4string, simObj$proj) ) {
  # Project first, then set.
  coastPoly <- spTransform(coastPoly, simObj$proj)
}

# Cycle through required scenarios for sub-plots.
layers <- setPlotLayers()
whichScenarios <- c(1,3,5,7)
for ( scenario in whichScenarios ) {
  scenarioDir <- paste0(expDir, "/", scenariosObj$namesScenarios[scenario])
  tmp <- load(paste0(scenarioDir, "/DataDumps/DataExample-run5000.RData"))  # will give PO, Count, PA, BG, cellObj, surveyObj
  
  # Initialise plotting raster.
  rlplot <- domainObj$mask
  rlplot[cellObj$cells] <- 0
  
  # Get the number of surveys per cell.
  numSamplesPerCell <- table(surveyObj$cells)
  
  # Set this in the plotting raster.
  rlplot[as.integer(names(numSamplesPerCell))] <- numSamplesPerCell
  
  # Add as plotting layer (start new sub-plot)
  layers <- addPlotData(layers, rlplot, plotfunc = "plot", asp=1)
  
  # Add coastline.
  layers <- addPlotData(layers, coastPoly, plotfunc="plot", add=TRUE)
  
}
    
# Plot the layers
opar <- par()
fileName <- makeFileName("PatchyExample", paste0(expDir,"/Plots"), plotObj$device)
plot.plotLayers(layers, plotObj$device, fileName, 
                fileUnits="cm", fileHeight=13.8, fileWidth=15.75, fileRes=600,
                mfrow=c(2,2), mar=c(2,2,2,2), oma=c(0,0,0,0))
par(opar)
