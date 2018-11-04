makeSpeciesIntensityCoeffs <- function(PA, lambda.formula, envirCovars, domainMask) {
                           
  # Estimate the species intensity coefficients for the given formula.
  #
  # Arguments ...
  # domainObj:      a domain object
  # PA:             species presence-absence data contained within a data.frame object.  Must
  #                 contain at least four columns with column names x, y, area and species.
  #                 Unique locations (i.e. unique x and y) are the rows and there are 
  #                 numSpecies + 3 columns.  Each species column contains a zero in the 
  #                 row if it was not found at the locations, and a one if it was.
  # lambda.formula: a formula object that contains how to combine the information for a 
  #                 species intensity function.  Assumes there is an intercept!
  # envircovars:    the species intensity covariates
  # domainMask:     a RasterLayer that defines the domain

  # Check formulae.
  if ( !inherits(lambda.formula,"formula") ) lambda.formula <- as.formula(lambda.formula)

  # Numbers of things.
  numSpecies <- dim(PA)[2] - 3
  namesSpecies <- names(PA)[4:(numSpecies+3)]
  numCoeffs <- numCoefficients(lambda.formula)
  
  # Initialise return value.
  coeffLst <- list(alpha=vector("double", numSpecies), beta=matrix(nrow=numCoeffs-1, ncol=numSpecies))
  colnames(coeffLst$beta) <- namesSpecies 
  
  # Get the survey cells.
  surveyCells <- cellFromXY(domainMask, PA[ ,c("x","y")])
  surveyCovars <- as.data.frame(envirCovars[surveyCells], stringsAsFactors=FALSE)
  
  # Run the presence-absence glm.  Up to here 22/06/17 test new centring in runSDM.Cloglog function!
  tryRes <- try(runSDM.Cloglog(lambda.formula, surveyCells, surveyCovars, 
                               PA[ ,namesSpecies], PA$area))
  if ( inherits(tryRes, "try-error") ) {
    # An error occurred that stopped the function.
    stop(tryRes[1])
  } else { 
    # Some errors may have occurred inside the function that did not require it to stop.
    if ( tryRes$isError ) {
      numErrors <- length(tryRes$errors$errMsg)
      for ( i in 1:numErrors) {
        warning(paste0("Species ", tryRes$errors$species, ": ", tryRes$errors$errMsg))
      }
    }
    
    # Set the estimated coefficients.
    coeffLst$alpha <- tryRes$estCoeffs[1, ]
    coeffLst$beta <- tryRes$estCoeffs[-1, ]
  }

  # Return results.
  return(coeffLst)
  
} 

#-----------------------------------------------------------------------------------------

makeProbabilityObsCoeffs <- function(PO, bias.formula, biasCovars, 
                                      lambda.formula, envirCovars, alpha, beta,
                                      domainObj, numBGPoints, 
                                     centreCovars=rep(0,nlayers(biasCovars))) {
  
  # Estimate the probability of observation part of the sample biased species intensity
  # (i.e. b(x,y) from lambda(x,y)*b(x,y) in Fithian, et al.).  Use the previously estimated 
  # log of the species intensity (ln(lambda(x,y)) = alpha + beta*envirCovars(x,y)) as an 
  # offset to get just b(x,y).
  #
  # domain : owin version of domain!
  
  # Check formulae.
  if ( !inherits(bias.formula,"formula") ) bias.formula <- as.formula(bias.formula)
  if ( !inherits(lambda.formula,"formula") ) lambda.formula <- as.formula(lambda.formula)
  
  # Make the "all species at once" formula (replace x and y as PPM has these reserved).
  all.formula <- delete.response.formula(bias.formula)
  all.formula <- replaceVarName(all.formula, "x", "x1")
  all.formula <- replaceVarName(all.formula, "y", "y1")
  all.formula <- update.formula(all.formula, 
                                as.formula("~ -1 + species + . + offset(intensity)"))
  
  # Numbers and names of things.
  namesSpecies <- unique(PO$species)
  numSpecies <- length(namesSpecies)
  numDeltas <- numCoefficients(bias.formula, includeIntercept = FALSE)
  namesCovars <- names(biasCovars)
  
  # Initialise return value.
  coeffLst <- list(gamma=vector("double", numSpecies), delta=vector("double", numDeltas))

  # Make the background points.
  BGtmp <- makeSimBG(numBGPoints, domainObj)
  BG <- data.frame(x=rep(BGtmp$x, numSpecies), y=rep(BGtmp$y, numSpecies), 
                   species=rep(namesSpecies, each=numBGPoints))
  pppBG <- ppp(BG$x, BG$y, window=domainObj$owin)
  
  # Get the environmental covariate data at the presence-only and background points.
  namesCovars <- names(biasCovars)
  biasVals <- extract(biasCovars, BGtmp[ ,c("x","y")])
  # Up to here 25/06/2017
  BG[ ,namesCovars] <- as.data.frame(matrix(rep(t(biasVals), numSpecies), 
                                            ncol=length(namesCovars), byrow = TRUE),
                                                                stringsAsFactors = FALSE)
  PO[ ,namesCovars] <- as.data.frame(extract(biasCovars, PO[ ,c("x","y")]), stringsAsFactors=FALSE)
  
  # Get the offset values at the presence-only and background points.
  indSpecies <- 0
  for ( species in namesSpecies ) {
    indSpecies <- indSpecies + 1
    
    # This species coefficients.
    thisCoeffs <- c(alpha[species], beta[ ,species])
    
    # The rows of PO that apply to this species.
    indThisSpecies <- PO$species == species
    PO[indThisSpecies,"intensity"] <- lambda.xy(PO$x[indThisSpecies], PO$y[indThisSpecies], 
                                                lambda.formula, thisCoeffs, envirCovars, 
                                                lnLambda = TRUE)
    
    # The rows of BG that apply to this species.
    indThisSpecies <- (((indSpecies - 1) * numBGPoints) + 1):(indSpecies * numBGPoints)
    BG[indThisSpecies,"intensity"] <- lambda.xy(BG$x[indThisSpecies], BG$y[indThisSpecies],
                                                lambda.formula, thisCoeffs, envirCovars, 
                                                lnLambda = TRUE)
  }
  
  # Create a quadrature scheme that contains the presence-only points and the 
  # quadrature (or background) points.
  pppPO <- ppp(PO$x, PO$y, window=domainObj$owin)
  qsData <- quadscheme(data = pppPO, dummy = pppBG, method = "dirichlet")
  
  # Put both data together (in the same columns, not extra columns!).
  dfPPMData <- rbind(PO, BG)
  colnames(dfPPMData)[1] <- "x1"      # Rename column x as x1
  colnames(dfPPMData)[2] <- "y1"      # Rename column y as y1
  
  # Fit the PPM (non-stationary Poisson point process model).
  glmFit <- ppm(qsData, trend=all.formula, interaction=Poisson(), covariates=dfPPMData,
                gcontrol=list(maxit=100))
  if ( ! glmFit$internal$glmfit$converged ) 
    stop("PPM has not converged when calculating true probability of observation coefficients.")
  
  # Rename and reorder gamma coeffs.
  coeffLst$gamma <- glmFit$coef[1:numSpecies]
  names(coeffLst$gamma) <- sub("species", "", names(coeffLst$gamma))
  
  # Return estimated coefficients.
  coeffLst$delta <- glmFit$coef[(numSpecies+1):(numSpecies+numDeltas)]
  return(coeffLst)
  
}

#-----------------------------------------------------------------------------------------

getPOPAData <- function(domainObj, fileNamePA, fileNamePO, 
                        XcolName="long", YcolName="lat", speciesColName="species", 
                        areaColName=NULL, areaVal=NULL) {
  
  # Get the PA and PO data that will be used to estimate the true coefficients for the domain.
  # Assumes both are provided using long (x) and lat (y) coordinates.
  # Provide either areaColName or areaVal.  Both can't be NULL.
  # Column of area data will take precedence over area value, when both given.
  
  # Read in PO data and project.
  POObj <- readDataPoints(fileNamePO, xColName = XcolName, yColName = YcolName)
  POObj <- projectDataPoints(POObj, domainObj$proj, domainObj$poly)
  
  # Read in PA data and project.
  PAObj <- readDataPoints(fileNamePA, xColName = XcolName, yColName = YcolName)
  PAObj <- projectDataPoints(PAObj, domainObj$proj, domainObj$poly)
  
  # Organise into data.frame objects with column names used by simulation.
  PO <- as.data.frame(POObj$data@coords)
  PO$species <- POObj$data@data[ ,speciesColName]
  PA <- as.data.frame(PAObj$data@coords)
  
  # Sort out area column for PA data.
  if ( is.null(areaColName) && is.null(areaVal) ) {
    # No area column and no area val.
    stop("Please provide either an area column name OR area value for estimation of true coefficients.")
  } else if ( is.null(areaVal) ) {
    # Area column given.  Use column and change name to simulation name.
    PA$area <- PAObj$data@data[ ,areaColName]
  } else {
    # Area value given.  Make column and add.
    numPALocs <- dim(PA)[1]
    PA$area <- rep(areaVal, numPALocs)
  }
  
  # Add PA species columns to the coordinates and area columns.
  PAColNames <- names(PAObj$data@data)
  indColsAlreadyAdded <- which(PAColNames %in% 
                               c(XcolName, YcolName, areaColName))
  PA <- cbind(PA, PAObj$data@data[ ,-indColsAlreadyAdded])

  # Return data.
  return(list(PA=PA, PO=PO))
  
}