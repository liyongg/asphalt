#### STARTUP ####
Date_Packages <- '2019-11-19'
Packages <- c('doParallel', 'plyr', 'dplyr','ggplot2','grid', 'gridExtra', 'gdata', 'data.table', 'stats', 'stringr', 'zoo', 'fastDummies',
              'evaluate', 'highr', 'markdown', 'yaml', 'htmltools', 'knitr', 'readxl', 'lubridate', 'openxlsx', 'ggridges',
              'jsonlite', 'base64enc', 'mime', 'rmarkdown', 'survival', 'fda', 'fda.usc',  'fdANOVA', 'profvis', 'purrr', 'tidyr',
              'gridExtra', 'isotone', 'mgcv', 'funData', 'quantreg', 'MASS', 'fitdistrplus', 'nortest', 'rlist', 'latex2exp',
              'scales', 'gam', 'stargazer', 'xtable', 'scam', 'QuantileNPCI', 'quantreg', 'forecast', 'cowplot', 'diptest', 'log4r'
              )

Packages_New <- Packages[!(Packages %in% installed.packages())]
install.packages(Packages_New,
                 repos = paste0('https://mran.microsoft.com/snapshot/',
                                Date_Packages)
                 )

# Load packages
invisible(lapply(Packages, 
                 library, 
                 character.only = TRUE)
          )

# Load the Weg objects.
for(i in list.files(pattern = glob2rx('LCMS_DB*'))) load(i)

#### STATICS ####
ColNames_Static <- c('Weg', 'Baan', 'Strook', 'HmStart', 'HmStop', 'Vehicle', 'Geldigheid', 'Errorcode', 'lengte_meting', 'Datum_tijd')
  
MonthReplace <- c('Jan' = '01',
                  'Feb' = '02',
                  'Mar' = '03',
                  'Apr' = '04',
                  'May' = '05',
                  'Jun' = '06',
                  'Jul' = '07',
                  'Aug' = '08',
                  'Sep' = '09',
                  'Oct' = '10',
                  'Nov' = '11',
                  'Dec' = '12')

theme_set(theme_bw())


#### OBJECTS ####

## DEFINE OBJECTS ---------------------------------------------------------------------

## -- temporal.LCMS_Trace
## container for the primary data of left wheel, right wheel and overall, at
## a certain date. the date is also specified. this causes redundancy, but 
## prevents loss of reference
##
setClass("temporal.LCMS_Trace", 
         representation = representation(Datum_tijd = "Date",
                                         Vehicle = "numeric",
                                         Errorcode = "numeric",
                                         lengte_meting = "numeric",
                                         overallData = "vector",
                                         leftData = "vector",
                                         rightData = "vector")
         )

##
## -- temporal.Strook
## container for all temporal LCMS_Traces instances of a specific Strook
## i.e. a historic overview). the object also contains a dateVector, which 
## provides a fast overview on which dates are contained in the historic overview
## please note that different temporal.Strook objects may have different dateVectors,
## for example due to new constructions or demolitions
##
setClass("temporal.Strook", representation = representation(strookID = "character",
                                                            dateVector = "Date",
                                                            temporal.LCMS_Traces = "list"))


##
## -- temporal.hmVak
## container for all Stroken in a given hmVak. the object has a strookVector, which
## gives rapid insight in the Strook-composition of the hmVak
setClass("temporal.hmVak", representation = representation(hmStartPos = "numeric",
                                                           strookVector = "vector",
                                                           temporal.Stroken = "list"))

##
## -- temporal.Baan
## container for all hmVakken in given Baan. the object has a hmVakVector, which
## provides a fast overview of the hmVakken present in the Baan

setClass("temporal.Baan", representation = representation(baanID = "character", 
                                                          hmVakVector = "vector",
                                                          temporal.hmVakken = "list"))

setClass("Weg", representation = representation(wegID = 'character',
                                                banen = 'list'))

## CREATE TEMPLATE OBJECTS ------------------------------------------------------------


temporal.LCMS_Trace.template<-new("temporal.LCMS_Trace",
                                  Datum_tijd = as.Date('2010-10-10'),
                                  Vehicle = 0,
                                  Errorcode = 0,
                                  lengte_meting = 0,
                                  overallData = vector(mode = "numeric", length = 100),
                                  leftData = vector(mode = "numeric", length = 100),
                                  rightData = vector(mode = "numeric", length = 100))

temporal.Strook.template<-new("temporal.Strook", 
                              strookID = "",
                              dateVector = as.Date('2010-10-10'),
                              temporal.LCMS_Traces = list(temporal.LCMS_Trace.template)) 

temporal.hmVak.template<-new("temporal.hmVak", 
                             hmStartPos = 0,
                             strookVector = "",
                             temporal.Stroken = list(temporal.Strook.template))

temporal.Baan.template<-new("temporal.Baan",
                            baanID = "",
                            hmVakVector = 0,
                            temporal.hmVakken = list(temporal.hmVak.template))

Weg.template <- new("Weg",
                    wegID = 'R',
                    banen = list(temporal.Baan.template))


#### GENERAL FUNCTIONS ####

# ---------------------------------------------------------------------------------------------------- #
# 
# Function which can save R objects with specified names, taken from Marco Wirthlin
# https://stackoverflow.com/questions/21248065/r-rename-r-object-while-save-ing-it
#
# ---------------------------------------------------------------------------------------------------- #

saveit <- function(..., string, file) {
  x <- list(...)
  names(x) <- string
  save(list=names(x), file=file, envir=list2env(x))
}

# ---------------------------------------------------------------------------------------------------- #
# 
# Function which returns the if/else input dependent on condition check.
#
# ---------------------------------------------------------------------------------------------------- #

IfElse <- function(condition, if.cond, else.cond){
  if(condition) else.cond <- if.cond
  return(else.cond)
}

# ---------------------------------------------------------------------------------------------------- #
# 
# Function which requires a folder to store all the data in a nested list.
# The nested list is grouped based on 'HmStart', 'HmStop', 'Baan'.
#
# ---------------------------------------------------------------------------------------------------- #

Create_LCMS_DF_RAW <- function(LCMS_folder){
  Files <- list.files(path = LCMS_folder,
                      full.names = TRUE)
  LCMS_List <- lapply(Files, read.xlsx)
  for(i in seq_along(LCMS_List)){
    # Change '.' to blanks in between column names.
    colnames(LCMS_List[[i]]) <- str_replace_all(colnames(LCMS_List[[i]]), '[.]', '')
    
    # Change characters to their respective numbers.
    LCMS_List[[i]]$Datum_tijd <- str_replace_all(LCMS_List[[i]]$Datum_tijd,
                                                 MonthReplace)
    
    # For every DF per year, change the date to uniform format and save as character.
    for(j in seq_along(LCMS_List[[i]]$Datum_tijd)){
      if(is.na(as.Date(LCMS_List[[i]]$Datum_tijd[j], '%d-%m-%Y %H:%M:%OS'))){
          LCMS_List[[i]]$Datum_tijd[j] <- format(with_tz(as.POSIXct.Date(as.Date(as.numeric(LCMS_List[[i]]$Datum_tijd[j]), 
                                                                                 origin = '1899-12-30')
                                                                         ),
                                                         'UTC'), 
                                                 '%d-%m-%Y %H:%M:%OS')
      }
    }
    LCMS_List[[i]]$Datum_tijd <- as.Date(LCMS_List[[i]]$Datum_tijd, '%d-%m-%Y %H:%M:%OS')
    
    # Clean the data by only considering the observations with the most recent dates.
    LCMS_List[[i]] <- LCMS_List[[i]] %>% group_by(Baan, Strook, HmStart, HmStop) %>% slice(which.max(as.Date(Datum_tijd, '%d-%m-%Y %H:%M:%OS')))
    LCMS_List[[i]] <- subset(LCMS_List[[i]], select = AllColumns)
  }
  LCMS_DF <- rbindlist(LCMS_List)
  return(LCMS_DF)
}

# ---------------------------------------------------------------------------------------------------- #
# 
# Function which returns a string of the column names. 
#
# ---------------------------------------------------------------------------------------------------- #

ColNames <- function(Position, combineRemaining){
  if(Position == 'Left'){
      Columns <- paste0('svL_', 1:100)
  } else if(Position == 'Right'){
      Columns <- paste0('svR_', 1:100)
  } else if(Position == 'Overall'){
      Columns <- paste0('sv_', 1:100)
  } else{
     stop('`Position` is not Left/Right/Overall')
  }
  
  if(combineRemaining == 'ALL'){
        Columns <- c(ColNames_Static, Columns)
    } else if (combineRemaining == 'Date'){
        Columns <- c('Datum_tijd', Columns)
    } else if (combineRemaining == 'No'){
        Columns <- c(Columns)
    } else{
        stop('combineRemaining is not All/Date/No')
    }
  
  return(Columns)
}

AllColumns <- c(ColNames_Static, ColNames('Overall', 'No'), ColNames('Left', 'No'), ColNames('Right', 'No'))

# ---------------------------------------------------------------------------------------------------- #
# 
# Function which returns a list containing Baan, HmStart and Strook indices.
#
# ---------------------------------------------------------------------------------------------------- #

Random_Segment <- function(Weg, checkAnomalies){
  if(class(Weg) != 'Weg') stop('Input not of class `Weg`.')
  
  WegID <- Weg@wegID

  # Extract the specified anomalies dataframe.
  Ref_DF <- Anomalies_List[[WegID]]
  
  # Set condition to FALSE
  Anomalies <- TRUE
  
  while(Anomalies){
    # Select random index subsequently for Baan, hmVak and Strook.
    Random_Baan.index <- sample(length(Weg@banen), 1)
    Random_hmVak.index <- sample(length(Weg@banen[[Random_Baan.index]]@hmVakVector), 1)
    Random_Strook.index <- sample(length(Weg@banen[[Random_Baan.index]]@temporal.hmVakken[[Random_hmVak.index]]@strookVector), 1)
    
    # Find the corresponding IDs.
    BaanID <- Weg@banen[[Random_Baan.index]]@baanID
    hmVakID <- Weg@banen[[Random_Baan.index]]@temporal.hmVakken[[Random_hmVak.index]]@hmStartPos
    StrookID <- Weg@banen[[Random_Baan.index]]@temporal.hmVakken[[Random_hmVak.index]]@temporal.Stroken[[Random_Strook.index]]@strookID
    
    if(checkAnomalies == TRUE){
    # Check if there were any anomalies.
      if(nrow(Ref_DF[Ref_DF$Baan == BaanID & Ref_DF$HmStart <= hmVakID & Ref_DF$HmStop >= hmVakID & Ref_DF$Strook == StrookID,]) == 0){
        Anomalies <- FALSE
      }
    } else if(checkAnomalies == FALSE){
      Anomalies <- FALSE
    }
  }
  
  Title <- paste(WegID, BaanID, hmVakID, StrookID, sep = ' - ')
  
  return(list('Index List' = list('Baan' = Random_Baan.index, 
                                  'HmStart' = Random_hmVak.index, 
                                  'Strook' = Random_Strook.index),
              'ID List' = list('WegID' = WegID, 
                               'BaanID' = BaanID, 
                               'HmStartID' = hmVakID, 
                               'StrookID' = StrookID,
                               'Title' = Title)
              ))
}

# ---------------------------------------------------------------------------------------------------- #
# 
# Function which returns a list containing Baan, HmStart and Strook indices.
#
# ---------------------------------------------------------------------------------------------------- #

Specified_Segment <- function(Weg, Baan, HmStart, Strook){
  if(class(Weg) != 'Weg') stop('Input not of class `Weg`.')
  
  # WegID <- Weg@wegID
  WegID <- sub(x = Weg@wegID, pattern = 'R0+', replacement = 'A')
  
  # Select specified index subsequently for Baan, hmVak and Strook.
  Specified_Baan.index <- which(lapply(Weg@banen, 
                                       function(i) i@baanID) == Baan)
  Specified_hmVak.index <- which(lapply(Weg@banen[[Specified_Baan.index]]@temporal.hmVakken, 
                                        function(i) i@hmStartPos) == HmStart)
  Specified_Strook.index <- which(lapply(Weg@banen[[Specified_Baan.index]]@temporal.hmVakken[[Specified_hmVak.index]]@temporal.Stroken, 
                                         function(i) i@strookID) == Strook)
  
  # Find the corresponding IDs.
  BaanID <- Weg@banen[[Specified_Baan.index]]@baanID
  hmVakID <- Weg@banen[[Specified_Baan.index]]@temporal.hmVakken[[Specified_hmVak.index]]@hmStartPos
  StrookID <- Weg@banen[[Specified_Baan.index]]@temporal.hmVakken[[Specified_hmVak.index]]@temporal.Stroken[[Specified_Strook.index]]@strookID
  
  
  Title <- paste(WegID, BaanID, hmVakID, StrookID, sep = ' - ')
  
  return(list('Index List' = list('Baan' = Specified_Baan.index, 
                                  'HmStart' = Specified_hmVak.index, 
                                  'Strook' = Specified_Strook.index),
              'ID List' = list('WegID' = WegID, 
                               'BaanID' = BaanID, 
                               'HmStartID' = hmVakID, 
                               'StrookID' = StrookID,
                               'Title' = Title)
  ))
}

# ---------------------------------------------------------------------------------------------------- #
# 
# Function which returns a list of data.tables given Weg, Baan, HmStart, and Strook indices.
#
# ---------------------------------------------------------------------------------------------------- #

BVS_DF <- function(Weg, Index_List){
  if(class(Weg) != 'Weg') stop('Input not of class `Weg`.')
  LCMS_List <- Weg@banen[[Index_List$Baan]]@temporal.hmVakken[[Index_List$HmStart]]@temporal.Stroken[[Index_List$Strook]]@temporal.LCMS_Traces
  
  # Define and preallocate a data.table template.
  DT_Seq <- seq_along(LCMS_List)
  DT_Template <-setNames(data.table(matrix(numeric(), nrow = length(DT_Seq), ncol = 104)),
                         c('Datum_tijd', 'Vehicle', 'Errorcode', 'lengte_meting', ColNames('Overall', 'No')))
  DT_Template <- DT_Template[, Datum_tijd := as.Date(Datum_tijd)]
  
  # Fill the DT_Templates
  for(j in 1L:length(DT_Seq)) set(DT_Template, j, c('Datum_tijd', 'Vehicle', 'Errorcode', 'lengte_meting'),
                                  as.list(c(LCMS_List[[j]]@Datum_tijd, LCMS_List[[j]]@Vehicle, LCMS_List[[j]]@Errorcode, LCMS_List[[j]]@lengte_meting)))
  
  # Define the columns and create a list of data.tables.
  Cols <- ColNames('Overall', 'No')
  DT_List <- list('Left' = copy(DT_Template), 'Right' = copy(DT_Template), 'Overall' = copy(DT_Template))
  
  for(Row in seq_along(DT_Seq)){
    set(DT_List$Left, Row, Cols, as.list(LCMS_List[[Row]]@leftData))
    set(DT_List$Right, Row, Cols, as.list(LCMS_List[[Row]]@rightData))
    set(DT_List$Overall, Row, Cols, as.list(LCMS_List[[Row]]@overallData))
  }
  

  return(DT_List)
}

RemoveMeasurements <- function(PD, ID_List){
  # browser()
  Stat_DF <- Status_List[[ID_List$WegID]]
  HmChecks2 <- IfElse(str_ends(ID_List$BaanID, 'RR'),
                      Stat_DF$HmStart <= ID_List$HmStartID & Stat_DF$HmStop >= ID_List$HmStartID + 0.1,
                      Stat_DF$HmStart <= ID_List$HmStartID - 0.1 & Stat_DF$HmStop >= ID_List$HmStartID)
  Status <- Stat_DF[(Stat_DF$Baan == ID_List$BaanID | Stat_DF$Baan == 'ALL') &
                      HmChecks2  & 
                      (Stat_DF$Strook == ID_List$StrookID | Stat_DF$Strook == 'ALL'), ]
  StartDate <- Status$Date
  PD_Copy <- lapply(PD, copy)

  for(Position in names(PD)){
    PD_Copy[[Position]] <- subset(PD_Copy[[Position]], Datum_tijd >= StartDate + 365)
  }
  return(PD_Copy)
}

# ---------------------------------------------------------------------------------------------------- #
# 
# Function which returns a list of data.tables but with isotone regression performed.
#
# ---------------------------------------------------------------------------------------------------- #

Isotonise <- function(DT_List, convex = TRUE, Position = 'Mixed'){
  # browser()
  # Make sure that original DT_List with data.tables is not altered.
  DT_List_Copy <- lapply(DT_List, copy)
  
  Date <- DT_List_Copy$Left$Datum_tijd
  if('Mixed' %in% names(DT_List)){
    for(Col in c(setdiff(colnames(DT_List_Copy$Mixed), c(ColNames_Static, ColNames('Overall', 'No'), 'Position')), 
                 ColNames('Overall', 'No'))){
      Numeric <- DT_List_Copy$Mixed[[Col]]
      ColDF <- data.frame(Date = Date, Numeric = Numeric)
      Values <- IfElse(convex == TRUE,
                       quickConvexIncreasing(ColDF),
                       isoreg(DT_List_Copy[[Position]][[Col]])$yf)
      set(DT_List_Copy$Mixed, j = Col, value = Values)
    }
  }
  if(Position != 'Mixed'){
    for(Col in ColNames('Overall', 'No')){
      for(Position in names(DT_List_Copy)){
        Numeric <- DT_List_Copy[[Position]][[Col]]
        ColDF <- data.frame(Date = Date, Numeric = Numeric)
        Values <- IfElse(convex == TRUE,
                         quickConvexIncreasing(ColDF),
                         isoreg(DT_List_Copy[[Position]][[Col]])$yf)
        set(DT_List_Copy[[Position]], j = Col, value = Values)
      }
    }
  }
  
  return(DT_List_Copy)
}

quickConvexIncreasing <- function(DF){
  set.seed(10)
  convexModel <- scam(Numeric ~ s(as.numeric(Date), k = 4, bs = 'micx'), data = DF)
  return(convexModel$fitted.values)
}


# ---------------------------------------------------------------------------------------------------- #
# 
# Function which returns a list of data.tables but with a (x-median)/MAD transformation.
#
# ---------------------------------------------------------------------------------------------------- #

s.MAD <- function(x){
  return((x-median(x))/mad(x))
}

# ---------------------------------------------------------------------------------------------------- #
# 
# Function which returns a list of data.tables but with a f() prior transformation and s() standardisation.
#
# ---------------------------------------------------------------------------------------------------- #

DataTransform <- function(DT_List, f = identity, s = identity, mixed = FALSE){
  DT_List_Copy <- lapply(DT_List, copy)
  # browser()
  if(mixed){DT_List_Copy$Mixed <- MixData(DT_List)}
  Cols <- ColNames('Overall', 'No')
  
  for(Row in 1:nrow(DT_List_Copy$Left)){
    LeftData <- f(as.numeric(DT_List_Copy$Left[Row, ColNames('Overall', 'No'), with = FALSE]))
    RightData <- f(as.numeric(DT_List_Copy$Right[Row, ColNames('Overall', 'No'), with = FALSE]))
    OverallData <- f(as.numeric(DT_List_Copy$Overall[Row, ColNames('Overall', 'No'), with = FALSE]))
    
    
    set(DT_List_Copy$Left, Row, Cols, as.list(s(LeftData)))
    set(DT_List_Copy$Right, Row, Cols, as.list(s(RightData)))
    set(DT_List_Copy$Overall, Row, Cols, as.list(s(OverallData)))
    if(mixed) {
      MixedData <- f(as.numeric(DT_List_Copy$Mixed[Row, !colnames(DT_List_Copy$Mixed) %in% c('Position', ColNames_Static)]))
      DT_List_Copy$Mixed[Row, !colnames(DT_List_Copy$Mixed) %in% c('Position', ColNames_Static)] <- s(MixedData)
    }
  }

  return(DT_List_Copy)
}

MixData <- function(DT_List, f = identity, s = identity){
  Left.DF <- data.frame(copy(DT_List$Left))
  Right.DF <- data.frame(copy(DT_List$Right))
  Mixed.DF <- cbind(Left.DF, Right.DF)
  Mixed.DF <- Mixed.DF[, !colnames(Mixed.DF) %in% c(ColNames_Static, 'Position')]
  Mixed.DF$Position <- rep('Mixed', nrow(Mixed.DF))
  Mixed.DF <- cbind(Left.DF[, c('Datum_tijd', 'Vehicle', 'Errorcode', 'lengte_meting')],
                    Mixed.DF)
  return(Mixed.DF)
}

# ---------------------------------------------------------------------------------------------------- #
# 
# Function which returns a list of data.tables but with the estimated parameters fitting a location scale family.
#
# ---------------------------------------------------------------------------------------------------- #

parPercentiles <- function(DT_List, Distribution, estimator = 'mle', percentile = 0.75){
  DT_List_Copy <- lapply(DT_List, copy)
  
  paramNames <- names(fitdist(as.numeric(DT_List_Copy$Left[1, ColNames('Overall', 'No'), with = FALSE]), Distribution, method = estimator)$estimate)
                      
  
  for(param in seq_along(paramNames)){
    for(Row in 1:nrow(DT_List_Copy$Left)){
      DT_List_Copy$Left[Row, eval(paramNames[param]) := fitdist(as.numeric(DT_List_Copy$Left[Row, ColNames('Overall', 'No'), with = FALSE]), 
                                                                Distribution, 
                                                                method = estimator)$estimate[param]]
      DT_List_Copy$Right[Row, eval(paramNames[param]) := fitdist(as.numeric(DT_List_Copy$Right[Row, ColNames('Overall', 'No'), with = FALSE]),
                                                                 Distribution,
                                                                 method = estimator)$estimate[param]]
      DT_List_Copy$Overall[Row, eval(paramNames[param]) := fitdist(as.numeric(DT_List_Copy$Overall[Row, ColNames('Overall', 'No'), with = FALSE]), 
                                                                   Distribution,
                                                                   method = estimator)$estimate[param]]
    }
  }
  qFunctionName <- match.fun(paste0('q', Distribution))
  qName <- paste0('percentile', as.character(100*percentile))

  DT_List_Copy$Left[, (qName) := do.call(qFunctionName, c(list(p = percentile), mget(paramNames)))]
  DT_List_Copy$Right[, (qName) := do.call(qFunctionName, c(list(p = percentile), mget(paramNames)))]
  DT_List_Copy$Overall[, (qName) := do.call(qFunctionName, c(list(p = percentile), mget(paramNames)))]
  
  KeepColumns <- c('Datum_tijd', paramNames, qName)
  DT_List_Copy$Left <- DT_List_Copy$Left[, KeepColumns, with = FALSE]
  DT_List_Copy$Right <- DT_List_Copy$Right[, KeepColumns, with = FALSE]
  DT_List_Copy$Overall <- DT_List_Copy$Overall[, KeepColumns, with = FALSE]
  return(DT_List_Copy)
}

# ---------------------------------------------------------------------------------------------------- #
# 
# Function which returns lower and upper bounds of a percentile at a given confidence level for a vector of data.
#
# ---------------------------------------------------------------------------------------------------- #

LowerUpper <- function(random, perc = 0.75, clevel = 0.95){
  alpha <- 1-clevel
  n <- length(random)
  
  # Sample quantile
  sQuantile <- quantile(random, probs = perc, names = FALSE)
  
  # Lower bound
  probs <- pbinom(q = (1:n)-1, size = n, p = perc)
  ind <- (1:n)[probs <= alpha/2]
  if(length(ind) == 0){
    l <- 0
    l.bound <- -Inf
  } else{
    l <- max(ind)
    l.bound <- sort(random)[max(ind)]
  }
  
  # Upper bound
  ind <- (1:n)[probs >= 1-alpha/2]
  if(length(ind) == 0){
    u <- n+1
    u.bound <- -Inf
  } else{
    u <- min(ind)
    u.bound <- sort(random)[min(ind)]
  }
  return(list(sQuantile, l.bound, u.bound))
}

MedMAD <- function(Numeric){
  med <- median(Numeric)
  MAD <- mad(Numeric)
  return(list(med, MAD))
}

# ---------------------------------------------------------------------------------------------------- #
# 
# Function which returns a list of data.tables but with the estimated nonparametric statistics and quantiles
#
# ---------------------------------------------------------------------------------------------------- #


GaussKernDensity <- function(x, x.i, bw, p) sum(pnorm(x-x.i, sd = bw))/length(x.i) - p

nonparQuantiles <- function(DT_List, percentile = 0.75, method = 'kernel', estimates = 'standardised'){
  # browser()
  DT_List_Copy <- lapply(DT_List, copy)
  
  qName <- paste0('percentile', as.character(100*percentile))
  Columns <- c('median', 'MAD', qName)
  # browser()
  Total <- PD_Long(DataTransform(DT_List_Copy, s = s.MAD), 'All')
    
  LeftTotal <- subset(Total, Position == 'Left')$value
  q.Left <- IfElse(method == 'empirical',
                   quantile(LeftTotal, p = percentile, names = FALSE, na.rm = TRUE),
                   uniroot(GaussKernDensity, range(-4,4), x.i = LeftTotal, bw = bw.ucv(LeftTotal), p = percentile)$root)
  RightTotal <- subset(Total, Position == 'Right')$value
  q.Right <- IfElse(method == 'empirical',
                    quantile(RightTotal, p = percentile, names = FALSE, na.rm = TRUE),
                    uniroot(GaussKernDensity, range(-4,4), x.i = RightTotal, bw = bw.ucv(RightTotal), p = percentile)$root)
  OverallTotal <- subset(Total, Position == 'Overall')$value
  q.Overall <- IfElse(method == 'empirical',
                      quantile(OverallTotal, p = percentile, names = FALSE, na.rm = TRUE),
                      uniroot(GaussKernDensity, range(-4,4), x.i = OverallTotal, bw = bw.ucv(OverallTotal), p = percentile)$root)
  
  if('Mixed' %in% names(DT_List)){
    MixedTotal <- subset(PD_Long(DataTransform(DT_List, s = s.MAD, mixed = TRUE), 'All'), Position == 'Mixed')$value
    q.Mixed <- IfElse(method == 'empirical',
                      quantile(MixedTotal, p = percentile, names = FALSE, na.rm = TRUE),
                      uniroot(GaussKernDensity, range(-4,4), x.i = MixedTotal, bw = bw.ucv(MixedTotal), p = percentile)$root)
  }
  
  
  for(Row in 1:nrow(DT_List_Copy$Left)){
    LeftData <- as.numeric(DT_List_Copy$Left[Row, ColNames('Overall', 'No'), with = FALSE])
    RightData <- as.numeric(DT_List_Copy$Right[Row, ColNames('Overall', 'No'), with = FALSE])
    OverallData <- as.numeric(DT_List_Copy$Overall[Row, ColNames('Overall', 'No'), with = FALSE])
    
    med.Left <- median(LeftData); mad.Left <- mad(LeftData)
    med.Right <- median(RightData);  mad.Right <- mad(RightData)
    med.Overall <- median(OverallData); mad.Overall <- mad(OverallData)
    q.current.Left <- IfElse(estimates == 'standardised',
                             med.Left + mad.Left*q.Left,
                             uniroot(GaussKernDensity, range(-4,20), x.i = LeftData, bw = bw.ucv(LeftData), p = percentile)$root)
    q.current.Right <- IfElse(estimates == 'standardised',
                              med.Right + mad.Right*q.Right,
                              uniroot(GaussKernDensity, range(-4,20), x.i = RightData, bw = bw.ucv(RightData), p = percentile)$root)
    q.current.Overall <- IfElse(estimates == 'standardised',
                                med.Overall + mad.Overall*q.Overall,
                                uniroot(GaussKernDensity, range(-4,20), x.i = OverallData, bw = bw.ucv(OverallData), p = percentile)$root)
    
    DT_List_Copy$Left[Row, (Columns) := list(med.Left, mad.Left, q.current.Left)]
    DT_List_Copy$Right[Row, (Columns) := list(med.Right, mad.Right, q.current.Right)]
    DT_List_Copy$Overall[Row, (Columns) := list(med.Overall, mad.Overall, q.current.Overall)]

    if('Mixed' %in% names(DT_List)){
      MixedData <- as.numeric(DT_List_Copy$Mixed[Row, !colnames(DT_List_Copy$Mixed) %in% c('Position', ColNames_Static, Columns)])
      med.Mixed <- median(MixedData); mad.Mixed <- mad(MixedData)
      DT_List_Copy$Mixed[Row, Columns] <- c(med.Mixed, mad.Mixed, med.Mixed + mad.Mixed*q.Mixed)
    }
  }
  KeepColumns <- c('Datum_tijd', Columns)
  DT_List_Copy$Left <- DT_List_Copy$Left[, KeepColumns, with = FALSE]
  DT_List_Copy$Right <- DT_List_Copy$Right[, KeepColumns, with = FALSE]
  DT_List_Copy$Overall <- DT_List_Copy$Overall[, KeepColumns, with = FALSE]
  if('Mixed' %in% names(DT_List)){ DT_List_Copy$Mixed <- subset(DT_List_Copy$Mixed, select = KeepColumns)}
  return(DT_List_Copy)
}

# ---------------------------------------------------------------------------------------------------- #
# 
# Function which returns dataframe of medians per WT, lane, Hm, 
#
# ---------------------------------------------------------------------------------------------------- #

WTL <- function(Road, All_DF){
  RoadString <- Road@wegID
  Name <- paste0('WTL', RoadString, '_', strftime(Sys.time(), format = '%Y%m%d_%H%M%S'))
  
  # Create logfile
  logger <- create.logger()
  logfile(logger) <- paste0(Name, '.log')
  level(logger) <- 'ERROR'
  
  Road.Summary <- data.frame()
  Road_DF <- unique(All_DF[All_DF$Weg == RoadString, c('Weg', 'Baan', 'HmStart', 'HmStop', 'Strook')])
  Ref_DF <- Anomalies_List[[RoadString]]
  Stat_DF <- Status_List[[RoadString]]
  for(i in 1:nrow(Road_DF)){
    Segment <- Specified_Segment(Weg = Road, Baan = Road_DF$Baan[i], HmStart = Road_DF$HmStart[i], Strook = Road_DF$Strook[i])
    PD <- BVS_DF(Road, Segment$`Index List`)
    HmChecks <- IfElse(str_ends(Road_DF$Baan[i], 'RR'),
                       Ref_DF$HmStart <= Road_DF$HmStart[i] & Ref_DF$HmStop >= Road_DF$HmStop[i],
                       Ref_DF$HmStart <= Road_DF$HmStop[i] & Ref_DF$HmStop >= Road_DF$HmStart[i])
    Target_DF <- Ref_DF[(Ref_DF$Baan == Road_DF$Baan[i] | Ref_DF$Baan == 'ALL') &
                          HmChecks  & 
                          (Ref_DF$Strook == Road_DF$Strook[i] | Ref_DF$Strook == 'ALL'), ]
    
    HmChecks2 <- IfElse(str_ends(Road_DF$Baan[i], 'RR'),
                        Stat_DF$HmStart <= Road_DF$HmStart[i] & Stat_DF$HmStop >= Road_DF$HmStop[i],
                        Stat_DF$HmStart <= Road_DF$HmStop[i] & Stat_DF$HmStop >= Road_DF$HmStart[i])
    Status <- Stat_DF[(Stat_DF$Baan == Road_DF$Baan[i] | Stat_DF$Baan == 'ALL') &
                        HmChecks2  & 
                        (Stat_DF$Strook == Road_DF$Strook[i] | Stat_DF$Strook == 'ALL'), ]
    StartDate <- IfElse(nrow(Status) == 0, 0, Status$Date)
    
    # for(Position in names(PD)){
    #   PD[[Position]] <- PD[[Position]][Datum_tijd >= StartDate + 365]
    # }
    # 
    PD <- lapply(PD, function(x) subset(x, Datum_tijd >= StartDate + 365))
    
    if(nrow(Target_DF) != 0){
    #   for(Position in names(PD)){
    #     PD[[Position]] <- PD[[Position]][year(Datum_tijd) <= year(min(Target_DF$Date)) - 1]
    #   }
    # }
      # Satisfying this condition means that there are anomalies.
      PD <- lapply(PD, function(x) subset(x, Datum_tijd <= min(Target_DF$Date) - 365))
    }
    if(nrow(PD$Left) == 0) next
    
    for(j in 1:nrow(PD$Left)){
      L <- as.numeric(PD$Left[j, ColNames('Overall', 'No'), with = FALSE])
      R <- as.numeric(PD$Right[j, ColNames('Overall', 'No'), with = FALSE])
      if(sum(is.na(L)) > 0) error(logger, paste(PD$Left$Datum_tijd[j],
                                                Segment$`ID List`$BaanID,
                                                Segment$`ID List`$HmStartID,
                                                Segment$`ID List`$StrookID, 'contains NA values. Length measurement < 100?'))
      message(paste(i,
                    PD$Left$Datum_tijd[j],
                    Segment$`ID List`$BaanID,
                    Segment$`ID List`$HmStartID,
                    Segment$`ID List`$StrookID))
      # if(Road_DF$HmStart[i] == 2.3) browser() else next
      
      LMed <- median(L); LMAD <- mad(L); LQ <- quantile(L, 0.75, names = FALSE, na.rm = TRUE)
      RMed <- median(R); RMAD <- mad(R); RQ <- quantile(R, 0.75, names = FALSE, na.rm = TRUE)
      
      Road.Summary <- rbind(Road.Summary,
                            data.frame(Date = PD$Left$Datum_tijd[j],
                                       Age = IfElse(StartDate == 0, 0,  as.numeric(PD$Left$Datum_tijd[j] - StartDate)),
                                       Cway = Road_DF$Baan[i], 
                                       Hm = Road_DF$HmStart[i], 
                                       Lane = Road_DF$Strook[i],
                                       WT = 'LWT', 
                                       Median = LMed, 
                                       MAD = LMAD,
                                       Quantile = LQ))
      Road.Summary <- rbind(Road.Summary,
                            data.frame(Date = PD$Left$Datum_tijd[j],
                                       Age = IfElse(StartDate == 0, 0,  as.numeric(PD$Left$Datum_tijd[j] - StartDate)),
                                       Cway = Road_DF$Baan[i], 
                                       Hm = Road_DF$HmStart[i], 
                                       Lane = Road_DF$Strook[i],
                                       WT = 'RWT',
                                       Median = RMed, 
                                       MAD = RMAD,
                                       Quantile = RQ))
      
    }
  }
  return(Road.Summary)
}

# ---------------------------------------------------------------------------------------------------- #
# 
# Function which returns dataframe of predictions and other relevant information.
#
# ---------------------------------------------------------------------------------------------------- #

RoadPredictions <- function(Road, All_DF, Threshold = 10, Position = 'Mixed', convex = FALSE, Hm = NULL){
  RoadString <- Road@wegID
  Name <- paste0('RoadPredictions_', RoadString, IfElse(convex, '_Convex', ''),  '_', strftime(Sys.time(), format = '%Y%m%d_%H%M%S'))
  
  # Create logfile
  logger <- create.logger()
  logfile(logger) <- paste0(Name, '.log')
  level(logger) <- 'ERROR'
  
  Predictions_DF <- data.frame()
  Plots <- list('Mixed' = list(), 'Left' = list(), 'Right' = list())
  Road_DF <- unique(All_DF[All_DF$Weg == RoadString, c('Weg', 'Baan', 'HmStart', 'HmStop', 'Strook')])
  Road_DF <- Road_DF[order(Road_DF$Baan, Road_DF$HmStart, Road_DF$Strook)]
  # browser()
  if(hasArg(Hm)) Road_DF <- subset(Road_DF, HmStart %in% Hm)
  Ref_DF <- Anomalies_List[[RoadString]]
  Stat_DF <- Status_List[[RoadString]]
  
  # Create PDF for plots
  pdf(file = paste0(Name, '.pdf'),
      paper = 'a4r',
      width = 90,
      height = 12)
  
  for(i in 1:nrow(Road_DF)){
  # for(i in 54){
    HmChecks <- IfElse(str_ends(Road_DF$Baan[i], 'RR'),
                       Ref_DF$HmStart <= Road_DF$HmStart[i] & Ref_DF$HmStop >= Road_DF$HmStop[i],
                       Ref_DF$HmStart <= Road_DF$HmStop[i] & Ref_DF$HmStop >= Road_DF$HmStart[i])
    Target_DF <- Ref_DF[(Ref_DF$Baan == Road_DF$Baan[i] | Ref_DF$Baan == 'ALL') &
                          HmChecks  & 
                          (Ref_DF$Strook == Road_DF$Strook[i] | Ref_DF$Strook == 'ALL'), ]
    
    HmChecks2 <- IfElse(str_ends(Road_DF$Baan[i], 'RR'),
                        Stat_DF$HmStart <= Road_DF$HmStart[i] & Stat_DF$HmStop >= Road_DF$HmStop[i],
                        Stat_DF$HmStart <= Road_DF$HmStop[i] & Stat_DF$HmStop >= Road_DF$HmStart[i])
    Status <- Stat_DF[(Stat_DF$Baan == Road_DF$Baan[i] | Stat_DF$Baan == 'ALL') &
                        HmChecks2  & 
                        (Stat_DF$Strook == Road_DF$Strook[i] | Stat_DF$Strook == 'ALL'), ]
    
    StartDate <- IfElse(nrow(Status) == 0, 0, Status$Date)
    
    Segment <- Specified_Segment(Weg = Road, Baan = Road_DF$Baan[i], HmStart = Road_DF$HmStart[i], Strook = Road_DF$Strook[i])
    message(paste0(i, ': ', Segment$`ID List`$Title))
    PD <- BVS_DF(Road, Segment$`Index List`)
    PD <- DataTransform(PD, mixed = TRUE)
    # if(Road_DF$HmStart[i] !=  203.7) next
    # else browser()
    if(nrow(Target_DF) != 0){
      # Satisfying this condition means that there are anomalies.
      PD <- lapply(PD, function(x) subset(x, Datum_tijd <= min(Target_DF$Date) - 365 + 365))
    }
    
    tryCatch(expr = {# Minimum of 4 observations needed for a prediction
                     if(nrow(PD$Left) < 5) next

                     PercentilesCheck <- nonparQuantiles(PD)
                     UnderThresh <- which(PercentilesCheck$Mixed$percentile75 < Threshold)
                     UpperThresh <- first(which(PercentilesCheck$Mixed$percentile75 >= Threshold))
                     nmin <- min(last(UnderThresh), UpperThresh - 1)

                     # Remove rows where the Threshold is exceeded.
                     PD <- lapply(PD, head, n = nmin)
                     
                     # Minimum of 4 observations needed for a prediction
                     if(nrow(PD$Left) < 5) next   
                     if(convex == TRUE) PD <- Isotonise(PD, Position = Position)
                     
                     Percentiles_Kernel <- nonparQuantiles(PD, method = 'kernel')
                     Percentiles_Kernelmin <- nonparQuantiles(lapply(PD, head, n = -1), method = 'kernel')
                     Percentiles_Empirical <- nonparQuantiles(PD, method = 'empirical')
                     Percentiles_Empiricalmin <- nonparQuantiles(lapply(PD, head, n = -1), method = 'empirical')
                     
                     for(Pos in Position){
                       DF_Ker <- data.frame(Percentiles_Kernel[[Pos]][, c('Datum_tijd', 'percentile75')])
                       DF_Kermin <- data.frame(Percentiles_Kernelmin[[Pos]][, c('Datum_tijd', 'percentile75')])
                       Pred_Ker <- PredictionDifferences(DF = DF_Ker, DFmin = DF_Kermin)

                       DF_Emp <- data.frame(Percentiles_Empirical[[Pos]][, c('Datum_tijd', 'percentile75')])
                       DF_Empmin <- data.frame(Percentiles_Empiricalmin[[Pos]][, c('Datum_tijd', 'percentile75')])
                       Pred_Emp <- PredictionDifferences(DF = DF_Emp, DFmin = DF_Empmin)
                       
                       LastDate <- last(DF_Ker$Datum_tijd)
                       Predictions_DF <- rbind(Predictions_DF,
                                               data.frame(Carriageway = Road_DF$Baan[i], 
                                                          HmStart = Road_DF$HmStart[i], 
                                                          Lane = Road_DF$Strook[i],
                                                          n = nrow(DF_Ker),
                                                          q = last(DF_Ker$percentile75),
                                                          Age = IfElse(StartDate == 0, 0,  as.numeric(last(DF_Ker$Datum_tijd) - StartDate)/365),
                                                          Date = LastDate,
                                                          Position = Pos,
                                                          TSD.Emp = Pred_Emp$Threshold$`Date Complete`,
                                                          TSD.Kern = Pred_Ker$Threshold$`Date Complete`,
                                                          PRL.Emp = Pred_Emp$Threshold$`Date Complete` - LastDate,
                                                          PRL.Kern = Pred_Ker$Threshold$`Date Complete` - LastDate,
                                                          dPRL.Emp = Pred_Emp$Threshold$Diff,
                                                          dPRL.Kern = Pred_Ker$Threshold$Diff
                                               ))
                       
                       Naming <- paste(Road_DF$Baan[i], Road_DF$HmStart[i], Road_DF$Strook[i], sep = ' - ')
                       PlotBase <- PlotCurves.PD(PD, Position = Pos) +
                         labs(title = Naming, subtitle = Pos) +
                         scale_x_date(date_breaks = '4 years', date_labels = '%Y')
                       grid.draw(PlotBase)
                       Plots[[Pos]][[i]] <- PlotBase + labs(subtitle = Naming) + theme(axis.title = element_blank(),
                                                                                       axis.ticks = element_blank(),
                                                                                       plot.title = element_blank(),
                                                                                       plot.subtitle = element_text(face = 'bold', size = 10),
                                                                                       axis.text = element_blank(),
                                                                                       legend.position = 'none')
                     }
                     },
             error = function(e) error(logger, paste(Segment$`ID List`$Title, '\n', e)))
    
  }
  dev.off()
  Plots <- lapply(Plots, function(x) x[!sapply(x, is.null)])
  Predictions_DF[,sapply(Predictions_DF, is.Date)] <- sapply(Predictions_DF[, sapply(Predictions_DF, is.Date)], as.character)
  return(list(DF = Predictions_DF, Plot = Plots))
}


#### PLOT FUNCTIONS ####

RWS_Thresh <- function(Threshold = 10, ...) geom_hline(yintercept = c(Threshold, Threshold), 
                                                       colour = c('#ffff19', '#0054ad'), 
                                                       linetype = c('solid', '88'), 
                                                       ...)

ThemeDef <- function(N = 1, E = 1, S = 1, W = 1) theme(plot.title = element_text(face = 'bold'),
                                                       plot.margin = margin(N, E, S, W, 'mm'),
                                                       legend.title = element_text(face = 'bold', size = 12),
                                                       legend.text = element_text(size = 11),
                                                       strip.text = element_text(colour = 'black', face = 'bold', size = rel(1))
                                                       )

# ---------------------------------------------------------------------------------------------------- #
# 
# Function which returns the histogram given transformation and road + section.
#
# ---------------------------------------------------------------------------------------------------- #

Plot_Histogram <- function(Weg, Section, f = identity, s = identity, Position = 'All', ...){
  f.character <- ifelse(hasArg(f), as.character(match.call()['f']), 'identity')
  s.character <- ifelse(hasArg(s), as.character(match.call()['s']), 'identity')
  Subtitle <- paste0('Transformation = ', f.character, ', Standardisation = ', s.character)
  PD <- BVS_DF(Weg, Section$`Index List`)
  PD.T <- DataTransform(PD, f, s)
  PDLong <- PD_Long(PD.T, Position)
  Plot <- Create_Histogram_Multiple(PDLong, Section$`ID List`, subtitle = Subtitle, Positions = c('Left', 'Right'))
  return(Plot)
}

# ---------------------------------------------------------------------------------------------------- #
# 
# Function which returns the ECDF given transformation and road + section.
#
# ---------------------------------------------------------------------------------------------------- #

Plot_ECDF <- function(PDLong, ID_List, p = pnorm, ...){
  
  Base <- ggplot(PDLong, aes(value, colour = Datum_tijd)) +
    stat_ecdf(geom = 'step') +
    stat_function(show.legend = TRUE, fun = p, ..., aes(colour = 'Reference')) +
    labs(title = paste('ECDF:', ID_List$Title),
         subtitle = paste('Reference Distribution:', ifelse(hasArg(p), as.character(match.call()['p']), 'pnorm'))) +
    ThemeDef() +
    theme(plot.title = element_text(face = 'bold'),
          plot.subtitle = element_text(colour = 'grey'),
          legend.title = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.spacing = unit(1, 'lines'))

    
  
  PDLong$Datum_tijd <- 'Aggregated'
  
  Agg <- ggplot(PDLong, aes(value, colour = Datum_tijd)) +
    stat_ecdf(geom = 'step') +
    stat_function(show.legend = TRUE, fun = p, ..., aes(colour = 'Reference')) +
    labs(title = paste('ECDF:', ID_List$Title),
         subtitle = paste('Reference Distribution:', ifelse(hasArg(p), as.character(match.call()['p']), 'pnorm'))) +
    ThemeDef() +
    theme(plot.title = element_text(face = 'bold'),
          plot.subtitle = element_text(colour = 'grey'),
          legend.title = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.spacing = unit(1, 'lines'))
  return(list('Base' = Base,
              'Agg' = Agg))
}

# ---------------------------------------------------------------------------------------------------- #
# 
# Function which returns the QQ plots given transformation and road + section.
#
# ---------------------------------------------------------------------------------------------------- #

Plot_QQ <- function(PDLong, ID_List, q = qnorm, col = 4, ...){
  
  ggplot(data = PDLong, aes(sample = value, colour = Datum_tijd)) +
    stat_qq() +
    stat_qq_line(distribution = q, ...) +
    facet_wrap(vars(Datum_tijd), ncol = col) +
    labs(title = paste('Q-Q:', ID_List$Title),
         subtitle = paste('Reference Distribution:', ifelse(hasArg(q), as.character(match.call()['q']), 'qnorm'))) +
    ThemeDef() +
    theme(plot.title = element_text(face = 'bold'),
          plot.subtitle = element_text(colour = 'grey'),
          legend.title = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = 'none',
          aspect.ratio = 1,
          panel.spacing = unit(1, 'lines'))
    
}




# ---------------------------------------------------------------------------------------------------- #
# 
# Functions that creates the data in long format that can be used to plot figures.
#
# ---------------------------------------------------------------------------------------------------- #

PD_Long <- function(PlotData, Position){
  # browser()
  # Add column which indicates the position.
  PlotData$Overall <- PlotData$Overall[, ('Position') := rep('Overall', nrow(PlotData$Overall))]
  PlotData$Left <- PlotData$Left[, ('Position') := rep('Left', nrow(PlotData$Left))]
  PlotData$Right <- PlotData$Right[, ('Position') := rep('Right', nrow(PlotData$Right))]
  
  # Filter if necessary or combine the data into one.
  if(Position == 'Overall') PlotData.filtered <- PlotData$Overall
  else if(Position == 'Left')  PlotData.filtered <- PlotData$Left
  else if(Position == 'Right')  PlotData.filtered <- PlotData$Right
  else PlotData.filtered <- rbindlist(PlotData[1:3])
  
  # Filter the important columns.
  if(all(ColNames('Overall', 'No') %in% colnames(PlotData$Left))){
    removeColumns <- c('Vehicle', 'Errorcode', 'lengte_meting')
    PlotData.filtered <- PlotData.filtered[, !removeColumns, with = FALSE]
  }
  
  # Transform data to long format by considering the ids, read the date column as a date.
  LongData <- as.data.frame(melt(PlotData.filtered, id = c('Datum_tijd', 'Position')))

  if('Mixed' %in% names(PlotData)){
    MixedData.filtered <- data.table(PlotData$Mixed[, !colnames(PlotData$Mixed) %in% removeColumns])
    LongData <- rbind(melt(MixedData.filtered, id = c('Datum_tijd', 'Position')),
                      LongData)
  }
  
  
  if(all(ColNames('Overall', 'No') %in% colnames(PlotData$Left))){
    LongData$Datum_tijd <- as.character(format(LongData$Datum_tijd, "%Y"))
  }
  
  return(LongData)
}

# ---------------------------------------------------------------------------------------------------- #
# 
# Functions that defines the Freedman-Diaconis method to calculate binwidths.
#
# ---------------------------------------------------------------------------------------------------- #

FD_Bin <- function(x){
  n <- length(x)
  r <- IQR(x)
  2*r/n^(1/3)
}

# ---------------------------------------------------------------------------------------------------- #
# 
# Functions that creates the data in long format that can be used to plot figures.
#
# ---------------------------------------------------------------------------------------------------- #

Create_Histogram_Single <- function(Long_DF, Names_List, ...){
  GGPlot <- ggplot(Long_DF, aes(x = value, fill = ..x..)) + 
    geom_histogram(position = "identity", size = 0.01, binwidth = FD_Bin, color = 'black') +
    facet_grid(Datum_tijd ~ .) +
    labs(title = paste0('Histograms: ', Names_List$Title),
         x = 'Aggregate Loss (%)',
         y = 'Frequency',
         fill = '%',
         ...) +
    theme(plot.title = element_text(face = 'bold'),
          plot.subtitle = element_text(colour = 'grey')
          ) + 
    ThemeDef()
  if(all(Long_DF$value >= 0)) GGPlot <- GGPlot + scale_fill_viridis_c(limits = c(0,20),
                                                                      option = 'magma',
                                                                      direction = -1)
  return(GGPlot)
}

Create_Histogram_Multiple <- function(Long_DF, Names_List, Positions = c('Left', 'Right'),  ...){
  Long_DF <- subset(Long_DF, Position %in% Positions)
  Create_Histogram_Single(Long_DF, Names_List, ...) + facet_grid(Datum_tijd ~ Position, scales = 'free_y')
}

Create_ParameterPlot <- function(Long_DF, Names_List){
  PercentileName <- last(as.character(Long_DF$variable))
  ggplot(Long_DF, aes(x = Datum_tijd, y = value)) +
    geom_point() +
    scale_x_date(date_breaks = '1 year',
                 labels = date_format('%Y'),
                 limits = as.Date(c('2012-01-01', '2020-01-01'))) +
    labs(title = Names_List$Title,
         x = 'Date') +
    theme(plot.title = element_text(face = 'bold'),
          plot.margin = margin(1, 1, 1, 1, 'cm'),
          axis.title.x = element_text(vjust = -3),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 45, vjust = 0.5)
    ) + 
    facet_grid(variable ~ Position, scales = 'free') +
    # Add red dashed line for RWS threshold.
    geom_hline(data = data.frame(variable = PercentileName, y = 10), aes(yintercept = y), linetype = 'dashed', color = 'red') +
    # Manipulate y-axis limits.
    geom_hline(data = data.frame(variable = rep(PercentileName, 2), y = c(0, 20)), aes(yintercept = y), alpha = 0) +
    geom_hline(data = data.frame(variable = as.character(unique(Long_DF$variable)), y = rep(0, length(as.character(unique(Long_DF$variable))))), 
               aes(yintercept = y), alpha = 0)
}

ggacf <- function(Data){
  ACF <- acf(Data, plot = FALSE, lag.max = 30)
  DF <- with(ACF, data.frame(lag, acf))
  ciline <- qnorm((1 + 0.95)/2)/sqrt(length(Data))
  ggplot(data = DF, mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0)) +
    geom_hline(aes(yintercept = -ciline), linetype = 2, color = 'red', size = 1.1) +
    geom_hline(aes(yintercept = ciline), linetype = 2, color = 'red', size = 1.1) +
    scale_x_continuous(breaks = seq(0,length(Data), 5)) +
    labs(x = 'Lag',
         y = 'Sample ACF') +
    ThemeDef() +
    theme(plot.title = element_text(face = 'bold'),
          plot.subtitle = element_text(colour = 'grey'),
          legend.title = element_blank(),
          legend.position = 'none')

}

ACF_Plot <- function(PDLong, Names_List, lag.max = 30, Tracks = c('Left', 'Right')){
  LongDF <- PDLong
  finalDF <- data.frame()
  for(Year in unique(LongDF$Datum_tijd)){
    Positions <- IfElse(length(unique(LongDF$Position)) == 3, c('Left', 'Overall', 'Right'), unique(LongDF$Position))
    for(Position in Positions){
      tempDF <- LongDF[LongDF$Datum_tijd == Year & LongDF$Position == Position,]
      ACF <- acf(tempDF$value, plot = FALSE, lag.max = lag.max)
      ACF_DF <- with(ACF, data.frame(lag, acf, Position, Year))
      finalDF <- rbind(finalDF, ACF_DF)
    }
  }
  ciline <- qnorm((1 + 0.95)/2)/sqrt(100)
  ggplot(data = subset(finalDF, Position %in% Tracks), mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_hline(aes(yintercept = ciline), linetype = 2, size = 0.4, color = 'red') +
    geom_hline(aes(yintercept = -ciline), linetype = 2, size = 0.4, color = 'red') +
    geom_segment(mapping = aes(xend = lag, yend = 0), size = 0.4) +
    facet_grid(Year ~ Position) +
    scale_x_continuous(breaks = seq(0, 100, 5)) +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.major = element_blank(), 
          axis.line = element_line(colour = "black")) +
    ThemeDef() +
    labs(title = paste0('Sample autocorrelation: ', Names_List$Title),
         x = 'Lag',
         y = 'Sample ACF')
}

PlotKernel <- function(Fun, xi = 0, h = 1, col = '#00A6D6'){
  ggplot(data = data.frame(x = 0, y = 1), mapping = aes(x = x, y = y)) +
    geom_point(alpha = 0) +
    stat_function(fun = Fun, n = 3000, colour = col, args = list(xi = xi, h = h)) +
    labs(title = deparse(substitute(Fun)), x = '', y = '') +
    theme(plot.title = element_text(face = 'bold')) +
    scale_y_continuous(breaks = c(0,1,2)) +
    xlim(-5,5) +
    ThemeDef(E = 1)
}

Extrapolated_DF <- function(DF, Threshold = 10, Method = 'poly', ...){
  colnames(DF) <- c('Date', 'Percentile')
  # Fit an monotonically increasing convex function.
  SCAM.model <- scam(Percentile ~ s(as.numeric(Date), k = 4, bs = 'micx'), data = DF)
  Start <- max(DF$Date)
  if(Threshold < max(DF$Percentile)){
    Threshold <- last(DF$Percentile)
    Start <- DF$Date[nrow(DF)-1]
    SCAM.model <- scam(Percentile ~ s(as.numeric(Date), k = 4, bs = 'micx'), data = head(DF, -1))
  }
  LinData <- data.frame(Date = c(Start, Start + 1))
  LinPred <- predict(SCAM.model, newdata = LinData)
  Steps <- round(as.numeric((Threshold - first(LinPred))/diff(LinPred) + 1))
  PredData <- data.frame(Date = seq.int(from = Start, length.out = Steps, by = 1))
  if(Method == 'lin'){
    Percentile <- predict(SCAM.model, newdata = PredData)
  } else if(Method == 'poly'){
    Date <- seq.int(from = first(DF$Date), to = last(DF$Date), by = 1)
    Fits <- predict(SCAM.model, newdata = data.frame(Date = Date))
    PolyModel <- lm(Fits ~ poly(Date, 3))
    Percentile <- predict(PolyModel, newdata = PredData)
    Percentile <- Percentile[Percentile <= 10]
    # browser()
    PredData <- data.frame(Date = PredData[1:length(Percentile), ])
  }
  return(cbind(PredData, Percentile)[c(1:nrow(PredData)),])
}

quickPrediction <- function(DF, Threshold, Method = 'poly'){
  set.seed(10)
  SCAM.model <- scam(Percentile ~ s(as.numeric(Date), k = 4, bs = 'micx'), data = DF)
  Start <- max(DF$Date)
  LinData <- data.frame(Date = c(Start, Start + 1))
  LinPred <- predict(SCAM.model, newdata = LinData)
  Steps <- round(as.numeric((Threshold - first(LinPred))/diff(LinPred) + 1))
  PredData <- data.frame(Date = seq.int(from = Start, length.out = Steps, by = 1))
  if(Method == 'poly'){
    Date <- seq.int(from = first(DF$Date), to = last(DF$Date), by = 1)
    Fits <- predict(SCAM.model, newdata = data.frame(Date = Date))
    PolyModel <- lm(Fits ~ poly(Date, 3))
    Percentile <- predict(PolyModel, newdata = PredData)
    Percentile <- Percentile[Percentile <= 10]
    PredData <- data.frame(Date = PredData[1:length(Percentile), ])
  }
  return(last(PredData))
}

PredictionDifferences <- function(DF, DFmin, Threshold = 10, Method = 'poly', ...){
  diffPrediction <- NA
  tryCatch(expr = {colnames(DF) <- c('Date', 'Percentile')
                   colnames(DFmin) <- c('Date', 'Percentile')
                   if(last(DF$Percentile) < Threshold){
                     completePrediction <- quickPrediction(DF, Threshold, Method)
                     nminonePrediction <- quickPrediction(DFmin, Threshold, Method)
                     diffPrediction <- completePrediction - nminonePrediction
                   }
                   
                   # lastPrediction <- quickPrediction(DFmin, Threshold = last(DF$Percentile), Method)
                   # lastValue <- last(DF$Date)
                   # diffLast <- lastValue - lastPrediction
                   },
           error = function(e){message(e)})
  
  return(list('Threshold' = list(Diff = diffPrediction, `Date Complete` = completePrediction, `Date MinOne` = nminonePrediction)))
              # 'Last' = list(Diff = diffLast, Value = last(DF$Percentile), Date = lastValue, `Pred Date` = lastPrediction)))
}


PlotCurves.DF <- function(OneDF, Threshold = 10){
  stopifnot(ncol(OneDF) == 2)
  # browser()
  colnames(OneDF) <- c('Date', 'Percentile')
  Base <- 
  ggplot(OneDF, mapping = aes(x = Date, y = Percentile)) + 
    RWS_Thresh(size = 1) +
    geom_point() + 
    geom_smooth(data = head(OneDF, -1),
                method = 'scam',
                formula = y ~ s(x, k = 4, bs = 'micx'),
                se = F,
                aes(colour = 'n - 1')) +
    geom_smooth(data = OneDF,
                method = 'scam',
                formula = y ~ s(x, k = 4, bs = 'micx'),
                se = F,
                aes(colour = 'n')) +
    scale_color_manual(values = c('n' = hue_pal()(1), 'n - 1' = hue_pal()(2)[2])) +
    scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
    scale_y_continuous(breaks = seq(0, max(OneDF$Percentile) + 5, 2.5),
                       labels = function(x) paste0(x, '%')) +
    labs(title = TeX('\\textbf{Progression and Extrapolation of} $\\textit{q}_{0.75}$'),
         colour = 'Models',
         y = expression('75'^{'th'}~'Percentile')) + 
    theme(plot.title = element_text(face = 'bold'),
          legend.text = element_text(face = 'italic', size = 11),
          legend.title = element_text(face = 'bold', size = 12),
          axis.title.x = element_blank())
  if(last(OneDF$Percentile) < Threshold){
    Base <- Base + 
      geom_line(data = Extrapolated_DF(head(OneDF, -1)), linetype = 'dashed', size = 1, col = hue_pal()(2)[2]) +
      geom_line(data = Extrapolated_DF(OneDF), linetype = 'dashed', size = 1, col = hue_pal()(1))
  } else {
    Base <- Base + geom_line(data = Extrapolated_DF(OneDF), linetype = 'dashed', size = 1, col = hue_pal()(2)[2])
  }
  return(Base)
}

PlotCurves.PD <- function(PD, Threshold = 10, Position = 'Mixed'){
  # browser()
  set.seed(10)
  OneDF <- nonparQuantiles(PD)[[Position]][, c(1, 4)]
  OneDFmin <- nonparQuantiles(lapply(PD, head, n = - 1))[[Position]][, c(1, 4)]
  colnames(OneDF) <- c('Date', 'Percentile')
  colnames(OneDFmin) <- c('Date', 'Percentile')
  Base <- 
    ggplot(OneDF, mapping = aes(x = Date, y = Percentile)) + 
    RWS_Thresh(size = 1) +
    geom_point() + 
    geom_smooth(data = OneDFmin,
                method = 'scam',
                formula = y ~ s(x, k = 4, bs = 'micx'),
                se = F,
                aes(colour = 'n - 1')) +
    geom_smooth(data = OneDF,
                method = 'scam',
                formula = y ~ s(x, k = 4, bs = 'micx'),
                se = F,
                aes(colour = 'n')) +
    scale_color_manual(values = c('n' = hue_pal()(1), 'n - 1' = hue_pal()(2)[2])) +
    scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
    scale_y_continuous(breaks = seq(0, max(OneDF$Percentile) + 5, 2.5)) +
    labs(title = TeX('\\textbf{Progression and Extrapolation of} $\\textit{q}_{0.75}$'),
         colour = 'Models',
         y = '75th Percentile') +
    theme(plot.title = element_text(face = 'bold'),
          legend.text = element_text(face = 'italic'),
          legend.title = element_text(face = 'bold'),
          axis.title.x = element_blank())
  if(last(OneDF$Percentile) < Threshold){
    Base <- Base + 
      geom_line(data = Extrapolated_DF(OneDFmin), linetype = 'dashed', size = 1, col = hue_pal()(2)[2]) +
      geom_line(data = Extrapolated_DF(OneDF), linetype = 'dashed', size = 1, col = hue_pal()(1))
  } else {
    Base <- Base + geom_line(data = Extrapolated_DF(OneDF), linetype = 'dashed', size = 1, col = hue_pal()(2)[2])
  }
  return(Base)
}



Create_Density <- function(PDLong, kernel = 'gaussian', bw = 'ucv'){
  ggplot(data = PDLong, aes(x = value)) +
    geom_histogram(alpha = 0.5, binwidth = FD_Bin, aes(y = ..density..),
                   fill = '#abc6d8', colour = 'black') +
    geom_density(color = '#00A6D6', fill = '#77d3e9', alpha = 0.5, size = 1,
                 kernel = kernel, bw = bw) +
    labs(title = str_to_title(paste(kernel, 'kernel density'))) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(face = 'bold'))
  
}

PlotExtrapolation <- function(MainData, ExtraDataPol, ExtraData = NULL){
  Base <- ggplot(data = MainData, aes(x = Datum_tijd, y = value, colour = variable)) +
    RWS_Thresh() +
    geom_point() +
    geom_line(data = ExtraDataPol, linetype = 'dashed', size = 1) +
    geom_smooth(method = 'scam',
                formula = y ~ s(x, k = 4, bs = 'micx'),
                se = F) +
    labs(title = TeX('\\textbf{Progression and Extrapolation of} $\\textit{q}_{0.75}$'),
         x = '',
         y = expression('75'^{'th'}~'Percentile'),
         shape = 'Methods',
         colour = 'Methods',
         linetype = 'Methods',
         dotted = 'Poly') +
    guides(fill = guide_legend(direction = 'horizontal')) +
    scale_x_date(breaks = '1 year', date_labels = '%Y') +
    scale_y_continuous(labels = function(x) paste0(x, '%')) +
    theme(plot.title = element_text(face = 'bold'),
          legend.title = element_text(face = 'bold'),
          legend.justification = 'right',
          plot.margin = unit(c(0,0.1,0,0.1), 'cm'),
          legend.text = element_text(size = 11),
          legend.background = element_rect(fill = alpha('white', 0)))
  if(hasArg(ExtraData)) Base <- Base + geom_line(data = ExtraData, linetype = 'dotted', size = 1)
  return(Base)
}

Boxplots <- function(Stats, 
                     Var = 'Median',
                     facet = FALSE,
                     Lanes = c('1RR', '2RR', '3RR', '1RL', '2RL', '3RL'), 
                     LaneLevels = NULL){
  # browser()
  Stats.Long <- reshape2::melt(Stats, id.vars = c('Date', 'Cway', 'Hm', 'Lane', 'WT'))
  if(hasArg(LaneLevels)) Stats.Long$Lane <- factor(Stats.Long$Lane, levels = LaneLevels)
  Stats.Long$Year <- as.character(year(Stats.Long$Date))
  
  Base <- ggplot(subset(Stats.Long, variable == Var & Lane %in% Lanes),
                 aes(x = value, y = Year, fill = interaction(WT, Lane))) +
    geom_boxplot(position = position_dodge2(width = 2, padding = 0.1)) +
    labs(title = paste('Boxplots'),
         x = '',
         y = paste(Var, 'Aggregate Loss (%)'),
         colour = 'Wheel Track') +
    theme(plot.title = element_text(face = 'bold'),
          legend.title = element_text(face = 'bold'),
          legend.justification = 'right',
          legend.background = element_rect(fill = alpha('white', 0)))
  # if(max(Stats$Quantile) >= 10) Base <- Base + RWS_Thresh()
  if(facet) Base <- Base + facet_wrap(vars(Lane))
  return(Base)
}

RoadStretch <- function(Stats,
                        Var = 'Quantile',
                        breaks = 0.5,
                        facet = FALSE,
                        Lanes = NULL, 
                        LaneLevels = NULL,
                        points = FALSE){
  # browser()
  Stats.Long <- reshape2::melt(Stats, id.vars = c('Date', 'Cway', 'Hm', 'Lane', 'WT'))
  if(hasArg(LaneLevels)) Stats.Long$Lane <- factor(Stats.Long$Lane, levels = LaneLevels)
  Stats.Long$Year <- as.character(year(Stats.Long$Date))
  Stats.Long <- complete(data = Stats.Long[, !colnames(Stats.Long) %in% c('Date', 'Cway')], 
                         Hm = seq(min(Stats.Long$Hm), max(Stats.Long$Hm), by = 0.1), Lane, Year, WT, variable)
  if(!hasArg(Lanes)) Lanes <- c('1RR', '2RR', '3RR', '1RL', '2RL', '3RL')
  Stats.Long <- subset(Stats.Long, variable == Var & Lane %in% Lanes)
  
  Base <- ggplot(Stats.Long, aes(x = Hm, y = value, colour = Year)) +
    geom_line() +
    facet_grid(Lane ~ WT) +
    coord_cartesian(xlim = c(min(Stats.Long$Hm), max(Stats.Long$Hm))) +
    scale_x_continuous(breaks = seq(round(min(Stats.Long$Hm)), round(max(Stats.Long$Hm)), by = breaks)) +
    scale_y_continuous(labels = function(x) paste0(x, '%')) +
    labs(x = 'Hectometer Post',
         # y = paste(IfElse(Var == 'Quantile', '75-th Percentile', Var), '(%)'),
         y = '',
         colour = 'Year') +
    ThemeDef() +
    theme(plot.title = element_text(face = 'bold'),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 45, vjust = 0.5),
          legend.title = element_text(face = 'bold'),
          legend.justification = 'right',
          legend.background = element_rect(fill = alpha('white', 0)))
  if(points == TRUE) Base <- Base + geom_point(size = 1)
  return(Base)
}

Hectometer <- function(PD, Positions = c('Left', 'Right')){
  PDLong <- PD_Long(PD, 'All')
  PDLong.nomix <- subset(PDLong, Position != 'Mixed')
  PDLong.nomix$variable <- as.numeric((sub('.*sv_', '', PDLong.nomix$variable)))
  ggplot(data = subset(PDLong.nomix,  Position %in% Positions), aes(x = variable, y = value, colour = value)) +
    geom_point() +
    scale_colour_viridis_c('%', option = 'C',
                           direction = -1) +
    labs(x = 'Meter', y = 'Aggregate Loss (%)') +
    facet_grid(Datum_tijd ~ Position) +
    ThemeDef()
}
