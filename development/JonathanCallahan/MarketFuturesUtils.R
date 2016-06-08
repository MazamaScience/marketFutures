#################################################
# Jon's refactoring of Henry's functions for Market Futures Data
#       (functions for r-package)
# 
#
# Mazama Science
# Author: Henry Nguyen
# January 2015
#################################################

# https://www.quandl.com/c/markets/crude-oil

library(stringr)    # For string handling functions
library(dplyr)      # To use left_join() to merge data.

library(Quandl)     # To use Quandl packages/datasets.
library(zoo)        # To use na.locf to fill in data.
library(lubridate)  # To use years().

#################################################
# Global variables

FUTURES_DATA_DIR <<- ""

# TODO:  Need a global variable with getter and setter for quandlToken
# Henry Nguyen's Quandl token
quandlToken <- "8MsMk6Rkrm3dz3U5Fr4P"
Quandl.auth(quandlToken)


#################################################
# Utility functions (lowest level)

# Set data directory. Guarantee that DATA_DIR is a full path
setDataDir <- function(dataDir) {
  FUTURES_DATA_DIR <<- normalizePath(dataDir)
}

# Return data directory 
getDataDir <- function() {
  if (FUTURES_DATA_DIR == "") {
    warning(paste0('The data directory has not been set. Please set it with setDataDir(YOUR_DATA_DIR).'))
  }
  return(FUTURES_DATA_DIR)
}

# Generatecontract namess for a commodity for a range of years
contractNames <- function(commodityCode='CL',startYear=1983,endYear=2020) {
  months <- c('F', 'G', 'H', 'J', 'K', 'M', 'N', 'Q', 'U', 'V', 'X', 'Z')
  years <- startYear:endYear
  allMonths <- rep(months, length(years))
  allYears <- rep(years, each=length(months))
  contracts <- paste0(commodityCode, allMonths, allYears)
  return(contracts)
}

#################################################

# Download futures dataset from Quandl
quandlRawFutures <- function(contract='') {
  
  # Determine the exchange from the contract  
  
  # The commodity code is everything up to the last 5 characters
  commodityCode <- str_sub(contract,end=-6)
  # On Quandl, every commodity is from the CME exchange except cocoa, coffee, cotton, orange juice, and sugar #11.
  if (commodityCode %in% c('CC','KC','CT','OJ','SB')) {
    exchange <- 'ICE'
  } else {
    exchange <- 'CME'
  }  
    
  # Download Quandl data and handle errors
  result <- try(rawFutures <- Quandl(paste0(exchange, '/', contract), type='raw'), silent=TRUE)
  if (class(result) == 'try-error') {
    err_msg <- geterrmessage()
    if ( str_detect(err_msg,'Requested entity does not exist')) {
      stop(paste0('The contract ', contract, ' does not exist.'), call.=FALSE)      
    }
  }
  
  return(rawFutures)
}


# Convert raw futures data to a daily axis using zoo::na.locf.
rawFutures2Daily <- function(rawFutures) {
  # Create an empty data frame with all of the 'dates' from the start to end. 
  # NOTE:  The Quandl data comes in with latest date first
  datesDF <- data.frame(Date=seq(as.Date(rawFutures[nrow(rawFutures),'Date']), as.Date(rawFutures[1,'Date']), by="days"))
  
  # Use dplyr's left_join command to merge the data.
  dailyData <- dplyr::left_join(datesDF, rawFutures, by='Date')
  
  # Use zoo's na.locf command to fill in data for the non-trading days.
  dailyData <- zoo::na.locf(dailyData, na.rm=TRUE)
  
  # The na.locf command ruins the time series as well as changes the other values to 'characters'.
  dailyData[,'Date'] <- as.Date(dailyData[,'Date'])
  for (i in 2:ncol(dailyData)) {
    dailyData[,i] <- as.numeric(dailyData[,i])  
  }
  
  return(dailyData)
}



# Create 'Settle' and 'Volume' dataframes organized as row=time, column=contract
createFuturesDataframes <- function(commodityCode='CL',
                                    startDate='2000-01-01',
                                    endDate='2000-12-31',
                                    verbose=TRUE) {

  # ----- Step 1:  Create a list with daily contract dataframes that cover our period of interest
  
  # Create contract names for this time period
  contracts <- contractNames(commodityCode, year(startDate), year(endDate))
  
  # Empty list to store daily dataframes
  dailyDFList <- list()
  
  for (contract in contracts) {      
    if (verbose) cat(paste0('  ',contract,' ...\n'))
    result <- try(
      dailyDFList[[contract]] <- rawFutures2Daily( quandlRawFutures(contract)[,c('Date','Settle','Volume')] )
    )
    # In case of error, retain the 'contract' column but just insert NA
    if (class(result) == "try-error") {
      daiilyDFList[[contract]] <- NA
    }
  }

  # ----- Step 2:  Create a shared time axis
  
  # Get the date range from the downloaded contracts
  firstIndex <- min(which(!is.na(dailyDFList)))
  firstDF <- dailyDFList[[firstIndex]]
  firstDate <- firstDF$Date[1]

  lastIndex <- max(which(!is.na(dailyDFList)))
  lastDF <- dailyDFList[[lasIndex]]
  lastDate <- lastDF$Date[length(lastDF$Date)]
  
  # Create the full time axis
  # NOTE:  seq.Date() exists but seq.POSIXct() does not
  allDays <- seq(as.Date(firstDate), as.Date(lastDate), by="days")
  allDaysEmptyDF <- data.frame(Date=allDays)
  
  # Create the empty dataframes that we will add columns to
  settleDF <- data.frame(Date=allDays)
  volumeDF <- data.frame(Date=allDays)
  
  # ----- Step 3:  Put all these daily contracts on the shared time axis

  for (contract in contracts) {
    dailyDF <- dailyDFList[[contract]]
    if ( is.na(dailyDF) ) {
      # If the dailyDF is missing, just fill with NA
      settleDF[,contract] <- NA
      volumeDF[,contract] <- NA
    } else {
      # Merge daily contract onto the allDays axis and add the contract column
      allDaysDF <- dplyr::left_join(allDaysEmptyDF, dailyDF, by='Date')
      settleDF[,contract] <- allDaysDF['Settle']
      volumeDF[,contract] <- allDaysDF['Volume']      
    }
  }
  
  return(list(Settle=settleDF,
              Volume=volumeDF))
    
}



###############################################################################
###############################################################################
###############################################################################

if (FALSE) {
  
  setDataDir('~/Data/Quandl')
    
  # Create several small datasets that can be 
  for (startYear in seq(1980,1990,5)) {
    
    endYear <- startYear + 5
    start <- paste0(startYear,'-01-01')
    end <- paste0(startYear+4,'-12-31')
    nameSettle <- paste0('CL_Settle_',startYear,'_',endYear)
    nameVolume <- paste0('CL_Volume_',startYear,'_',endYear)
    filePathSettle <- paste0(FUTURES_DATA_DIR,'/',nameSettle,'.RData')
    filePathVolume <- paste0(FUTURES_DATA_DIR,'/',nameVolume,'.RData')
    
    # Look for local versions of files before downloading more data
    if (!exists(filePathSettle) && !exists(filePathVolume)) {
      
      # Download data and convert to dataframe
      suppressMessages( dataList <- createFuturesDataframes('CL',start,end) )
      
      # Pull Settle data out of dataList and save it
      assign(nameSettle, dataList$Settle)
      save(list=nameSettle, file=filePathSettle)
      
      # Pull Volume data out of dataList and save it
      assign(nameVolume, dataList$Volume)
      save(list=nameVolume, file=filePathVolume)
      
    }
    
  }
  
}

###############################################################################
###############################################################################
###############################################################################

