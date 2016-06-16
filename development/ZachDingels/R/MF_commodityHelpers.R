################################################################################
#This file contains functions that are used in multiple place for commodities
#but aren't important enough to be in their own file
################################################################################

library(stringr)    # For string handling functions
library(dplyr)      # To use left_join() to merge data.

library(Quandl)     # To use Quandl packages/datasets.
library(zoo)        # To use na.locf to fill in data.
library(lubridate)  # To use years().

quandlToken <- "8MsMk6Rkrm3dz3U5Fr4P"
Quandl.api_key(quandlToken)

CONTRACT_MONTHS <<- c('F', 'G', 'H', 'J', 'K', 'M', 'N', 'Q', 'U', 'V', 'X', 'Z')

generateContractNames <- function(commodityCode='CL',startDate='1983-01-01',endDate='1995-06-06') {
  startMonth <- as.numeric(month(startDate))
  endMonth <- as.numeric(month(endDate))
  
  years <- year(startDate):year(endDate)
  allMonths <- rep(CONTRACT_MONTHS, length(years))
  allYears <- rep(years, each=length(CONTRACT_MONTHS))
  contracts <- paste0(commodityCode, allMonths, allYears)
  contracts <- contracts[startMonth:(length(contracts) - (12 - endMonth))]
  
  return(contracts)
}

generateQuandlCode <- function(contract) {
  commodityCode <- str_sub(contract,end=-6)
  # On Quandl, every commodity is from the CME exchange except cocoa, coffee, cotton, orange juice, and sugar #11.
  if (commodityCode %in% c('CC','KC','CT','OJ','SB')) {
    exchange <- 'ICE'
  } else {
    exchange <- 'CME'
  }  
  quandlCode <- paste0(exchange, '/', contract)
  return(quandlCode)
}

downloadRawFutures <- function(contracts) {
  rawFuturesList <- list()
  for (contract in contracts) {
    quandlCode <- generateQuandlCode(contract)
    
    print( paste0("Downloading ", contract) )
    # Download Quandl data and handle errors
    result <- try(
      rawFutures <- Quandl(quandlCode, type='raw')[,c('Date','Settle','Volume')],
      silent=TRUE)
    
    if (class(result) == 'try-error') {
      err_msg <- geterrmessage()
      if ( str_detect(err_msg,'Requested entity does not exist')) {
        stop(paste0('The contract ', contract, ' does not exist.'), call.=FALSE)      
      }
    } else {
      rawFuturesList[[contract]] <- rawFutures
    }
  }
  return(rawFuturesList)
}

getCommodityFromRawFutures <- function(rawFuturesList) {
  # ----- Step 2:  Create a shared time axis
  
  # Get the date range from the downloaded contracts
  dates <- list()
  for (DF in rawFuturesList) {
    dates <- append(dates, DF$Date)
  }
  
  firstDate <- min(dates)
  lastDate <- max(dates)
  
  # Create the full time axis
  # NOTE:  seq.Date() exists but seq.POSIXct() does not
  allDays <- seq(as.Date(firstDate), as.Date(lastDate), by="days")
  allDays <- as.POSIXct(allDays)
  allDaysEmptyDF <- data.frame(Date=allDays)
  
  # Create the empty dataframes that we will add columns to
  settleDF <- data.frame(Date=allDays)
  volumeDF <- data.frame(Date=allDays)
  
  # ----- Step 3:  Put all these daily contracts on the shared time axis
  contracts <- names(rawFuturesList)
  for (contract in contracts) {
    dailyDF <- rawFuturesList[[contract]]
    if ( is.na(rawFuturesList) ) {
      print("yea")
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
  
  commodityID <- substr(contract[1], 1, 2)
  dateRange <- c(contractToDate(contracts[1]), contractToDate(tail(contracts, 1)))
  commodity <- list(Meta=list(Commodity=commodityID, DateRange=dateRange, LastUpdate=as.POSIXct(Sys.Date()),
                    Settle=settleDF,
                    Volume=volumeDF)
  
  return( structure(commodity, class = c('Commodity', 'list')) )
}

contractToDate <- function(contract) {
  day <- '01'
  month <- as.character(which(CONTRACT_MONTHS %in% substr(contract, 3, 3)))
  year <- substr(contract, 4, 7)
  return( as.POSIXct(paste(year, month, day, sep = '-')) )
}
