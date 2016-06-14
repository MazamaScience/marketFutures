#' @keywords commodity
#' @export
#' @title getCommodity 
#' @param commodityID A two character code for a commodity (e.g. 'CL')
#' @param startDate The first date to start downloading from.
#' @param endDate The last date to download (inclusive).
#' @param dataDir Path to the directory data should be stored in.
#' @description Creates a Commodity object from Quandl databases or from a file if the data has already been downloaded. 
#' @return Commodity object
#' @examples
#' \dontrun{
#' CLCommodity <- getCommodity('CL')
#' }

library(stringr)    # For string handling functions
library(dplyr)      # To use left_join() to merge data.

library(Quandl)     # To use Quandl packages/datasets.
library(zoo)        # To use na.locf to fill in data.
library(lubridate)  # To use years().

source('~/Projects/marketFutures/development/ZachDingels/R/MF_generateContractNames.r', local=TRUE)

#################################################
# Global variables


# TODO:  Need a global variable with getter and setter for quandlToken
# Henry Nguyen's Quandl token
quandlToken <- "8MsMk6Rkrm3dz3U5Fr4P"
Quandl.api_key(quandlToken)
##################################################

getCommodity <- function(commodityID, startDate='1980-01-01', endDate='2030-12-31', 
                                      dataDir='~/Data/Quandl') {
  
  startYear <- year(startDate)
  endYear <- year(endDate)
  
  startMonth <- as.numeric(month(startDate))
  endMonth <- as.numeric(month(endDate))
  
  if (substr(dataDir, nchar(dataDir), nchar(dataDir)) == '/') {
    dataDir <- substr(dataDir, 1, nchar(dataDir) - 1)
  }
  filePath <- paste0(dataDir,'/',commodityID,'.RData')
  
  dailyDFList <- list()
  
  if ( !file.exists(filePath) ) {
    contracts <- generateContractNames(commodityID, startDate, endDate)
    
    # For each contract we want we need to generate a quandlCode to give to Quandl. Then we will try to download that contract. 
    # If we can do that we will add it to our commodity data structure. 
    for (contract in contracts) {
      commodityCode <- str_sub(contract,end=-6)
      # On Quandl, every commodity is from the CME exchange except cocoa, coffee, cotton, orange juice, and sugar #11.
      if (commodityCode %in% c('CC','KC','CT','OJ','SB')) {
        exchange <- 'ICE'
      } else {
        exchange <- 'CME'
      }  
      quandlCode <- paste0(exchange, '/', contract)
      
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
        dailyDFList[[contract]] <- rawFutures
      }
      
    }
    
    # ----- Step 2:  Create a shared time axis
    
    # Get the date range from the downloaded contracts
    firstIndex <- min(which(!is.na(dailyDFList)))
    firstDF <- dailyDFList[[firstIndex]]
    firstDate <- firstDF$Date[1]
    
    lastIndex <- max(which(!is.na(dailyDFList)))
    lastDF <- dailyDFList[[lastIndex]]
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
    
    commodity <- list(Meta=list(Commodity=commodityID, Date=c(startDate, endDate), lastUpdate=Sys.Date()),
                      Settle=settleDF,
                      Volume=volumeDF)
    
    
    print(paste0("Saving data to ", filePath))
    
    commodity <- structure(commodity, class = c('Commodity', 'list'))
    assign(commodityID, commodity)
    save(commodity, file=filePath)
    
    return(commodity)
    
  } else {
    return(get(load(paste0(dataDir, '/', commodityID, '.RData'))))
  }
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