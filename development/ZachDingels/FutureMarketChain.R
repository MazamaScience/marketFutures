####################################################
# This file contains an S3 class to represent Market 
# Future Chain's (FMC) called MarketFutureChain. This 
# class will contain a list containing all the 
# data. The first item named 'Date' will contain 
# the date of the first FMC and the last FMC. The 
# rest of the list will contain FMC's, which are
# each represented by lists. These FMC are each lists,
# with an item named 'DFS' (Days from Start) being 
# the number of days from the first FMC and another item 
# named 'Settle' and 'Volume' with settle and volume
# data for each day.
####################################################

library(stringr)    # For string handling functions
library(dplyr)      # To use left_join() to merge data.

library(Quandl)     # To use Quandl packages/datasets.
library(zoo)        # To use na.locf to fill in data.
library(lubridate)  # To use years().

source('MF_generateContractNames.r', local=TRUE)

#################################################
# Global variables

FUTURES_DATA_DIR <<- ""

# TODO:  Need a global variable with getter and setter for quandlToken
# Henry Nguyen's Quandl token
quandlToken <- "8MsMk6Rkrm3dz3U5Fr4P"
Quandl.api_key(quandlToken)
##################################################

loadFutureMarketChain <- function(commodities='CL', startDate='1995-06-06', endDate='1999-12-31') {
  
  startYear <- year(startDate)
  endYear <- year(endDate)
  
  startMonth <- as.numeric(month(startDate))
  endMonth <- as.numeric(month(endDate))
  
  nameSettle <- paste0(commodities, '_Settle_',startYear,'_',endYear)
  nameVolume <- paste0(commodities, '_Volume_',startYear,'_',endYear)
  filePathSettle <- paste0(FUTURES_DATA_DIR,'/',nameSettle,'.RData')
  filePathVolume <- paste0(FUTURES_DATA_DIR,'/',nameVolume,'.RData')
  
  RawFutureChain <- list('Date' = c(startDate, endDate))
  
  if (!exists(filePathSettle) && !exists(filePathVolume)) {
    contracts <- generateContractNames(commodities, startDate, endDate)
    
    for (contract in contracts) {
      commodityCode <- str_sub(contract,end=-6)
      # On Quandl, every commodity is from the CME exchange except cocoa, coffee, cotton, orange juice, and sugar #11.
      if (commodityCode %in% c('CC','KC','CT','OJ','SB')) {
        exchange <- 'ICE'
      } else {
        exchange <- 'CME'
      }  
      
      # Download Quandl data and handle errors
      result <- try(
        rawFutures <- Quandl(paste0(exchange, '/', contract), type='raw')[,c('Date','Settle','Volume')],
        silent=TRUE)
      
      if (class(result) == 'try-error') {
        err_msg <- geterrmessage()
        if ( str_detect(err_msg,'Requested entity does not exist')) {
          stop(paste0('The contract ', contract, ' does not exist.'), call.=FALSE)      
        }
      }
      
      # Add data to our RawFutureChain in the correct format.
      RawFutureChain[[contract]] <- list('DFB' = as.numeric(rawFutures$Date[1] - as.Date(startDate)), 
                                         'Settle' = rawFutures$Settle,
                                         'Volume' = rawFutures$Volume)
      
      return(RawFutureChain)
    }
    
    
  } else {
    # Load files
  }
}


####################################################
# FUNCTIONS -------
#- load()
#- getContract(contract)
#- getDay
####################################################