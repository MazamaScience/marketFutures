####################################################

library(stringr)    # For string handling functions
library(dplyr)      # To use left_join() to merge data.

library(Quandl)     # To use Quandl packages/datasets.
library(zoo)        # To use na.locf to fill in data.
library(lubridate)  # To use years().

source('~/Projects/marketFutures/development/ZachDingels/R/MF_generateContractNames.r', local=TRUE)
source('~/Projects/marketFutures/development/ZachDingels/R/MF_downloadFutureMarketChain.R', local=TRUE)

#################################################

updateFutureMarketChain <- function(futureMarketChain) {
  commodity <- futureMarketChain[['Commodity']]
  lastUpdate <- futureMarketChain[['LastUpdate']]
  
  newFMC <- getFutureMarketChain(commodity = commodity,
                       startDate = lastUpdate,
                       endDate = Sys.Date())
  
  for ( contract in generateContractNames(commodity, lastUpdate, Sys.Date()) ) {
    futureMarketChain[[contract]] <- newFMC[[contract]]
  }
  
  return(futureMarketChain)
}