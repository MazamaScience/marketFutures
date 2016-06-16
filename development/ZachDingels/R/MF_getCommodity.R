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

source('~/Projects/marketFutures/development/ZachDingels/R/MF_commodityHelpers.R', local=TRUE)
getCommodity <- function(commodityID, startDate='1980-01-01', endDate='2030-12-31', 
                                      dataDir='~/Data/Quandl') {
  
  startYear <- year(startDate)
  endYear <- year(endDate)
  
  if (substr(dataDir, nchar(dataDir), nchar(dataDir)) == '/') {
    dataDir <- substr(dataDir, 1, nchar(dataDir) - 1)
  }
  filePath <- paste0(dataDir,'/',commodityID,'.RData')
  

  if ( !file.exists(filePath) ) {
    
    contracts <- generateContractNames(commodityID, startDate, endDate)
    rawFutures <- downloadRawFutures(contracts)
    commodity <- getCommodityFromRawFutures(rawFutures)
    
    commodity$Meta[['FilePath']] <- filePath
    saveCommodity(commodity)
    return(commodity)
    
  } else {
    return(get(load(paste0(dataDir, '/', commodityID, '.RData'))))
  }
}