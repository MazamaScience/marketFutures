#################################################
# Zach's refactoring of Jon's refactoring of Henry's functions 
# for Market Futures Data
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

FUTURES_DATA_DIR <<- "~/Data/Futures/"

# TODO:  Need a global variable with getter and setter for quandlToken
# Henry Nguyen's Quandl token
quandlToken <- "8MsMk6Rkrm3dz3U5Fr4P"
Quandl.auth(quandlToken)

#################################################

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