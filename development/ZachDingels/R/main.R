library(stringr)    # For string handling functions
library(dplyr)      # To use left_join() to merge data.

library(Quandl)     # To use Quandl packages/datasets.
library(zoo)        # To use na.locf to fill in data.
library(lubridate)  # To use years().
library(futile.logger)

################################################################################
################################################################################
############################# Helper Functions #################################
################################################################################

# This function will create all the contracts for a given commodity within a given range of days
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

# This function will generate a list of query codes that you can pass to Quandl to get actual data.
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

# This function downloads raw data from quandl for a list of contracts. This function returns a dataframe
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

# This function turns raw quandl data into a commodity object. The difference between a raw quandl dataframe and commodity is that the commodity also contains meta data.
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
  allDaysEmptyDF <- data.frame(Date=allDays)
  
  # Create the empty dataframes that we will add columns to
  settleDF <- data.frame(Date=allDays)
  volumeDF <- data.frame(Date=allDays)
  
  # ----- Step 3:  Put all these daily contracts on the shared time axis
  contracts <- names(rawFuturesList)
  for (contract in contracts) {
    dailyDF <- rawFuturesList[[contract]]
    if ( is.na(rawFuturesList) ) {
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
  commodity <- list(Meta=list(Commodity=commodityID, DateRange=dateRange, LastUpdate=as.POSIXct(Sys.Date())),
                    Settle=settleDF,
                    Volume=volumeDF)
  
  return( structure(commodity, class = c('Commodity', 'list')) )
}

# Returns the date that a contract is associated with.
contractToDate <- function(contract) {
  day <- '01'
  month <- as.character(which(CONTRACT_MONTHS %in% substr(contract, 3, 3)))
  year <- substr(contract, 4, 7)
  return( as.POSIXct(paste(year, month, day, sep = '-')) )
}


################################################################################
########################## Top Level Functions #################################
################################################################################

# Get's a commodity given a commodity ID, start and end dates and a path to a data directory. There is also an option not return anything if you just want to download and save data. 
getCommodity <- function(commodityID, startDate='1980-01-01', endDate='2030-12-31', 
                         dataDir='~/Data/Quandl', returnEmpty = FALSE) {
  
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
    
  } else {
    commodity <- get(load(paste0(dataDir, '/', commodityID, '.RData')))
  }
  if (!returnEmpty) {
    return(commodity)
  }
}

# Given a list of commodites, a start and end date and a path to a data directory this function returns a list of commodities. 
getCommodities <- function(commodityIDs, startDate='1980-01-01', endDate='2030-12-31', 
                           dataDir='~/Data/Quandl') {
  commodities <- list()
  for (commodityID in commodityIDs) {
    commodity <- getCommodity(commodityID, startDate, endDate, dataDir)
    commodities[[commodityID]] <- commodity
  }
  return(commodities)
}

# If you know which contracts you want you can use this function to create a commodity.
getCommodityWithContracts <- function(contracts, dataDir = '~/Data/Quandl') {
  rawFutures <- downloadRawFutures(contracts)
  commodity <- getCommodityFromRawFutures(rawFutures)
  commodity$Meta$FilePath <- paste(dataDir, paste0(commodity$Meta$Commodity, '.RData'), sep = '/')
  return(commodity)
}

# This function updates a commodity with the most current data. 
updateCommodity <- function(commodity) {
  # Get all the contracts that need to be updated
  activeContracts <- vector()
  for (contract in names(commodity$Settle[-1])) {
    if (as.POSIXct(Sys.Date()) < contractToDate(contract)) {
      activeContracts <- append(activeContracts, contract)
    }
  }
  
  going <- TRUE
  date <- commodity$Meta$DateRange[2]
  while(going) {
    newContract <- generateContractNames(commodity$Meta$Commodity, date, date)
    quandlCode <- generateQuandlCode(newContract)
    result <- try(
      Quandl(quandlCode, type='raw')[,c('Date','Settle','Volume')],
      silent=TRUE)
    
    if (class(result) == 'try-error') {
      going=FALSE
    } else {
      activeContracts <- c(activeContracts, newContract) 
      day <- 01
      month <- ifelse(month(date) == 12, 1, month(date) + 1)
      year <- ifelse(month(date) == 1, year(date) + 1, year(date))
      date <- as.POSIXct( paste(year, month, day, sep = '-') )
    }
    
  }
  if (length(activeContracts) == 0) {
    return(commodity)
  }
  
  uniqueActiveContracts <- unique(activeContracts)
  updatedCommodity <- getCommodityWithContracts(uniqueActiveContracts)
  
  # The new DF is going to have rows that we don't have because it has additional days
  # So we need to add new rows
  numAdditionalDays <-  sum(!(rownames(updatedCommodity$Settle) %in% rownames(commodity$Settle)))
  for (i in 1:numAdditionalDays) {
    commodity$Settle <- rbind(commodity$Settle, NA)
    commodity$Volume <- rbind(commodity$Volume, NA)
  }
  
  
  # Replace the old data with the updated data
  # Also update the new dates.
  for (contract in c(uniqueActiveContracts, 'Date')) {
    commodity$Settle[[contract]] <- updatedCommodity$Settle[[contract]]
    commodity$Volume[[contract]] <- updatedCommodity$Volume[[contract]]
  }
  
  commodity$Meta$lastUpdate <- Sys.Date()
  saveCommodity(commodity)
  return(commodity)
  
}

# Save a given commodity. 
saveCommodity <- function(commodity) {
  print(paste0("Saving Commodity: ", commodity$Meta$Commodity, " to ", commodity$Meta$FilePath))
  assign(commodity$Meta$Commodity, commodity)
  save(commodity, file=commodity$Meta$FilePath)
}

################################################################################
################################################################################
################################ Code ##########################################
################################################################################


quandlToken <- "8MsMk6Rkrm3dz3U5Fr4P"
Quandl.api_key(quandlToken)

CONTRACT_MONTHS <<- c('F', 'G', 'H', 'J', 'K', 'M', 'N', 'Q', 'U', 'V', 'X', 'Z')

main <- function(commodityList, startDate = '1980-01-01', endDate = '2030-01-01', 
                 dataDir='~/Data/Quandl', outputDir='~/Data/Quandl', 
                 logLevel='INFO', transcript='MF_TRANSCRIPT.txt') {
  
  # Set up a new transcript file
  if (!is.null(transcript)) {
    transcriptPath <- file.path(outputDir,transcript)
    file.remove(transcriptPath)
    futile.logger::flog.appender(futile.logger::appender.file(transcriptPath))
  }
  
  # Set log level
  futile.logger::flog.threshold(get('logLevel'))
  
  # Silence other warning messages
  options(warn=-1)
  
  commodities <- getCommodities(commodityList, startDate, endDate, dataDir)
  
}