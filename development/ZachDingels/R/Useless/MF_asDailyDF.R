#' @keywords commodity
#' @export
#' @title asDailyDF 
#' @param commodity A Commodity object
#' @description Creates a list with two data frames. The first is for Settle values with days as rows and contracts as columns. The second is for Volume values with days and contract names.  
#' @return A list of data frames. 
#' @examples
#' \dontrun{
#' CLCommodity <- getCommodity('CL')
#' dailyDF <- asDailyDF()
#' }

asDailyDF <- function(commodity) {
  contracts <- names(commodity$Data$ContractData)
  dailyDFList <- list()
  for (contract in contracts) {
    dailyDFList[[contract]] <- rawFutures2Daily(commodity$Data$ContractData[[contract]])
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
  
  return(list(Settle=settleDF,
              Volume=volumeDF))
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