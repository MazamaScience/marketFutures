#' @keywords Market Futures
#' @export
#' @title Set raw futures to daily format
#' @param rawFutures A dataframe of raw Futures.
#' @description Convert raw futures data to a daily axis using zoo::na.locf.
#' @return Data frame of rawFutures with a daily format.
#' @examples
#' \dontrun{
#' rawFutures <- rawFutures2Daily(rawFutures)
#' }

library(zoo)
library(dplyr)

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