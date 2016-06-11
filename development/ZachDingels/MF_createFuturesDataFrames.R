#' @keywords Market Futures
#' @export
#' @title Create a data frame reperesenting futures data
#' @param commodityCode A 2 character code for a commodity
#' @param startDate A date to begin collecting data from
#' @param endDate A date to finish collecting data
#' @param verbose A boolean flag for verbose debugging
#' @description Create a dataframe containing futures data for a certain time frame. 
#' @return Data frame with futures data.
#' @examples
#' \dontrun{
#' futuresDF <- createFuturesDataframes('CL', '2000-01-01', '2000-12-31')
#' }


createFuturesDataframes <- function(commodityCode='CL',
                                    startDate='2000-01-01',
                                    endDate='2000-12-31',
                                    verbose=TRUE) {
  
  # ----- Step 1:  Create a list with daily contract dataframes that cover our period of interest
  
  # Create contract names for this time period
  contracts <- generateContractNames(commodityCode, year(startDate), year(endDate))
  
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