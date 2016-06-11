#' @keywords Market Futures
#' @export
#' @title Download futures dataset from Quandl
#' @param contract A list of contract codes
#' @description This creates a data frame of raw futures. 
#' @return Data frame of raw futures.
#' @examples
#' \dontrun{
#' rawFutures <- downloadRawFutures(contracts)
#' }
 
library(stringr)
library(Quandl)

# Download futures dataset from Quandl
downloadRawFutures <- function(contract='') {
  
  # Determine the exchange from the contract  
  
  # The commodity code is everything up to the last 5 characters
  commodityCode <- str_sub(contract,end=-6)
  # On Quandl, every commodity is from the CME exchange except cocoa, coffee, cotton, orange juice, and sugar #11.
  if (any(commodityCode %in% c('CC','KC','CT','OJ','SB'))) {
    exchange <- 'ICE'
  } else {
    exchange <- 'CME'
  }  
  
  # Download Quandl data and handle errors
  result <- try(rawFutures <- Quandl(paste0(exchange, '/', contract), type='raw'), silent=TRUE)
  if (class(result) == 'try-error') {
    err_msg <- geterrmessage()
    if ( str_detect(err_msg,'Requested entity does not exist')) {
      stop(paste0('The contract ', contract, ' does not exist.'), call.=FALSE)      
    }
  }
  
  return(rawFutures)
}