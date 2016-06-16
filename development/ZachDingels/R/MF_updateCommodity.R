#' @keywords commodity
#' @export
#' @title updateCommodity 
#' @param commodity
#' @description Updates a commodity with all the current Quandl data.
#' @return Commodity object
#' @examples
#' \dontrun{
#' commodityCommodity <- getCommodity('commodity')
#' updateCommodity(commodityCommodity)
#' }

source('~/Projects/marketFutures/development/ZachDingels/R/MF_commodityHelpers.R', local=TRUE)
source('~/Projects/marketFutures/development/ZachDingels/R/MF_getCommodityWithContracts.R', local=TRUE)

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
  ###### TODO ####
  if (length(activeContracts) == 0) {
    return(commodity)
  }
  
  updatedCommodity <- getCommodityWithContracts(activeContracts)
  
  # Replace the old data with the updated data
  for (contract in activeContracts) {
    print(contract)
    commodity$Settle[[contract]] <- updatedCommodity$Settle[[contract]]
    commodity$Volume[[contract]] <- updatedCommodity$Volume[[contract]]
  }
  
  commodity$Meta$lastUpdate <- Sys.Date()
  saveCommodity(commodity)
  return(commodity)
  
}