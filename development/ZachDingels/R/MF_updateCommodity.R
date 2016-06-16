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
  activeContracts <- list()
  for (contract in names(commodity$Settle[-1])) {
    if (as.POSIXct(Sys.Date()) < contractToDate(contract)) {
      activeContracts <- append(activeContracts, contract)
    }
  }
  # Search for new contracts
  ###### TODO ####
  if (length(activeContracts) == 0) {
    return(commodity)
  }
  
  updatedCommodity <- getCommodityWithContracts(activeContracts)
  
  # Replace the old data with the updated data
  for (contract in activeContracts) {
    commodity$Settle[[contract]] <- updatedCommodity$Settle[[contract]]
    commodity$Volume[[contract]] <- updatedCommodity$Volume[[contract]]
  }
  
  commodity$Meta$lastUpdate <- Sys.Date()
  return(commodity)
  
}