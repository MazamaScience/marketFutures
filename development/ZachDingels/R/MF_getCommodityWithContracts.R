#' @keywords commodity
#' @export
#' @title getCommodityWithContracts
#' @param contracts A list of contracts
#' @param dataDir Path to the directory data should be stored in.
#' @description Creates a Commodity object from Quandl databases or from a file if the data has already been downloaded. 
#' @return Commodity object
#' @examples
#' \dontrun{
#' CLCommodity <- getCommodity('CL')
#' }

source('~/Projects/marketFutures/development/ZachDingels/R/MF_commodityHelpers.R', local=TRUE)

getCommodityWithContracts <- function(contracts, dataDir = '~/Data/Quandl') {
  rawFutures <- downloadRawFutures(contracts)
  commodity <- getCommodityFromRawFutures(rawFutures)
  return(commodity)
}