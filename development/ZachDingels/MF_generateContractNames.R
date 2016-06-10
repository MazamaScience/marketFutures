#' @keywords Market Futures
#' @export
#' @title Generate a list of market future contract names
#' @param commodityCode The code for a commodity (e.g. 'CL')
#' @param startYear First year to begin generating contract names
#' @param endYear Last year to generate contract names. 
#' @description This function generates a list of contract names for a given commodity 
#' withing a given time frame chronological order. 
#' @return List of strings representing contract names
#' @examples
#' \dontrun{
#' contractNames <- generateContractNames('BZ', 1995, 2016)
#' }
# Generatecontract namess for a commodity for a range of years
generateContractNames <- function(commodityCode='CL',startYear=1983,endYear=2020) {
  months <- c('F', 'G', 'H', 'J', 'K', 'M', 'N', 'Q', 'U', 'V', 'X', 'Z')
  years <- startYear:endYear
  allMonths <- rep(months, length(years))
  allYears <- rep(years, each=length(months))
  contracts <- paste0(commodityCode, allMonths, allYears)
  return(contracts)
}