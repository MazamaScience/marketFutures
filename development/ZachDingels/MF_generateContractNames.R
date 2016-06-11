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

library(lubridate)

generateContractNames <- function(commodityCode='CL',startDate='1983-01-01',endDate='1995-06-06') {
  startMonth <- as.numeric(month(startDate))
  endMonth <- as.numeric(month(endDate))
  
  months <- c('F', 'G', 'H', 'J', 'K', 'M', 'N', 'Q', 'U', 'V', 'X', 'Z')
  years <- year(startDate):year(endDate)
  allMonths <- rep(months, length(years))
  allYears <- rep(years, each=length(months))
  contracts <- paste0(commodityCode, allMonths, allYears)
  contracts <- contracts[startMonth:(length(contracts) - (12 - endMonth))]
  
  return(contracts)
}