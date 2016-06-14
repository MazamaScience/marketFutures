subsetByContracts <- function(commodity, contracts) {
  
  contractData <- list()
  dateRange <- list()
  for (contract in contracts) {
    contractData[[contract]] <- commodity$Data$ContractData[[contract]]
    dateRange[[contract]] <- commodity$Data$DateRange[[contract]]
  }
  
  dates <- as.Date(unlist(dateRange, recursive = TRUE, use.names = FALSE))
  commodity <- list( 'Meta' = list('Commodity' = commodity$Meta$Commodity, 'Date' = c(min(dates), max(dates)), 'LastUpdate' = commodity$Meta$LastUpdate), 
                     'Data' = list('DateRange' = dateRange, 'ContractData' = contractData) )
  
  return(structure(commodity, class = c('Commodity', 'list')))
  
}
