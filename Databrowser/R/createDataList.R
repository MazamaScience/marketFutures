########################################################################
# createDataList.R
#
# Databrowser specific creation of dataframes for inclusion in dataList.
#
# Author: Jonathan Callahan
########################################################################

createDataList <- function(infoList) {
  
  # Create dataList
  if (infoList$plotType == "Futures") {
    dataList <- list(futuresdf=get(load(paste0(infoList$dataDir, 'CL_Settle_1980_2025.RData'))), 
                     spotPrices=get(load(paste0(infoList$dataDir, 'CL_Spot_Prices.RData'))))
  } else {
    dataList <- list()
  }

  return(dataList)
}

