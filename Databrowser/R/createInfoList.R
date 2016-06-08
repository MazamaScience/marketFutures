########################################################################
# createInfoList.R
#
# Create an infoList from a jsonArgs string.
#
# Besides basic conversion from strings to other data types, a lot of
# specific choices can made here that will be used later on in different
# plotting scripts.
#
# Author: Jonathan Callahan
########################################################################

createInfoList <- function(jsonArgs='{}') {

  # ----- Minumum set of infoList parameters from the UI -----------------------
  
  # Initialize the infoList from the jsonArgs
  infoList <- as.list(fromJSON(jsonArgs))
  
  # Guarantee that the following variables always have a default
  infoList$language <- ifelse(is.null(infoList$language),'en',infoList$language)
  infoList$responseType <- ifelse(is.null(infoList$responseType),'json',infoList$responseType)
  infoList$plotDevice <- ifelse(is.null(infoList$plotDevice),'png',infoList$plotDevice)
  infoList$plotWidth <- ifelse(is.null(infoList$plotWidth),1000,as.numeric(infoList$plotWidth))
  infoList$plotHeight <- ifelse(is.null(infoList$plotHeight),infoList$plotWidth,as.numeric(infoList$plotHeight))

  if (is.null(infoList$plotType)) {
    stop(paste0("ERROR in createInfoList.R: required parameter plotType is missing."),call.=FALSE)
  }
  
  # ----- Databrowser specific parameters from the UI --------------------------
  
  infoList$plotType <- ifelse(is.null(infoList$plotType),'Futures',infoList$plotType)
  infoList$yearRange <- ifelse(is.null(infoList$yearRange),'last15', infoList$yearRange)
  infoList$yRange <- ifelse(is.null(infoList$yRange),TRUE,infoList$yRange)
  infoList$date <- ifelse(is.null(infoList$date),'2015-01-26',infoList$date)

  return(infoList)
}
