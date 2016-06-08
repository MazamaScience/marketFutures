

############################################################
# futuresPlot
#

futuresChainPlot <- function(dataList, infoList, textList) {
  
  ########################################
  # Extract data from 'dataList' object 
  ########################################
  
  timepoint <- (proc.time())[3]
  futuresdf <- dataList$futuresdf
  spotPrices <- dataList$spotPrices
  
  ########################################
  # Extract variables from the 'infoList' object
  ########################################
  
  # Extra information
  yearRange <- ifelse(is.null(infoList$yearRange),'last15', infoList$yearRange)
  yRange <- ifelse(is.null(infoList$yRange),TRUE,infoList$yRange)
  date <- ifelse(is.null(infoList$date),'2015-01-26',infoList$date)
  
  ########################################
  # Plot the data
  ########################################
    
  if (yearRange == 'full') {
    startYear <- 1985
  } else if (yearRange == 'last5') {
    startYear <- 2010
  } else if (yearRange == 'last10') {
    startYear <- 2005
  } else if (yearRange == 'last15') {
    startYear <- 2000
  } else if (yearRange == 'last20') {
    startYear <- 1995
  } else {
    startYear <- 1990
  }
  
  if (yRange == 'true') {
    yRange <- TRUE
  } else {
    yRange <- FALSE
  }
  
  futuresPlot(futuresdf, spotPrices, startYear, 2020, yRange, date, TRUE)
    
  ########################################
  # Return values
  ########################################
  
  return(c(1.0,2.0,3.0,4.0))
  
}


