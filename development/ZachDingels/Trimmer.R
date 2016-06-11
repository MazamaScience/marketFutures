load("~/Data/Quandl/CL_Settle_1983_2016.RData")

settle <- CL_Settle_1983_2016

trimAndSave(settle)



trimAndSave <- function(marketFutureDF) {
  
  dataToSave <- list()
  dataToSave[['date']] <- c(marketFutureDF[1,1], marketFutureDF[nrow(marketFutureDF), 1])
  
  findBeg <- function(l) {
    pivot <- length(l) / 2
    if ( !is.na(l[pivot]) ) {
      return(pivot)
    } else {
      return()
    }
  }
  
  for ( i in 1:(ncol(marketFutureDF) - 1) ) {
    column <- marketFutureDF[,i+1]
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    index <- 1
    begNAIndex <- FALSE
    trimmedCol <- list()
    # Find the two spots where our data begins
    while ( is.logical(begNAIndex) ) {
      if ( is.logical(begNAIndex) && !is.na(column[index + 1]) ) {
        begNAIndex <- index - 1
      }
      
      index <- index + 1
    }
    
    trimmedCol <- na.trim(column)
    dataToSave[[ names(marketFutureDF)[i+1] ]] <- c(begNAIndex, trimmedCol)
  }
  
  assign('data', dataToSave)
  save(dataToSave, file='~/Data/Quandl/again.RData')
}

loadTrimmedDF <- function(file) {
  pathToFile <- '~/Data/Quandl/test.RData'
  varName <- load(pathToFile)
  # Figure out name of variable...
  
  DF <- data.frame('Date' = seq(data$date[1], data$date[2], 'days'))
  data <- data
  
  # Go through every column in the data except the first one (it's just dates) 
  for (colName in names(data[-1]) ) {
    numNAInfront <- data[[colName]][1]
    numNABehind <- dim(DF)[1] - numNAInfront - length(data[[colName]][-1]) 
    DF[[colName]] <- c(rep(NA, numNAInfront), 
                    data[[colName]][-1], 
                    rep(NA, numNABehind) )
  }
  
}