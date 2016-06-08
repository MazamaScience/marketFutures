#################################################
# Data Utils for Market Futures Data
#       (functions for r-package)
# 
#
# Mazama Science
# Author: Henry Nguyen 
# January 2015
#################################################

# https://www.quandl.com/c/markets/crude-oil

library(dplyr)      # To use left_join() to merge data.
library(lubridate)  # To use years().
library(Quandl)     # To use Quandl packages/datasets.
library(stringr)    # For string handling functions
library(zoo)        # To use na.locf to fill in data.

# Table of Contents --------------------------------------------------------------------------------
#
# Global Variables
#
# setDataDir(dataDir)
# getDataDir()
# setQuandlToken(token)
# getQuandlToken()
# contractNames(commodityCode, startYear, endYear)
# getCommodityCode(commodity)
# quandlRawFutures(contract)
# quandlSpotPrices(commodity)
# rawData2Daily(rawFutures)
# createFuturesDataFrame(commodityCode, startDate, endDate, verbose)
# futuresPlot(futuresdf, spotPrices, startYear, endYear, ymin, ymax, date, show)

#################################################
# Global variables

FUTURES_DATA_DIR <<- ""
USER_QUANDL_TOKEN <<- ""

# Henry Nguyen's Quandl token and directory
quandlToken <- "8MsMk6Rkrm3dz3U5Fr4P"
Quandl.auth(quandlToken)
dataDir <- "./code/data_local/"


#################################################
# Utility functions (lowest level)

# Set data directory. Guarantee that DATA_DIR is a full path
setDataDir <- function(dataDir) {
  FUTURES_DATA_DIR <<- normalizePath(dataDir)
}

# Return data directory 
getDataDir <- function() {
  if (FUTURES_DATA_DIR == "") {
    warning(paste0('The data directory has not been set. Please set it with setDataDir(YOUR_DATA_DIR).'))
  }
  return(FUTURES_DATA_DIR)
}

# Set Quandl token
setQuandlToken <- function(token) {
  Quandl.auth(token)
}

# Return the Quandl token
getQuandlToken <- function() {
  if (USER_QUANDL_TOKEN <<- "") {
    warning(paste0('The Quandl token has not been set. Please set it with setQuandlToken(token).'))
  }
  return(USER_QUANDL_TOKEN)
}

# Grab the commodity code corresponding to the commodity
getCommodityCode <- function(commodity='crudeOil') {
  dict <- list('crudeOil'='CL', 'gasoline'='RB', 'heatingOil'='HO', 'naturalGas'='NG', 'ethanol'='EH',
               'gold'='GC', 'silver'='SI', 'platinum'='PL', 'palladium'='PA', 'corn'='C', 'oats'='O', 
               'roughRice'='RR', 'soybeanMeal'='SM', 'soybeanOil'='BO', 'wheat'='W', 'feederCattle'='FC',
               'porkBelly'='PB', 'leanHogs'='LN', 'liveCattle'='LC', 'cocoa'='CC', 'coffee'='KC', 
               'cotton'='CT', 'lumber'='LB', 'orangeJuice'='OJ', 'sugar11'='SB')
  return(dict[[commodity]])
}

# Generate contract names for a commodity for a range of years
contractNames <- function(commodityCode='CL',startYear=1983,endYear=2020) {
  months <- c('F', 'G', 'H', 'J', 'K', 'M', 'N', 'Q', 'U', 'V', 'X', 'Z')
  years <- startYear:endYear
  allMonths <- rep(months, length(years))
  allYears <- rep(years, each=length(months))
  contracts <- paste0(commodityCode, allMonths, allYears)
  return(contracts)
}


#################################################
# Utility functions (upper level)

# Download futures dataset from Quandl
quandlRawFutures <- function(contract='') {
  
  # Determine the exchange from the contract  
  
  # The commodity code is everything up to the last 5 characters
  commodityCode <- str_sub(contract,end=-6)
  # On Quandl, every commodity is from the CME exchange except cocoa, coffee, cotton, orange juice, and sugar #11.
  if (commodityCode %in% c('CC','KC','CT','OJ','SB')) {
    exchange <- 'ICE'
  } else {
    exchange <- 'CME'
  }  
  
  # Download Quandl data and handle errors
  result <- try(rawFutures <- Quandl(paste0(exchange, '/', contract), type='raw'), silent=TRUE)
  if (class(result) == 'try-error') {
    err_msg <- geterrmessage()
    if ( str_detect(err_msg,'Requested entity does not exist')) {
      stop(paste0('The contract ', contract, ' does not exist.'), call.=FALSE)      
    }
  }
  
  return(rawFutures)
}

# Download the spot prices data from Quandl
getSpotPrices <- function(commodity='CL') {
  
  # Determine the Quandl code for the spot prices. (need to complete this)
  dict <- list('CL'='DOE/RWTC', 'NG'='WSJ/NG_HH')
  
  # Download Quandl data
  rawSpotPrices <- Quandl(dict[[commodity]], type='raw')
  
  return(rawSpotPrices)
}

# Convert raw futures and spot price data to a daily axis using zoo::na.locf.
rawData2Daily <- function(rawFutures) {
  # Create an empty data frame with all of the 'dates' from the start to end. 
  # NOTE:  The Quandl data comes in with latest date first
  datesDF <- data.frame(Date=seq(as.Date(rawFutures[nrow(rawFutures),'Date']), as.Date(rawFutures[1,'Date']), by="days"))
  
  # Use dplyr's left_join command to merge the data.
  dailyData <- dplyr::left_join(datesDF, rawFutures, by='Date')
  
  # Use zoo's na.locf command to fill in data for the non-trading days.
  dailyData <- zoo::na.locf(dailyData, na.rm=TRUE)
  
  # The na.locf command ruins the time series as well as changes the other values to 'characters'.
  dailyData[,'Date'] <- as.Date(dailyData[,'Date'])
  for (i in 2:ncol(dailyData)) {
    dailyData[,i] <- as.numeric(dailyData[,i])  
  }
  
  return(dailyData)
}



# Create 'Settle' and 'Volume' dataframes organized as row=time, column=contract
createFuturesDataframes <- function(commodityCode='CL',
                                    startDate='2000-01-01',
                                    endDate='2000-12-31',
                                    verbose=TRUE) {
  
  # ----- Step 1:  Create a list with daily contract dataframes that cover our period of interest
  
  # Create contract names for this time period
  contracts <- contractNames(commodityCode, year(startDate), year(endDate))
  
  # Empty list to store daily dataframes
  dailyDFList <- list()
  
  for (contract in contracts) {      
    if (verbose) cat(paste0('  ',contract,' ...\n'))
    result <- try(
      dailyDFList[[contract]] <- rawData2Daily( quandlRawFutures(contract)[,c('Date','Settle','Volume')] )
    )
    # In case of error, retain the 'contract' column but just insert NA
    if (class(result) == "try-error") {
      dailyDFList[[contract]] <- NA
    }
  }
  
  # ----- Step 2:  Create a shared time axis
  
  # Get the date range from the downloaded contracts
  firstIndex <- min(which(!is.na(dailyDFList)))
  firstDF <- dailyDFList[[firstIndex]]
  firstDate <- firstDF$Date[1]
  
  lastIndex <- max(which(!is.na(dailyDFList)))
  lastDF <- dailyDFList[[lastIndex]]
  lastDate <- lastDF$Date[length(lastDF$Date)]
  
  # Create the full time axis
  # NOTE:  seq.Date() exists but seq.POSIXct() does not
  allDays <- seq(as.Date(firstDate), as.Date(lastDate), by="days")
  allDaysEmptyDF <- data.frame(Date=allDays)
  
  # Create the empty dataframes that we will add columns to
  settleDF <- data.frame(Date=allDays)
  volumeDF <- data.frame(Date=allDays)
  
  # ----- Step 3:  Put all these daily contracts on the shared time axis
  
  for (contract in contracts) {
    dailyDF <- dailyDFList[[contract]]
    if ( is.na(dailyDF) ) {
      # If the dailyDF is missing, just fill with NA
      settleDF[,contract] <- NA
      volumeDF[,contract] <- NA
    } else {
      # Merge daily contract onto the allDays axis and add the contract column
      allDaysDF <- dplyr::left_join(allDaysEmptyDF, dailyDF, by='Date')
      settleDF[,contract] <- allDaysDF['Settle']
      volumeDF[,contract] <- allDaysDF['Volume']      
    }
  }
  
  return(list(Settle=settleDF,
              Volume=volumeDF))
  
}


futuresPlot <- function(commodityCode='CL',
                        futuresDF=futuresDF, 
                        spotPrices=spotPrices, 
                        startYear=2000,
                        endYear=2020,
                        yRange=TRUE,
                        date='2015-01-01', 
                        show=TRUE) {
  
  # Spot Price dates are currently of class 'double'. Needs to be converted to POSIXct.
  spotDates <- as.POSIXct(spotPrices[,1], tz='GMT')
  spotPrices <- spotPrices[,2]
  
  # More space on top for comments
  oldPar <- par()
  par(mar=c(5,4,6,2)+.1)
  
  startDate <- paste0(startYear, '-01-01')
  endDate <- paste0(endYear + 1, '-01-01')
  
  # Plot the Spot Prices as well as today's 9 month futures data.
  xlim <- c(as.POSIXct(startDate, tz='GMT'), as.POSIXct(endDate, tz='GMT'))
  if (yRange == TRUE) {
    ranges <- list('CL'=c(0, 160), 'NG'=c(0, 20))
    ylim <- ranges[[commodityCode]]
  } else {
    start <- ifelse(startDate < spotDates[1],1,which(format(spotDates,tz='GMT')==startDate))
    end <- ifelse(endDate > spotDates[length(spotDates)], length(spotPrices), which(format(spotDates,tz='GMT')==endDate))
    ylim <- c(min(spotPrices[start:end], na.rm=TRUE), max(spotPrices[start:end], na.rm=TRUE)) / 10
    ylim <- c(floor(ylim[1]), ceiling(ylim[2])) * 10
  }
  plot(spotDates, spotPrices, type='p', las=1,
       ylab='Settlement Price ($ / bbl)', ylim=ylim,
       xlab='', xlim=xlim,
       pch=16,cex=0.5, axes=FALSE)
  box()
  
  fiveYearIntervals <- seq(as.POSIXct(startDate, tz='GMT'), as.POSIXct(endDate, tz='GMT'), by='5 years')
  axis(1, at=fiveYearIntervals, labels=seq(startYear,endYear,5))
  axis(2, at=seq(ylim[1],ylim[2],(ylim[2]-ylim[1])/10),labels=paste0('$',seq(ylim[1],ylim[2],(ylim[2]-ylim[1])/10)),las=1)
  title('Futures Chain Projections',line=3)
  mtext('Data: quandl.com | Graphic: mazamascience.com',side=1,line=3)
  
  # Add a grid
  abline(h=seq(ylim[1],ylim[2],(ylim[2]-ylim[1])/10),col="gray70",lty="dotted")
  abline(v=fiveYearIntervals,col="gray70",lty="dotted")
  
  # Show the futures chain projections for the past quarter. 
  if (show == TRUE) {
    for (i in 90:1) {
      result <- try({
      day <- as.POSIXct(date, tz='GMT')
      day <- day - days(i-1)
      index <- which(futuresDF[,1] == as.Date(day))
      y <- na.trim(as.numeric(futuresDF[index,-1]), sides='both')
      x <- seq(day, day %m+% months(length(y) - 1), length.out=length(y))
      cols <- c('black', rep('red', 6), rep('blue', 23), rep('grey', 60))
      size <- c(0.5, rep(0.35, 30), rep(0.2, 60))
      points(x, y, ylim=ylim, pch=16, cex=size[i], col=cols[i])
      }, silent=TRUE)
      if(class(result) == 'try-error') {
        print(i)
      }
    }
  }
  
#   legend('topright', legend=c(date, 'Prior week', 'Prior month', 'Prior Quarter', 'Spot Prices'), 
#          col=c('black', 'red', 'blue', 'grey', 'black'), pch=16)
  
  par(oldPar)
  
}




# TODO: [x] Rewrite the names section because it's long and tedious
# TODO: [x] Break apart the code to possibly have up to 12 functions to turn into a package
#
#       [x] Fix the timeseries to fit 1990-2020 (function createFuturesDataframes does this)
#       [x] Build NA vector for months without data (yearlyData function)
#       [ ] change xlim dates (ie. use min and max year and just display full year)    
#
# TODO: [ ] View other energy/commodoties to match the futures data with their corresponding 
# TODO:     spot prices. (Just hard code it).
# TODO: [ ] Figure out the units and ranges for each commodity and hard code that too.
# TODO: [ ] Create a databrowser for the futures data (individual contracts). Think abou the
# TODO:     possible selectors/radio/checkboxes that could be used



# List of possible energies to consider (maybe add more in the future)
# [X] Crude Oil                 Type: CL    Range of Data: M1983/Z2020*   Spot Price:       CME
# [ ] Gasoline                  Type: RB    Range of Data: F2006/Z2017    Spot Price:       CME
# [ ] Heating Oil               Type: HO    Range of Data: F1980/Z2018    Spot Price:       CME
# [ ] Natural Gas               Type: NG    Range of Data: M1990/Z2026    Spot Price:       CME
# [ ] Ethanol                   Type: EH    Range of Data: V2011/Z2017    Spot Price:       CME
# [ ] Gold                      Type: GC    Range of Data: G1975/Z2016*+  Spot Price:       CME
# [ ] Silver                    Type: SI    Range of Data: H1964/Z2018*+  Spot Price:       CME
# [ ] Palladium                 Type: PA    Range of Data: H1977/Z2015+   Spot Price:       CME
# [ ] Platinum                  Type: PL    Range of Data: F1970/V2015+   Spot Price:       CME
# [ ] Corn                      Type: C     Range of Data: H1960/Z2017+   Spot Price:       CME
# [ ] Oats                      Type: O     Range of Data: H1970/Z2016+   Spot Price:       CME
# [ ] Rough Rice                Type: RR    Range of Data: X1986/X2015*+  Spot Price:       CME
# [ ] Soybean Meal              Type: SM    Range of Data: F1964/Z2017+   Spot Price:       CME
# [ ] Soybean Oil               Type: BO    Range of Data: F1961/Z2017*+  Spot Price:       CME
# [ ] Soybeans                  Type: S     Range of Data: F1970/Z2016+   Spot Price:       CME 
# [ ] US Wheat                  Type: W     Range of Data: Z1959/N2017+   Spot Price:       CME
# [ ] Feeder Cattle             Type: FC    Range of Data: H1974/X2015*+  Spot Price:       CME
# [ ] Frozen Pork Bellies       Type: PB    Range of Data: Q1963/K2012*+  Spot Price:       CME
# [ ] Lean Hogs                 Type: LN    Range of Data: G1970/M2016+   Spot Price:       CME
# [ ] Live Cattle               Type: LC    Range of Data: J1965/M2016+   Spot Price:       CME
# [ ] Cocoa                     Type: CC    Range of Data: H1970/Z2016+   Spot Price:       ICE
# [ ] Coffee                    Type: KC    Range of Data: Z1973/U2020+   Spot Price:       ICE
# [ ] Cotton                    Type: CT    Range of Data: H1972/Z2017+   Spot Price:       ICE
# [ ] Lumber                    Type: LB    Range of Data: U1973/X2015+   Spot Price:       CME
# [ ] Orange Juice              Type: OJ    Range of Data: K1967/X2017*+  Spot Price:       ICE
# [ ] Sugar #11                 Type: SB    Range of Data: H1964/V2017+   Spot Price:       ICE


create10YearData <- function(commodityCode='CL', year) {
  
  settledf <- list()
  volumedf <- list()
  
  startYears <- seq(year,year+5,5)
  for (i in seq(length(startYears))) {  
    startYear <- startYears[i]
    endYear <- startYear + 5
    start <- paste0(startYear,'-01-01')
    end <- paste0(startYear+4,'-12-31')
    nameSettle <- paste0(commodityCode, '_Settle_',startYear,'_',endYear)
    nameVolume <- paste0(commodityCode, '_Volume_',startYear,'_',endYear)
    filePathSettle <- paste0(FUTURES_DATA_DIR,'/',nameSettle,'.RData')
    filePathVolume <- paste0(FUTURES_DATA_DIR,'/',nameVolume,'.RData')
    
    # Look for local versions of files before downloading more data
    if (!file.exists(filePathSettle) && !file.exists(filePathVolume)) {
      
      # Download data and convert to dataframe
      suppressMessages( dataList <- createFuturesDataframes(commodityCode,start,end) )
      
      # Pull Settle data out of dataList and save it
      assign(nameSettle, dataList$Settle)
      save(list=nameSettle, file=filePathSettle)
      
      # Pull Volume data out of dataList and save it
      assign(nameVolume, dataList$Volume)
      save(list=nameVolume, file=filePathVolume)
      
      settledf[[i]] <- dataList$Settle
      volumedf[[i]] <- dataList$Volume
      
    } else {
      
      settledf[[i]] <- get(load(filePathSettle))
      volumedf[[i]] <- get(load(filePathVolume))
    
    }
    
  }
  
  settledf <- dplyr::full_join(settledf[[1]], settledf[[2]], by='Date')
  volumedf <- dplyr::full_join(volumedf[[1]], volumedf[[2]], by='Date')
  
  return(list(Settle=settledf,
              Volume=volumedf))
}








# Create 5 year blocks of data. Save it. Load and combine them when necessary?

if (FALSE) {
  
  setDataDir('./development/HenryNguyen/data_local/')
  
  # Create several small datasets that can be 
  for (startYear in seq(1980,2020,5)) {
    
    endYear <- startYear + 5
    start <- paste0(startYear,'-01-01')
    end <- paste0(startYear+4,'-12-31')
    nameSettle <- paste0('CL_Settle_',startYear,'_',endYear)
    nameVolume <- paste0('CL_Volume_',startYear,'_',endYear)
    filePathSettle <- paste0(FUTURES_DATA_DIR,'/',nameSettle,'.RData')
    filePathVolume <- paste0(FUTURES_DATA_DIR,'/',nameVolume,'.RData')
    
    # Look for local versions of files before downloading more data
    if (!file.exists(filePathSettle) && !file.exists(filePathVolume)) {
      
      # Download data and convert to dataframe
      suppressMessages( dataList <- createFuturesDataframes('CL',start,end) )
      
      # Pull Settle data out of dataList and save it
      assign(nameSettle, dataList$Settle)
      save(list=nameSettle, file=filePathSettle)
      
      # Pull Volume data out of dataList and save it
      assign(nameVolume, dataList$Volume)
      save(list=nameVolume, file=filePathVolume)
      
    }
    
  }
  
  # Essentially only need to view 10 years at a time so only full_join once.
  if (save==TRUE) {
    df <- dplyr::full_join(CL_Settle_1980_1985, CL_Settle_1985_1990, by='Date')
    df <- dplyr::full_join(df, CL_Settle_1990_1995, by='Date')
    df <- dplyr::full_join(df, CL_Settle_1995_2000, by='Date')
    df <- dplyr::full_join(df, CL_Settle_2000_2005, by='Date')
    df <- dplyr::full_join(df, CL_Settle_2005_2010, by='Date')
    df <- dplyr::full_join(df, CL_Settle_2010_2015, by='Date')
    df <- dplyr::full_join(df, CL_Settle_2015_2020, by='Date')
    df <- dplyr::full_join(df, CL_Settle_2020_2025, by='Date')
    save(df, file=paste0(FUTURES_DATA_DIR,'/CL_Settle_1980_2025.RData')) 
  }
}









updateData <- function(commodityCode='CL', futuresDF) {
  
  lastRow <- futuresDF[nrow(futuresDF),]
  firstNonNA <- which(!is.na(lastRow[-1]))[1] + 1
  startContract <- names(lastRow[firstNonNA])
  startYear <- substr(startContract, 4, 7)
  
  # Need to change the end year? Never know when they start to include more contracts. It only ends at 2022 now.
  contracts <- contractNames(commodityCode, as.numeric(startYear), 2025)
  
  newData <- list()
  
  if (commodityCode %in% c('CC','KC','CT','OJ','SB')) {
    exchange <- 'ICE'
  } else {
    exchange <- 'CME'
  }  
  
  for (contract in contracts) {
    result <- try({
      rawFutures <- Quandl(paste0(exchange, '/', contract), sort='asc', type='raw', start_date=lastRow[[1]])[,c('Date','Settle')]
      datesDF <- data.frame(Date=seq(as.Date(rawFutures[1,'Date']), as.Date(rawFutures[nrow(rawFutures),'Date']), by="days"))
      dailyData <- dplyr::left_join(datesDF, rawFutures, by='Date')
      dailyData <- zoo::na.locf(dailyData, na.rm=TRUE)
      dailyData[,'Date'] <- as.Date(dailyData[,'Date'])
      dailyData[,2] <- as.numeric(dailyData[,2]) 
      newData[[contract]] <- dailyData
    }, silent=TRUE)
    if (class(result) == "try-error") {
      newData[[contract]] <- NA
    }
  }
  
  firstIndex <- min(which(!is.na(newData)))
  firstDF <- newData[[firstIndex]]
  firstDate <- firstDF$Date[1]
  
  lastIndex <- max(which(!is.na(newData)))
  lastDF <- newData[[lastIndex]]
  lastDate <- lastDF$Date[length(lastDF$Date)]
  
  dates <- seq(as.Date(firstDate), as.Date(lastDate), by='days')
  settleDF <- data.frame(Date=dates)
  
  for (contract in contracts) {
    dailyDF <- newData[[contract]]
    if ( is.na(dailyDF) ) {
      # If the dailyDF is missing, just fill with NA
      settleDF[,contract] <- NA
    } else {
      # Merge daily contract onto the allDays axis and add the contract column
      settleDF[,contract] <- dailyDF['Settle']
    }
  }
  
  # Replace the old futuresDF with the joined data. 
  futuresDF <- dplyr::full_join(futuresDF, settleDF, by=intersect(names(futuresDF),names(settleDF)))
  
  save(futuresDF, file=paste0(dataDir, 'CL_Settle.RData'))
}




