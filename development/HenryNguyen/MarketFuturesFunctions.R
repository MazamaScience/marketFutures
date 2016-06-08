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

library(Quandl)     # To use Quandl packages/datasets.
library(zoo)        # To use na.locf to fill in data.
###library(drc)        # To use EXD.3() to plot an exponential fit.
library(lubridate)  # To use years().
library(dplyr)      # To use left_join() to merge data.

# Table of Contents --------------------------------------------------------------------------------
# [x] cleanData 
# [x] yearlyData
# [x] downloadSpotPrice
# [ ] downloadData
# [x] futuresPlot <- spotPricePlot
# [x] yearlyFuturesPlot <- futuresChainPlot


# ------------------Changes needed to be made by the user once the package goes public -------------
# Quandl Authentication Token 
token <- "8MsMk6Rkrm3dz3U5Fr4P"
Quandl.auth(token)
dir <- './development/HenryNguyen/data_local/'
# --------------------------------------------------------------------------------------------------


# cleanData ----------------------------------------------------------------------------------------

# Download individual contract data, clean it up, and return a dataframe.
# * fileName        -- five character string consistenting of 
#                           a month index (F = January, G = February, and so on. FGHJKMNQUVXZ)
#                           %Y (see ?strftime for for date formatting)
cleanData <- function(dir='./development/HenryNguyen/data_local/', fileName='CLF2010') {
  
  # Download the individual contract
  rawData <- readRDS(file=paste0(dir, fileName, '.rds'))
  
  # Create an empty data frame with all of the 'dates' from the start to end. 
  dates <- data.frame(Date=seq(as.Date(rawData[nrow(rawData),'Date']), as.Date(rawData[1,'Date']), by="days"))
  
  # Use dplyr's left_join command to merge the data.
  data <- dplyr::left_join(dates, rawData, by='Date')
  
  # Use zoo's na.locf command to fill in data for the non-trading days.
  data <- zoo::na.locf(data, na.rm=TRUE)
  
  # The na.locf command ruins the time series as well as changes the other values to 'characters'.
  for (i in 2:9) {
    data[,i] <- as.numeric(data[,i])  
  }
  data[,1] <- dates 
  
  # Merge the data with the full date range from 2010 to 2020 individual contracts.
  fullDates <- data.frame(Date=seq(as.Date('1983-03-30'), as.Date('2015-01-22'), by='days'))
  data <- dplyr::left_join(fullDates, data, by='Date')
  
  # Return the cleaned up dataframe.
  return(data)
}


# Not sure if I want to leave it as yearlyData or change it to yearlySettleData
# yearlyData ---------------------------------------------------------------------------------------

# Download individual contract data, clean it up, and return a dataframe.
# * year              -- four character string $Y (see ?strftime for date formatting)
# * cleanData         -- pass the cleanData function
yearlyData <- function(comm='crudeOil', dir='./development/HenryNguyen/data_local/',  year='2015', cleanData) {
  
  # Create a dictionary for commodities
  dict <- list('crudeOil'='CL', 'gasoline'='RB', 'heatingOil'='HO', 'naturalGas'='NG', 'ethanol'='EH',
               'gold'='GC', 'silver'='SI', 'platinum'='PL', 'palladium'='PA', 'corn'='C', 'oats'='O', 
               'roughRice'='RR', 'soybeanMeal'='SM', 'soybeanOil'='BO', 'wheat'='W', 'feederCattle'='FC',
               'porkBelly'='PB', 'leanHogs'='LN', 'liveCattle'='LC', 'cocoa'='CC', 'coffee'='KC', 
               'cotton'='CT', 'lumber'='LB', 'orangeJuice'='OJ', 'sugar11'='SB')
  comm <- dict[comm]
  
  # Create a index for the individual contracts to be downloaded. 
  months <- c('F', 'G', 'H', 'J', 'K', 'M', 'N', 'Q', 'U', 'V', 'X', 'Z')
  years <- rep(year, 12)
  contracts <- paste0(comm, months, years)
  
  # Change the number of times NA is repeated based on the length of the time series.
  empty <- data.frame(rep(as.numeric(NA), 11622))
  df <- data.frame(rep(empty, 12))
  
  # How do I remove the warning messages? I tried doing option(warn = x)
  for (i in 1:length(contracts)) {
    result <- try(x <- cleanData(dir=dir, fileName=contracts[i]), silent=TRUE)
    if (class(result) == 'try-error') {
      print(paste0('The contract ', contracts[i], ' does not exist.'))
      df[,i] <- empty
      next
    }
    
    df[,i] <- x['Settle']
    
  }
      
  return(df)
}


# downloadSpotPrice --------------------------------------------------------------------------------

# Downloads and saves the spot price data for the specified commodity
# * comm            -- commodity (ie. 'crudeOil', 'naturalGas', etc.)
# * dir             -- directory where the data will be saved.
downloadSpotPrice <- function(comm='crudeOil', dir='./development/HenryNguyen/data_local/', save=FALSE) {
  
  # Create a dictionary for commodities
  dict <- list('crudeOil'='CL', 'gasoline'='RB', 'heatingOil'='HO', 'naturalGas'='NG', 'ethanol'='EH',
               'gold'='GC', 'silver'='SI', 'platinum'='PL', 'palladium'='PA', 'corn'='C', 'oats'='O', 
               'roughRice'='RR', 'soybeanMeal'='SM', 'soybeanOil'='BO', 'wheat'='W', 'feederCattle'='FC',
               'porkBelly'='PB', 'leanHogs'='LN', 'liveCattle'='LC', 'cocoa'='CC', 'coffee'='KC', 
               'cotton'='CT', 'lumber'='LB', 'orangeJuice'='OJ', 'sugar11'='SB')
  comm <- dict[comm]
  
  if (save==TRUE) {
    
    data <- Quandl('DOE/RWTC', type='raw', sort='asc')
    
    # Make this more generic for other spot price data. For some reason it keeps giving me 16:00:00 
    # for time instead of 00:00:00 when I use data[1,1] and data[nrow(data),1] with POSIXct.
    dates <- data.frame(Date=seq(as.Date('1986-01-02'), as.Date('2015-01-22'), by='day'))
    data <- dplyr::left_join(dates, data, by='Date')
    
    data <- zoo::na.locf(data, na.rm=TRUE)
    data <- as.numeric(data[,2])
      
    dates <- data.frame(Date=seq(as.POSIXct('1986-01-02', tz='GMT'), as.POSIXct('2015-01-22', tz='GMT'), by='day'))
    data <- data.frame(Dates=dates, Settle=data)
    
    # Save the spot price data to the specified directory.
    saveRDS(data, file=paste0(dir, comm, 'SpotPriceData.rds'))
    
  }
  
  data <- readRDS(file=paste0(dir, comm, 'SpotPriceData.rds'))
  
  return(data)
}


# downloadData -------------------------------------------------------------------------------------

# Downloads all of the individual contracts for a specified commodity. 
# * comm            -- commodity (ie. 'crudeOil', 'naturalGas', etc.)
# * dir             -- directory where the data will be saved.
# * save            -- the option to save all of the individual contracts.
downloadData <- function(comm='crudeOil', dir='./development/HenryNguyen/data_local/', save=FALSE) {
  
  # Create a dictionary for commodities
  dict <- list('crudeOil'='CL', 'gasoline'='RB', 'heatingOil'='HO', 'naturalGas'='NG', 'ethanol'='EH',
              'gold'='GC', 'silver'='SI', 'platinum'='PL', 'palladium'='PA', 'corn'='C', 'oats'='O', 
              'roughRice'='RR', 'soybeanMeal'='SM', 'soybeanOil'='BO', 'wheat'='W', 'feederCattle'='FC',
              'porkBelly'='PB', 'leanHogs'='LN', 'liveCattle'='LC', 'cocoa'='CC', 'coffee'='KC', 
              'cotton'='CT', 'lumber'='LB', 'orangeJuice'='OJ', 'sugar11'='SB')
  comm <- dict[comm]
  
  # Create a index for the individual contracts to be downloaded. 
  months <- c('F', 'G', 'H', 'J', 'K', 'M', 'N', 'Q', 'U', 'V', 'X', 'Z')
  years <- 1983:2020
  allMonths <- rep(months, length(years))
  allYears <- rep(years, each=length(months))
  contracts <- paste0(comm, allMonths, allYears)
  
  if (save==TRUE) {
    # The type of Exchange that the commodity is under. Everything is under CME except cocoa, coffee,
    # cotton, orange juice, and sugar #11. 
    ex <- 'CME'
    if (comm == 'cocoa' || comm=='coffee' || comm=='cotton' || comm=='orangeJuice' || comm=='sugar11') {
      ex <- 'ICE'
    }
    
    # Download all of the individual contracts.
    for (i in 1:length(contracts)) {
      result <- try(contractData <- Quandl(paste0(ex, '/', contracts[i]), type='raw'), silent=TRUE)
      if (class(result) == 'try-error') {
        options(warn=-2)
        print(paste0('The contract ', contracts[i], ' does not exist.'))
        next
      }
      saveRDS(contractData, file=paste0(dir, contracts[i], '.rds'))
    }
  }
  
  df1983 <- yearlyData('crudeOil', dir, '1983', cleanData)
  df1984 <- yearlyData('crudeOil', dir, '1984', cleanData)
  df1985 <- yearlyData('crudeOil', dir, '1985', cleanData)
  df1986 <- yearlyData('crudeOil', dir, '1986', cleanData)
  df1987 <- yearlyData('crudeOil', dir, '1987', cleanData)
  df1988 <- yearlyData('crudeOil', dir, '1988', cleanData)
  df1989 <- yearlyData('crudeOil', dir, '1989', cleanData)
  df1990 <- yearlyData('crudeOil', dir, '1990', cleanData)
  df1991 <- yearlyData('crudeOil', dir, '1991', cleanData)
  df1992 <- yearlyData('crudeOil', dir, '1992', cleanData)
  df1993 <- yearlyData('crudeOil', dir, '1993', cleanData)
  df1994 <- yearlyData('crudeOil', dir, '1994', cleanData)
  df1995 <- yearlyData('crudeOil', dir, '1995', cleanData)
  df1996 <- yearlyData('crudeOil', dir, '1996', cleanData)
  df1997 <- yearlyData('crudeOil', dir, '1997', cleanData)
  df1998 <- yearlyData('crudeOil', dir, '1998', cleanData)
  df1999 <- yearlyData('crudeOil', dir, '1999', cleanData)
  df2000 <- yearlyData('crudeOil', dir, '2000', cleanData)
  df2001 <- yearlyData('crudeOil', dir, '2001', cleanData)
  df2002 <- yearlyData('crudeOil', dir, '2002', cleanData)
  df2003 <- yearlyData('crudeOil', dir, '2003', cleanData)
  df2004 <- yearlyData('crudeOil', dir, '2004', cleanData)
  df2005 <- yearlyData('crudeOil', dir, '2005', cleanData)
  df2006 <- yearlyData('crudeOil', dir, '2006', cleanData)
  df2007 <- yearlyData('crudeOil', dir, '2007', cleanData)
  df2008 <- yearlyData('crudeOil', dir, '2008', cleanData)
  df2009 <- yearlyData('crudeOil', dir, '2009', cleanData)
  df2010 <- yearlyData('crudeOil', dir, '2010', cleanData)
  df2011 <- yearlyData('crudeOil', dir, '2011', cleanData)
  df2012 <- yearlyData('crudeOil', dir, '2012', cleanData)
  df2013 <- yearlyData('crudeOil', dir, '2013', cleanData)
  df2014 <- yearlyData('crudeOil', dir, '2014', cleanData)
  df2015 <- yearlyData('crudeOil', dir, '2015', cleanData)
  df2016 <- yearlyData('crudeOil', dir, '2016', cleanData)
  df2017 <- yearlyData('crudeOil', dir, '2017', cleanData)
  df2018 <- yearlyData('crudeOil', dir, '2018', cleanData)
  df2019 <- yearlyData('crudeOil', dir, '2019', cleanData)
  df2020 <- yearlyData('crudeOil', dir, '2020', cleanData)
  
  # Create a time series that ranges from through all of the 2010-2020 individual contracts
  fullDates <- data.frame(Date=seq(as.Date('1983-03-30'), as.Date('2015-01-22'), by='days'))
  data <- cbind(fullDates, df1983, df1984, df1985, df1986, df1987, df1988, df1989, df1990,
                df1991, df1992, df1993, df1994, df1995, df1996, df1997, df1998, df1999, df2000,
                df2001, df2002, df2003, df2004, df2005, df2006, df2007, df2008, df2009, df2010,
                df2011, df2012, df2013, df2014, df2015, df2016, df2017, df2018, df2019, df2020)
  
  # Relabel the column names correctly.
  labels <- paste0(allMonths, allYears)
  names(data) <- c('Date', labels)
  
  # Change the timeseries column into a POSIXct.
  data$Date <- seq(as.POSIXct('1983-03-30', tz='GMT'), as.POSIXct('2015-01-22', tz='GMT'), by='days')
  
  return(data)
}


# futuresPlot --------------------------------------------------------------------------------------

# Download individual contract data, clean it up, and return a dataframe.
# * spotPriceData      -- dataframe consisting of the spot prices 
futuresPlot <- function(data=data, spotPriceData=spotPriceData, date='01-01', show=TRUE) {
  
  spotDates <- spotPriceData[,1]
  spotPrices <- spotPriceData[,2]
  
  # More space on top for comments
  oldPar <- par()
  par(mar=c(5,4,6,2)+.1)
  
  # Plot the Spot Prices as well as today's 9 month futures data.
  xlim <- c(as.POSIXct("1983-01-01", tz='GMT'), as.POSIXct("2020-02-01", tz='GMT'))
  plot(spotDates, spotPrices, type='p', las=1,
       ylab='Settlement Price', ylim=c(0, 160), xlim=xlim,
       pch=16,cex=0.5,
       xlab='',axes=FALSE)
  box()
  
  fiveYearIntervals <- seq(as.POSIXct("1985-01-01", tz='GMT'), as.POSIXct("2020-01-01", tz='GMT'), by='5 years')
  axis(1, at=fiveYearIntervals, labels=seq(1985,2020,5))
  axis(2, at=seq(0,160,20),labels=paste0('$',seq(0,160,20)),las=1)
  title('Futures Chain Projections',line=3)
  mtext('Data: quandl.com | Graphic: mazamascience.com',side=1,line=3)
  
  # Add a grid
  abline(h=seq(0,160,20),col="gray70",lty="dotted")
  abline(v=fiveYearIntervals,col="gray70",lty="dotted")
  
  if (show == TRUE) {
    for (i in 1983:2015) {
      if (as.POSIXct(paste0(i, '-', date), tz='GMT') < as.POSIXct('1983-03-30', tz='GMT') ||
            as.POSIXct(paste0(i, '-', date), tz='GMT') > as.POSIXct('2015-01-22', tz='GMT')) {
        next
      }
      day <- as.POSIXct(paste0(i, '-', date), tz='GMT')
      index <- which(data[,1] == day)
      y <- na.trim(as.numeric(data[index,-1]), sides='both')
      x <- seq(day, day + months(length(y) - 1), by='month')
      points(x, y, pch=16, ylim=c(0, 160), cex=0.5, col='red')  
    }
  }
  
  par(oldPar)
  
}


# To get the most recent data. Run through all of the datasets and grab newest row?
# Then cbind them together and rbind it with the full dataframe?
# Need to make a new mini time series to account for the non-trading days.
# Figure out if there's a specifc date that a new contract is introduced (new column)
# function <- arguments: 
# x <- Quandl(fileName, rows = 1)


# TODO: [x] Rewrite the names section because it's long and tedious
# TODO: [ ] Break apart the code to possibly have up to 12 functions to turn into a package
#
#       [ ] Fix the fullDates timeseries in cleanData/downloadSpotData to fit 1990-2020? 
#       [ ] Build NA vector for months without data (yearlyData function)
#       [ ] change xlim dates (ie. use min and max year and just display full year)    
#       [ ] Make list of packages needed / include their quandl token?
#
# TODO: [ ] View other energy/commodoties to match the futures data with their corresponding 
# TODO:     spot prices. (Just hard code it).
# TODO: [ ] Figure out the units and ranges for each commodity and hard code that too.
# TODO: [ ] Write a function/code to access Quandl to append new data to the end of the dataframe
# TODO: [ ] Create a databrowser for the futures data (individual contracts). Think abou the
# TODO:     possible selectors/radio/checkboxes that could be used


# Functions for package:
# token argument
# Save/Load
# install data (ie. dir, crude oil, range)
### downloadData(com='crudeOil')                      # Download all of the data + spotPrices?
# try() for finding year data starts on?
# use sort='asc', start_date/end_date for Quandl()
### updateData(x, com='crudeOil')           
### downloadSpotData(com='crudeOil')
#
# cleanData(fileName='F2015')
# yearlyData(year='2015', cleanData)
# futuresPlot(data=data, spotPriceData, date='2015-01-01')
# yearlyFuturesPlot(data=data, spotPriceData, date='01-01')

# [x] Change the argument for futuresPlot/yearlyFuturesPlot to only take the 
#     dataframe returned from downloadSpotData and split it up in the function.
### Maybe include argument for directory name?

### If I make a function to download all of the data, makes it pointless to have a 
### function to clean the data and append it all? Could just include it all in one
### function.


# Do I make a huge list of varoius criterias for data for each category?
# (ie. a large string to tell us which files to download?)
# * = Missing data between months.
# + = Irregular individual contracts (ie. only 6 months of the year)

# Maybe set a range like 1990-2020. But also implement a tryblock to download 
# empty NA's. etc.

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




















