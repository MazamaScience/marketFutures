#################################################
# Data Utils for Market Futures Data
#
#
# Mazama Science
# Author: Henry Nguyen 
# January 2015
#################################################


# https://www.quandl.com/c/markets/crude-oil

library(Quandl)     # To use Quandl packages/datasets.
library(zoo)        # To use na.locf to fill in data.
library(drc)        # To use EXD.3() to plot an exponential fit.
library(lubridate)  # To use years().
library(dplyr)      # To use left_join() to merge data.

# Quandl Authentication Token 
token <- "8MsMk6Rkrm3dz3U5Fr4P"
Quandl.auth(token)

# Start: CME/CLM1983
# End:   CME/CLZ2020
# Month: FGHJKMNQUVXZ
# 7th column is Settle

# # Create an index for each month's individual contract
# monthIndex <- c('F', 'G', 'H', 'J', 'K', 'M', 'N', 'Q', 'U', 'V', 'X', 'Z')
# dir <- './development/HenryNguyen/data_local/'
# 
# # Download all of the individual contracts. 
# # Need to figure out how to organize it all and combine it.
# for (i in 1984:2020) {
#   if (i == 1983) {
#     for (j in 6:12) {
#       data <- Quandl(paste0('CME/CL', monthIndex[j], i), type='raw')
#       saveRDS(data, file=paste0('CL', monthIndex[j], i, '.rds'))
#     }
#   }
#   for (j in 1:12) {
#     data <- Quandl(paste0('CME/CL', monthIndex[j], i), type='raw')
#     saveRDS(data, file=paste0(dir, 'CL', monthIndex[j], i, '.rds'))
#   }
# }


# cleanData ----------------------------------------------------------------------------------------

# Download individual contract data, clean it up, and return a dataframe.
# * fileName        -- five character string consistenting of 
#                           a month index (F = January, G = February, and so on. FGHJKMNQUVXZ)
#                           %Y (see ?strftime for for date formatting)
cleanData <- function(fileName='F2010') {
  
  # Download the individual contract
  dir <- './development/HenryNguyen/data_local/'
  rawData <- readRDS(file=paste0(dir, 'CL', fileName, '.rds'))
  
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
  fullDates <- data.frame(Date=seq(as.Date('1983-03-30'), as.Date('2015-01-15'), by='days'))
  data <- dplyr::left_join(fullDates, data, by='Date')
  
  # Return the cleaned up dataframe.
  return(data)
}


# yearlySettleData ---------------------------------------------------------------------------------

# Download individual contract data, clean it up, and return a dataframe.
# * year              -- four character string $Y (see ?strftime for date formatting)
# * cleanData         -- pass the cleanData function
yearlySettleData <- function(year='2015', cleanData) {
  if (year == '1983') {
    m <- cleanData(paste0('M', year))
    n <- cleanData(paste0('N', year))
    q <- cleanData(paste0('Q', year))
    u <- cleanData(paste0('U', year))
    v <- cleanData(paste0('V', year))
    x <- cleanData(paste0('X', year))
    z <- cleanData(paste0('Z', year))
    
    data <- cbind(m[,7], n[,7], q[,7], u[,7], v[,7], x[,7], z[,7])
    return(data)
  }
  
  f <- cleanData(paste0('F', year))
  g <- cleanData(paste0('G', year))
  h <- cleanData(paste0('H', year))
  j <- cleanData(paste0('J', year))
  k <- cleanData(paste0('K', year))
  m <- cleanData(paste0('M', year))
  n <- cleanData(paste0('N', year))
  q <- cleanData(paste0('Q', year))
  u <- cleanData(paste0('U', year))
  v <- cleanData(paste0('V', year))
  x <- cleanData(paste0('X', year))
  z <- cleanData(paste0('Z', year))
  
  data <- cbind(f[,7], g[,7], h[,7], j[,7], k[,7], m[,7], n[,7], q[,7], u[,7], v[,7], x[,7], z[,7])
  return(data)
}

df1983 <- yearlySettleData('1983', cleanData)
df1984 <- yearlySettleData('1984', cleanData)
df1985 <- yearlySettleData('1985', cleanData)
df1986 <- yearlySettleData('1986', cleanData)
df1987 <- yearlySettleData('1987', cleanData)
df1988 <- yearlySettleData('1988', cleanData)
df1989 <- yearlySettleData('1989', cleanData)
df1990 <- yearlySettleData('1990', cleanData)
df1991 <- yearlySettleData('1991', cleanData)
df1992 <- yearlySettleData('1992', cleanData)
df1993 <- yearlySettleData('1993', cleanData)
df1994 <- yearlySettleData('1994', cleanData)
df1995 <- yearlySettleData('1995', cleanData)
df1996 <- yearlySettleData('1996', cleanData)
df1997 <- yearlySettleData('1997', cleanData)
df1998 <- yearlySettleData('1998', cleanData)
df1999 <- yearlySettleData('1999', cleanData)
df2000 <- yearlySettleData('2000', cleanData)
df2001 <- yearlySettleData('2001', cleanData)
df2002 <- yearlySettleData('2002', cleanData)
df2003 <- yearlySettleData('2003', cleanData)
df2004 <- yearlySettleData('2004', cleanData)
df2005 <- yearlySettleData('2005', cleanData)
df2006 <- yearlySettleData('2006', cleanData)
df2007 <- yearlySettleData('2007', cleanData)
df2008 <- yearlySettleData('2008', cleanData)
df2009 <- yearlySettleData('2009', cleanData)
df2010 <- yearlySettleData('2010', cleanData)
df2011 <- yearlySettleData('2011', cleanData)
df2012 <- yearlySettleData('2012', cleanData)
df2013 <- yearlySettleData('2013', cleanData)
df2014 <- yearlySettleData('2014', cleanData)
df2015 <- yearlySettleData('2015', cleanData)
df2016 <- yearlySettleData('2016', cleanData)
df2017 <- yearlySettleData('2017', cleanData)
df2018 <- yearlySettleData('2018', cleanData)
df2019 <- yearlySettleData('2019', cleanData)
df2020 <- yearlySettleData('2020', cleanData)


# Create a time series that ranges from through all of the 2010-2020 individual contracts
fullDates <- data.frame(Date=seq(as.Date('1983-03-30'), as.Date('2015-01-15'), by='days'))
data <- cbind(fullDates, df1983, df1984, df1985, df1986, df1987, df1988, df1989, df1990,
              df1991, df1992, df1993, df1994, df1995, df1996, df1997, df1998, df1999, df2000,
              df2001, df2002, df2003, df2004, df2005, df2006, df2007, df2008, df2009, df2010,
              df2011, df2012, df2013, df2014, df2015, df2016, df2017, df2018, df2019, df2020)


months <- c('F', 'G', 'H', 'J', 'K', 'M', 'N', 'Q', 'U', 'V', 'X', 'Z')
years <- 1983:2020
allMonths <- rep(months, length(years))
allYears <- rep(years, each=length(months))
labels <- paste0(allMonths, allYears)
labels <- labels[-(1:5)]
names(data) <- c('Date', labels)

data$Date <- seq(as.POSIXct('1983-03-30', tz='GMT'), as.POSIXct('2015-01-15', tz='GMT'), by='days')


#---------------------------------------------------------------------------------------------------


# Where do I put this? Include it with the download all data?
spotPriceData <- readRDS("CLSpotPrice.rds")


#---------------------------------------------------------------------------------------------------


# futuresPlot --------------------------------------------------------------------------------------

# Download individual contract data, clean it up, and return a dataframe.
# * spotDates       -- dataframe consisting of the time series
# * spotPrices      -- dataframe consisting of the spot prices corresponding to the spot dates
# * date            -- chosen date to display futures data (Must be between '1983-03-30' and '2015-01-15)
futuresPlot <- function(data=data, spotPriceData=spotPriceData, date='2015-01-01') {
  
  # Manipulate the spot price data to use for plotting
  spotDates <- CLSpotPriceData[,1]
  spotPrices <- na.locf(CLSpotPriceData[,2], na.rm=FALSE)
  
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
  
  # Add a grid
  abline(h=seq(0,160,20),col="gray70",lty="dotted")
  abline(v=fiveYearIntervals,col="gray70",lty="dotted")
  
  day <- as.POSIXct(date, tz='GMT')
  index <- which(data[,1] == day)
  y <- na.trim(as.numeric(data[index,-1]), sides='both')
  x <- seq(day, day + months(length(y) - 1), by='month')
  points(x, y, pch=16, cex=0.5, col='red') 
  
  par(oldPar)

}


# yearlyFuturesPlot --------------------------------------------------------------------------------------

# Download individual contract data, clean it up, and return a dataframe.
# * spotDates       -- dataframe consisting of the time series
# * spotPrices      -- dataframe consisting of the spot prices corresponding to the spot dates
# * date            -- five character string '%m-$d' (see ?strftime for date formatting)
yearlyFuturesPlot <- function(data=data, spotPriceData=spotPriceData, date='01-01') {
  
  # Manipulate the spot price data to use for plotting
  spotDates <- CLSpotPriceData[,1]
  spotPrices <- na.locf(CLSpotPriceData[,2], na.rm=FALSE)
  
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
  
  # Add a grid
  abline(h=seq(0,160,20),col="gray70",lty="dotted")
  abline(v=fiveYearIntervals,col="gray70",lty="dotted")
  
  for (i in 1984:2015) {
    day <- as.POSIXct(paste0(i, '-', date), tz='GMT')
    index <- which(data[,1] == day)
    y <- na.trim(as.numeric(data[index,-1]), sides='left')
    x <- seq(day, day + months(length(y) - 1), by='month')
    points(x, y, pch=16, ylim=c(0, 160), cex=0.5, col='red')  
  }
  
}


















# Attempt at graphing the NG data. 

# # Quandl Authentication Token 
# token <- "8MsMk6Rkrm3dz3U5Fr4P"
# Quandl.auth(token)
# 
# # Should I be downloading the Historical Data from Stevens or the alternative source (Chris)?
# # We want to look at the "Settle" and the "Volume column". Other values aren't very interesting.
# # CL1 represents 1 month futures data, CL2 represents 2 month futures data, and so on.
# 
# # Download the CME NYMEX WTI Crude Oil Futures (CL) data from the "All Continous Contracts"
# CL1data <- Quandl("SCF/CME_CL1_FW", type="raw")
# CL2data <- Quandl("SCF/CME_CL2_FW", type="raw")
# CL3data <- Quandl("SCF/CME_CL3_FW", type="raw")
# CL4data <- Quandl("SCF/CME_CL4_FW", type="raw")
# CL5data <- Quandl("SCF/CME_CL5_FW", type="raw")
# CL6data <- Quandl("SCF/CME_CL6_FW", type="raw")
# CL7data <- Quandl("SCF/CME_CL7_FW", type="raw")
# CL8data <- Quandl("SCF/CME_CL8_FW", type="raw")
# CL9data <- Quandl("SCF/CME_CL9_FW", type="raw")
# 
# # Download the WTI Crude Oil Spot Price Cushing
# CLSpotPriceData <- Quandl("DOE/RWTC", type="raw")
# 
# # To see what class each column of the data is.
# for (name in names(CL1data)) {
#   print(class(CL1data[[name]]))
# }
# 
# # Create an empty data frame with all of the 'dates' from the start to end. 
# # I want to keep the 'type' for the date to be the same because na.locf/merge messes it up.
# empty <- data.frame(Date=seq(as.Date("1983-03-31"), as.Date("2015-01-10"), by="days"))
# 
# # Merge all of the data from the futures data so that we have a full time series.
# # Repeat process for the other future data. (ie. CL2, CL3, and so on)
# CL1 <- merge(CL1data, empty, by.x='Date', by.y='Date', all.x=T, all.y=T)
# CL2 <- merge(CL2data, empty, by.x='Date', by.y='Date', all.x=T, all.y=T)
# CL3 <- merge(CL3data, empty, by.x='Date', by.y='Date', all.x=T, all.y=T)
# CL4 <- merge(CL4data, empty, by.x='Date', by.y='Date', all.x=T, all.y=T)
# CL5 <- merge(CL5data, empty, by.x='Date', by.y='Date', all.x=T, all.y=T)
# CL5 <- merge(CL5data, empty, by.x='Date', by.y='Date', all.x=T, all.y=T)
# CL6 <- merge(CL6data, empty, by.x='Date', by.y='Date', all.x=T, all.y=T)
# CL7 <- merge(CL7data, empty, by.x='Date', by.y='Date', all.x=T, all.y=T)
# CL8 <- merge(CL8data, empty, by.x='Date', by.y='Date', all.x=T, all.y=T)
# CL9 <- merge(CL9data, empty, by.x='Date', by.y='Date', all.x=T, all.y=T)
# 
# # Create a POSIXct time series that will be used for all of the dataframes in order to maintain consistency.
# dates <- seq(as.POSIXct("1983-03-31", tz="GMT"), as.POSIXct("2015-01-10", tz="GMT"), by="days")
# CL1[,1] <- dates
# CL2[,1] <- dates
# CL3[,1] <- dates
# CL4[,1] <- dates
# CL5[,1] <- dates
# CL6[,1] <- dates
# CL7[,1] <- dates
# CL8[,1] <- dates
# CL9[,1] <- dates
# 
# # Save all of the data so that we don't have to download it again everytime we run the code.
# saveRDS(CL1, file="CL1.rds")
# saveRDS(CL2, file="CL2.rds")
# saveRDS(CL3, file="CL3.rds")
# saveRDS(CL4, file="CL4.rds")
# saveRDS(CL5, file="CL5.rds")
# saveRDS(CL6, file="CL6.rds")
# saveRDS(CL7, file="CL7.rds")
# saveRDS(CL8, file="CL8.rds")
# saveRDS(CL9, file="CL9.rds")
# saveRDS(CLSpotPrice, file="CLSpotPrice.rds")

# Applying the code to Natural Gas. 
com <- 'NG'

# Load data from the last session. It already includes data from 1983-2014 (Part of 01/2015)
F1 <- readRDS(paste0(com, "1.rds"))
F2 <- readRDS(paste0(com, "2.rds"))
F3 <- readRDS(paste0(com, "3.rds"))
F4 <- readRDS(paste0(com, "4.rds"))
F5 <- readRDS(paste0(com, "5.rds"))
F6 <- readRDS(paste0(com, "6.rds"))
F7 <- readRDS(paste0(com, "7.rds"))
F8 <- readRDS(paste0(com, "8.rds"))
F9 <- readRDS(paste0(com, "9.rds"))
spotPriceData <- readRDS(paste0(com, "SpotPrice.rds"))

# Combine all of the 'settle' and 'volume' data from the continuous contracts into one dataframe.
settleData <- cbind(F1[,5], F2[,5], F3[,5], F4[,5], F5[,5], F6[,5], F7[,5], F8[,5], F9[,5])
volumeData <- cbind(F1[,6], F2[,6], F3[,6], F4[,6], F5[,6], F6[,6], F7[,6], F8[,6], F9[,6])

# Fill in all of the NA's because there are lots of non-trading days.
settleData <- na.locf(settleData, na.rm=TRUE)
volumeData <- na.locf(volumeData, na.rm=TRUE)

# Create a POSIXct time series that matches our data. na.locf() command ruins it.
# Build dataframe with timeseries and data.
dates <- seq(F1$Date[1], F1$Date[nrow(F1)], by="days")
settleData <- data.frame(dates, settleData)
volumeData <- data.frame(dates, volumeData)

# Change the column names to make the dataframe look more appealing and easy to read.
names(settleData) <- c("Date", "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9")
names(volumeData) <- c("Date", "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9")

# Spot prices
spotDates <- spotPriceData[,1]
spotPrices <- na.locf(spotPriceData[,2], na.rm=FALSE)

# More space on top for comments
oldPar <- par()
par(mar=c(5,4,6,2)+.1)

# Plot the Spot Prices as well as today's 9 month futures data.
xlim <- c(as.POSIXct("1990-01-01", tz='GMT'), as.POSIXct("2020-02-01", tz='GMT'))
plot(spotDates, spotPrices, type='p', las=1,
     ylab='Settlement Price', ylim=c(0, 16), xlim=xlim,
     pch=16,cex=0.5,
     xlab='',axes=FALSE)
box()

fiveYearIntervals <- seq(as.POSIXct("1990-01-01", tz='GMT'), as.POSIXct("2020-01-01", tz='GMT'), by='5 years')
axis(1, at=fiveYearIntervals, labels=seq(1990,2020,5))
axis(2, at=seq(0,16,2),labels=paste0('$',seq(0,16,2)),las=1)
title('Futures Chain Projections',line=3)
mtext('Data: quandl.com | Graphic: mazamascience.com',side=1,line=3)

# Add a grid
abline(h=seq(0,16,2),col="gray70",lty="dotted")
abline(v=fiveYearIntervals,col="gray70",lty="dotted")

# Create futures chain dates starting on the 25'th, a few days after last possible settlement
targetDates <- seq(as.POSIXct("1990-12-25",tz="GMT"),as.POSIXct("2014-12-25",tz="GMT"),by="3 months")

for (i in seq(length(targetDates))) {
  
  targetDate <- targetDates[i]
  
  # Use the drm and EXD.3 command to create an exponential fit for each futures data.
  # x is an index counter. y is the dates which we want to use (our 9 month futures contracts)
  x <- 1:9
  y <- settleData[which(settleData$Date == targetDate),-1]
  y <- as.numeric(y)
  result <- try( fit <- drc::drm(y[-1] ~ x[-1], fct=EXD.3()) )
  if (class(result) == 'try-error') {
    print(paste0('failed at ', i))
  }
  
  # Grab the coefficients from the exponential fit (they have really bad names).
  # Function: f(x) = c + (d-c) * exp(-x/e)
  c <- fit$coefficients[1]
  d <- fit$coefficients[2]
  e <- fit$coefficients[3]
  
  # Create a POSIXct time series for the extended time period to plot our fit.
  time <- seq(targetDate, targetDate + lubridate::years(6), by='month')
  index <- seq(1:length(time))
  
  # Find the values corresponding to the dates using the exponential fit and plot it.
  futuresChainModel <- c + (d-c) * exp(-index / e)
  lines(time, futuresChainModel, col=adjustcolor('salmon',0.2) , lwd=8)
  
  # Plot the 9 month futures data for each year.
  x <- seq(targetDate, targetDate + months(8), by='month')
  y <- settleData[which(settleData$Date == targetDate),-1]
  points(x,y,pch=16,cex=0.5, col='red')
  
}

par(oldPar)



