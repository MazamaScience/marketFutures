#################################################
# Data Utils for Market Futures Data
#
#
# Mazama Science
# Author: Henry Nguyen 
# January 2015
#################################################


# https://www.quandl.com/c/markets/crude-oil

library(Quandl)     # To use Quandl packages/datasets
library(zoo)        # To use na.locf to fill in data
library(drc)        # To use EXD.3() to plot an exponential fit.
library(lubridate)  # To use years()

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

############################################################
############################################################
# Jon starts here
############################################################
############################################################


# Load data from the last session. It already includes data from 1983-2014 (Part of 01/2015)
CL1 <- readRDS("CL1.rds")
CL2 <- readRDS("CL2.rds")
CL3 <- readRDS("CL3.rds")
CL4 <- readRDS("CL4.rds")
CL5 <- readRDS("CL5.rds")
CL6 <- readRDS("CL6.rds")
CL7 <- readRDS("CL7.rds")
CL8 <- readRDS("CL8.rds")
CL9 <- readRDS("CL9.rds")
CLSpotPriceData <- readRDS("CLSpotPrice.rds")

# Combine all of the 'settle' and 'volume' data from the continuous contracts into one dataframe.
settleData <- cbind(CL1[,5], CL2[,5], CL3[,5], CL4[,5], CL5[,5], CL6[,5], CL7[,5], CL8[,5], CL9[,5])
volumeData <- cbind(CL1[,6], CL2[,6], CL3[,6], CL4[,6], CL5[,6], CL6[,6], CL7[,6], CL8[,6], CL9[,6])

# Fill in all of the NA's because there are lots of non-trading days.
settleData <- na.locf(settleData, na.rm=TRUE)
volumeData <- na.locf(volumeData, na.rm=TRUE)

# Create a POSIXct time series that matches our data. na.locf() command ruins it.
# Build dataframe with timeseries and data.
dates <- seq(CL1$Date[1], CL1$Date[nrow(CL1)], by="days")
settleData <- data.frame(dates, settleData)
volumeData <- data.frame(dates, volumeData)

# Change the column names to make the dataframe look more appealing and easy to read.
names(settleData) <- c("Date", "CL1", "CL2", "CL3", "CL4", "CL5", "CL6", "CL7", "CL8", "CL9")
names(volumeData) <- c("Date", "CL1", "CL2", "CL3", "CL4", "CL5", "CL6", "CL7", "CL8", "CL9")

# Spot prices
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
title('Crude Oil Futures Chain Projections',line=3)
mtext('Every 3 months, the CL futures chain is fit to an exponential to see what "right price" it would "decay" to.',side=3,line=1)
text(as.POSIXct("1986-01-05",tz="GMT"),60,'From 1986 to 2005 the "right price" was $20/bbl.',pos=4)
text(as.POSIXct("2004-01-05",tz="GMT"),155,'From 2005 to 2011 there was no "right price".',pos=4)
text(as.POSIXct("2011-01-05",tz="GMT"),120,'Is $90/bbl the new "right price"?',pos=4)
mtext('Data: quandl.com | Graphic: mazamascience.com',side=1,line=3)

# Add a grid
abline(h=seq(0,160,20),col="gray70",lty="dotted")
abline(v=fiveYearIntervals,col="gray70",lty="dotted")


library(lubridate) # 

# Create futures chain dates starting on the 25'th, a few days after last possible settlement
targetDates <- seq(as.POSIXct("1983-12-25",tz="GMT"),as.POSIXct("2014-12-25",tz="GMT"),by="3 months")

for (i in seq(length(targetDates))) {
  
  targetDate <- targetDates[i]
  
  # Use the drm and EXD.3 command to create an exponential fit for each futures data.
  # x is an index counter. y is the dates which we want to use (our 9 month futures contracts)
  x <- 1:9
  y <- settleData[which(settleData$Date == targetDate),-1]
  y <- as.numeric(y)
  fit <- drc::drm(y[-1] ~ x[-1], fct=EXD.3()) # Test removing the first point to see if that improves the fit.
  
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

