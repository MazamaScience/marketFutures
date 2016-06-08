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

# Quandl Authentication Token 
token <- "8MsMk6Rkrm3dz3U5Fr4P"
Quandl.auth(token)

# Should I be downloading the Historical Data from Stevens or the alternative source (Chris)?
# We want to look at the "Settle" and the "Volume column". Other values aren't very interesting.
# CL1 represents 1 month futures data, CL2 represents 2 month futures data, and so on.

# Download the CME NYMEX WTI Crude Oil Futures (CL) data from the "All Continous Contracts"
CL1data <- Quandl("SCF/CME_CL1_FW", type="raw")
CL2data <- Quandl("SCF/CME_CL2_FW", type="raw")
CL3data <- Quandl("SCF/CME_CL3_FW", type="raw")
CL4data <- Quandl("SCF/CME_CL4_FW", type="raw")
CL5data <- Quandl("SCF/CME_CL5_FW", type="raw")
CL6data <- Quandl("SCF/CME_CL6_FW", type="raw")
CL7data <- Quandl("SCF/CME_CL7_FW", type="raw")
CL8data <- Quandl("SCF/CME_CL8_FW", type="raw")
CL9data <- Quandl("SCF/CME_CL9_FW", type="raw")

# Download the WTI Crude Oil Spot Price Cushing
CLSpotPriceData <- Quandl("DOE/RWTC", type="raw")

# To see what class each column of the data is.
for (name in names(CL1data)) {
  print(class(CL1data[[name]]))
}

# Create an empty data frame with all of the 'dates' from the start to end. 
# I want to keep the 'type' for the date to be the same because na.locf/merge messes it up.
empty <- data.frame(Date=seq(as.Date("1983-03-31"), as.Date("2015-01-14"), by="days"))

# Merge all of the data from the futures data so that we have a full time series.
# Repeat process for the other future data. (ie. CL2, CL3, and so on)
CL1 <- merge(CL1data, empty, by.x='Date', by.y='Date', all.x=T, all.y=T)
CL2 <- merge(CL2data, empty, by.x='Date', by.y='Date', all.x=T, all.y=T)
CL3 <- merge(CL3data, empty, by.x='Date', by.y='Date', all.x=T, all.y=T)
CL4 <- merge(CL4data, empty, by.x='Date', by.y='Date', all.x=T, all.y=T)
CL5 <- merge(CL5data, empty, by.x='Date', by.y='Date', all.x=T, all.y=T)
CL5 <- merge(CL5data, empty, by.x='Date', by.y='Date', all.x=T, all.y=T)
CL6 <- merge(CL6data, empty, by.x='Date', by.y='Date', all.x=T, all.y=T)
CL7 <- merge(CL7data, empty, by.x='Date', by.y='Date', all.x=T, all.y=T)
CL8 <- merge(CL8data, empty, by.x='Date', by.y='Date', all.x=T, all.y=T)
CL9 <- merge(CL9data, empty, by.x='Date', by.y='Date', all.x=T, all.y=T)

# Create a POSIXct time series that will be used for all of the dataframes in order to maintain consistency.
dates <- seq(as.POSIXct("1983-03-31", tz="GMT"), as.POSIXct("2015-01-14", tz="GMT"), by="days")
CL1[,1] <- dates
CL2[,1] <- dates
CL3[,1] <- dates
CL4[,1] <- dates
CL5[,1] <- dates
CL6[,1] <- dates
CL7[,1] <- dates
CL8[,1] <- dates
CL9[,1] <- dates

# Clean up the Crude Oil Spot Price data. 
# 1. Merge with empty time series.
# 2. Use na.locf() to fill in missing days with the last data.
# 4. Create a POSIXct time series to match our data.
# 3. Build a dataframe with a new POSIXct time series since na.locf() messes it up.
CLSpotPrice <- merge(CLSpotPriceData, empty, by.x='Date', by.y='Date', all.x=T, all.y=T)
CLSpotPrice <- na.locf(CLSpotPrice[,2], na.rm=FALSE)
dates <- seq(as.POSIXct("1983-03-31", tz="GMT"), as.POSIXct("2015-01-14", tz="GMT"), by="days")
CLSpotPrice <- data.frame(dates, spotPrice=CLSpotPrice)

# Save all of the data so that we don't have to download it again everytime we run the code.
saveRDS(CL1, file="CL1.rds")
saveRDS(CL2, file="CL2.rds")
saveRDS(CL3, file="CL3.rds")
saveRDS(CL4, file="CL4.rds")
saveRDS(CL5, file="CL5.rds")
saveRDS(CL6, file="CL6.rds")
saveRDS(CL7, file="CL7.rds")
saveRDS(CL8, file="CL8.rds")
saveRDS(CL9, file="CL9.rds")
saveRDS(CLSpotPrice, file="CLSpotPrice.rds")

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
CLSpotPrice <- readRDS("CLSpotPrice.rds")

# Combine all of the 'settle' and 'volume' data from the continuous contracts into one dataframe.
settleData <- cbind(CL1[,5], CL2[,5], CL3[,5], CL4[,5], CL5[,5], CL6[,5], CL7[,5], CL8[,5], CL9[,5])
volumeData <- cbind(CL1[,6], CL2[,6], CL3[,6], CL4[,6], CL5[,6], CL6[,6], CL7[,6], CL8[,6], CL9[,6])

# Fill in all of the NA's because there are lots of non-trading days.
settleData <- na.locf(settleData, na.rm=TRUE)
volumeData <- na.locf(volumeData, na.rm=TRUE)

# Create a POSIXct time series that matches our data. na.locf() command ruins it.
# Build dataframe with timeseries and data.
dates <- seq(as.POSIXct("1983-03-31", tz="GMT"), as.POSIXct("2015-01-14", tz="GMT"), by="days")
settleData <- data.frame(dates, settleData)
volumeData <- data.frame(dates, volumeData)

# Change the column names to make the dataframe look more appealing and easy to read.
names(settleData) <- c("Date", "CL1", "CL2", "CL3", "CL4", "CL5", "CL6", "CL7", "CL8", "CL9")
names(volumeData) <- c("Date", "CL1", "CL2", "CL3", "CL4", "CL5", "CL6", "CL7", "CL8", "CL9")

### So we can view the data in the same order that we downloaded it in. Not entirely sure if we need this.
### I needed to leave it in the original format so that the na.locf function was working properly 
### It was grabbing the previous data but it was in the reverse direction.
# settleData <- settleData[nrow(settleData):1,]
# volumeData <- volumeData[nrow(volumeData):1,]


####################################################################################################


# Rewrote the code to apply generically to a chosen date. 

# We want to plot the Spot Prices on the closing days for every month as well as the 9 month 
# futures data for today for every year.

# Create a new dataframe that includes the close dates and the corresponding values.
closeDates <- seq(as.POSIXct("1983-04-20", tz='GMT'), as.POSIXct("2014-12-20", tz='GMT'), by="month")
closeIndex <- rep(as.numeric(NA), 50)       ### dummy vector so that we can add values
for (i in seq(381)) {
  closeIndex[i] <- CLSpotPrice[,2][which(CLSpotPrice$dates==closeDates[i])]
}
closePrices <- data.frame(Date=closeDates, SpotPrice=closeIndex)

# Create an extended POSIXct time series to allow space to plot today's 9 month futures data.
extendedDates <- seq(as.POSIXct("1983-04-20", tz='GMT'), as.POSIXct("2016-01-20", tz='GMT'), by="month")
closeIndex[382:394] <- as.numeric(NA)

# Plot the Spot Prices as well as today's 9 month futures data.
xlim <- c(as.POSIXct("1985-01-01", tz='GMT'), as.POSIXct("2020-01-01", tz='GMT'))
plot(extendedDates, closeIndex, type='l', las=1,
     main='Crude Oil Futures Chain Projections',
     ylab='Settlement Price ( $ / gal )', ylim=c(0, 150), xlim=xlim,
     xaxt='n', xlab='Delivery Date', frame=F)

axis(1, at=seq(as.POSIXct("1985-01-01", tz='GMT'), as.POSIXct("2020-01-01", tz='GMT'), by='year'), labels=1985:2020)

# Arguments that can be changed based on your choosing. 
year <- "1990"
month <- "01"
day <- "01"

# Make an index so that we can create points 9 months ahead. This code seems impractical at the moment.
# I'll attempt to fix it in the future. Just want a working generic code for now.
plus9 <- (as.numeric(month) + 9) %% 12
if (plus9 == 0) {
  plus9 = 12
}

# If the picked month is April or later, you want 9 months ahead to be the next year.
plus9year <- year
if (as.numeric(month) > 3) {
  plus9year <- as.numeric(year) + 1
}

# Plot the 9 month futures data for the chosen date.
points(seq(as.POSIXct(paste0(year, "-", month, "-", day), tz='GMT'), 
           as.POSIXct(paste0(plus9year, "-", plus9, "-", day), tz='GMT'), by='month'), 
       settleData[which(settleData$Date==as.POSIXct(paste0(year, "-", month, "-", day), tz='GMT')),], 
       cex=0.5, col='blue')

# Plot the 9 month futures data for the chosen date for every year.
for (i in 1986:2015) {
  
  # If the picked month is April or later, you want 9 months ahead to be the next year.
  plus9year <- i
  if (as.numeric(month) > 3) {
    plus9year <- as.numeric(i) + 1
  }
  
  # Use the drm and EXD.3 command to create an exponential fit for each futures data.
  # x is an index counter. y is the dates which we want to use (our 9 month futures contracts)
  x <- 1:9
  y <- settleData[which(settleData$Date==as.POSIXct(paste0(i, "-", month, "-", day), tz='GMT')),-1]
  y <- as.numeric(y)
  fit <- drm(y ~ x, fct=EXD.3())
  
  # Grab the coefficients from the exponential fit (they have really bad names).
  # Function: f(x) = c + (d-c) * exp(-x/e)
  c <- fit$coefficients[1]
  d <- fit$coefficients[2]
  e <- fit$coefficients[3]
  
  # Create a POSIXct time series for the extended time period to plot our fit.
  time <- seq(as.POSIXct(paste0(i, '-', month, '-', day), tz='GMT'), as.POSIXct(paste0((as.numeric(i)+6), '-', month, '-', day), tz='GMT'), by='month')
  index <- seq(1:length(time))
  
  # Find the values corresponding to the dates using the exponential fit and plot it.
  oil <- c + (d-c) * exp(-index / e)
  lines(time, oil, ylim=c(0, 150), col=adjustcolor('salmon',0.5) , lwd=8)
  
  # If it's the current date picked, you want to go to the next loop to avoid overlapping our data.
  # I chose to draw the chosen date in blue (this avoids having the same points drawn in red on top).
  if (i == year) {
    next
  }
  
  # Plot the 9 month futures data for each year.
  points(seq(as.POSIXct(paste0(i, "-", month, "-", day), tz='GMT'), 
             as.POSIXct(paste0(plus9year, "-", plus9, "-", day), tz='GMT'), by='month'), 
         settleData[which(settleData$Date==as.POSIXct(paste0(i, "-", month, "-", day), tz='GMT')),], 
         cex=0.5, col='red')
}







# Not sure how to figure out the CI, best fit value, etc? All I can find are the residuals or 
# confidence intervals/etc for each variable (c, d, e)
#confint(fit)     # confidence intervals for each variable (c, d, e)
#fitted(fit)      # extracts fitted values from a 'drc'.
#residuals(fit)   # residuals 






# TODO: [X] Rewrite the code for plotting so that it can apply to any date chosen (not just today)
# TODO: [X] Clean up code, I have a couple repeated items
# TODO: [ ] Write code so that it will add any new values to the current dataframe of futures data
# TODO: [X] Clean up and make better labels for our graph (style/design graph to be more appealing)
# TODO: [ ] Check out other Quandl data (ie. other commodities/futures data) and see if it's in the same layout.
# TODO: [ ] Possible consider looking up calendar picker? Look at Will's write up/code. Think about how to apply to databrowser.
# TODO: [ ] Figure out how to plot the futures data for volume. Maybe look for a dataset that 
# TODO:     includes the amounts sold for each day? (It's the predicted amount that they would purchase?)

# IDEA: [ ] Maybe look at crude oil consumption and plot it against this data to see our trends
# IDEA:     based on prices.

# Possible Databrowser selectors/options
# [ ] Show previous years
# [ ] Choose a day to show 9 month futures data
# [ ] Type of data I want to view (going to see if other Quandl data is organized the same)
# [ ] Range? So that we don't have to look at the full data from 1986 till current
 



















# Write up a for loop to download all of the individual contracts. Then bind them together.
# FGHJKMNQUVXZ 
CLF2015 <- Quandl("CME/CLF2015")
CLG2015 <- Quandl("CME/CLG2015")
CLH2015 <- Quandl("CME/CLH2015")
CLJ2015 <- Quandl("CME/CLJ2015")
CLK2015 <- Quandl("CME/CLK2015")
CLM2015 <- Quandl("CME/CLM2015")
CLN2015 <- Quandl("CME/CLN2015")
CLQ2015 <- Quandl("CME/CLQ2015")
CLU2015 <- Quandl("CME/CLU2015")
CLV2015 <- Quandl("CME/CLV2015")
CLX2015 <- Quandl("CME/CLX2015")
CLZ2015 <- Quandl("CME/CLZ2015")

















# Crude Oil
# com <- 'CL'
# spotLink <- 'DOE/RWTC'

# Natural Gas
# com <- 'NG'
# spotLink 'ODA/PNGASUS_USD'

# Attempting to recreate all of my code but be able to choose between natural gas or crude oil 
# Implement the option to download any data from Quandl? 

# '### ***' Represents parameters that can be altered depending on type of commodity.

# Natural gas is SCF/CME_NG1_FW. com represents commodity.
# This assumes that they are all under this similar format.
# If not, we can easily just concatenate a list of the download 'links'
com <- 'NG'                                                           ### *** 

f1data <- Quandl(paste0("SCF/CME_", com, "1_FW"), type="raw")
f2data <- Quandl(paste0("SCF/CME_", com, "2_FW"), type="raw")
f3data <- Quandl(paste0("SCF/CME_", com, "3_FW"), type="raw")
f4data <- Quandl(paste0("SCF/CME_", com, "4_FW"), type="raw")
f5data <- Quandl(paste0("SCF/CME_", com, "5_FW"), type="raw")
f6data <- Quandl(paste0("SCF/CME_", com, "6_FW"), type="raw")
f7data <- Quandl(paste0("SCF/CME_", com, "7_FW"), type="raw")
f8data <- Quandl(paste0("SCF/CME_", com, "8_FW"), type="raw")
f9data <- Quandl(paste0("SCF/CME_", com, "9_FW"), type="raw")
startDate <- f1data[dim(f1data)[1],][[1]]

# Not entirely sure which to use for the 'spot prices' for natural gas
spotLink <- "ODA/PNGASUS_USD"                                         ### *** 
spotPriceData <- Quandl(spotLink, type="raw")

# Create an empty data frame with all of the 'dates' from the start to end. 
# I want to keep the 'type' for the date to be the same because na.locf/merge messes it up.
empty <- data.frame(Date=seq(as.Date(startDate), as.Date("2015-01-14"), by="days"))

### NOTE: Natural gas starts at 1990-04-04 while Crude Oil starts at 1983-03-31

# Merge all of the data from the futures data so that we have a full time series.
# Repeat process for the other future data. (ie. CL2, CL3, and so on)
F1 <- merge(f1data, empty, by.x='Date', by.y='Date', all.x=T, all.y=T)
F2 <- merge(f2data, empty, by.x='Date', by.y='Date', all.x=T, all.y=T)
F3 <- merge(f3data, empty, by.x='Date', by.y='Date', all.x=T, all.y=T)
F4 <- merge(f4data, empty, by.x='Date', by.y='Date', all.x=T, all.y=T)
F5 <- merge(f5data, empty, by.x='Date', by.y='Date', all.x=T, all.y=T)
F5 <- merge(f5data, empty, by.x='Date', by.y='Date', all.x=T, all.y=T)
F6 <- merge(f6data, empty, by.x='Date', by.y='Date', all.x=T, all.y=T)
F7 <- merge(f7data, empty, by.x='Date', by.y='Date', all.x=T, all.y=T)
F8 <- merge(f8data, empty, by.x='Date', by.y='Date', all.x=T, all.y=T)
F9 <- merge(f9data, empty, by.x='Date', by.y='Date', all.x=T, all.y=T)

# Create a POSIXct time series that will be used for all of the dataframes in order to maintain consistency.
dates <- seq(as.POSIXct(startDate, tz="GMT"), as.POSIXct("2015-01-14", tz="GMT"), by="days")
F1[,1] <- dates
F2[,1] <- dates
F3[,1] <- dates
F4[,1] <- dates
F5[,1] <- dates
F6[,1] <- dates
F7[,1] <- dates
F8[,1] <- dates
F9[,1] <- dates

# Clean up the Crude Oil Spot Price data. 
# 1. Merge with empty time series.
# 2. Use na.locf() to fill in missing days with the last data.
# 4. Create a POSIXct time series to match our data.
# 3. Build a dataframe with a new POSIXct time series since na.locf() messes it up.
spotPrice <- merge(spotPriceData, empty, by.x='Date', by.y='Date', all.x=T, all.y=T)
spotPrice <- na.locf(spotPrice[,2], na.rm=FALSE)
dates <- seq(as.POSIXct(startDate, tz="GMT"), as.POSIXct("2015-01-14", tz="GMT"), by="days")
spotPrice <- data.frame(dates, spotPrice=spotPrice)

# Save all of the data so that we don't have to download it again everytime we run the code.
saveRDS(F1, file=paste0(com, "1.rds"))
saveRDS(F2, file=paste0(com, "2.rds"))
saveRDS(F3, file=paste0(com, "3.rds"))
saveRDS(F4, file=paste0(com, "4.rds"))
saveRDS(F5, file=paste0(com, "5.rds"))
saveRDS(F6, file=paste0(com, "6.rds"))
saveRDS(F7, file=paste0(com, "7.rds"))
saveRDS(F8, file=paste0(com, "8.rds"))
saveRDS(F9, file=paste0(com, "9.rds"))
saveRDS(spotPrice, file=paste0(com, "SpotPrice.rds"))

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
spotPrice <- readRDS(paste0(com, "SpotPrice.rds"))

# Combine all of the 'settle' and 'volume' data from the continuous contracts into one dataframe.
settleData <- cbind(F1[,5], F2[,5], F3[,5], F4[,5], F5[,5], F6[,5], F7[,5], F8[,5], F9[,5])
volumeData <- cbind(F1[,6], F2[,6], F3[,6], F4[,6], F5[,6], F6[,6], F7[,6], F8[,6], F9[,6])

# Fill in all of the NA's because there are lots of non-trading days.
settleData <- na.locf(settleData, na.rm=TRUE)
volumeData <- na.locf(volumeData, na.rm=TRUE)

# Create a POSIXct time series that matches our data. na.locf() command ruins it.
# Build dataframe with timeseries and data.
dates <- seq(as.POSIXct(startDate, tz="GMT"), as.POSIXct("2015-01-14", tz="GMT"), by="days")
settleData <- data.frame(dates, settleData)
volumeData <- data.frame(dates, volumeData)

# Change the column names to make the dataframe look more appealing and easy to read.
names(settleData) <- c("Date", "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9")
names(volumeData) <- c("Date", "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9")

# We want to plot the Spot Prices on the closing days for every month as well as the 9 month 
# futures data for today for every year.

# Create a new dataframe that includes the close dates and the corresponding values.
closeDates <- seq(as.POSIXct(startDate, tz='GMT'), as.POSIXct("2014-12-20", tz='GMT'), by="month")
closeIndex <- rep(as.numeric(NA), 50)       ### dummy vector so that we can add values
for (i in seq(length(closeDates))) {
  closeIndex[i] <- spotPrice[,2][which(spotPrice$dates==closeDates[i])]
}

# Plot the Spot Prices as well as today's 9 month futures data.
xlim <- c(as.POSIXct(startDate, tz='GMT'), as.POSIXct("2020-01-01", tz='GMT'))
plot(closeDates, closeIndex, type='l', las=1,
     main='Futures Chain Projections',
     ylab='Settlement Price ( $ / gal )', xlim=xlim,
     xaxt='n', xlab='Delivery Date', frame=F)

startYear <- as.numeric(strftime(startDate, format="%Y"))
axis(1, at=seq(as.POSIXct(startDate, tz='GMT'), as.POSIXct("2020-01-01", tz='GMT'), by='year'), labels=startYear:2019)

# Arguments that can be changed based on your choosing. 
year <- "2015"
month <- "01"
day <- "01"

# Make an index so that we can create points 9 months ahead. This code seems impractical at the moment.
# I'll attempt to fix it in the future. Just want a working generic code for now.
plus9 <- (as.numeric(month) + 9) %% 12
if (plus9 == 0) {
  plus9 = 12
}

# If the picked month is April or later, you want 9 months ahead to be the next year.
plus9year <- year
if (as.numeric(month) > 3) {
  plus9year <- as.numeric(year) + 1
}

# Plot the 9 month futures data for the chosen date.
points(seq(as.POSIXct(paste0(year, "-", month, "-", day), tz='GMT'), 
           as.POSIXct(paste0(plus9year, "-", plus9, "-", day), tz='GMT'), by='month'), 
       settleData[which(settleData$Date==as.POSIXct(paste0(year, "-", month, "-", day), tz='GMT')),], 
       cex=0.5, col='blue')

if (as.numeric(strftime(startDate, format="%m")) != 1) {
  startYear <- startYear + 1
}

# Plot the 9 month futures data for the chosen date for every year.
for (i in startYear:2015) {
  
  # If the picked month is April or later, you want 9 months ahead to be the next year.
  plus9year <- i
  if (as.numeric(month) > 3) {
    plus9year <- as.numeric(i) + 1
  }
  
  # Use the drm and EXD.3 command to create an exponential fit for each futures data.
  # x is an index counter. y is the dates which we want to use (our 9 month futures contracts)
  x <- 1:9
  y <- settleData[which(settleData$Date==as.POSIXct(paste0(i, "-", month, "-", day), tz='GMT')),-1]
  y <- as.numeric(y)
  fit <- drm(y ~ x, fct=EXD.3())
  
  # Grab the coefficients from the exponential fit (they have really bad names).
  # Function: f(x) = c + (d-c) * exp(-x/e)
  c <- fit$coefficients[1]
  d <- fit$coefficients[2]
  e <- fit$coefficients[3]
  
  # Create a POSIXct time series for the extended time period to plot our fit.
  time <- seq(as.POSIXct(paste0(i, '-', month, '-', day), tz='GMT'), as.POSIXct(paste0((as.numeric(i)+6), '-', month, '-', day), tz='GMT'), by='month')
  index <- seq(1:length(time))
  
  # Find the values corresponding to the dates using the exponential fit and plot it.
  expFit <- c + (d-c) * exp(-index / e)
  lines(time, expFit, ylim=c(0, 150), col=adjustcolor('salmon',0.5) , lwd=8)
  
  # If it's the current date picked, you want to go to the next loop to avoid overlapping our data.
  # I chose to draw the chosen date in blue (this avoids having the same points drawn in red on top).
  if (i == year) {
    next
  }
  
  # Plot the 9 month futures data for each year.
  points(seq(as.POSIXct(paste0(i, "-", month, "-", day), tz='GMT'), 
             as.POSIXct(paste0(plus9year, "-", plus9, "-", day), tz='GMT'), by='month'), 
         settleData[which(settleData$Date==as.POSIXct(paste0(i, "-", month, "-", day), tz='GMT')),], 
         cex=0.5, col='red')
}


