# Time Series Analysis Exam

rm(list=ls()) 
graphics.off() 
cat("\014") 

# libraries
library(Metrics)
library("optparse")
library(gridExtra)
library(grid)
library("readxl")
library(lubridate)

# setting working directory
setwd("/Users/mac/Desktop/r_code/time_series_analysis/")

# passing arguments when running script
option_list = list(
  make_option(c("-o", "--output"), type="character", default='exam.pdf', 
              help="pdf file name for output plots")
); 

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

# opening output file
pdf(file=opt$output)

# ----- script parameters -----
# -----------------------------

# loading data
elec.data <- read_excel("data/Elec-train.xlsx")

# preprocessing
names(elec.data)[1] <- "Time"
names(elec.data)[2] <- "Power"
names(elec.data)[3] <- "Temp"
elec.data$Time <- as.POSIXct(elec.data$Time, format ="%m/%d/%Y %H:%M", tz = "GMT")

# splitting train/test data
train.data <- elec.data[elec.data$Time<ymd_hm("2010-02-17 00:00"),]
val.data <- train.data[train.data$Time>=ymd_hm("2010-02-16 00:00"),]
test.data <- elec.data[elec.data$Time>=ymd_hm("2010-02-17 00:00"),]

# Power as ts
powerTrain.ts <- ts(train.data$Power, frequency = 96) 
plot(powerTrain.ts, main="Power - training ts", xlab="days", ylab="kW")

powerVal.ts <- ts(val.data$Power, start=0, frequency = 96) 
plot(powerVal.ts, main="Power variation on 16/02/2010", xlab="24 hours", ylab="kW")

boxplot(train.data$Power, main="Boxplot Power - training ts",ylab="kW")

quarter <- as.factor(format(train.data$Time, "%H"))
avgPower <- tapply(train.data$Power, quarter, mean)
plot(avgPower, type = "b", pch = 20, main='mean aggregated Power - training ts',ylab="kW")

boxplot(train.data$Power ~ quarter, col = "lightblue", pch = 20, cex = 0.5, main='Boxplot on aggreated Power',ylab="kW")

# Temperature as ts
tempTrain.ts <- ts(train.data$Temp, frequency = 96) 
plot(tempTrain.ts, main="Temperature - training ts", xlab="days", ylab="Celsius")

tempVal.ts <- ts(val.data$Temp, start=0, frequency = 96) 
plot(tempVal.ts, main="Temperature variation on 16/02/2010", xlab="24 hours", ylab="Celsius")

boxplot(train.data$Temp, main="Boxplot Temperature - training ts",ylab="Celsius")

quarter <- as.factor(format(train.data$Time, "%H"))
avgTemp <- tapply(train.data$Temp, quarter, mean)
plot(avgTemp, type = "b", pch = 20, main='mean aggregated Temperature - training ts',ylab="Celsius")

boxplot(train.data$Temp ~ quarter, col = "lightblue", pch = 20, cex = 0.5, main='Boxplot on aggreated Temperature',ylab="Celsius")

# Autocorrelation

tmp=acf(train.data$Power,type="cor",plot = FALSE)
plot(tmp, main='autocorr for Power')

tmp=pacf(train.data$Power,type="cor",plot = FALSE)
plot(tmp, main='partial autocorr for Power')

tmp=acf(train.data$Temp,type="cor",plot = FALSE)
plot(tmp, main='autocorr for Temperature')

tmp=pacf(train.data$Temp,type="cor",plot = FALSE)
plot(tmp, main='partial autocorr for Temperature')

# Trend

# Seasonalite



