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
library(forecast)

# setting working directory
setwd("/Users/mac/Desktop/r_code/time_series_analysis/")

# passing arguments when running script
option_list = list(
  make_option(c("-o", "--output"), type="character", default='output.pdf', 
              help="pdf file name for output plots")
); 

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

# opening output file
pdf(file=opt$output)

# ----- script parameters -----
args.df = data.frame(model=c('NNAR','NNAR'),p=c(36,37),neurons=c(18,19),repeats=c(20,20),reg=c('None','Temperature'))
row.names(args.df) <- c('NNAR 1','NNAR 2')
grid.table(args.df)
# -----------------------------

# loading data
elec.data <- read_excel("data/Elec-train.xlsx")

# preprocessing
names(elec.data)[1] <- "timestamp"
names(elec.data)[2] <- "power"
names(elec.data)[3] <- "temp"

elec.chunk = data.frame(timestamp=c("1/1/2010 00:00","1/1/2010 00:15","1/1/2010 00:30","1/1/2010 00:45","1/1/2010 1:00"),power=c(182,180,176,173,170),temp=c(10.6,10.6,10.6,10.6,10.6))
elec.data <- rbind(elec.chunk,elec.data)
elec.data$timestamp <- as.POSIXct(elec.data$timestamp, format ="%m/%d/%Y %H:%M", tz = "GMT")

# splitting train/test data
powerFull.ts <- ts(elec.data$power, frequency = 96)
elec.data$time <- as.numeric(time(powerFull.ts))

wholetrain.data <- elec.data[elec.data$timestamp<ymd_hm("2010-02-17 00:00"),]
powerWholetrain.ts <- ts(wholetrain.data$power, frequency = 96)
wholetrain.data$time <- as.numeric(time(powerWholetrain.ts))

train.data <- wholetrain.data[wholetrain.data$timestamp<ymd_hm("2010-02-16 00:00"),]
powerTrain.ts <- ts(train.data$power, frequency = 96)
train.data$time <- as.numeric(time(powerTrain.ts))

nFull <- length(powerFull.ts)
nWholetrain <- length(powerWholetrain.ts)
nTest <- nFull-nWholetrain
nTrain <- length(powerTrain.ts)
nVal <- nWholetrain - nTrain

val.data <- wholetrain.data[wholetrain.data$timestamp>=ymd_hm("2010-02-16 00:00"),]
val.data$time <- wholetrain.data$time[(nTrain+1):nWholetrain]
powerVal.ts <- ts(val.data$power, start=val.data$time[1] , frequency = 96)

test.data <- elec.data[elec.data$timestamp>=ymd_hm("2010-02-17 00:00"),]
test.data$time <- elec.data$time[(nWholetrain+1):nFull]

## Temperature features min max avg

#plot(ts(train.data$temp,frequency=96),col='cyan',ylab='Celsius',main='Temperature - train set',ylim=c(min(train.data$temp),max(train.data$temp)))

minWindowFunc <- function(x) { min(tail(wholetrain.data$temp[1:x],n=100))}
maxWindowFunc <- function(x) { max(tail(wholetrain.data$temp[1:x],n=100))}
avgWindowFunc <- function(x) { mean(tail(wholetrain.data$temp[1:x],n=100))}

minTemp <- as.numeric(lapply(1:length(wholetrain.data$temp), FUN=minWindowFunc))
maxTemp <- as.numeric(lapply(1:length(wholetrain.data$temp), FUN=maxWindowFunc))
avgTemp <- as.numeric(lapply(1:length(wholetrain.data$temp), FUN=avgWindowFunc))

# Neural Network

plot(ts(val.data$power,frequency=96),ylab='kW',main='Power - val set')
fitNN <- nnetar(train.data$power)
predNN <- forecast(fitNN,h=96)
par(new=TRUE)
plot(ts(predNN$mean,frequency=96),col='green',xlab = "", ylab = "",ann=FALSE,axes=FALSE)
rmse1 <- rmse(val.data$power, predNN$mean)

fitNN <- nnetar(train.data$power, xreg=train.data$temp)
predNN <- forecast(fitNN,xreg=val.data$temp, h=96)
par(new=TRUE)
plot(ts(predNN$mean,frequency=96),col='cyan',xlab = "", ylab = "",ann=FALSE,axes=FALSE)
rmse2 <- rmse(val.data$power, predNN$mean)

legend("topleft",c('NNAR(36,18)','NNAR(36,18)-TempReg'),lty=c(1),col=c("green","cyan"))

# saving results
plot(1,1,ann=FALSE,axes=FALSE)
rmse.df <- data.frame(col1=c(round(rmse1,digits=2)),col2=c(round(rmse2,digits=2)))
names(rmse.df) <-c('NNAR(36,18)','NNAR(36,18)-TempReg')
row.names(rmse.df) <-c('RMSE')
grid.table(rmse.df)
dev.off()


