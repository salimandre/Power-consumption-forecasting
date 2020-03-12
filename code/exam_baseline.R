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
library(marima)
library(fractal)

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
args.df = data.frame(model=c('Baseline last day','Baseline aggregate'),method=c('replicate last day','mean aggregate on each daily timestamp'))
#row.names(args.df) <- c('baseline 1','baseline 2')
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

# baseline last day
plot(ts(val.data$power,frequency=96),ylab='kW',main='Power - val set',ylim=c(100,400))
p<-tail(train.data$power,n=96)
rmse1<-rmse(val.data$power, p)
par(new=TRUE)
plot(ts(p,frequency=96),col='red', ann=FALSE,axes=FALSE,ylim=c(100,400))

# baseline by aggregate on each daily timestamp 
quarter <- as.factor(format(train.data$timestamp, "%H,%M"))
avgPowerBaseline <- as.numeric(tapply(train.data$power, quarter, mean))
par(new=TRUE)
plot(ts(avgPowerBaseline,frequency=96),col='purple',ann=FALSE,axes=FALSE,ylim=c(100,400))
rmse2<-rmse(val.data$power, avgPowerBaseline)

legend("topleft",c('Baseline_last_day','Baseline_aggregate'),lty=c(1,1,1),col=c("red","purple"))


# saving results
plot(1,1,ann=FALSE,axes=FALSE)
rmse.df = data.frame(baseline_last_day=c(round(rmse1,digits=2)), baseline_aggregate=c(round(rmse2,digits=3)))
row.names(rmse.df) <-c("RMSE")
grid.table(rmse.df)
dev.off()




