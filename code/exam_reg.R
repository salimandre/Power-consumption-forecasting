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
  make_option(c("-o", "--output"), type="character", default='output.pdf', 
              help="pdf file name for output plots"),
  make_option(c("-d", "--degree"), type="numeric", default=1, 
              help="degree for linear regression features"),
  make_option(c("-r", "--raw"), type="logical", default=TRUE, 
              help="raw = FALSE -> orthogonal poly, else raw poly")
); 

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

# opening output file
pdf(file=opt$output)

# ----- script parameters -----
degree_features <- opt$degree
raw_poly <- opt$raw

args.df = data.frame(model='Regression',degree=degree_features,rawPoly=raw_poly)
row.names(args.df) <- 'parameters'
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

# features selection
train.data$features=poly(train.data$time,degree=degree_features ,raw=raw_poly)
val.data$features=poly(val.data$time,degree=degree_features,raw=raw_poly)

# fitting linear model on train set
model <- lm(power~features,train.data) 
train.data$pred <- predict(model)
plot(powerTrain.ts,ylab="kW")
lines(train.data$time, train.data$pred, col='red')
pc<-predict(model, interval="confidence")
pp<-predict(model, interval="prediction")
matlines(train.data$time, pc[,2:3], lty=c(2,2), col="blue")
matlines(train.data$time, pp[,2:3], lty=c(3,3), col="red")
legend("topleft",c("confiance","prediction"),lty=c(2,3),col=c("blue","red"))

# compute RMSE on train set
rmse(train.data$power, train.data$pred)

# inference of linear model on test set
val.data$pred <- predict(model, newdata = val.data)
plot(powerVal.ts,ylab="kW")
val.data$time
length(val.data$pred)
lines(val.data$time, val.data$pred, col='red')
pc<-predict(model, newdata = val.data, interval="confidence")
pp<-predict(model, newdata =  val.data, interval="prediction")
matlines(val.data$time, pc[,2:3], lty=c(2,2), col="blue")
matlines(val.data$time, pp[,2:3], lty=c(3,3), col="red")
legend("topleft",c("confidence","prediction"),lty=c(2,3),col=c("blue","red"))

# compute RMSE on test set
rmse(val.data$power, val.data$pred)

# saving results
plot(1,1,ann=FALSE,axes=FALSE)
rmse.df = data.frame(train=rmse(train.data$power, train.data$pred),test=rmse(val.data$power, val.data$pred))
row.names(rmse.df) <- 'RMSE'
grid.table(rmse.df)
dev.off()
