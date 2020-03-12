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
              help="pdf file name for output plots")
); 

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

# opening output file
pdf(file=opt$output)

# ----- script parameters -----
args.df = data.frame(model=c('SimpleES','MultiSeasonalES','AddiSeasonalES','AddiSeasonalES-Log'),data=c('power','power','power','log(power)'),alpha=c(0.1,'learnt','learnt','learnt'),beta=c('None','learnt','learnt','learnt'),gamma=c('None','learnt','learnt','learnt'),seasonal=c('None','Multiplicative','Additive','Additive'))
row.names(args.df) <- c('Expo Smooth 1','Expo Smooth 2','Expo Smooth 3','Expo Smooth 4')
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

# Expo
plot(powerVal.ts,ylab = "kW",col="blue")
LES=HoltWinters(powerTrain.ts,alpha=0.1,beta=FALSE,gamma=FALSE)
p<-predict(LES,n.ahead=96)
rmse1<-rmse(val.data$power, p)
lines(p,col='green',xlab = "", ylab = "")

LES=HoltWinters(powerTrain.ts,alpha=NULL,beta=NULL,gamma=NULL, seasonal = "multi")
p<-predict(LES,n.ahead=96)
rmse2<-rmse(val.data$power, p)
lines(p,col='brown')

LES=HoltWinters(powerTrain.ts,alpha=NULL,beta=NULL,gamma=NULL, seasonal = "addi")
p<-predict(LES,n.ahead=96)
rmse3<-rmse(val.data$power, p)
lines(p,col='red')

par(new=TRUE)
LES=HoltWinters(ts(log(train.data$power),frequency=96),alpha=NULL,beta=NULL,gamma=NULL, seasonal = "addi")
p<-predict(LES,n.ahead=96)
rmse4<-rmse(val.data$power, exp(p))
lines(exp(p),col='pink')
#lines(p,col='pink')
legend("topleft",c("SimpleES", "MultiSeasonalES","AddiSeasonalES","AddiSeasonalES-Log"),lty=c(1,1,1),col=c("green","brown","red","pink"))

# saving results
plot(1,1,ann=FALSE,axes=FALSE)
rmse.df = data.frame(rmse=c(rmse1,rmse2,rmse3,rmse4))
row.names(rmse.df) <-c("SimpleES", "MultiSeasonalES","AddiSeasonalES","AddiSeasonalES-Log")
grid.table(rmse.df)
dev.off()



