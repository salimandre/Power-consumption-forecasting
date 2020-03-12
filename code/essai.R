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
#pdf(file=opt$output)

# ----- script parameters -----
args.df = data.frame(model='SARIMA')
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

# val power
nvaldays <- 3
plot(ts(tail(wholetrain.data$power, nvaldays*96),frequency=96),ylab='kW',main='Power - val set',ylim=c(125,350),xlim=c(1,nvaldays+1))

# baseline avg
quarter <- as.factor(format(wholetrain.data$timestamp, "%H,%M"))
avgPowerBaseline <- as.numeric(tapply(wholetrain.data$power, quarter, mean))
par(new=TRUE)
avgPred <- rep(avgPowerBaseline,nvaldays)
plot(ts(avgPred,frequency=96),col='orange',ann=FALSE,axes=FALSE,ylim=c(125,350),xlim=c(1,nvaldays+1))
rmse(tail(wholetrain.data$power,nvaldays*96), avgPred)

# EXPO on FILTERED power with addi
LES=HoltWinters(ts(log(head(wholetrain.data$power,-nvaldays*96)),frequency=96),alpha=NULL,beta=NULL,gamma=NULL, seasonal = "addi")
p<-predict(LES,n.ahead=nvaldays*96)
par(new=TRUE)
plot(ts(exp(p),frequency=96),col='green',ann=FALSE,axes=FALSE,ylim=c(125,350),xlim=c(1,nvaldays+1))
rmse(tail(wholetrain.data$power,nvaldays*96), exp(p))

# EXPO on NN power without reg
fitNN <- nnetar(head(wholetrain.data$power,-nvaldays*96), lambda="auto")
predNN <- forecast(fitNN, h=nvaldays*96)
par(new=TRUE)
plot(ts(predNN$mean,frequency=96),col='purple',xlab = "", ylab = "",ann=FALSE,axes=FALSE,ylim=c(125,350),xlim=c(1,nvaldays+1))
rmse(tail(wholetrain.data$power,nvaldays*96), predNN$mean)

legend("topleft",c("baselineAVG",'ES-log','NNAR-Reg'),lty=c(1,1,1),col=c("orange","green",'purple'))

# Ensembing by lr

pred_stack <- function(nvaldays,trainMat,valMat, ind, lambda_){
  dataBag <- matrix(trainMat[,ind], nrow = nvaldays-1, byrow=TRUE)
  X <- dataBag[,1:3]
  #print(X)
  X[,1] <- X[,1]+runif(nvaldays-1)
  
  #print('hello')
  #print(dataBag[,4])
  target <- dataBag[,4]+0.5*runif(nvaldays-1)
  glmmod <- glmnet(X, y=target, alpha=0.5, lambda = lambda_, family="gaussian")
  
  pred_on_val <- predict(glmmod, newx = rbind(valMat[1:3,ind],X[1,]),s=c(0.01))[1]
  rmse(X[,1],target)
  rmse(X[,2],target)
  rmse(X[,3],target)
  rmse(predict(glmmod, newx = X[1:(nvaldays-1),],s=c(0.01)), target)
  return(pred_on_val)
}

stacking <- function(nvaldays,pred1,pred2,pred3,train_target,val_target,lambda_=20){
  train_pred1 <- pred1[1:((nvaldays-1)*96)]
  val_pred1 <- pred1[((nvaldays-1)*96+1):(nvaldays*96)]
  
  train_pred2 <- pred2[1:((nvaldays-1)*96)]
  val_pred2 <- pred2[((nvaldays-1)*96+1):(nvaldays*96)]
  
  train_pred3 <- pred3[1:((nvaldays-1)*96)]
  val_pred3 <- pred3[((nvaldays-1)*96+1):(nvaldays*96)]
  
  tempMat <- rbind(train_pred1,train_pred2,train_pred3,train_target)
  trainMat <- c()
  for (i in seq(1:(nvaldays-1))){
    trainMat <- rbind(trainMat,tempMat[,((i-1)*96+1):(i*96)])
  }

  valMat <- rbind(val_pred1,val_pred2,val_pred3,val_target)
  
  #print(dim(trainMat))
  #print(dim(valMat))
  
  valMat <- rbind(val_pred1,val_pred2,val_pred3,val_target)
  
  predStacking <- c()
  for (i in seq(1,96)){
    predStacking <- rbind(predStacking,pred_stack(nvaldays,trainMat,valMat,i,lambda_))
  }
  
  return(predStacking)
}

pred1 <- as.numeric(rep(avgPowerBaseline,nvaldays))
pred2 <- as.numeric(exp(p))
pred3 <- as.numeric(predNN$mean)
train_target <- head(tail(wholetrain.data$power,nvaldays*96), -96)
val_target <- tail(wholetrain.data$power,96)

final_pred_stacking <- stacking(nvaldays,pred1,pred2,pred3,train_target,val_target,30)
rmse(final_pred_stacking,val.data$power)

plot(ts(val.data$power,frequency=96),ylab='kW',main='Power - val set')
par(new=TRUE)
plot(ts(final_pred_stacking,frequency=96),col='green',xlab = "", ylab = "",ann=FALSE,axes=FALSE)








