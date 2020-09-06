
library(dplyr)
library(DataExplorer)
library(ggplot2)
library(data.table)
library(StatMeasures)
library(plyr)
library(randomForest)
library(ROCR)
library(caret)
library(xgboost)
library(data.table)
library(readr)
library(readr)
library(RcppRoll)

train<-read.csv("~/Downloads/train 2.csv")
test<-read.csv("~/Downloads/test.csv")
submit<-read.csv("~/Downloads/sample_submission.csv")

#checking for NA values
sum(is.na(train))
sum(is.na(test))
#introduce(train)

#trying to visualize sampled train data
train %>% sample_n(5000)%>%
  ggplot(aes(x = time, y = signal))+
  geom_line(color = "blue")+
  geom_vline(xintercept = seq(0, 500, 50))

#Dividing train to batches
#t<-seq(50,500,by=50)
#batch<-lapply(seq_along(t),function(i)train[(t-49)[i]:t[i],])

## plot for signal and number of open channels for all batches
ggplot(train,aes(x = time,y = signal,col="Signal"))+
  geom_point(shape=".")+
  geom_point(aes(y=open_channels,col="Open channels"),shape=".")+
  scale_y_continuous(sec.axis=sec_axis(~.,name="open channels"))+
  labs(title="Signal and number of open channels per time", x="time", y="signal")
#here we can see that as the signal value increase so do the no. of open channels for batches 0-5 but thats not the case for 
#batches 6-9, probably coz of the signal pattern

## time Vs open channel 
ggplot(train,aes(x = time, y = open_channels)) + 
  geom_line(colour="blue") +
  labs(title="training data") 

##feature engineering
options(repr.plot.width=14, repr.plot.height=6)
sele=seq(0,nrow(train), by=1000)
batches=seq(0, 500, by=50)

plot(signal ~ time, data=train[sele,], type="l", lwd=0.3, las=1, col="blue")
abline(v=batches)
abline(v=60)
##################
w=2*pi/(50*2)
delta=0
A=5
##################

##########
indi=which(train$time > 50 &  train$time <=60)
time=train$time[indi]
omega=pi
SS=A*sin(w*time + omega)+ delta
train$signal[indi]=train$signal[indi]-SS
lines(time, SS, col=2, lwd=2)

#############
indi=which(train$time > 300 &  train$time <=350)
time=train$time[indi]
omega=0
SS=A*sin(w*time + omega)+ delta
train$signal[indi]=train$signal[indi]-SS
lines(time, SS, col=2, lwd=2)
############
indi=which(train$time > 350 &  train$time <=400)
time=train$time[indi]
omega=pi
SS=A*sin(w*time + omega)+ delta
train$signal[indi]=train$signal[indi]-SS
lines(time, SS, col=2, lwd=2)
############
indi=which(train$time > 400 &  train$time <=450)
time=train$time[indi]
omega=0
SS=A*sin(w*time + omega)+ delta
train$signal[indi]=train$signal[indi]-SS
lines(time, SS, col=2, lwd=2)
############
indi=which(train$time > 450 &  train$time <=500)
time=train$time[indi]
omega=pi
SS=A*sin(w*time + omega)+ delta
train$signal[indi]=train$signal[indi]-SS
lines(time, SS, col=2, lwd=2)

plot(signal ~ time, data=train[sele,], type="l", lwd=0.3, las=1, col="blue")
abline(v=batches)
abline(v=60)

## cleaning test ##
w=2*pi/(50*2)
delta=0
A=5
##################

sele=seq(0,nrow(test), by=400)

TeBound=matrix(c(500, 510, 540, 560, 570, 580, 600, 510, 520, 550, 570, 580, 590, 650), 7, 2)

plot(signal ~ time, data=test[sele,], type="l", lwd=0.3, las=1, col="blue", ylim=c(-5,10))
batches=seq(500, 700, by=50)
abline(v=c(batches, TeBound[,1]))

for(j in 1:7){
  xp=mean(c(TeBound[j,1],TeBound[j,2]))
  text(xp,8, j, cex=1.2)
}

########## segment 1
indi=which(test$time > 500 &  test$time <=510)
time=test$time[indi]
omega=0
SS=A*sin(w*time + omega)+ delta
test$signal[indi]=test$signal[indi]-SS
lines(time, SS-2, col=2, lwd=2)

########## segment 2
indi=which(test$time > 510 &  test$time <=520)
time=test$time[indi]
omega=1.8*pi
SS=A*sin(w*time + omega)+ delta
test$signal[indi]=test$signal[indi]-SS
lines(time, SS, col=2, lwd=2)

########## segment 3
indi=which(test$time > 540 &  test$time <=550)
time=test$time[indi]
omega=1.2*pi
SS=A*sin(w*time + omega)+ delta
test$signal[indi]=test$signal[indi]-SS
lines(time, SS, col=2, lwd=2)

########## segment 4
indi=which(test$time > 560 &  test$time <=570)
time=test$time[indi]
omega=0.8*pi
SS=A*sin(w*time + omega)+ delta
test$signal[indi]=test$signal[indi]-SS
lines(time, SS, col=2, lwd=2)

########## segment 5
indi=which(test$time > 570 &  test$time <=580)
time=test$time[indi]
omega=0.6*pi
SS=A*sin(w*time + omega)+ delta
test$signal[indi]=test$signal[indi]-SS
lines(time, SS, col=2, lwd=2)

########## segment 6
indi=which(test$time > 580 &  test$time <=590)
time=test$time[indi]
omega=0.4*pi
SS=A*sin(w*time + omega)+ delta
test$signal[indi]=test$signal[indi]-SS
lines(time, SS, col=2, lwd=2)

########## segment 7
indi=which(test$time > 600 &  test$time <=650)
time=test$time[indi]
omega=0
SS=A*sin(w*time + omega)+ delta
test$signal[indi]=test$signal[indi]-SS
lines(time, SS, col=2, lwd=2)

sele=seq(0,nrow(test), by=400)
plot(signal ~ time, data=test[sele,], type="l", lwd=0.3, col=4, ylim=c(-5,10), las=1)
abline(v=c(batches, TeBound[,1]))
for(j in 1:7){
  xp=mean(c(TeBound[j,1],TeBound[j,2]))
  text(xp,8, j, cex=1.2)
}

decode=function(x){
  bre=quantile(x, seq(0.1, 0.9, by=0.05))
  clase=rep(NA, length(x))
  for(j in 1:length(bre)){
    indi= which(x >=  bre[j] & x < bre[j+1]) 
    clase[indi]=j
  }
  return(clase)
}

##generating features
features=function(data){
  data[, batch := 0:(nrow(data)-1) %/% 25000]
  data[, batch_index := 0:(nrow(data)-1)- (batch * 25000)]
  data[, batch_slices := batch_index %/% 2500]
  data[, batch_slices2 := paste(batch,"_", batch_slices, sep="")]
  
  data[, signalSq := signal^2]
  data[, signale33 := sign(signal)*abs(signal)^(1/3)]
  
  data[, paste("cls1_","batch", sep="") := decode(signal), by="batch"]
  data[, paste("RollMean_",10, "_", "batch_slices2", sep="") := roll_mean(signal, n = 10, fill=mean(signal)), by="batch_slices2"]
}
train=data.table(train)
test=data.table(test)
train=features(train)
test=features(test)

##removing NA values from train
for (i in names(train))
  train[is.na(get(i)), (i):=0]
##removing NA values from test
for (i in names(test))
  test[is.na(get(i)), (i):=0]


train=select(train,c('open_channels','signal','RollMean_10_batch_slices2','signale33','signalSq','cls1_batch'))
##xgboost
train$open_channels=as.factor(train$open_channels)
str(train)
i.tr=sample(1:nrow(train), size=nrow(train)*7/10)
i.val=setdiff(1:nrow(train), i.tr)

dtrain <- xgb.DMatrix(data= as.matrix(train[i.tr, -1]),
                      label= as.matrix(train[i.tr, 'open_channels']))

dval <- xgb.DMatrix(data= as.matrix(train[i.val, -1]),
                    label= as.matrix(train[i.val,'open_channels']))

wl <- list(train = dtrain, eval = dval)

param <- list(max_depth = 10, eta = 1, nthread = 5, watchlist=wl,
              colsample_bytree=0.375, learning_rate=0.05,
              subsample=1, num_class=11)

model <- xgb.train(dtrain, nrounds = 150, params=param, watchlist=wl,
                   early_stopping_rounds = 20, # maximize=T, feval = F1Score,
                   objective="multi:softmax")

test<-select(test,c('signal','RollMean_10_batch_slices2','signale33','signalSq','cls1_batch'))
str(test)
dtest<-xgb.DMatrix(data= as.matrix(test))

submit$open_channels = predict(model, dtest, ntreelimit=model$best_ntree_limit)
summary(submit)
#head(submi$open_channels)
write.csv(submit, "predictions_1.csv")
