#----------------set up--------------
library(fpp) # for forecasting
library(xts) # for ts dataframe
library(tidyverse) # include ggplot2, stringr, dplyr 
library(lubridate) # for datetime ymd_hms
library(data.table) #fread
library(gridExtra) # for multiple ggplots in one figure
library(forecast) # msts multi seasonal time series object
#setwd('C:\\Users\\Alson\\WorkDocs\\1. Predictive analytics\\Assignment2\\2018AssignmetData\\output')

#forecast performance function
fcast_accuracy <- function (yhat, yy, yylag) {
  stats<-matrix(0,1,4)
  colnames(stats)<-c("RMSE","MAPE","MASE","ACF")
  dev<-yy-yhat
  q<-dev/mean(abs(yy-yylag))
  stats[1]=sqrt(mean(dev^2))
  stats[2]=mean(100*abs(dev/yy))
  stats[3]=mean(abs(q))
  stats[4]=acf(dev,plot=FALSE)$acf[2]
  return(stats)}

xinrong = function(df, time){
  n = nrow(df)
  for (i in 1:(time-1)){
    df = rbind(df, df[1:n,])
  }
  
  return(df)
}
#--------------data--------------------
#dataframe
df = fread('cleaned with weekdays.csv')

# which(as.Date(df$Time)==as.Date('2016-12-31')) #70127

df$Index = ymd_hms(df$Index)
colnames(df)[colnames(df)=="Index"] <- "Time"

df$demand_diff48 = df$demand - lag(df$demand, 48)
df$demand_diff48_1 = df$demand_diff48 - lag(df$demand_diff48, 1)
df$demand_diff1 = df$demand - lag(df$demand, 1)

df$temperature_diff48 = df$temperature - lag(df$temperature, 48)
df$temperature_diff48_1 = df$temperature_diff48 - lag(df$temperature_diff48, 1)
df$temperature_diff1 = df$temperature - lag(df$temperature, 1)

df$humidity_diff48 = df$humidity - lag(df$humidity, 48)
df$humidity_diff48_1 = df$humidity_diff48 - lag(df$humidity_diff48, 1)
df$humidity_diff1 = df$humidity - lag(df$humidity, 1)

df$wind_diff48 = df$wind - lag(df$wind, 48)
df$wind_diff48_1 = df$wind_diff48 - lag(df$wind_diff48, 1)
df$wind_diff1 = df$wind - lag(df$wind, 1)

df$price_diff48 = df$price - lag(df$price, 48)
df$price_diff48_1 = df$price_diff48 - lag(df$price_diff48, 1)
df$price_diff1 = df$price - lag(df$price, 1)

q1 = quantile(df$price, probs = c(0.25,0.75))[1]
q3 = quantile(df$price, probs = c(0.25,0.75))[2]
high = q3+1.5*(q3-q1)
low = q1-1.5*(q3-q1)
df$price2 = df$price
df$price2[df$price2 >high] = high
df$price2[df$price2 <low] = low

df$price2_diff48 = df$price2 - lag(df$price2, 48)
df$price2_diff48_1 = df$price2_diff48 - lag(df$price2_diff48, 1)
df$price2_diff1 = df$price2 - lag(df$price2, 1)
#----------all plot together--------------
#all years
print(1)
g0 = ggplot(df,aes(y = demand,x = Time))+ geom_line()
g1 = ggplot(df,aes(y = price2,x = Time))+ geom_line()
g2 = ggplot(df,aes(y = temperature,x = Time))+ geom_line()
g3 = ggplot(df,aes(y = humidity,x = Time))+ geom_line()
g4 = ggplot(df,aes(y = wind,x = Time))+ geom_line()
grid.arrange(g0,g1,g2,g3,g4, nrow = 5)
dev.copy(png,"graphs\\all plot.png", width = 896, height= 598)
dev.off()

# 2013
#which(as.Date(df$Time)==as.Date('2013-12-31')) #17519
g0 = ggplot(df[1:17519],aes(y = demand,x = Time))+ geom_line()
g1 = ggplot(df[1:17519],aes(y = price2,x = Time))+ geom_line()
g2 = ggplot(df[1:17519],aes(y = temperature,x = Time))+ geom_line()
g3 = ggplot(df[1:17519],aes(y = humidity,x = Time))+ geom_line()
g4 = ggplot(df[1:17519],aes(y = wind,x = Time))+ geom_line()
grid.arrange(g0,g1,g2,g3,g4, nrow = 5)
dev.copy(png,"graphs\\all plot 2013.png", width = 896, height= 598)
dev.off()

# 2013 first quarter
#which(as.Date(df$Time)==as.Date('2013-03-31')) #4319
g0 = ggplot(df[1:4319],aes(y = demand,x = Time))+ geom_line()
g1 = ggplot(df[1:4319],aes(y = price2,x = Time))+ geom_line()
g2 = ggplot(df[1:4319],aes(y = temperature,x = Time))+ geom_line()
g3 = ggplot(df[1:4319],aes(y = humidity,x = Time))+ geom_line()
g4 = ggplot(df[1:4319],aes(y = wind,x = Time))+ geom_line()
grid.arrange(g0,g1,g2,g3,g4, nrow = 5)
dev.copy(png,"graphs\\all plot 2013 q1.png", width = 896, height= 598)
dev.off()

# 2013 first month
#which(as.Date(df$Time)==as.Date('2013-01-31')) #1487
g0 = ggplot(df[1:1487],aes(y = demand,x = Time))+ geom_line()
g1 = ggplot(df[1:1487],aes(y = price2,x = Time))+ geom_line()
g2 = ggplot(df[1:1487],aes(y = temperature,x = Time))+ geom_line()
g3 = ggplot(df[1:1487],aes(y = humidity,x = Time))+ geom_line()
g4 = ggplot(df[1:1487],aes(y = wind,x = Time))+ geom_line()
grid.arrange(g0,g1,g2,g3,g4, nrow = 5)
dev.copy(png,"graphs\\all plot 2013 m1.png", width = 896, height= 598)
dev.off()
#------------demand stationary?--------------
# png("graphs\\demand plot.png",896,598)
g0 = ggplot(df,aes(y = demand,x = Time))+ geom_line()
g1 = ggplot(df,aes(y = demand_diff48,x = Time))+ geom_line()
g2 = ggplot(df,aes(y = demand_diff48_1 ,x = Time))+ geom_line()
g3 = ggplot(df,aes(y = demand_diff1 ,x = Time))+ geom_line()
grid.arrange(g0,g1,g2,g3, nrow = 4)
dev.copy(png,"graphs\\demand plot.png", width = 896, height= 598)
dev.off()


par(mfrow = c(4,2))
acf(df$demand,lag.max = 100)
pacf(df$demand,lag.max = 100)
acf(na.omit(df)$demand_diff48,lag.max = 100)
pacf(na.omit(df)$demand_diff48,lag.max = 100)
acf(na.omit(df)$demand_diff48_1,lag.max = 100)
pacf(na.omit(df)$demand_diff48_1,lag.max = 100)
acf(na.omit(df)$demand_diff1,lag.max = 100)
pacf(na.omit(df)$demand_diff1,lag.max = 100)
dev.copy(png,"graphs\\demand acf.png", width = 896, height= 598)
dev.off()

adf.test(df$demand)$p.value #p<0.01 stationary
kpss.test(df$demand)$p.value #p<0.01 non-stationary

adf.test(na.omit(df)$demand_diff48)$p.value #p<0.01 stationary
kpss.test(na.omit(df)$demand_diff48)$p.value #p>0.1 stationary

adf.test(na.omit(df)$demand_diff48_1)$p.value #p<0.01 stationary
kpss.test(na.omit(df)$demand_diff48_1)$p.value #p>0.1 stationary

adf.test(na.omit(df)$demand_diff1)$p.value #p<0.01 stationary
kpss.test(na.omit(df)$demand_diff1)$p.value #p>0.1 stationary

# after diff48, its already stationary, but diff1 again looks better

# ----------temperature stationary?---------------------


g0 = ggplot(df,aes(y = temperature,x = Time))+ geom_line()
g1 = ggplot(df,aes(y = temperature_diff48,x = Time))+ geom_line()
g2 = ggplot(df,aes(y = temperature_diff48_1 ,x = Time))+ geom_line()
g3 = ggplot(df,aes(y = temperature_diff1 ,x = Time))+ geom_line()
grid.arrange(g0,g1,g2,g3, nrow = 4)
dev.copy(png,"graphs\\temp plot.png", width = 896, height= 598)
dev.off()


par(mfrow = c(4,2))
acf(df$temperature,lag.max = 100)
pacf(df$temperature,lag.max = 100)
acf(na.omit(df)$temperature_diff48,lag.max = 100)
pacf(na.omit(df)$temperature_diff48,lag.max = 100)
acf(na.omit(df)$temperature_diff48_1,lag.max = 100)
pacf(na.omit(df)$temperature_diff48_1,lag.max = 100)
acf(na.omit(df)$temperature_diff1,lag.max = 100)
pacf(na.omit(df)$temperature_diff1,lag.max = 100)
dev.copy(png,"graphs\\temp acf.png", width = 896, height= 598)
dev.off()


adf.test(df$temperature)$p.value #p<0.01 stationary
kpss.test(df$temperature)$p.value #p<0.01 non-stationary

adf.test(na.omit(df)$temperature_diff48)$p.value #p<0.01 stationary
kpss.test(na.omit(df)$temperature_diff48)$p.value #p>0.1 stationary

adf.test(na.omit(df)$temperature_diff48_1)$p.value #p<0.01 stationary
kpss.test(na.omit(df)$temperature_diff48_1)$p.value #p>0.1 stationary

adf.test(na.omit(df)$temperature_diff1)$p.value #p<0.01 stationary
kpss.test(na.omit(df)$temperature_diff1)$p.value #p>0.1 stationary
#---------humidity stationary?--------------


g0 = ggplot(df,aes(y = humidity,x = Time))+ geom_line()
g1 = ggplot(df,aes(y = humidity_diff48,x = Time))+ geom_line()
g2 = ggplot(df,aes(y = humidity_diff48_1 ,x = Time))+ geom_line()
g3 = ggplot(df,aes(y = humidity_diff1 ,x = Time))+ geom_line()
grid.arrange(g0,g1,g2,g3, nrow = 4)
dev.copy(png,"graphs\\humidity plot.png", width = 896, height= 598)
dev.off()

par(mfrow = c(4,2))
acf(df$humidity,lag.max = 100)
pacf(df$humidity,lag.max = 100)
acf(na.omit(df)$humidity_diff48,lag.max = 100)
pacf(na.omit(df)$humidity_diff48,lag.max = 100)
acf(na.omit(df)$humidity_diff48_1,lag.max = 100)
pacf(na.omit(df)$humidity_diff48_1,lag.max = 100)
acf(na.omit(df)$humidity_diff1,lag.max = 100)
pacf(na.omit(df)$humidity_diff1,lag.max = 100)
dev.copy(png,"graphs\\humidity acf.png", width = 896, height= 598)
dev.off()

adf.test(df$humidity)$p.value #p<0.01 stationary
kpss.test(df$humidity)$p.value #p=0.0167 non-stationary

adf.test(na.omit(df)$humidity_diff48)$p.value #p<0.01 stationary
kpss.test(na.omit(df)$humidity_diff48)$p.value #p>0.1 stationary

adf.test(na.omit(df)$humidity_diff48_1)$p.value #p<0.01 stationary
kpss.test(na.omit(df)$humidity_diff48_1)$p.value #p>0.1 stationary

adf.test(na.omit(df)$humidity_diff1)$p.value #p<0.01 stationary
kpss.test(na.omit(df)$humidity_diff1)$p.value #p>0.1 stationary

#---------wind stationary?--------------
g0 = ggplot(df,aes(y = wind,x = Time))+ geom_line()
g1 = ggplot(df,aes(y = wind_diff48,x = Time))+ geom_line()
g2 = ggplot(df,aes(y = wind_diff48_1 ,x = Time))+ geom_line()
g3 = ggplot(df,aes(y = wind_diff1 ,x = Time))+ geom_line()
grid.arrange(g0,g1,g2,g3, nrow = 4)
dev.copy(png,"graphs\\wind plot.png", width = 896, height= 598)
dev.off()

par(mfrow = c(4,2))
acf(df$wind,lag.max = 100)
pacf(df$wind,lag.max = 100)
acf(na.omit(df)$wind_diff48,lag.max = 100)
pacf(na.omit(df)$wind_diff48,lag.max = 100)
acf(na.omit(df)$wind_diff48_1,lag.max = 100)
pacf(na.omit(df)$wind_diff48_1,lag.max = 100)
acf(na.omit(df)$wind_diff1,lag.max = 100)
pacf(na.omit(df)$wind_diff1,lag.max = 100)
dev.copy(png,"graphs\\wind acf.png", width = 896, height= 598)
dev.off()

adf.test(df$wind)$p.value #p<0.01 stationary
kpss.test(df$wind)$p.value #p>0.1 stationary

adf.test(na.omit(df)$wind_diff48)$p.value #p<0.01 stationary
kpss.test(na.omit(df)$wind_diff48)$p.value #p>0.1 stationary

adf.test(na.omit(df)$wind_diff48_1)$p.value #p<0.01 stationary
kpss.test(na.omit(df)$wind_diff48_1)$p.value #p>0.1 stationary

adf.test(na.omit(df)$wind_diff1)$p.value #p<0.01 stationary
kpss.test(na.omit(df)$wind_diff1)$p.value #p>0.1 stationary
# write.csv(df,'df_diff.csv')

#---------price stationary?--------------
g0 = ggplot(df,aes(y = price2,x = Time))+ geom_line()
g1 = ggplot(df,aes(y = price2_diff48,x = Time))+ geom_line()
g2 = ggplot(df,aes(y = price2_diff48_1 ,x = Time))+ geom_line()
g3 = ggplot(df,aes(y = price2_diff1 ,x = Time))+ geom_line()
grid.arrange(g0,g1,g2,g3, nrow = 4)
dev.copy(png,"graphs\\price plot.png", width = 896, height= 598)
dev.off()

par(mfrow = c(4,2))
acf(df$price2,lag.max = 100)
pacf(df$price2,lag.max = 100)
acf(na.omit(df)$price2_diff48,lag.max = 100)
pacf(na.omit(df)$price2_diff48,lag.max = 100)
acf(na.omit(df)$price2_diff48_1,lag.max = 100)
pacf(na.omit(df)$price2_diff48_1,lag.max = 100)
acf(na.omit(df)$price2_diff1,lag.max = 100)
pacf(na.omit(df)$price2_diff1,lag.max = 100)
dev.copy(png,"graphs\\price acf.png", width = 896, height= 598)
dev.off()

adf.test(df$price2)$p.value #p<0.01 stationary
kpss.test(df$price2)$p.value #p>0.01 non-stationary

adf.test(na.omit(df)$price2_diff48)$p.value #p<0.01 stationary
kpss.test(na.omit(df)$price2_diff48)$p.value #p>0.1 stationary

adf.test(na.omit(df)$price2_diff48_1)$p.value #p<0.01 stationary
kpss.test(na.omit(df)$price2_diff48_1)$p.value #p>0.1 stationary

adf.test(na.omit(df)$price2_diff1)$p.value #p<0.01 stationary
kpss.test(na.omit(df)$price2_diff1)$p.value #p>0.1 stationary

#------------get rid of useless cols and create lagged xreg-------
df_final = na.omit(df[,c(1:19,21,24,27,31,37,20,22)])
df_final$lagprice_1 = lag(df_final$price2_diff48_1,1)
df_final$lagprice_2 = lag(df_final$price2_diff48_1,2)
df_final$lagprice_3 = lag(df_final$price2_diff48_1,3)
df_final$lagprice_4 = lag(df_final$price2_diff48_1,4)
df_final$lagprice_24 = lag(df_final$price2_diff48_1,24)
df_final$lagprice_48 = lag(df_final$price2_diff48_1,48)

df_final$laghumid_1 = lag(df_final$humidity_diff48_1,1)
df_final$laghumid_2 = lag(df_final$humidity_diff48_1,2)
df_final$laghumid_3 = lag(df_final$humidity_diff48_1,3)
df_final$laghumid_4 = lag(df_final$humidity_diff48_1,4)
df_final$laghumid_24 = lag(df_final$humidity_diff48_1,24)
df_final$laghumid_48 = lag(df_final$humidity_diff48_1,48)

df_final$lagtemp_1 = lag(df_final$temperature_diff48_1,1)
df_final$lagtemp_2 = lag(df_final$temperature_diff48_1,2)
df_final$lagtemp_3 = lag(df_final$temperature_diff48_1,3)
df_final$lagtemp_4 = lag(df_final$temperature_diff48_1,4)
df_final$lagtemp_24 = lag(df_final$temperature_diff48_1,24)
df_final$lagtemp_48 = lag(df_final$temperature_diff48_1,48)

df_final$lagwind_1 = lag(df_final$wind_diff1,1)
df_final$lagwind_2 = lag(df_final$wind_diff1,2)
df_final$lagwind_3 = lag(df_final$wind_diff1,3)
df_final$lagwind_4 = lag(df_final$wind_diff1,4)
df_final$lagwind_24 = lag(df_final$wind_diff1,24)
df_final$lagwind_48 = lag(df_final$wind_diff1,48)

df_final = na.omit(df_final)

#---------convert df to time series--------------
df_ts = ts(df_final,frequency=48)
hi = colnames(df_ts)


#------------arima 1---------------------------
#fit model
arima1 = Arima(df_ts[,20], order = c(6,0,4), seasonal = list(order= c(2,0,1), period=48),
               xreg = df_ts[,c(8:13,17,18,21,22,24,31,32,37,38,43,44)],
               method = "CSS")
browseURL("https://www.youtube.com/watch?v=YQHsXMglC9A")
model = arima1

#summary
summary(model)


# arima2 = Arima(df_final[,20],order = c(4,0,0), seasonal = c(0,0,0),
#                xreg = df_final[,c(8:13,17,18,21,22,24,31,32,37,38,43,44)],
#                method = "CSS")
# model2 = arima2
# 
# #summary
# summary(model2)

# t-stat and p-value
t_model =  model$coef/diag((model$var.coef)^0.5)
p_model = round(2*pnorm(abs(t_model),mean = 0,sd=1, lower.tail = FALSE),3)
t_model
p_model

# residual diagnostics acf and box
tsdiag(model)
Box.test(model$residual, fitdf=length(model$coef)+1,lag=50,type="Lj")

# residuals acf and pacf
par(mfrow =c(3,1))
plot(model$residuals)
acf(model$residuals, lag.max = 100)
pacf(model$residuals, lag.max = 100)

#----------------expanding windows forecast-----------------
which(as.Date(df_final$Time) == as.Date('2016-12-31'))
dim(df_ts)
#1:70074 training
#70075:87595 testing

train_end = 70030
test_start = 70031
test_end = 87551
n = 52 #number of week forecast
y_train = df_ts[1:train_end,]

y_f3= matrix(nrow = n*48*7)
y_residual = matrix(nrow = n*48*7)
for (i in 1:n){
  print(i)
  #estimate the models
  fit1=Arima(y_train[,20],order = c(6,0,4), seasonal = list(order= c(2,0,1), period=48),
             xreg = y_train[,c(8:13,17,18,21,22,24,31,32,37,38,43,44)],method = "CSS")

  #forecast h steps ahead
  fcast1=forecast(fit1,h=336,xreg = cbind(df_ts[(nrow(y_train)+1):(nrow(y_train)+336),c(8:13,17,18)],
                                          xinrong(df_ts[(nrow(y_train)-47):(nrow(y_train)),c(21,22,24,31,32,37,38,43,44)],7)))
  start = 336*(i-1)+1
  end = 336*i
  y_f3[start:end]=fcast1$mean
  y_residual[start:end] = fcast1$residuals
  #expand the training set
  if(i<n){
    y_train=df_ts[1:(train_end+336*i),]
  }
}
browseURL("https://www.youtube.com/watch?v=YQHsXMglC9A")

#------convert back to level--------------------
alson = data.frame(index = seq_along(y_f3), 'y_f1' = y_f3, 
                   'y' = df_ts[test_start:(test_start+336*n-1),20],
                   'y_1' = df_ts[(test_start-1):(test_start+336*n-2),20],
                   'y_level' = df_ts[test_start:(test_start+336*n-1),2],
                   'y_level_1' = df_ts[(test_start-1):(test_start+336*n-2),2])

# loop convert to level with iterations
y_f1_level= matrix(nrow = n*48*7)

#df_ts real value
#y_f1 predicted value
for (i in 1:n){
  for (j in 1:336){
    t = (i-1)*336+j
    t_adj = (i-1)*336+j+train_end
    if (j == 1){
      y_f1_level[t] = y_f3[t] + df_ts[t_adj-1,2] + df_ts[t_adj-48,2] - df_ts[t_adj-49,2]}
    else if (j <= 48){
      y_f1_level[t] = y_f3[t] + y_f1_level[t-1] + df_ts[t_adj-48,2] - df_ts[t_adj-49,2]}
    else if (j == 49){
      y_f1_level[t] = y_f3[t] + y_f1_level[t-1] + y_f1_level[t-48] - df_ts[t_adj-49,2]}
    else {
      y_f1_level[t] = y_f3[t] + y_f1_level[t-1] + y_f1_level[t-48] - y_f1_level[t-49]}
  }
}

alson$y_f1_level = y_f1_level

write.csv(alson,'604,101.csv')
#-----------forecast performace evaluation--------------
# on difference
fcast_accuracy(alson$y_f1,alson$y,alson$y_1)

ggplot() + 
  geom_line(data = alson[1:336,], aes(x = index,y = y, color = "actual"))+
  geom_line(data = alson[1:336,], aes(x = index, y = y_f1, color = "pred"), alpha=0.4)+
  theme_bw()
  
  
  
  

# on level
fcast_accuracy(alson$y_f1_level,alson$y_level,alson$y_level_1)
ggplot() + 
  geom_line(data = alson, aes(x = index,y = y_level), color = "red")+
  geom_line(data = alson, aes(x = index, y = y_f1_level), color = "blue")

  

#----------------point forecast-----------------------

arima1 = Arima(df_ts[,20],order = c(4,0,4), seasonal = c(2,0,0),
               xreg = df_ts[,c(8:13,17,18,21,22,24,31,32,37,38,43,44)],
               method = "CSS")

# accuracy(fcast,df_ts[nrow(df_ts) - 336:nrow(df_ts),2])
alson = read.csv('406201.csv')[,-1]
fitted_2017 = alson[,2]
ytest = alson[,3]
# 1) Normal error (Lecture page 41)
LL_norm<-function(sig){
  R = ytest - fitted_2017 #residual
  R = dnorm(R,0,sig,log=TRUE)
  -sum((R))
}

fit_norm=mle(minuslogl = LL_norm, start = list(sig=3.0), method = "L-BFGS-B",
             lower = c(-Inf, -Inf, 0), upper = c(Inf, Inf, Inf))
summary(fit_norm)
AIC(fit_norm)

# 2) Student t error (Lecture page 41)
library(sn)
LL_t<-function(sig,df){
  R = ytest - fitted_2017 #residual
  R=dst(R, xi=0, omega=sig,nu=df, alpha=0, log = TRUE)
  -sum(R)
}

fit_t=mle(minuslogl = LL_t, start = list(sig=3.0, df=5), method = "L-BFGS-B",
          lower = c(0,4), upper = c(Inf,20)) # <4 kurtosis not defined. >20 use normal
summary(fit_t)
AIC(fit_t)

# 3) Skewed normal error (Lecture page 41)
LL_sn<-function(sig,sk){
  R = ytest - fitted_2017 #residual
  R=dsn(R, xi=0, omega=sig, alpha=sk, log = TRUE)
  -sum(R)
}
fit_sn=mle(minuslogl = LL_sn, start = list(sig=0.00000000001, sk=0.00000000001), method = "L-BFGS-B",
           lower = c(0.00000000001,-Inf), upper = c(Inf,Inf))
summary(fit_sn)
AIC(fit_sn)

# 4) Skewed student t error (Lecture page 41)
LL_st<-function(sig,df,sk){
  R = ytest - fitted_2017
  R=dst(R, xi=0, omega=sig,nu=df, alpha=sk, log = TRUE)
  -sum(R)
}
fit_st=mle(minuslogl = LL_st, start = list(sig=10, df=3, sk=0), method = "L-BFGS-B",
           lower = c(0, 2, -Inf), upper = c(Inf, 20, Inf))
summary(fit_st)
AIC(fit_st)
# Best fitting according to AIC: Skewed t


#----------CC Test-----------------
cc.test <- function (q, y, alpha, where) {
  stats<-matrix(0,1,3)
  colnames(stats)<-c("phat","LR","p-val")
  n<-length(y)
  if(where=="body"){ #where is a string indicating the area to test
    #If where="body", q must have two columns
    x<-(sum(y>=q[,1])-sum(y>q[,2]))
  }else if(where=="lower"){
    x<-sum(y<=q)
  } else {
    x<-n-sum(y<=q)
  }
  
  stats[1]<-x/n
  lu<-(n-x)*log(1-stats[1])+x*log(stats[1])
  lr<-(n-x)*log(1-alpha)+x*log(alpha)
  stats[2]<-2*(lu-lr) #LR test statistic
  stats[3]<-pchisq(stats[2],1,lower.tail=FALSE)
  return(stats)
}

alpha=0.99
q1 = matrix(0, length(ytest), 4)
for (i in 1:length(ytest)){
  #estimate the 
  cat(i)
  q1[i,2]=qst(0.99,0,fit_t@coef[1],0,fit_t@coef[2])#student t
  #q1[i,4]=qst(0.99,0,fit_st@coef[1],fit_st@coef[3],fit_st@coef[2])#skewed t
  q1[i,1]=qnorm(0.99,0,fit_norm@coef[1]) #norm
  #q1[i,3]=qsn(0.99,0,fit_sn@coef[1],fit_sn@coef[2])#skilled norm
  cat("\014")
}
print("normal")
cc.test((q1[,1]+fitted_2017), ytest, alpha,"lower")
print("student-t")
cc.test((q1[,2]+fitted_2017), ytest, alpha,"lower")
print("skewed normal")
cc.test((q1[,3]+fitted_2017), ytest, alpha,"lower")
print("skewed t")
cc.test((q1[,4]+fitted_2017), ytest, alpha,"lower")


ggplot() + 
  geom_line(data = alson, aes(x = index, y = y_level), color = "red")+
  geom_line(data = alson, aes(x = index, y = y_f1_level), color = "blue")
