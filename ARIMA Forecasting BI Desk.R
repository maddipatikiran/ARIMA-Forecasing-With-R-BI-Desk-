
#Set Source Path
setwd('E:/Forecasting')

#Reading Data 
Source_Data <-read.table ("passengers.csv", header =TRUE  , sep=",",
                          strip.white = TRUE,na.strings = "NA");

#Factot To Date Conversion 
factor_Date_conversion<- as.Date(factor(Source_Data$ds), '%Y-%m-%d')
factor_Date_conversion

#source  Data frame after conversion 
Dataframe_source<-data.frame(ds=factor_Date_conversion,y=Source_Data$y)

#Plot Actual Data 
plot(Dataframe_source)

# Import required library 
library(forecast)
library(plotly)

# Creation of Time series to data
Time_series_Data<-ts(Dataframe_source$y, start = c(1949,01,01),
                     end=c(1960,12,01), frequency = 12)

#------------------ARIMA--------

#checking Difference Lag of data 
Diff_value<-diff(Time_series_Data,differences = 1)
plot(Diff_value)

#ACF(Auto Correlation Function)         
acf(Diff_value)

#PACF(Partial Auto Correlation Function)
pacf(Diff_value)

#Building ARIMA Model 

fit <- arima((Time_series_Data), c(2, 1, 1),
             seasonal = list(order = c(0, 1, 1), period = 12))

#Predict Data for 1 Year
pred <- predict(fit, n.ahead = 1*12)

#Forecasting ARIMA Model
Arima_model_forecasting<-forecast(fit,h=12)
Arima_model_forecasting

#ploting Arima_model_forecasting
plot(Arima_model_forecasting)

#checking Accuracy 
accuracy(Arima_model_forecasting)

#-------Auto ARIMA-------

#Auto ARIMA Model Fitting 
Auto_arima_model<-auto.arima(Time_series_Data)
Auto_arima_model

#Auto ARIMA Forecasting 
Auto_arima_model_forecasting<-forecast(Auto_arima_model, h=10)
Auto_arima_model_forecasting

#Checking Accuracy
accuracy(Auto_arima_model_forecasting)


#plot Auto_arima_model_forecasting
plot(Auto_arima_model_forecasting)

