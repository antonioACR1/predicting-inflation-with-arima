#the dataset includes inflation in Mexico year after year from 1960 till 2016
#some of the steps I will follow are:
#examination, decomposition, make data stationary, find autocorrelation orders (p,q,d), fit ARIMA and evaluate (plus applying auto.arima and adding seasonality).


#required libraries
library(ggplot2)
library(forecast)
library(tseries)
library(stringr)

#read data
df<-read.csv2("inflation1.csv",sep=";",header=TRUE)
head(df)


#> head(df)
#  Year   Inflation
#1 1960          NA         
#2 1961 3.245870346
#3 1962 2.746731733
#4 1963  2.92510431
#5 1964 5.895985632
#6 1965 1.556615699

#check data types

str(df)
#> str(df)
#'data.frame':   57 obs. of  2 variables:
# $ Year     : int  1960 1961 1962 1963 1964 1965 1966 1967 1968 1969 ...
# $ Inflation: num  NA 3.25 2.75 2.93 5.9 ...

#add a date formate to Year
df$Year<-paste0(df$Year,"-01-01")


#convert to date and check how the series look like
df$Year<-as.Date(df$Year)

png(filename="inflation.png")
ggplot(df, aes(Year,Inflation)) + geom_line()+ scale_x_date("year")+ ylab("Inflation through several years") +xlab("")
dev.off()

#ts() function is to convert data to a time series object
#I will convert the column 'Inflation' into a time series object
inflation_ts<-ts(df[,c('Inflation')])


#the first thing to do is check any outliers and remove them
#for this, I have to convert my data to a series time object using ts() and apply tsclean()

df$Inflation_Cleaned<-tsclean(inflation_ts)

#how it looks like after removing outliers
png(filename="inflation without outliers.png")
ggplot() +  geom_line(data = df, aes(x = Year, y = Inflation_Cleaned)) + ylab('Cleaned Inflation')
dev.off()

#to smoothen a little bit more, use moving average (MA(q)) of order say 10 applied to Inflation_Cleaned

df$Inflation_5 = ma(df$Inflation_Cleaned, order=5) 
df$Inflation_10 = ma(df$Inflation_Cleaned, order=10)

#plot again inflation and inflation with moving average of orders 5 and 10 (MA(q) so q=5 or q=10)

png(filename="inflation after moving averages.png")
ggplot() +
geom_line(data = df, aes(x = Year, y = Inflation_Cleaned, colour = "Inflation")) +
geom_line(data = df, aes(x = Year, y = Inflation_5,   colour = "Inflation 5 year order"))  +
geom_line(data = df, aes(x = Year, y = Inflation_10, colour = "Inflation 10 year order"))  +
ylab('Inflation')
dev.off()

#now apply decomposition

#I should get the seasonality, cycle and trend with stl() function to understand the series a little better
#I use order 5 for now (in particular, q=5), so I set frequency =5 

Inflation_5 <- ts(na.omit(df$Inflation_5), frequency=5)
#I assume that my model is additive, so that I will get the series by adding seasonality, cycle and trend
decomposition <- stl(Inflation_5, s.window="periodic")

#plot decomposition

png(filename="inflation decomposition.png")
plot(decomposition)
dev.off()

#I remove seasonality from the series because the model becomes simpler which is my objective for now
Inflation_Deseasonal <- seasadj(decomposition)

#checking whether data is stationary
#ARIMA requires the series to be stationary (i.e. the mean, variance and autocovariance are time invariant) 

#to check that data is stationary we use a hypothesis test called ADF
#null hypothesis is that the data is nonstationary
#the hypothesis is applied on the smoothed series of order 5 (including seasonality so far)
adf.test(Inflation_5,alternative="stationary")

#> adf.test(Inflation_5,alternative="stationary")
#
#        Augmented Dickey-Fuller Test
#
#data:  Inflation_5
#Dickey-Fuller = -1.9712, Lag order = 3, p-value = 0.5861
#alternative hypothesis: stationary

#the p-value is bigger than 0.05, so I can't reject the null hypothesis, i.e. the series is not stationary

#now I difference the series sufficiently enough until getting a p-value less than 0.05

#to find the order d for the differences I use either ACF (or PACF if the order is not clear enough from the plot)

png(filename="inflation ACF to find d.png")
Acf(Inflation_5, main='')
dev.off()

#the PACF plot gives a better view

png(filename="inflation PACF to find d.png")
Pacf(Inflation_5, main='')
dev.off()

#I choose d=1 because it shows higher correlation between the series and the lag when d=1 (based on PACF plot)

#I take the difference with d=1 (this time applied to the series without seasonality) and apply ADF test again

Inflation_Difference <- diff(Inflation_Deseasonal, differences = 1)

#plot
png(filename="Inflation difference of order d=1.png")
plot(Inflation_Difference)
dev.off()

#apply test again
adf.test(Inflation_Difference, alternative = "stationary")

#> adf.test(Inflation_Difference, alternative = "stationary")
#
#        Augmented Dickey-Fuller Test
#
#data:  Inflation_Difference
#Dickey-Fuller = -4.0189, Lag order = 3, p-value = 0.01554
#alternative hypothesis: stationary

#the p-value is less than 0.05, so rejeCt null hypothesis
#the series becomes stationary

#to find the value p corresponding to the ARIMA equation, I apply the ACF and PACF plots again to Inflation_Difference
#the spikes will suggest what values are better for p 

png("inflation ACF plot after difference.png")
Acf(Inflation_Difference, main='ACF for Differenced Series')
dev.off()

png("inflation PACF plot after difference.png")
Pacf(Inflation_Difference, main='PACF for Differenced Series')
dev.off()
#the PACF plot suggests order equal to 1, so I set p=1

#now I choose a subset for testing
testing<-window(ts(Inflation_Difference),start=50)
#> testing
#Time Series:
#Start = 50 
#End = 52 
#Frequency = 1 
#[1]  0.3298519 -0.3745495 -0.5800290


#and I choose the remaining part for training
training<-Inflation_Difference[1:49]

#apply ARIMA with (p,q,d)=(1,1,5)
modelo<-arima(ts(training), order=c(1,1,5))

#now I forecast the last 3 observations which should correspond to the testing part
prediction<-forecast(modelo,h=3)
#> prediction
#  Point Forecast     Lo 80    Hi 80     Lo 95    Hi 95
#50      -0.661495 -2.333912 1.010922 -3.219235 1.896246
#51      -1.208131 -3.695187 1.278926 -5.011756 2.595494
#52      -1.076790 -4.077575 1.923995 -5.666094 3.512515

#> testing
#Time Series:
#Start = 50 
#End = 52 
#Frequency = 1 
#[1]  0.3298519 -0.3745495 -0.5800290

#NOT SO GOOD BUT MEH

#plot prediction
png("inflation my prediction on the difference.png")
plot(prediction,main="")
dev.off()

#to measure how good the model is, the errors should be normally distributed
#I plot the errors with the following

png(filename="inflation distribution of errors after ARIMA on difference series.png")
tsdisplay(residuals(modelo), lag.max=15, main='Deseasonal Model Residuals On Difference')
dev.off()

#NOT SO GOOD INDEED

#now I try prediction on series without difference but deseasonalized
#choose a subset for testing
testing<-window(ts(Inflation_Deseasonal),start=50)

#and choose the remaining part for training
training<-ts(Inflation_Deseasonal)[1:49]

#apply ARIMA with (p,q,d)=(1,1,5)
modelo<-arima(ts(training), order=c(1,1,5))

#now I forecast the last 3 observations which should correspond to the testing part
prediction<-forecast(modelo,h=3)
#> prediction
#   Point Forecast      Lo 80     Hi 80     Lo 95     Hi 95
#50       4.243040  2.5945258  5.891554  1.721855  6.764224
#51       4.405333  0.6258087  8.184858 -1.374951 10.185617
#52       4.031217 -2.1821614 10.244595 -5.471325 13.533759

#> testing
#Time Series:
#Start = 50 
#End = 53 
#Frequency = 1 
#[1] 3.609246 3.939098 3.564549 2.984520

#LOOKS BETTER

#plot prediction
png("inflation my prediction on the series without difference.png")
plot(prediction,main="")
dev.off()

#to measure how good the model is, the errors should be normally distributed
#I plot the errors with the following

png(filename="inflation distribution of errors after ARIMA but without difference series.png")
tsdisplay(residuals(modelo), lag.max=15, main='Deseasonal Model Residuals Without Difference')
dev.off()


#alternatively, I could try auto.arima() to automatically choose p,q and d for me

modelo<-auto.arima(training, seasonal=FALSE)

#> modelo
#Series: training 
#ARIMA(0,2,0)                    

#sigma^2 estimated as 3.442:  log likelihood=-95.74
#AIC=193.47   AICc=193.56   BIC=195.32

#note that p, q and d are different from my selection

prediction<-forecast(modelo,h=4)

#> prediction
#   Point Forecast      Lo 80     Hi 80       Lo 95     Hi 95
#50       3.975098   1.597575  6.352620   0.3389913  7.611204
#51       3.513316  -1.802985  8.829618  -4.6172644 11.643897
#52       3.051535  -5.844338 11.947409 -10.5535287 16.656599
#53       2.589754 -10.432471 15.611979 -17.3260203 22.505529
 
#> testing
#Time Series:
#Start = 50 
#End = 53 
#Frequency = 1 
#[1] 3.609246 3.939098 3.564549 2.984520

#MORE OR LESS OK

png(filename="inflation auto.arima distribution of errors after ARIMA but without difference series.png")
tsdisplay(residuals(modelo), lag.max=15, main='Seasonal Model Residuals Without Difference')
dev.off()

#and to improve the model, I could include seasonality into the model 

modelo<-auto.arima(Inflation_Deseasonal, seasonal=TRUE)

#> auto.arima(Inflation_Deseasonal, seasonal=TRUE)
#Series: Inflation_Deseasonal 
#ARIMA(0,2,0)(0,0,1)[5]                    
#
#Coefficients:
#         sma1
#      -0.8174
#s.e.   0.1506
#
#sigma^2 estimated as 1.785:  log likelihood=-89.36
#AIC=182.73   AICc=182.98   BIC=186.59

prediction<-forecast(modelo,h=4)

#> prediction
#      Point Forecast      Lo 80     Hi 80        Lo 95     Hi 95
#11.60       2.690344  0.9747874  4.405900   0.06662658  5.314061
#11.80       2.406442 -1.4296588  6.242543  -3.46036806  8.273253
#12.00       1.614961 -4.8040639  8.033986  -8.20209044 11.432012
#12.20       1.120890 -8.2756004 10.517380 -13.24980187 15.491582

#looks not as good as auto.arima without seasonality

#finally, let's go back to the actual inflation

#Run again my ARIMA model deseasonalized but without difference

testing<-window(ts(Inflation_Deseasonal),start=50)
training<-ts(Inflation_Deseasonal)[1:49]
modelo<-arima(ts(training), order=c(1,1,5))

#choose the remaining seven observations of df$Inflation
real_inflation<-window(ts(df$Inflation),start=50)
prediction<-forecast(modelo,h=7)



#> prediction
#   Point Forecast       Lo 80     Hi 80      Lo 95     Hi 95
#50       4.243040   2.5945258  5.891554   1.721855  6.764224
#51       4.405333   0.6258087  8.184858  -1.374951 10.185617
#52       4.031217  -2.1821614 10.244595  -5.471325 13.533759
#53       3.876686  -5.2418205 12.995192 -10.068866 17.822238
#54       3.848825  -8.4965450 16.194196 -15.031789 22.729440
#55       3.834030 -11.1938528 18.861912 -19.149133 26.817193
#56       3.826172 -13.5384285 21.190773 -22.730693 30.383037

#> real_inflation
#Time Series:
#Start = 50 
#End = 57 
#Frequency = 1 
#[1] 3.536935 4.485158 5.287869 3.252454 1.758812 4.709185 2.521437       NA

#get metrics
accuracy(modelo)

#> accuracy(modelo)
#                       ME     RMSE       MAE       MPE     MAPE      MASE
#Training set 0.0008851241 1.269514 0.8949666 0.6365914 5.644522 0.4047229
#                    ACF1
#Training set 0.003950949

#square error on the prediction
prediccion<-data.frame(prediction)$Point.Forecast
valor_real<-real_inflation[1:7]ç
install.packages("Metrics")
library("Metrics")
mse(prediccion,valor_real)
#> mse(prediccion,valor_real)
#[1] 1.330026
