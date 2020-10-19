# ---
#   title: "R Notebook"
# output:
#   html_document:
#   df_print: paged
# html_notebook: default
# pdf_document: default
# ---
  # Foreign Exchange Rates
  
  ## Libraries
  
library(tidyverse)
library(ggplot2)
library(plotly)


## DataSet

daily = read.csv('Foreign_Exchange_Rates.csv')[2:24]
daily



## Column Names

names(daily)
names(daily) = c('Date', 'Australia', 'Euro', 'NewZealand', 'UK', 'Brazil', 'Canada', 'China', 'HongKong', 'India', 'Korea', 'Mexico', 'SouthAfrica', 'Singapore', 'Denmark', 'Japan', 'Malaysia', 'Norway', 'Sweden', 'SriLanka', 'Switzerland', 'Taiwan', 'Thailand')


## Convert Dates

daily$Date = as.Date(daily$Date, format = "%Y-%m-%d")
head(daily)

## Column Data Type

sapply(daily, class)
daily[,2:23] = sapply(daily[2:23], as.numeric)



## Handling Missing Values

# Calculate NA Values
colSums(is.na.data.frame(daily))

# Delete NA Values
daily = na.omit(daily)


## Exploring the Indian Rupee

ggplot(data= daily, aes(x= Date, y= India)) + geom_line(color='royalblue', size =0.1)

ggplot() + 
  geom_line(data= daily, aes(x= Date, y= India, color='India'), size=0.25) +
  geom_line(data= daily, aes(x= Date, y= SriLanka, color='SriLanka'), size=0.25) +
  ylab('Indian Rupee vs Sri Lankan Rupee') + 
  scale_color_discrete(name = "Currency: Rupee", labels = c("India", "Sri Lanka")) +
  theme_minimal()


ggplot(data= daily, aes(x= Date, y= India)) + 
  geom_point(color='royalblue', size=0.1, alpha=0.25) +
  geom_smooth(method = 'loess', formula = y ~ x, se = F, 
              color='slategrey', size=0.25) +
  ylab('Indian Rupee')




with(daily, plot(Date, India, pch = 1, col = "blue"))
abline(lm(formula = India~Date, data = daily))

with(daily, plot(Date, Euro, pch = 1, col = "red"))
abline(lm(formula = Euro~Date, data = daily))

plot_ly(daily, x= ~Date, y= ~India, type='scatter', mode='lines', name= 'India') %>%
  add_trace(y= ~SriLanka, type='scatter', mode='lines', name= 'Sri Lanka') %>%
  add_trace(y= ~Euro, type='scatter', mode='lines', name= 'Euro') %>%
  layout(yaxis = list(type = "log", title='Currency'))



plot_ly(daily, x= ~Date) %>% 
  add_trace(y= ~Australia, type='scatter', mode='lines', name= 'Australia') %>% 
  add_trace(y= ~Euro, type='scatter', mode='lines', name= 'Euro') %>% 
  add_trace(y= ~UK, type='scatter', mode='lines', name= 'UK') %>%
  add_trace(y= ~Canada, type='scatter', mode='lines', name= 'Canada') %>%
  add_trace(y= ~Japan, type='scatter', mode='lines', name= 'Japan') %>%
  add_trace(y= ~Korea, type='scatter', mode='lines', name= 'Korea') %>%
  layout(yaxis = list(type = "log", title='Currency'))



Time Series analysis can be classified as:
  
  1. Parametric and Non-parametric
2. Linear and Non-linear and
3. Univariate and multivariate

Techniques used for time series analysis:
  
  1. ARIMA models
2. Box-Jenkins multivariate models
3. Holt winters exponential smoothing (single, double and triple)

# AutoRegressive Integrated Moving Average (ARIMA)
## Assumptions of ARIMA 
1. Stationary Data
Stationarity means that the statistical properties of a process generating a time series do not change over time . It does not mean that the series does not change over time, just that the way it changes does not itself change over time.
It means that the properties of the series doesn’t depend on the time when it is captured. A white noise series and series with cyclic behavior can also be considered as stationary series.

2. Univariate Data

## Converting Data.Frame to Time-Series 

df = ts(data = daily, start = c(2000, 01), end= c(2019, 12), frequency = 12)

ts.plot(daily[, c(3, 5, 7, 8, 11)], col = 1:5, log='y')
legend("right", 
       legend = c(names(daily[, c(3, 5, 7, 8, 11)])),
       col= 1:5, lty=1:5, lwd = 5)



## Convert India's Exchange Rate to time series

india = ts(data = daily[, 11], start = c(2000, 01), end = c(2019, 12), frequency = 12)
india
plot(india, ylab = 'Rupees', main="Indian Rupee growth 2000 through 2019")


## Decompose into time series components

india_components = decompose(india)
plot(india_components)


## Unit Root Test 
It tests whether a time series variable is non-stationary and possesses a unit root. 
The null hypothesis is generally defined as the presence of a unit root and the alternative hypothesis is either stationarity, trend stationarity or explosive root depending on the test used.
It's a feature to check stochastic processes, such as random walks, that can cause problems in statistical inference involving time series models.

# install.packages("fUnitRoots")
library("fUnitRoots")
urkpssTest(india, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)
tsstationary = diff(india, differences=1)
plot(tsstationary)


## Calculate Correlation, Covariance
### Correlation
Correlation is a statistical technique that can show whether and how strongly pairs of variables are related.
### Covariance
Covariance is a measure of the joint variability of two random variables. If the greater values of one variable mainly correspond with the greater values of the other variable, and the same holds for the lesser values, the covariance is positive.
### Partial Correlation
In time series analysis, the PACF gives the partial correlation of a stationary time series with its own lagged values, regressed the values of the time series at all shorter lags. It contrasts with the autocorrelation function, which does not control for other lags.

acf(india, plot=F)
acf(india, type = 'covariance', plot=F)

acf(india, type = 'covariance', plot=F)
acf(india, type = 'covariance')

pacf(india, plot=F)
pacf(india)


# Removing Seasonality

timeseriesseasonallyadjusted <- india- india_components$seasonal
tsstationary <- diff(timeseriesseasonallyadjusted)
plot(tsstationary)




acf(tsstationary)
acf(tsstationary, type='covariance')
pacf(tsstationary)


Order specifies the non-seasonal part of the ARIMA model: (p, d, q) refers to the AutoRegression order, the degree of difference, and the MovingAverage order.

Seasonal specifies the seasonal part of the ARIMA model, plus the period (which defaults to frequency(x) i.e 12 in this case). This function requires a list with components order and period, but given a numeric vector of length 3, it turns them into a suitable list with the specification as the ‘order’.

Method refers to the fitting method, which can be ‘maximum likelihood (ML)’ or ‘minimize conditional sum-of-squares (CSS)’. The default is conditional-sum-of-squares.

fitARIMA <- arima(india, 
                  order=c(1,1,1), 
                  seasonal = list(order = c(1,0,0), period = 12), 
                  method="ML")
library(lmtest)
coeftest(fitARIMA)




library(forecast)


### Manually Configured ARIMA

confint(fitARIMA)
predict(fitARIMA, n.ahead = 20)
plot(forecast(fitARIMA, h = 20, level=c(99.5)))


### Automatic Configured ARIMA

auto = auto.arima(india, trace=TRUE)
confint(auto)
predict(auto, n.ahead = 20)
plot(forecast(auto, h = 20, level = 99.5))


## Diagnosing HyperParameter Configuration Issues

acf(fitARIMA$residuals)
library(FitAR)
boxresult = LjungBoxTest (fitARIMA$residuals,k=2,StartLag=1)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(fitARIMA$residuals)
qqline(fitARIMA$residuals)



