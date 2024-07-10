rm(list=ls()) # Clean R's memory!

library(TSA)
library(ggplot2)


# Read csv file in
stock_df <- read.csv("assignment1Data2024.csv", header = TRUE)
plot(stock_df, xlab = "Day", ylab = "Stock Price", type = "l")

ggplot(stock_df, aes(x = X, y = x)) +
  geom_line(color = "#4185a4") +
  labs(x = "Day", y = "Stock Price") +
  ggtitle("Stock Price Over Time") +
  theme_minimal()  +
  theme(plot.title = element_text(hjust = 0.5))

# make a vector of days
days <- c("1", "2", "3", "4", "5") # "6", "7", "8", "9", "10", "11", "12", "13")

# add days in dataframe
stock_df$days <- days[(seq_len(nrow(stock_df)) - 1) %% length(days) + 1]

stock_df

# check seasonality with a lag of 60
acf(stock_df$x, lag.max = 60, main = "ACF plot for Stock Data Frame")

stock_ts <- ts(stock_df$x,frequency =13)
stock_ts

summary(stock_ts)

plot(stock_ts, xlab = "Day", ylab = "Stock Price", type = "o",
     main = "Time series plot of stock prices")


# Add points representing days of the week to the plot
points(y = stock_ts, x = time(stock_ts),
       pch = as.vector(days[(time(stock_ts)- 1) %% length(days) + 1]))

# test for stationarity 
adf_result <- adf.test(stock_ts)
print(adf_result)


# correlation
stock_price1 = stock_ts
lagged_stock_price1 = zlag(stock_ts)
index = 2:length(lagged_stock_price1) # Get rid of first NA value
cor(stock_price1[index],lagged_stock_price1[index])

plot(stock_price1[index],lagged_stock_price1[index],ylab='Price', xlab='The first lag of stock price', main = "Correlation plot of first lag stock price")

stock_price2 = stock_ts
lagged_stock_price2 = zlag(zlag(stock_ts))
index = 3:length(lagged_stock_price2) # Get rid of first NA value
cor(stock_price2[index],lagged_stock_price2[index])


plot(stock_price2[index],lagged_stock_price2[index],ylab='Price', xlab='The second lag of stock price', main = "Correlation plot of second lag stock price")

# model one linear regression:
t <- time(stock_ts)
linear_model = lm(stock_ts ~ t) # label the linear trend model as model1
summary(linear_model) # t is positive so every month increase of 2.811 dollars. significant 

plot(stock_ts,ylab='StockPrice',xlab='Day',type='o',
     main = "Time Series plot for the stock price series")
abline(linear_model) # add the fitted trend line

# Residual analysis
res.linear_model = rstudent(linear_model)
par(mfrow=c(2,2))
plot(y = res.linear_model, x = as.vector(time(stock_ts)),xlab = 'Time', ylab='Standardized Residuals',type='l',main = "Standardised residuals from linear model.")
hist(res.linear_model,xlab='Standardized Residuals', main = "Histogram of standardised residuals.")
qqnorm(y=res.linear_model, main = "QQ plot of standardised residuals.")
qqline(y=res.linear_model, col = 2, lwd = 1, lty = 2)
shapiro.test(res.linear_model) # reject null hypothesis that residuals are normal
acf(res.linear_model, main = "ACF of standardized residuals.") 
par(mfrow=c(1,1))



# model 2 quadratic
t = time(stock_ts)
t2 = t^2
quadratic_model = lm(stock_ts ~ t + t2)
summary(quadratic_model)



plot(ts(fitted(quadratic_model)), ylab='Stock Price', xlab = 'Days', main = "Fitted Quadratic Curve to Stock Price series.",  col = "blue",
     ylim = c(min(c(fitted(quadratic_model), as.vector(stock_ts))) ,
              max(c(fitted(quadratic_model), as.vector(stock_ts)))
     ) ) +
  lines(as.vector(stock_ts),type="o")

# Residual analysis
res.quadratic_model = rstudent(quadratic_model)
par(mfrow=c(2,2))
plot(y = res.quadratic_model, x = as.vector(time(stock_ts)),xlab = 'Time', ylab='Standardized Residuals',type='l',main = "Standardised residuals from linear model.")
hist(res.quadratic_model,xlab='Standardized Residuals', main = "Histogram of standardised residuals.")
qqnorm(y=res.quadratic_model, main = "QQ plot of standardised residuals.")
qqline(y=res.quadratic_model, col = 2, lwd = 1, lty = 2)
shapiro.test(res.quadratic_model) # reject null hypothesis that residuals are normal
acf(res.quadratic_model, main = "ACF of standardized residuals.") 
par(mfrow=c(1,1))

# model 3 Seasonal 

day. = season(stock_ts)
seasonal_model= lm(stock_ts ~ day. -1)
summary(seasonal_model)


model3.1=lm(stock_ts ~ day.)
summary(model3.1)

plot(ts(fitted(seasonal_model)), ylab='y', main = "Fitted Seasonal Model to Daily Stock Series.",
     ylim = c(min(c(fitted(seasonal_model), as.vector(stock_ts))) ,
              max(c(fitted(seasonal_model), as.vector(stock_ts)))
     ), col = "blue" ) +
lines(as.vector(stock_ts),type="o")


# residual analysis
res.seasonal_model = rstudent(seasonal_model)
par(mfrow=c(2,2))
plot(y = res.seasonal_model, x = as.vector(time(stock_ts)),xlab = 'Time', ylab='Standardized Residuals',type='l',main = "Standardised residuals from seasonal model.")
points(y=res.seasonal_model,x=time(stock_ts), pch = as.vector(days[(time(stock_ts) - 1) %% length(days) + 1]))
hist(res.seasonal_model,xlab='Standardized Residuals', main = "Histogram of standardised residuals.")
qqnorm(y=res.seasonal_model, main = "QQ plot of standardised residuals.")
qqline(y=res.seasonal_model, col = 2, lwd = 1, lty = 2)
shapiro.test(res.seasonal_model)
acf(res.seasonal_model, main = "ACF of standardized residuals.")
par(mfrow=c(1,1))

pacf(res.seasonal_model, main = "PACF of standardized residuals.")


# cyclic Model cos + sine
har. <- harmonic(stock_ts, 1) # calculate cos(2*pi*t) and sin(2*pi*t)
stock <- data.frame(stock_ts,har.)
cyclic <- lm(stock_ts ~ cos.2.pi.t. + sin.2.pi.t. , data = stock)
summary(cyclic)

plot(ts(fitted(cyclic)), ylab='y', main = "Fitted cosine wave to daily stock price series.",
     ylim = c(min(c(fitted(cyclic), as.vector(stock_ts))) ,
              max(c(fitted(cyclic), as.vector(stock_ts)))
     ), col = "blue" )
lines(as.vector(stock_ts),type="o")


# residual analysis
res.cyclic = rstudent(cyclic)
par(mfrow=c(2,2))
plot(y = res.cyclic, x = as.vector(time(stock_ts)),xlab = 'Time', ylab='Standardized Residuals',type='l',main = "Standardised residuals from cyclic model.")
points(y=res.cyclic,x=time(stock_ts), pch = as.vector(days[(time(stock_ts) - 1) %% length(days) + 1]))
hist(res.cyclic,xlab='Standardized Residuals', main = "Histogram of standardised residuals.")
qqnorm(y=res.cyclic, main = "QQ plot of standardised residuals.")
qqline(y=res.cyclic, col = 2, lwd = 1, lty = 2)
shapiro.test(res.cyclic)
acf(res.cyclic, main = "ACF of standardized residuals.")
par(mfrow=c(1,1))

pacf(res.cyclic, main = "PACF of standardized residuals.")

# model 5 combined seasonal with cosine
combined_model = lm(stock_ts~ day. + t + t2 -1) 
summary(combined_model)

fitted.combined_model <- fitted(combined_model)

plot(ts(fitted(combined_model)), ylim = c(min(c(fitted(combined_model), as.vector(stock_ts))), max(c(fitted(model5),as.vector(stock_ts)))),
     ylab='y' , main = "Fitted quadratic + Seasonal Model to Stock Price Series", type="l",lty=2,col="blue")
lines(as.vector(stock_ts),type="o")

res.combined_model = rstudent(combined_model)
par(mfrow=c(2,2))
plot(y = res.combined_model, x = as.vector(time(stock_ts)),xlab = 'Time', ylab='Standardized Residuals',type='l',main = "Standardised residuals from quadratic model.")
points(y=res.combined_model,x=time(stock_ts), pch = as.vector(days[(time(stock_ts) - 1) %% length(days) + 1]))
hist(res.combined_model,xlab='Standardized Residuals', main = "Histogram of standardised residuals.")
qqnorm(y=res.combined_model, main = "QQ plot of standardised residuals.")
qqline(y=res.combined_model, col = 2, lwd = 1, lty = 2)
shapiro.test(res.combined_model)
acf(res.combined_model, main = "ACF of standardized residuals.",lag.max = 60)
par(mfrow=c(1,1))

pacf(res.combined_model, main = "PACF of standardized residuals.")



# prediction
day_forecast <- 5 # 5 day forecast
t <- time(stock_ts)
dataFreq <- frequency(stock_ts)
last_time_point <- t[length(t)]
aheadTimes <- data.frame(day. = c("Season-11", "Season-12", "Season-13", "Season-1", "Season-2"),
                         t = seq(last_time_point+(1/dataFreq), last_time_point+day_forecast*(1/dataFreq), 1/dataFreq),
                         t2 =  seq(last_time_point+(1/dataFreq), last_time_point+day_forecast*(1/dataFreq), 1/dataFreq)^2) 

frc_combined_model <- predict(combined_model, newdata = aheadTimes, interval = "prediction")

plot(stock_ts, xlim= c(t[1],aheadTimes$t[nrow(aheadTimes)]), ylim = c(-50,250), ylab = "Stock Price Series", xlab = "Time",
     main = "Forecasts from the combined quadratic and seasonal model fitted to the  stock price series")
lines(ts(fitted.combined_model,start = t[1],frequency = dataFreq), col = "green") #  Alternatively you can use abline(model1)
lines(ts(as.vector(frc_combined_model[,3]), start = aheadTimes$t[1],frequency = dataFreq), col="blue", type="l")
lines(ts(as.vector(frc_combined_model[,1]), start = aheadTimes$t[1],frequency = dataFreq), col="red", type="l")
lines(ts(as.vector(frc_combined_model[,2]), start = aheadTimes$t[1],frequency = dataFreq), col="blue", type="l")
legend("topleft", lty=1, pch=1, col=c("black","blue","red"), 
       c("Data","5% forecast limits", "Forecasts"))

