#### -------------------- [Part : 1 - Forecasting Model] --------------- ####
# Read Data
PCdata <- read.csv("PCE.csv")

# Install and load Necessary Library
install.packages("ggplot2")
library(ggplot2)
install.packages("imputeTS")
library(imputeTS)
install.packages("forecast")
library(forecast)
install.packages("fpp")
library(fpp)

# Interpolate Dataset for Analysis
PC_DI <- na_interpolation(PCdata)


# Summary of Data
summary(PCdata)

# Background Color
par(bg = '#FFF5EE')

# Box-Plot
boxplot(PCdata$PCE, 
        ylab = "Personal Consumption Expenditure", 
        col = "#4682B4") 

# Data Points in Box-Plot
stripchart(PCdata$PCE,       
           method = "jitter", 
           pch = 19,          
           col = '#EE7600',   
           vertical = TRUE,   
           add = TRUE)        

## - - [ Checking for outliers ] - - ##
# Calculate quartiles
PC_DI <- na_interpolation(PCdata)
Q1 <- quantile(PC_DI$PCE, 0.25)
Q3 <- quantile(PC_DI$PCE, 0.75)

# Calculate IQR
IQR <- Q3 - Q1

# Calculate upper and lower bounds for outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Find outliers
outlier_indices <- which(PC_DI$PCE < lower_bound | PC_DI$PCE> upper_bound)
outlier_indices
### integer(0) ANS: no outliers

# Make Time Series
PC.ts <- ts(PCdata$PCE,start=c(1959, 1), end=c(2023, 11), frequency=12)
# Plot Time Series
plot(PC.ts)

# Distribution of missing values
ggplot_na_distribution(PC.ts)  + labs(x = "Monthly Observations : 01/01/1959 - 01/11/2023", y = "PCE", caption = "Red line shows the missing values in the data")       # Change plot background                            
+ theme(plot.background = element_rect(fill = "#FFF5EE", color = "pink"))

# Gap size in the dataset
ggplot_na_gapsize(PC.ts)  +        # Change plot background                            
  theme(plot.background = element_rect(fill = "#FFF5EE", color = "pink"))


# Imputations for Missing Data Points
PC_SI <- na_interpolation(PC.ts) # Simple Interpolation
PC_MA <- na_ma(PC.ts, k=12, weighting = "linear") # Moving Average
PC_MAW <- na_ma(PC.ts, k=12, weighting = "exponential") # Moving Average Weighted
PC_KL <- na_kalman(PC.ts) # Kalman
PC_KA <- na_kalman(PC.ts, model="auto.arima") # Kalman using Arima

# Testing All Methods 
Mtest <- cbind(PC.ts, PC_SI, PC_MA, PC_MAW, PC_KL, PC_KA)
Mtest

#### --- [ Simple Interpolation Selected : To be used for Analysis ] --- ####

# Imputed Values Plot
ggplot_na_imputations(PC.ts, PC_SI)   + # Change plot background         
  theme(plot.background = element_rect(fill = "#FFF5EE", color = "pink"))

# Plot PC_SI
tsdisplay(PC_SI) 

# Plot Season Plots
ggseasonplot(PC_SI) + labs(title = "Season-Plot by Months", x = "Month", y = "PCE")

# Plot Season Plots [POLAR] 
ggseasonplot(PC.ts, polar = TRUE) + 
  labs(title = "Season-Plot by Months | Polar = TRUE", 
       x = "Month", y = "PCE")

# Plot with Months : Subseries [ Check for Seasonality ]
ggsubseriesplot(PC_SI) + 
  labs(title = "Subseries-Plot", x = "Month", y = "PCE") + theme(plot.background = element_rect(fill = "#FFF5EE", color = "pink"))

# Subset to check Seasonality
PCA <- window(PC_SI, start = 1959, end = 1964)
plot(PCA)

# Autocorrelation Plot 
ggAcf(PC_SI) + ggtitle("Autocorrelation") + 
  theme(plot.background = element_rect(fill = "#FFF5EE", color = "pink"))

##### **************** [ Forecasting and Model Training ] **************** #####

#### ---------------- [ PART I using Simple Forecasting ] ---------------- ####

## - - [Method Used : Drift Method] - - ##
fd <- rwf(PC_SI, h = 40, drift = TRUE)
autoplot(fd)+ 
  labs(subtitle = "Model Type : Simple Forecasting | Method Type : Drift", 
       caption = "Forecast : Next 40 Months", 
       x = "Year", y = "PCE") + 
  theme(plot.background = element_rect(fill = "#FFF5EE", color = "pink"))

# - Summary - #
summary(fd)

# - Train Set : To be used for all Models - #
train <- subset(PC_SI, end = 620)

# Train and Test Drift Method
test_d <- rwf(train, h = 159, drift = TRUE)
# Check for accuracy
accuracy(test_d, PC_SI)  
autoplot(test_d, col=3) + autolayer(PC_SI) + 
  labs(title = "Performace-Plot - Trained Model | Split Ratio - 80:20 ", 
       x = "Year", y = "PCE", 
       caption = "PC_SI: Imputed Time Series", 
       subtitle = "Model Type : Simple Forecasting | Method Type : Drift") + 
  theme(plot.background = element_rect(fill = "#FFF5EE", color = "pink"))

# - Forecasting for November - #
summary(forecast(fd, h=11))

#### --------- [ PART II using Simple Exponential Smoothening ] --------- ####

# - Method Used : Holt Method - #
fh <- holt(PC_SI, h = 40)
autoplot(fh)+ 
  labs(subtitle = "Model Type : Exponential Smoothening | Method Type : Holt's", 
       caption = "Forecast : Next 40 Months", 
       x = "Year", y = "PCE") + 
  theme(plot.background = element_rect(fill = "#FFF5EE", color = "pink"))
summary(fh)
# -  Train and Test Holt's Method - #
test_h <- holt(train, h = 159)
accuracy(test_h, PC_SI)
autoplot(test_h) + autolayer(PC_SI) +
  labs(title = "Performace-Plot - Trained Model | Split Ratio - 80:20 ", 
       x = "Year", y = "PCE", 
       subtitle = "Model Type : Exponential Smoothening | Method Type : Holt's", 
       caption = "PC_SI: Imputed Time Series") + 
  theme(plot.background = element_rect(fill = "#FFF5EE", color = "pink"))

# - Forecasting for November - #
summary(forecast(fh, h=11))


#### ------------------- [ PART III using ARIMA MODEL ] ------------------ ####

# - Method Used: auto.arima() - #
# - Forecast for next 40 periods - #
fa <- auto.arima(PC_SI)
# - Plot Arima Plot - #
autoplot(forecast(fa, h =40))+ 
  labs(x = "Year", y = "PCE", 
       subtitle = "Model Type : Arima | Method Type : Auto Arima", 
       caption = "Forecast : Next 40 Months" ) + 
  theme(plot.background = element_rect(fill = "#FFF5EE", color = "pink"))

# - Train Arima - #
train_a <- auto.arima(train)
summary(train_a)
# - Testing Model - #
test_a <- forecast(train_a, h = 159)
autoplot(forecast(train_a, h = 159)) + autolayer(PC_SI) +
  labs(title = "Performace-Plot - Trained Model | Split Ratio - 80:20",
       x = "Year", y = "PCE",
       subtitle = "Model Type : Arima | Method Type : Auto Arima",
       caption = "PC_SI: Imputed Time Series") + 
  theme(plot.background = element_rect(fill = "#FFF5EE", color = "pink"))
# - Checking Accuracy - #
accuracy(test_a, PC_SI)
# - Checking Residuals - #
checkresiduals(test_a)
# - Forecasting for November - #
summary(forecast(fa, h=11))

#### -------------------------------------------------------------------- ####
# - [ Plot all models using this ] - #

plot(PC_SI, plot.conf=FALSE, main = "Forecasts for PCA", 
     ylab= "PCE", xlab = "Year") 
lines(test_a$mean, col=4, lwd = 2)
lines(test_h$mean, col=2, lwd = 2)
lines(test_d$mean, col=3, lwd = 2)
legend("topleft", lty=1, col=c(4,2,3), 
       legend=c("Arima Model", "Holt's Model", 
                "Drift Model"))

#### -------------------------------------------------------------------- ####
# - [ One Step Ahead Rolling Forecast Without Re-estimation ] - #

# - [ Train Set for One Step Ahead Rolling Forecast ] - #
train_roll <- window(PC_SI,end=1959.99)

# - [ Drift Model ] - #
fit_roll_d <- rwf(train_roll)
refit_roll_d <- rwf(PC_SI, model=fit_roll_d)
rfd <- window(fitted(refit_roll_d), start=1960)
accuracy(rfd, PC_SI)

# - [ Holt's Model ] - #
fit_roll_h <- holt(train_roll)
refit_roll_h <- holt(PC_SI, model=fit_roll_h)
rfh <- window(fitted(refit_roll_h), start=1960)
accuracy(rfh, PC_SI)

# - [ Arima Model ] - #
train_roll <- window(PC_SI,end=1959.99)
fit_roll <- auto.arima(train_roll)
refit_roll <- Arima(PC_SI, model=fit_roll)
rfa <- window(fitted(refit_roll), start=1960)
accuracy(rfa, PC_SI)
