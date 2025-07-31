
#Initialize data
tempdata <- read.csv("Final.csv")

#Narrow data down to 'Square Enix' company
tempdata2 <- subset(tempdata, Company == "square enix")

#Narrow data down further, keeping only 'Date' and 'Adj.Close Price'
data <- tempdata2[,c("Date", "Adj.Close")]
data$Date <- as.Date(data$Date, format="%Y-%m-%d")

#Capture every 30th entry to resemble monthly stock price
monthly_data <- data[seq(1, nrow(data), by = 21), ]

#Transform csv data to a time series data type
ts_data <- ts(monthly_data$Adj.Close, start=c(2001,1), frequency = 12)



#Plot original data
plot(ts_data, xlab = "Time (Year)", ylab = "Stock Price (Japanese Yen)")
#Plot ACF and PACF
acf2(ts_data, 100)

#Take the log difference of TS data
datalog <- diff(log(ts_data))

#Plot the log difference of TS data
plot(datalog, xlab = "Time (Year)", ylab = "Stock Price (log difference)")

#Plot ACF and PACF of the log-difference data
acf2(datalog, 60)

adf.test(diff(log(ts_data), lag = 12))
acf2(diff(log(ts_data), lag = 12))

sarima(datalog, 1,0,5)
sarima_model <- sarima(datalog, 0,0,0, P=1, D=0, Q=1, S=12)

fit <- autoarfima(data = datalog, 
                  criterion = "AIC", method = "full")




library(forecast)



# Load necessary packages
library(forecast)

# Assuming ts_data is your time series object
# Plot the time series data
plot(ts_data, main = "Monthly Stock Prices", ylab = "Adjusted Close", xlab = "Time")

# Plot ACF and PACF
acf(ts_data, main = "ACF of Monthly Stock Prices")
pacf(ts_data, main = "PACF of Monthly Stock Prices")

# Seasonal decomposition
decomposed <- stl(diff(log(ts_data)), s.window = "periodic")
plot(decomposed)






























