install.packages("KFAS")
install.packages("zoo")
install.packages("FinTS")
install.packages("e1071")
install.packages("rugarch")
install.packages("forecast")
library(KFAS)
library(xts)
library(rugarch)
library("fGarch")
library("zoo")
library("FinTS")
library("e1071")
library(tseries)
library(forecast)

tempdata <- read.csv("Amazon_Stock_Data.csv")
tempdata$Price <- as.Date(tempdata$Price, format="%Y-%m-%d")
tempdata <- tempdata[-c(1, 2), ]

plot(tempdata$Price, tempdata$Close, col='red', type = "l", lwd = 1, ylab = "Y Values", xlab = "X Values")
lines(tempdata$Price, tempdata$Open, col='black')
lines(tempdata$Price, tempdata$Low, col='purple')
lines(tempdata$Price, tempdata$High, col='green')
lines(tempdata$Price, tempdata$Volume, col='blue')

tempdata2 <- tempdata[,c("Price", "Close")]
tempdata2$Close <- as.numeric(tempdata2$Close)

adf.test(tempdata2$Close)

arimafit <- auto.arima(tempdata2$Close)
summary(arimafit)

# Forecast the next 30 days
forecasted_values <- forecast(arimafit, h = 90)

# Plot the forecasted values
plot(forecasted_values, main = "30-Day Stock Price Forecast")

# Check the residuals
checkresiduals(arimafit)

# You can also calculate accuracy (if you have a test set)
accuracy(forecasted_values)


arima_fit <- auto.arima(tempdata2$Close)

# Get the fitted values
fitted_values <- fitted(arima_fit)

# Plot the original data
plot(tempdata2$Price, tempdata2$Close, type = "l", col = "blue", lwd = 2,
     xlab = "Date", ylab = "Stock Price", main = "Original Data with Fitted Model")

# Add the fitted values
lines(tempdata2$Price, fitted_values, col = "red", lwd = 2)

# Add a legend
legend("topleft", legend = c("Original Data", "Fitted Model"),
       col = c("blue", "red"), lty = 1, lwd = 2)





split_ratio <- 0.7
split_index <- floor(nrow(tempdata2) * split_ratio)

# Split data into training and testing sets
train_data <- tempdata2[1:split_index, ]
test_data <- tempdata2[(split_index + 1):nrow(tempdata2), ]

# Fit ARIMA model on training data
arima_fit <- auto.arima(train_data$Close)

# Forecast for the testing period
forecast_length <- nrow(test_data)
forecasted_values <- forecast(arima_fit, h = forecast_length)
plot(forecasted_values)
predicted_values <- forecasted_values$mean

# Evaluate model performance
actual_values <- test_data$Close
mae <- mean(abs(actual_values - predicted_values))
rmse <- sqrt(mean((actual_values - predicted_values)^2))
mape <- mean(abs((actual_values - predicted_values) / actual_values)) * 100

# Print performance metrics
cat("MAE:", mae, "\n")
cat("RMSE:", rmse, "\n")
cat("MAPE:", mape, "%\n")

# Plot data
plot(tempdata2$Price, tempdata2$Close, type = "l", col = "blue", lwd = 2,
     xlab = "Date", ylab = "Stock Price", main = "Training, Testing, and Forecast")
abline(v = tempdata2$Price[split_index], col = "black", lty = 2)

lines(tempdata2$Price[1:split_index], tempdata2$Close[1:split_index], col = "green", lwd = 2)
lines(tempdata2$Price[(split_index + 1):nrow(tempdata2)], tempdata2$Close[(split_index + 1):nrow(tempdata2)], col = "yellow", lwd = 2)
lines(tempdata2$Price[(split_index + 1):nrow(tempdata2)], predicted_values, col = "red", lwd = 2)
lines(tempdata2$Price[(split_index + 1):nrow(tempdata2)], forecasted_values, col = "brown", lwd = 2)

legend("topleft", legend = c("Training Data", "Testing Data", "Forecast"),
       col = c("green", "yellow", "red"), lty = 1, lwd = 2)






tempdata <- read.csv("Amazon_Stock_Data.csv")

# Ensure columns are in proper formats
tempdata$Price <- as.Date(tempdata$Price, format = "%Y-%m-%d")
tempdata$Close <- as.numeric(tempdata$Close)
tempdata$Volume <- as.numeric(tempdata$Volume)

# Step 2: Split the Data into Training and Testing Sets
train_size <- floor(0.74 * nrow(tempdata))  # 80% for training
train_data <- tempdata[1:train_size, ]
train_data <- train_data[,c("Price", "Close", "Volume")]
test_data <- tempdata[(train_size + 1):nrow(tempdata), ]
test_data <- test_data[,c("Price", "Close", "Volume")]

# Exogenous variables (Trading Volume)
train_volume <- train_data$Volume
test_volume <- test_data$Volume

# Step 3: Fit ARIMAX Model
arimax_fit <- auto.arima(train_data$Close, xreg = train_volume)

# Display model summary
summary(arimax_fit)

# Step 4: Forecast with ARIMAX Model
forecast_length <- nrow(test_data)  # Forecast for the test set length
forecasted_values <- forecast(arimax_fit, h = forecast_length, xreg = test_volume)

# Step 5: Visualize Actual vs Forecasted Values
plot(tempdata$Price, tempdata$Close, type = "l", col = "blue", lwd = 2,
     xlab = "Date", ylab = "Stock Price", main = "Actual vs Forecasted Stock Prices")
lines(test_data$Price, forecasted_values$mean, col = "red", lwd = 2)  # Forecasted values
legend("topleft", legend = c("Actual", "Forecasted"), col = c("blue", "red"), lty = 1, lwd = 2)

# Step 6: Evaluate the Model
# Calculate residuals (difference between actual and forecasted)
residuals <- test_data$Close - forecasted_values$mean

# Metrics
MAE <- mean(abs(residuals))            # Mean Absolute Error
RMSE <- sqrt(mean(residuals^2))        # Root Mean Squared Error
MAPE <- mean(abs(residuals / test_data$Close)) * 100  # Mean Absolute Percentage Error

# Print evaluation metrics
cat("Evaluation Metrics:\n")
cat("MAE:", MAE, "\n")
cat("RMSE:", RMSE, "\n")
cat("MAPE:", MAPE, "%\n")

# Optional: Check Residuals for White Noise
checkresiduals(arimax_fit)

