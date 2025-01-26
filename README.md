# Create a time series object
        ts_revenue <- ts(Sales_revenue$REVENUE, start = c(2022, 10), frequency = 12)

# STL Decomposition
        stl_decomposed <- stl(ts_revenue, s.window = "periodic") # Decompose with seasonal adjustment
        print(stl_decomposed)
        plot(stl_decomposed)  

# Install and load necessary libraries
        install.packages("forecast", dependencies = TRUE)
        install.packages("tseries", dependencies = TRUE)
        library(forecast)
        library(tseries)

# Augmented Dickey-Fuller (ADF) Test
# First differencing
        ts_revenue_diff <- diff(ts_revenue, differences = 1) # Remove trend
        adf_result_diff <- adf.test(ts_revenue_diff)
        print(adf_result_diff)

# Second differencing if required (seasonal effects or remaining trends)
        if (adf_result_diff$p.value > 0.05) {
          ts_revenue_diff2 <- diff(ts_revenue_diff, differences = 1)
          adf_result_diff2 <- adf.test(ts_revenue_diff2)
          print(adf_result_diff2)
          ts_ready <- ts_revenue_diff2
        } else {
          ts_ready <- ts_revenue_diff
        }

# ACF and PACF plots
        acf(ts_ready, main = "ACF - Differenced Data")  # Check autocorrelation
        pacf(ts_ready, main = "PACF - Differenced Data")  # Check partial autocorrelation

# Fit SARIMA Model
# Use auto.arima to identify parameters or manually set based on ACF/PACF
        arima_model <- auto.arima(ts_revenue, seasonal = TRUE)
        summary(arima_model)

# Residual diagnostics
        checkresiduals(arima_model)

# Forecast
        forecast_arima <- forecast(arima_model, h = 12) # Forecast 12 periods ahead
        autoplot(forecast_arima) +
          ggtitle("SARIMA Forecast for Revenue") +
          xlab("Time") +
          ylab("Revenue")

# Save forecast to file
        write.csv(data.frame(forecast_arima), "forecast_revenue.csv", row.names = FALSE)
