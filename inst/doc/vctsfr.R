## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  fig.width = 6,
  fig.heigth = 2,
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(vctsfr)
plot_ts(USAccDeaths) # plotting a time series

## -----------------------------------------------------------------------------
plot_ts(USAccDeaths, sdp = FALSE)

## -----------------------------------------------------------------------------
library(forecast)
ets_fit <- ets(USAccDeaths)
ets_f <- forecast(ets_fit, h = 12)
plot_ts(USAccDeaths, prediction = ets_f$mean, method = "ets")

## -----------------------------------------------------------------------------
library(forecast)
ets_fit <- ets(USAccDeaths)
ets_f <- forecast(ets_fit, h = 12, level = 90)
plot_ts(USAccDeaths, 
        prediction = ets_f$mean, 
        method = "ets", 
        lpi = ets_f$lower, 
        upi = ets_f$upper, 
        level = 90
)

## -----------------------------------------------------------------------------
timeS <- window(USAccDeaths, end = c(1977, 12))
fut <- window(USAccDeaths, start = c(1978, 1))
ets_fit <- ets(timeS)
ets_f <- forecast(ets_fit, h = length(fut), level = 80)
plot_ts(timeS, 
        future = fut, 
        prediction = ets_f$mean, 
        method = "ets",
        lpi = ets_f$lower, 
        upi = ets_f$upper, 
        level = 80
)

## -----------------------------------------------------------------------------
timeS <- window(USAccDeaths, end = c(1977, 12)) # historical values
fut <- window(USAccDeaths, start = c(1978, 1))  # "future" values
ets_fit <- ets(timeS)                           # exponential smoothing fit
ets_f <- forecast(ets_fit, h = length(fut))     # exponential smoothing forecast
arima_fit <- auto.arima(timeS)                  # ARIMA fit
arima_f <- forecast(arima_fit, h = length(fut)) # ARIMA forecast
plot_predictions(timeS, future = fut, 
                 predictions = list(ets = ets_f$mean, arima = arima_f$mean)
)

## -----------------------------------------------------------------------------
collection1 <- list(ts_info(USAccDeaths), ts_info(UKDriverDeaths))

## -----------------------------------------------------------------------------
library(Mcomp)

# select the industry, quarterly series from M1 competition (18 series)
M1_quarterly <- subset(M1, 4, "industry")

# build the collection
collection2 <- vector("list", length = length(M1_quarterly))
for (ind in seq_along(M1_quarterly)) {
  timeS   <- M1_quarterly[[ind]]$x              # time series
  name    <- M1_quarterly[[ind]]$st             # time series's name
  fut     <- M1_quarterly[[ind]]$xx             # future values
  ets_fit <- ets(timeS)                         # ES fit
  ets_for <- forecast(ets_fit, h = length(fut)) # ES forecast
  collection2[[ind]] <- ts_info(timeS,
                                prediction_info("ets", ets_for$mean),
                                future = fut,
                                name = name
  )
}

## -----------------------------------------------------------------------------
collection3 <- vector("list", length = length(M1_quarterly))
for (ind in seq_along(M1_quarterly)) {
  t <- M1_quarterly[[ind]]$x                            # time series
  name <- M1_quarterly[[ind]]$st                        # time series's name
  f <- M1_quarterly[[ind]]$xx                           # "future" values
  ets_fit <- ets(t)                                     # ES fit
  ets_f <- forecast(ets_fit, h = length(f), level = 90) # ES forecast
  arima_fit <- auto.arima(t)                            # ARIMA fit
  arima_f <- forecast(arima_fit, h = length(f),         # ARIMA forecast
                      level = c(80, 90)
  )
  collection3[[ind]] <- ts_info(t,
                               future = f,
                               prediction_info("ets", 
                                               ets_f$mean, 
                                               pi_info(90, 
                                                       ets_f$lower, 
                                                       ets_f$upper)
                               ),
                               prediction_info("arima", 
                                               arima_f$mean,
                                               pi_info(80, 
                                                       arima_f$lower[, 1], 
                                                       arima_f$upper[, 1]
                                               ),
                                               pi_info(90, 
                                                       arima_f$lower[, 2], 
                                                       arima_f$upper[, 2]
                                               )
                               ),
                               name = name)
}

## -----------------------------------------------------------------------------
plot_collection(collection3, number = 3)

## -----------------------------------------------------------------------------
plot_collection(collection3, number = 3, methods = "ets")

## -----------------------------------------------------------------------------
plot_collection(collection3, number = 3, methods = "arima", level = 90)

## ---- eval=FALSE--------------------------------------------------------------
#  GUI_collection(collection3)

