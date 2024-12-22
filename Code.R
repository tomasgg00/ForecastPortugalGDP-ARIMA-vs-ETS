### 1 - Import Libraries
library(tseries)
library(forecast)
library(urca)
library(xts)
library(fpp3)
library(AER)
library(strucchange)
library(tsibble)
library(dplyr)
library(ggplot2)
library(feasts)

## Set the seed for reproducibility
set.seed(1234)

## Set path: personalize your working directory 
path <- 'C:\\Users\\tomas\\OneDrive\\Desktop\\CBS\\2nd Semester\\Predictive Analytics\\FinalExam2'
setwd(path)
getwd()



### 2 - Data Exploration
### 2.1 - Load Data 
pt <- read.csv("PortugalRealGDP.csv", sep = ",")
pt <- pt %>%
  mutate(DATE = as.Date(DATE, format="%m/%d/%Y"),
         date = yearquarter(DATE)) %>%
  select(-DATE) %>%  # Keep 'date' and remove 'DATE'
  mutate(gdp = GDP) %>%  # Create 'gdp' column from 'GDP'
  select(-GDP) %>%  # Remove the original 'GDP' column
  as_tsibble(index = date)

### 2.2 - Check Data
print(pt)

### 2.3 - Check Time Series
autoplot(pt, gdp) + ggtitle("Portugal Real GDP Time Series")

### 2.4 - Verify Missing Values
print(sum(is.na(pt)))
#A: 0 Missing Values

### 2.5 - Verify Outliers
# Using the IQR method to detect outliers
Q1 <- quantile(pt$gdp, 0.25)
Q3 <- quantile(pt$gdp, 0.75)
IQR <- Q3 - Q1

# Define outlier boundaries
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Identify outliers
outliers <- pt %>%
  filter(gdp < lower_bound | gdp > upper_bound)

cat("Number of outliers: ", nrow(outliers), "\n")

# Print outliers
print(outliers)

# Visualize outliers using boxplot
boxplot(pt$gdp, main = "Boxplot of Portugal Real GDP", ylab = "GDP", col = "lightblue")

# Mark outliers on the time series plot
autoplot(pt, gdp) + 
  geom_point(data = outliers, aes(x = date, y = gdp), color = "red", size = 2) +
  ggtitle("Portugal Real GDP Time Series with Outliers")



### 3 - Plots for preliminary analysis
# Plot 1: Original time series
autoplot(pt, gdp) + ggtitle("Portugal Real GDP Time Series")

# Plot 2: ACF of the original data
pt %>% ACF(gdp) %>% autoplot() + ggtitle("ACF of Portugal Real GDP")

# Plot 3: PACF of the original data
pt %>% PACF(gdp) %>% autoplot() + ggtitle("PACF of Portugal Real GDP")

# Plot 4: Seasonal plot
pt %>%
  mutate(quarter = quarter(date)) %>%
  ggplot(aes(x = year(date), y = gdp, color = factor(quarter))) +
  geom_line() +
  labs(title = "Seasonal Plot", x = "Year", y = "GDP", color = "Quarter") +
  theme_minimal() +
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(~ quarter, scales = "free_y", ncol = 1)

# Plot 5: Seasonal Plot 2 (Subseries)
gg_subseries(pt, y = gdp) + 
  ggtitle("Seasonal plot 2")

# Plot 6: Lag plot of the original data
gg_lag(pt, gdp, do.lines = FALSE) + 
  ggtitle("Lag Plot")

# Plot 7: SEATS Decomposition
seats_dcmp <- pt %>%
  model(seats = X_13ARIMA_SEATS(gdp ~ x11())) %>%
  components()
autoplot(seats_dcmp) +
  labs(title = "Decomposition of Real Portugal GDP using SEATS")

# Plot 8: STL Decomposition
stl_dcmp <- pt %>%
  model(
    STL(gdp ~ trend(window = 7) +
          season(window = "periodic"),
        robust = TRUE)
  ) %>%
  components()

# Plotting the STL decomposition
autoplot(stl_dcmp) +
  labs(title = "STL Decomposition of Real Portugal GDP")

### 4 - Box-Cox Transformation
lambda <- pt |>
  features(gdp, features = guerrero) |>
  pull(lambda_guerrero)

print(lambda)

# Apply Box-Cox Transformation
pt <- pt %>%
  mutate(gdp_boxcox = box_cox(gdp, lambda = lambda))

# Print
print(pt)



### 5 - Plots for preliminary analysis with Box-Cox Transformation
# Plot 1: Original time series (Box-Cox Transformed)
autoplot(pt, gdp_boxcox) + ggtitle("Portugal Real GDP Time Series (Box-Cox Transformed)")

# Plot 2: ACF of the original data (Box-Cox Transformed)
pt %>% ACF(gdp_boxcox) %>% autoplot() + ggtitle("ACF of Portugal Real GDP (Box-Cox Transformed)")

# Plot 3: PACF of the original data (Box-Cox Transformed)
pt %>% PACF(gdp_boxcox) %>% autoplot() + ggtitle("PACF of Portugal Real GDP (Box-Cox Transformed)")

# Plot 4: Seasonal plot (Box-Cox Transformed)
pt %>%
  mutate(quarter = quarter(date)) %>%
  ggplot(aes(x = year(date), y = gdp_boxcox, color = factor(quarter))) +
  geom_line() +
  labs(title = "Seasonal Plot", x = "Year", y = "GDP", color = "Quarter") +
  theme_minimal() +
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(~ quarter, scales = "free_y", ncol = 1)

# Plot 5: Seasonal Plot 2 (Subseries, Box-Cox Transformed)
gg_subseries(pt, y = gdp_boxcox) + 
  ggtitle("Seasonal plot 2")

# Plot 6: Lag plot of the original data (Box-Cox Transformed)
gg_lag(pt, gdp_boxcox, do.lines = FALSE) + 
  ggtitle("Lag Plot")

# Plot 7: SEATS Decomposition (Box-Cox Transformed)
seats_dcmp <- pt %>%
  model(seats = X_13ARIMA_SEATS(gdp_boxcox ~ x11())) %>%
  components()
autoplot(seats_dcmp) +
  labs(title = "Decomposition of Real Portugal GDP using SEATS (Box-Cox Transformed)")

# Plot 8: STL Decomposition (Box-Cox Transformed)
stl_dcmp <- pt %>%
  model(
    STL(gdp_boxcox ~ trend(window = 7) +
          season(window = "periodic"),
        robust = TRUE)
  ) %>%
  components()

# Plotting the STL decomposition
autoplot(stl_dcmp) +
  labs(title = "STL Decomposition of Real Portugal GDP (Box-Cox Transformed)")



### 6 - Unit Root Tests on Box-Cox Transformed Data
# Test 1: Augmented Dickey-Fuller test with a trend
summary(ur.df(as.ts(pt$gdp_boxcox), type = 'trend', lag = 24, selectlags = 'AIC'))

# Test 2: KPSS test with a trend
summary(ur.kpss(as.ts(pt$gdp_boxcox), type = 'tau'))

# Test 3: Augmented Dickey-Fuller test with a drift
summary(ur.df(as.ts(pt$gdp_boxcox), type = 'drift', lag = 24, selectlags = 'AIC'))

# Test 4: KPSS test with a level
summary(ur.kpss(as.ts(pt$gdp_boxcox), type = 'mu'))

# Test 5: Augmented Dickey-Fuller test with none
summary(ur.df(as.ts(pt$gdp_boxcox), type = 'none', lag = 24, selectlags = 'AIC'))



### 7 - Perform Differencing on the Box-Cox Transformed GDP Data
pt <- pt %>%
  mutate(d_gdp_boxcox = difference(gdp_boxcox)) %>%
  filter(!is.na(d_gdp_boxcox))

# Check Differencing
print(pt)



### 8 - Unit Root Tests on Differenced Box-Cox Transformed Data
# Test 1: Augmented Dickey-Fuller test with a trend
summary(ur.df(as.ts(pt$d_gdp_boxcox), type = 'trend', lag = 24, selectlags = 'AIC'))

# Test 2: KPSS test with a trend
summary(ur.kpss(as.ts(pt$d_gdp_boxcox), type = 'tau'))

# Test 3: Augmented Dickey-Fuller test with a drift
summary(ur.df(as.ts(pt$d_gdp_boxcox), type = 'drift', lag = 24, selectlags = 'AIC'))

# Test 4: KPSS test with a level
summary(ur.kpss(as.ts(pt$d_gdp_boxcox), type = 'mu'))

# Test 5: Augmented Dickey-Fuller test with none
summary(ur.df(as.ts(pt$d_gdp_boxcox), type = 'none', lag = 24, selectlags = 'AIC'))



### 9 - Seasonality Analysis
#STL Decomposition on Box-Cox transformed and differenced data
stl_decomp <- pt %>% model(STL(d_gdp_boxcox ~ season(window = "periodic")))
components <- components(stl_decomp)

# Extract seasonal and remainder components
S_t <- components$season_year
R_t <- components$remainder

# Calculate the variance of seasonal and remainder components
var_Rt <- var(R_t, na.rm = TRUE)
var_St_Rt <- var(S_t + R_t, na.rm = TRUE)

# Calculate seasonal strength F_s
F_s <- max(0, 1 - var_Rt / var_St_Rt)

#Print
print(F_s)



### 10 - Structural Break Test
# Prepare the lagged values
pt.lag <- cbind(
  Lag0 = pt %>% select(d_gdp_boxcox) %>% filter(!is.na(d_gdp_boxcox)) %>% as.ts(),
  Lag1 = stats::lag(pt %>% select(d_gdp_boxcox) %>% filter(!is.na(d_gdp_boxcox)) %>% as.ts())
)

### 10.1 - QLR Test (Chow Test)
qlr <- Fstats(Lag0 ~ 1 + Lag1, data = pt.lag, from = 0.10)
plot(qlr, alpha = 0.1, main = "F Statistics")
test <- sctest(qlr, type = "supF")
print(test)
breaks <- breakpoints(qlr, alpha = 0.01)
print(breaks)
breakpoint_dates <- pt$date[breaks$breakpoints]
print(breakpoint_dates)
plot(qlr, alpha = 0.1, main = "F Statistics with Breakpoints")
lines(breakpoints(qlr))

# Create a data frame with d_gdp_boxcox and its lag
pt.df <- pt %>%
  mutate(Lag1 = lag(d_gdp_boxcox)) %>%
  filter(!is.na(Lag1)) %>%
  as.data.frame()

### 10.2 - OLS-MOSUM Test
mosum_test <- efp(d_gdp_boxcox ~ Lag1, type = "OLS-MOSUM", data = pt.df)
plot(mosum_test, main = "OLS-based MOSUM test")

### 10.3 - OLS-CUSUM Test
cusum_test <- efp(d_gdp_boxcox ~ Lag1, type = "OLS-CUSUM", data = pt.df)
plot(cusum_test, main = "OLS-based CUSUM test")

### 10.4 - SIS
sis <- isat(pt.df$d_gdp_boxcox, t.pval = 0.1)
print(sis)
plot(sis, main = "SIS without additional regressors (2)")




### 11 - Create Train and Test Sets
# Identify the break date
break_date <- yearquarter("2020 Q1")

# Split the data
train <- pt %>% filter(date < break_date)
test <- pt %>% filter(date >= break_date)

# Verify splits
print(train)
print(test)



### 12 - Unit Root Tests on Training Set
# Test 1: Augmented Dickey-Fuller test with a trend
summary(ur.df(as.ts(train$d_gdp_boxcox), type = 'trend', lag = 24, selectlags = 'AIC'))

# Test 2: KPSS test with a trend
summary(ur.kpss(as.ts(train$d_gdp_boxcox), type = 'tau'))

# Test 3: Augmented Dickey-Fuller test with a drift
summary(ur.df(as.ts(train$d_gdp_boxcox), type = 'drift', lag = 24, selectlags = 'AIC'))

# Test 4: KPSS test with a level
summary(ur.kpss(as.ts(train$d_gdp_boxcox), type = 'mu'))

# Test 5: Augmented Dickey-Fuller test with none
summary(ur.df(as.ts(train$d_gdp_boxcox), type = 'none', lag = 24, selectlags = 'AIC'))



### 13 - Apply Second Differencing On Trainning Set
train <- train %>%
  mutate(dd_gdp_boxcox = difference(d_gdp_boxcox)) %>%
  filter(!is.na(dd_gdp_boxcox))

# Print the train dataset to verify the changes
print(train)



### 14 - Unit Root Tests on Second Differenced Box-Cox Transformed Data
# Test 1: Augmented Dickey-Fuller test with a trend
summary(ur.df(as.ts(train$dd_gdp_boxcox), type = 'trend', lag = 24, selectlags = 'AIC'))

# Test 2: KPSS test with a trend
summary(ur.kpss(as.ts(train$dd_gdp_boxcox), type = 'tau'))

# Test 3: Augmented Dickey-Fuller test with a drift
summary(ur.df(as.ts(train$dd_gdp_boxcox), type = 'drift', lag = 24, selectlags = 'AIC'))

# Test 4: KPSS test with a level
summary(ur.kpss(as.ts(train$dd_gdp_boxcox), type = 'mu'))

# Test 5: Augmented Dickey-Fuller test with none
summary(ur.df(as.ts(train$dd_gdp_boxcox), type = 'none', lag = 24, selectlags = 'AIC'))



### 15 - Model Selection ARIMA
library(forecast)
library(tseries)
library(tidyverse)
library(tsibble)
library(gridExtra)

# tsibble format
pt <- pt %>%
  as_tsibble(index = date)

### 15.1 - Auto ARIMA without drift
auto_arima_model_no_drift <- train %>%
  model(auto_arima_no_drift = ARIMA(gdp ~ 0))

# Report the model
report(auto_arima_model_no_drift)

# Print the report
print(auto_arima_model_no_drift)
#output: ARIMA(1,1,1)(1,0,1)[4]

### 15.2 - Auto ARIMA with drift
auto_arima_model_drift <- train %>%
  model(auto_arima = ARIMA(gdp ~ 1))

# Report the model
report(auto_arima_model_drift)

# Print the report
print(auto_arima_model_drift)
#output: ARIMA(1,1,1)(1,0,1)[4]

### 15.3 - ACF and PACF
ACF_fd <- train %>%
  ACF(dd_gdp_boxcox, lag_max = 48) %>%
  autoplot() + labs(title = "ACF of Second Differenced GDP")

PACF_fd <- train %>%
  PACF(dd_gdp_boxcox, lag_max = 48) %>%
  autoplot() + labs(title = "PACF of Second Differenced GDP")

# Plot ACF and PACF using grid.arrange
grid.arrange(ACF_fd, PACF_fd, ncol=1)

fit_acf_pacf <- train %>%
  model(
    arima121 = ARIMA(gdp ~ pdq(1, 2, 1)(1,0,1)),
    arima221 = ARIMA(gdp ~ pdq(2, 2, 1)(1,0,1)),
  )

# Choose the best model based on AICc
best_acf_pacf <- fit_acf_pacf %>%
  glance() %>%
  arrange(AICc) %>%
  slice(1) %>%
  pull(.model)

# Print the best model
print(best_acf_pacf)
# output: ARIMA(2,2,1)(1,0,1)



### 16 - ARIMA
modelsg <- train %>%
  model(
    auto_arima_model_drift = ARIMA(gdp ~ 1),
    guessed_arima_1 = ARIMA(gdp ~ pdq(2, 2, 1) + PDQ(1, 0, 1, 4)),
    guessed_arima_2 = ARIMA(gdp ~ pdq(1, 2, 1) + PDQ(1, 0, 1, 4))
  )

# Display the accuracy metrics
print(accuracy_metrics)

# Model Summary
model_summary <- glance(modelsg) %>%
  arrange(AICc) %>%
  select(.model, AIC, AICc, BIC) %>%
  mutate(across(AIC:BIC, ~format(round(.x, 2), nsmall = 2)))

print(model_summary)

# Accuracy
accuracy_metrics_arima <- bind_rows(
  train %>%
    model(auto_arima_model_drift = ARIMA(gdp ~ 1)) %>%
    forecast(test) %>%
    fabletools::accuracy(test %>% select(gdp)),
  
  train %>%
    model(guessed_arima_1 = ARIMA(gdp ~ pdq(2, 2, 1) + PDQ(1, 0, 1, 4))) %>%
    forecast(test) %>%
    fabletools::accuracy(test %>% select(gdp)),
  
  train %>%
    model(guessed_arima_2 = ARIMA(gdp ~ pdq(1, 2, 1) + PDQ(1, 0, 1, 4))) %>%
    forecast(test) %>%
    fabletools::accuracy(test %>% select(gdp))
) %>%
  select(.model, RMSE, MAE, MAPE) %>%
  mutate(across(RMSE:MAPE, ~format(round(.x, 2), nsmall = 2)))

print(accuracy_metrics_arima)




### 17 - Residuals ARIMA
# Residual diagnostics for auto ARIMA W/ Drift model
modelsg %>% 
  select(auto_arima_model_drift) %>% 
  gg_tsresiduals(type = "innovation") +
  ggtitle("ARIMA (1,1,1) W/ Drift Model Residual Diagnostics")

# Residual diagnostics for Guessed ARIMA Model 1
modelsg %>% 
  select(guessed_arima_1) %>% 
  gg_tsresiduals(type = "innovation") +
  ggtitle("ARIMA (2,2,1)(1,0,1)[4] Residual Diagnostics")

# Residual diagnostics for Guessed ARIMA Model 2
modelsg %>% 
  select(guessed_arima_2) %>% 
  gg_tsresiduals(type = "innovation") +
  ggtitle("ARIMA (1,2,1)(1,0,1)[4] Residual Diagnostics")





### 18 - Ljung-Box Test ARIMA

# Ljung-Box test for auto ARIMA W/ Drift model
lb_test_auto_arima_drift <- augment(modelsg %>% select(auto_arima_model_drift)) %>%
  features(.resid, ljung_box, lag = 10, dof = 2)

# Ljung-Box test for Guessed ARIMA Model 1
lb_test_guessed_arima_1 <- augment(modelsg %>% select(guessed_arima_1)) %>%
  features(.resid, ljung_box, lag =10, dof = 2)

# Ljung-Box test for Guessed ARIMA Model 2
lb_test_guessed_arima_2 <- augment(modelsg %>% select(guessed_arima_2)) %>%
  features(.resid, ljung_box, lag = 10, dof = 2)

# Display Ljung-Box test results
print(lb_test_auto_arima_drift)
print(lb_test_guessed_arima_1)
print(lb_test_guessed_arima_2)





### 19 - Shapiro-Wilk Test ARIMA
# Shapiro-Wilk normality test for auto ARIMA W/ Drift model
shapiro_test_auto_arima_drift <- shapiro.test(modelsg %>%
                                                select(auto_arima_model_drift) %>%
                                                residuals() %>%
                                                select(.resid) %>%
                                                as.ts())

# Shapiro-Wilk normality test for Guessed ARIMA Model 1
shapiro_test_guessed_arima_1 <- shapiro.test(modelsg %>%
                                               select(guessed_arima_1) %>%
                                               residuals() %>%
                                               select(.resid) %>%
                                               as.ts())

# Shapiro-Wilk normality test for Guessed ARIMA Model 2
shapiro_test_guessed_arima_2 <- shapiro.test(modelsg %>%
                                               select(guessed_arima_2) %>%
                                               residuals() %>%
                                               select(.resid) %>%
                                               as.ts())

# Print the results
print(shapiro_test_auto_arima_drift)
print(shapiro_test_guessed_arima_1)
print(shapiro_test_guessed_arima_2)





### 20 - Forecasting ARIMA 
auto_arima_drift_forecast <- modelsg %>%
  select(auto_arima_model_drift) %>%
  forecast(h = 16)

guessed_arima_1_forecast <- modelsg %>%
  select(guessed_arima_1) %>%
  forecast(h = 16)

guessed_arima_2_forecast <- modelsg %>%
  select(guessed_arima_2) %>%
  forecast(h = 16)


# Plotting the forecasts along with actual values in the test set
autoplot(train, gdp) +
  autolayer(auto_arima_drift_forecast, series = "Auto ARIMA Forecast") +
  autolayer(test, series = "Actual Values") +
  ggtitle("ARIMA (1,1,1) W/ Drift Model Forecast vs Actual Values") +
  labs(x = "Year", y = "GDP") +
  theme_minimal()

autoplot(train, gdp) +
  autolayer(guessed_arima_1_forecast, series = "Guessed ARIMA 1 Forecast") +
  autolayer(test, series = "Actual Values") +
  ggtitle("Guessed ARIMA Model 1 Forecast vs Actual Values") +
  labs(x = "Year", y = "GDP") +
  theme_minimal()

autoplot(train, gdp) +
  autolayer(guessed_arima_2_forecast, series = "Guessed ARIMA 2 Forecast") +
  autolayer(test, series = "Actual Values") +
  ggtitle("Guessed ARIMA Model 2 Forecast vs Actual Values") +
  labs(x = "Year", y = "GDP") +
  theme_minimal()


## Print the forecast values for each model
print(auto_arima_forecast)
print(guessed_arima_1_forecast)
print(guessed_arima_2_forecast)





### 21 - Decomposition / Model Selection ETS
## 21.1 - STL on Training Set
stl_dcmp <- train %>%
  model(stl = STL(gdp ~ season(window = "periodic"))) %>%
  components()

autoplot(stl_dcmp) +
  labs(title = "Decomposition of Real Portugal GDP using STL (Training Data)")


## 21.2 - ACF
pt %>% ACF(gdp) %>% autoplot() + ggtitle("ACF of Portugal Real GDP")


## 21.3 - PACF
pt %>% PACF(gdp) %>% autoplot() + ggtitle("PACF of Portugal Real GDP")





### 22 - ETS
# Fit different ETS models to the training data
ets_models <- train %>%
  model(
    auto_ets = ETS(gdp),
    ets_AAN = ETS(gdp ~ error("A") + trend("A") + season("N")),
    ets_AAdN = ETS(gdp ~ error("A") + trend("Ad") + season("N")),
    ets_MAN = ETS(gdp ~ error("M") + trend("A") + season("N"))
  )

# Model Summary
model_summary_ets <- glance(ets_models) %>%
  arrange(AICc) %>%
  select(.model, AIC, AICc, BIC) %>%
  mutate(across(AIC:BIC, ~format(round(.x, 2), nsmall = 2)))

print(model_summary_ets)

# Accuracy 
accuracy_metrics_ets <- bind_rows(
  train %>%
    model(auto_ets = ETS(gdp)) %>%
    forecast(test) %>%
    fabletools::accuracy(test %>% select(gdp)),
  
  train %>%
    model(ets_AAN = ETS(gdp ~ error("A") + trend("A") + season("N"))) %>%
    forecast(test) %>%
    fabletools::accuracy(test %>% select(gdp)),
  
  train %>%
    model(ets_AAdN = ETS(gdp ~ error("A") + trend("Ad") + season("N"))) %>%
    forecast(test) %>%
    fabletools::accuracy(test %>% select(gdp)),
  
  train %>%
    model(ets_MAN = ETS(gdp ~ error("M") + trend("A") + season("N"))) %>%
    forecast(test) %>%
    fabletools::accuracy(test %>% select(gdp))
) %>%
  select(.model, RMSE, MAE, MAPE) %>%
  mutate(across(RMSE:MAPE, ~format(round(.x, 2), nsmall = 2)))

print(accuracy_metrics_ets)


# Fit the Auto ETS model
auto_ets_model <- train %>%
  model(auto_ets = ETS(gdp))

# Print the report
print(auto_ets_model)
#(M,Ad,N)





### 23 - Residuals ETS
# Residual diagnostics for Auto ETS model
ets_models %>% 
  select(auto_ets) %>% 
  gg_tsresiduals(type = "innovation") +
  ggtitle("ETS (M,Ad,N) Residual Diagnostics")

# Residual diagnostics for ETS AAN model
ets_models %>% 
  select(ets_AAN) %>% 
  gg_tsresiduals(type = "innovation") +
  ggtitle("ETS (A,A,N) Residual Diagnostics")

# Residual diagnostics for ETS AAdN model
ets_models %>% 
  select(ets_AAdN) %>% 
  gg_tsresiduals(type = "innovation") +
  ggtitle("ETS (A,Ad,N) Model Residual Diagnostics")

# Residual diagnostics for ETS MAN model
ets_models %>% 
  select(ets_MAN) %>% 
  gg_tsresiduals(type = "innovation") +
  ggtitle("ETS (M,A,N) Model Residual Diagnostics")





### 24 - Ljung-Box Test ETS
# Perform the Ljung-Box test for the Auto ETS model
lb_test_auto_ets <- augment(ets_models %>% select(auto_ets)) %>%
  features(.resid, ljung_box, lag = 10, dof = 2)

# Perform the Ljung-Box test for the ETS AAN model
lb_test_ets_AAN <- augment(ets_models %>% select(ets_AAN)) %>%
  features(.resid, ljung_box, lag = 10, dof = 2)

# Perform the Ljung-Box test for the ETS AAdN model
lb_test_ets_AAdN <- augment(ets_models %>% select(ets_AAdN)) %>%
  features(.resid, ljung_box, lag = 10, dof = 2)

# Perform the Ljung-Box test for the ETS MAN model
lb_test_ets_MAN <- augment(ets_models %>% select(ets_MAN)) %>%
  features(.resid, ljung_box, lag = 10, dof = 2)

# Display Ljung-Box test results
print(lb_test_auto_ets)
print(lb_test_ets_AAN)
print(lb_test_ets_AAdN)
print(lb_test_ets_MAN)




### 25 - Shapiro-Wilk Test ETS
# Auto ETS Model
shapiro_test_auto_ets <- shapiro.test(ets_models %>%
                                        select(auto_ets) %>%
                                        residuals() %>%
                                        select(.resid) %>%
                                        as.ts())

# ETS AAN Model
shapiro_test_ets_AAN <- shapiro.test(ets_models %>%
                                       select(ets_AAN) %>%
                                       residuals() %>%
                                       select(.resid) %>%
                                       as.ts())

# ETS AAdN Model
shapiro_test_ets_AAdN <- shapiro.test(ets_models %>%
                                        select(ets_AAdN) %>%
                                        residuals() %>%
                                        select(.resid) %>%
                                        as.ts())

# ETS MAN Model
shapiro_test_ets_MAN <- shapiro.test(ets_models %>%
                                       select(ets_MAN) %>%
                                       residuals() %>%
                                       select(.resid) %>%
                                       as.ts())

# Print the results
print(shapiro_test_auto_ets)
print(shapiro_test_ets_AAN)
print(shapiro_test_ets_AAdN)
print(shapiro_test_ets_MAN)




### 26 - Smoothing Parameters ETS
# Auto ETS Model
report(ets_models %>%
         select(auto_ets))

# ETS AAN Model
report(ets_models %>%
         select(ets_AAN))

# ETS AAdN Model
report(ets_models %>%
         select(ets_AAdN))

# ETS MAN Model
report(ets_models %>%
         select(ets_MAN))




### 27 - Forecasting ETS
# Forecast the ETS models on the test data
ets_auto_forecast <- ets_models %>%
  select(auto_ets) %>%
  forecast(h = 16)

ets_AAN_forecast <- ets_models %>%
  select(ets_AAN) %>%
  forecast(h = 16)

ets_AAdN_forecast <- ets_models %>%
  select(ets_AAdN) %>%
  forecast(h = 16)

ets_MAN_forecast <- ets_models %>%
  select(ets_MAN) %>%
  forecast(h = 16)

# Plotting the forecasts along with actual values in the test set
autoplot(train, gdp) +
  autolayer(ets_auto_forecast, .mean, series = "Auto ETS Forecast") +
  autolayer(test, gdp, series = "Actual Values") +
  ggtitle("ETS (M,Ad,N) Model Forecast vs Actual Values") +
  labs(x = "Year", y = "GDP") +
  theme_minimal()

autoplot(train, gdp) +
  autolayer(ets_AAN_forecast, .mean, series = "ETS AAN Forecast") +
  autolayer(test, gdp, series = "Actual Values") +
  ggtitle("ETS AAN Model Forecast vs Actual Values") +
  labs(x = "Year", y = "GDP") +
  theme_minimal()

autoplot(train, gdp) +
  autolayer(ets_AAdN_forecast, .mean, series = "ETS AAdN Forecast") +
  autolayer(test, gdp, series = "Actual Values") +
  ggtitle("ETS AAdN Model Forecast vs Actual Values") +
  labs(x = "Year", y = "GDP") +
  theme_minimal()

autoplot(train, gdp) +
  autolayer(ets_MAN_forecast, .mean, series = "ETS MAN Forecast") +
  autolayer(test, gdp, series = "Actual Values") +
  ggtitle("ETS MAN Model Forecast vs Actual Values") +
  labs(x = "Year", y = "GDP") +
  theme_minimal()

# Print the forecast values for each model
print(ets_auto_forecast)
print(ets_AAN_forecast)
print(ets_AAdN_forecast)
print(ets_MAN_forecast)





### 28 - ARIMA VS ETS
## 28.1 - Visualization
# Convert forecasts to data frame and add Model column
arima_forecast <- auto_arima_drift_forecast %>%
  as_tibble() %>%
  mutate(Model = "ARIMA (1,1,1) W/ Drift")

ets_forecast <- ets_auto_forecast %>%
  as_tibble() %>%
  mutate(Model = "ETS M, Ad, N")

# Convert actual data to tibble and add Model column
actual_data <- test %>%
  as_tibble() %>%
  mutate(Model = "Actual data") %>%
  rename(.mean = gdp)

# Combine all data for plotting
combined_forecasts <- bind_rows(
  arima_forecast %>% select(date, .mean, Model),
  ets_forecast %>% select(date, .mean, Model),
  actual_data %>% select(date, .mean, Model)
)

# Create the plot with actual data in black
ggplot(combined_forecasts, aes(x = date, y = .mean, color = Model)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("Actual data" = "black", "ARIMA (1,1,1) W/ Drift" = "green", "ETS M, Ad, N" = "blue")) +
  labs(title = "Forecasts from Best ARIMA and ETS Models vs Actual Values",
       x = "Date",
       y = "GDP") +
  theme_minimal() +
  theme(legend.position = "right")



## 28.2 - Accuracy on Filtered Test Dataset
test_filtered <- test %>%
  filter(date >= yearquarter("2022 Q1"))

# Generate forecasts for the entire test period
auto_arima_forecast <- train %>%
  model(auto_arima = ARIMA(gdp ~ 1)) %>%
  forecast(test)

auto_ets_forecast <- train %>%
  model(auto_ets = ETS(gdp)) %>%
  forecast(test)

# Filter the forecasts to only include data from 2022-Q1 onwards
auto_arima_filtered <- auto_arima_forecast %>%
  filter(date >= yearquarter("2022 Q1"))

auto_ets_filtered <- auto_ets_forecast %>%
  filter(date >= yearquarter("2022 Q1"))

# Calculate accuracy metrics using the filtered forecast results
accuracy_metrics_filtered <- bind_rows(
  fabletools::accuracy(auto_arima_filtered, test_filtered %>% select(gdp)),
  fabletools::accuracy(auto_ets_filtered, test_filtered %>% select(gdp))
) %>%
  select(.model, RMSE, MAE, MAPE) %>%
  mutate(across(RMSE:MAPE, ~format(round(.x, 2), nsmall = 2)))

# Print the accuracy metrics
print(accuracy_metrics_filtered)





## 28.3 - Visualization on Filtered Dataset
# Convert forecasts to data frame and add Model column
arima_forecast_filtered <- auto_arima_filtered %>%
  as_tibble() %>%
  mutate(Model = "ARIMA (1,1,1) W/ Drift")

ets_forecast_filtered <- auto_ets_filtered %>%
  as_tibble() %>%
  mutate(Model = "ETS M, Ad, N")

# Convert actual filtered data to tibble and add Model column
actual_data_filtered <- test_filtered %>%
  as_tibble() %>%
  mutate(Model = "Actual data") %>%
  rename(.mean = gdp)

# Combine all data for plotting
combined_forecasts_filtered <- bind_rows(
  arima_forecast_filtered %>% select(date, .mean, Model),
  ets_forecast_filtered %>% select(date, .mean, Model),
  actual_data_filtered %>% select(date, .mean, Model)
)

# Create the plot with actual data in black
ggplot(combined_forecasts_filtered, aes(x = date, y = .mean, color = Model)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("Actual data" = "black", "ARIMA (1,1,1) W/ Drift" = "green", "ETS M, Ad, N" = "blue")) +
  labs(title = "Forecasts from Best ARIMA and ETS Models vs Actual Values (2022-Q1 Onwards)",
       x = "Date",
       y = "GDP") +
  theme_minimal() +
  theme(legend.position = "right")

