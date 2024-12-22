# Forecasting Portugal's GDP: Comparative Analysis of ARIMA and ETS Models Post-Pandemic Impact

## Description
This project explores the use of time series forecasting techniques to analyze the impact of the COVID-19 pandemic on Portugal's GDP. Two models were compared:
- **ARIMA (AutoRegressive Integrated Moving Average)**
- **ETS (Exponential Smoothing State Space)**

The analysis evaluates each model's performance in predicting GDP trends and their practical implications for policymakers.

## Features
- **Data Preprocessing**: Stationarity tests, differencing, and Box-Cox transformation.
- **Structural Break Analysis**: Focused on the COVID-19 pandemic period.
- **Model Fitting**: Residual diagnostics for ARIMA and ETS.
- **Forecast Visualization**: Comparative accuracy analysis between models.

## Dataset
- Portugal's Real GDP data (1995–2023) sourced from the **FRED database**.

## Results
- **ETS (M, Ad, N)**: Best overall model with RMSE of **3350.93**.
- **ARIMA (1,1,1)**: Close second with a slightly higher RMSE of **3482.22**.
