# Varietal-Specific Forecasting: Why One Model Doesn't Fit All Australian Wines

**Author:** Alexander Zhuk  
**Date:** November 30, 2025

---

## Links

- **Deployed Shiny App:** [https://019ab2f0-c599-c119-a1cb-0ce4d5acc432.share.connect.posit.cloud/](https://019ab2f0-c599-c119-a1cb-0ce4d5acc432.share.connect.posit.cloud/)
- **Code Repository:** [https://github.com/AZhuk30/ShinyApp](https://github.com/AZhuk30/ShinyApp)

---

## Overview

This project analyzes Australian wine sales data from **1980–1994** to explore whether a single forecasting model can adequately handle all wine varietals — or whether different wine types require different approaches.

The key finding: **not all wines behave the same way seasonally**, and matching the forecasting model to each varietal's behavior meaningfully improves inventory planning accuracy.

---

## Key Finding

- **Sparkling** and **Dry white** wines exhibit massive December spikes. Sparkling can jump to over **6,000 liters** in December when typical months are under 4,000 liters — more than triple the baseline. These extreme swings require **ARIMA** models capable of handling seasonal differencing.
- **Fortified** wine is far more stable year-round (staying between 2,000–4,500 liters), so a simpler **ETS** model performs just as well as ARIMA without the added complexity.

For wineries, choosing the right model per varietal can mean the difference between stocking out in December or carrying excess inventory in February.

---

## Data

| Detail | Value |
|---|---|
| Dataset | Australian wine sales |
| Time period | 1980–1994 |
| Varietals analyzed | Fortified, Sparkling, Dry white |
| Models compared | TSLM, ETS, ARIMA |
| Training split | 80% training / 20% validation |
| Forecast horizon | 12 months |

---

## Model Results

### Validation Accuracy (RMSE)

| Varietal | Best Model | RMSE | Runner-up | RMSE |
|---|---|---|---|---|
| Fortified | ETS | 259.51 | ARIMA | 277.74 |
| Sparkling | ARIMA | 317.42 | ETS | 342.41 |
| Dry white | ARIMA | 496.76 | TSLM | 525.73 |

### Final Model Specifications

- **Fortified:** `ETS(M,A,M)` — multiplicative error, additive trend, multiplicative seasonality
- **Sparkling:** `ARIMA(0,0,1)(0,1,1)[12]` — seasonal differencing with 12-month period
- **Dry white:** `ARIMA(1,0,0)(0,1,1)[12] w/ drift` — autoregressive component plus seasonal structure

---

## Recommendations

Split forecasting strategy by varietal volatility:

- **High-volatility wines (Sparkling, Dry white):** Use ARIMA. Its seasonal differencing captures intense December peaks that ETS misses.
- **Stable wines (Fortified):** Use ETS. The extra complexity of ARIMA provides no meaningful benefit (~7% difference in RMSE) and runs slower.

For Sparkling wine specifically, the ARIMA vs. ETS gap translates to roughly **25 liters per month** in average error — a meaningful margin when peak December demand exceeds 6,000 liters.

---

## Reproducing Results

Open the Shiny app and configure:

- **Varietals:** Fortified, Sparkling, Dry white
- **Models:** TSLM, ETS, ARIMA
- **Training Data:** 80%
- **Forecast Horizon:** 12 months

Navigate to the **Model Results** tab → **Accuracy Metrics** table, sorted by RMSE, with Validation in focus.
