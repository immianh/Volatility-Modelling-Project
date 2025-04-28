# Volatility Seminar Project 

Based on Peter Christoffersen's Elements of Financial Risk Management (2003), this project investigates the section Volatility Modeling Using Daily Data.

## Empirical Analysis

1. Objective: Three different stock indices:
- S&P 500 (USA) - representing the American market.
- DAX 30 (Germany) - representing the European market.
- Nikkei 225 (Japan) - representing the Asian market.

3. Data Source: All indices are sourced from the Wall Street Journal.

4. Time Period: January 1, 1996, to September 26, 2024.
   
5. Sample Size:
- The ideal sample size is 7,574 data points, as daily data from January 1, 1996, to September 26, 2024, is used.
- However, due to non-trading days and the calculation of first differences:
 - Nikkei 225: 5,079 data points.
 - S&P 500: 5,219 data points.
 - DAX 30: 5,274 data points.

6. Data Processes
- First, we extract the data and clean any missing values, then sort the data by date. After cleaning, we calculate the first difference of the logarithms
of the data to determine the return rate of the stock indices.
   Rt = 100∆ln(Pt)
- We apply four different GARCH models—standard GARCH (SGARCH), exponential GARCH (EGARCH), GJR-GARCH, and NGARCH model to the entire dataset.
To identify the optimal model for each of the three stock indices (DAX30, S&P 500, and Nikkei225), we use the minimum AIC criterion.
