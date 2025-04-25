######################
# seminar volatility
######################

install.packages("tidyverse")
install.packages("rugarch")
install.packages("tseries")
install.packages("dplyr")
install.packages("xts")
install.packages("fBasics")


# Library packages 
library(tidyverse)
library(rugarch)
library(tseries)
library(dplyr)
library(xts)
library(fBasics)

# Dataset 
##############
# Data import
##############
# Germany
##############
# DAX_DE here refers to the DAX Performance stock market index in Germany.
library(readr)
DAX_DE <- read_csv("Desktop/HistoricalPrices_DAX 2.csv")
# Change the Date Format make it easy to process
DAX_DE$Date = as.Date(DAX_DE$Date, format="%m/%d/%y")
time=DAX_DE$Date
# Extract time window
# From 2004 to 2024
DAX_DE_filtered = DAX_DE[DAX_DE$Date >= "2004-01-01" & DAX_DE$Date <= "2024-12-31", ]
# Ignore the missing data
DAX_DE_cleaned = na.omit(DAX_DE_filtered)
# Arrange the DATE from 2004 to 2024
DAX_DE_cleaned = DAX_DE_cleaned%>%arrange(Date)
# Difference the log of the dataset
DAX_DE_return = diff(log(DAX_DE_cleaned$Open), lag = 1) * 100 
# Change this to xts object, so late we can plot in forecast with right time
DAX_DE_return= na.omit(xts(x = DAX_DE_return, order.by = time[seq(2,length(DAX_DE_cleaned$Date))]))
View(DAX_DE_return)

# Plot the return
plot(x=DAX_DE_cleaned$Date[seq(2,length(DAX_DE_cleaned$Date))],
     y=DAX_DE_return,type='l',xlab='Year',ylab='Return', main = 'DAX30 Return', col='black')

# Plot the hist diagram
# Germany
hist(DAX_DE_return, freq=F, ylim=c(0,0.7))
lines(density(DAX_DE_return),col='black')

##############
# USA
##############
# SP_USA here refers to the Standard and Poor's 500 stock market index in the United States. 
library(readr)
SP_USA <- read_csv("Desktop/HistoricalPrices_SP 2.csv")
# Change the Date Format make it easy to process
SP_USA$Date = as.Date(SP_USA$Date, format="%m/%d/%y")
timeus=SP_USA$Date
# Extract time window
# From 2004 to 2024
SP_USA_filtered = SP_USA[SP_USA$Date >= "2004-01-01" & SP_USA$Date <= "2024-12-31", ]
# Ignore the missing data
SP_USA_cleaned = na.omit(SP_USA_filtered)
# Arrange the DATE from 2004 to 2024
SP_USA_cleaned = SP_USA_cleaned%>%arrange(Date)
# Difference the log of the dataset
SP_USA_return = diff(log(SP_USA_cleaned$Open), lag = 1) * 100 
# Change this to xts object
SP_USA_return = na.omit(xts(x = SP_USA_return, order.by = timeus[seq(2,length(SP_USA_cleaned$Date))]))
#Plot the return
plot(x=SP_USA_cleaned$Date[seq(2,length(SP_USA_cleaned$Date))],y=SP_USA_return,
     type='l',xlab='Year',ylab='Return', main = 'S&P500 Return', col= 'grey')

# Plot the hist diagram
# USA
hist(SP_USA_return, freq=F, ylim=c(0,0.7))
lines(density(SP_USA_return),col='grey')

##############
#Japan
##############
# NK_JP here refers to the Nikkei 225 stock market index in Japan.
library(readr)
NK_JP <- read_csv("Desktop/HistoricalPrices_Nikkei 2.csv")
# Change the Date Format make it easy to process
NK_JP$Date = as.Date(NK_JP$Date, format="%m/%d/%y")
timejp=NK_JP$Date
# Extract time window
# From 2004 to 2024
NK_JP_filtered = NK_JP[NK_JP$Date >= "2004-01-01" & NK_JP$Date <= "2024-12-31", ]
# Ignore the missing data
NK_JP_cleaned = na.omit(NK_JP_filtered)
# Arrange the DATE from 2004 to 2024
NK_JP_cleaned=NK_JP_cleaned%>%arrange(Date)
# Difference the log of the dataset
# We use the Open data here
NK_JP_return = diff(log(NK_JP_cleaned$Open), lag = 1) * 100
# Change this to xts object
NK_JP_return = na.omit(xts(x = NK_JP_return, order.by = timejp[seq(2,length(NK_JP_cleaned$Date))]))
# View(DAX_DE_return) could check the data
# Plot return
plot(x=NK_JP_cleaned$Date[seq(2,length(NK_JP_cleaned$Date))],y=NK_JP_return,
     type='l',xlab='Year',ylab='Return', main = 'Nikkei225 Return', col='blue')

# Plot the hist diagram
# Japan
hist(NK_JP_return, freq=F, ylim=c(0,0.5))
lines(density(NK_JP_return),col='blue')

##############
#plot three indices together 
plot(y = DAX_DE_cleaned$Open, type="l", x=DAX_DE_cleaned$Date, 
     xlab="Year", ylab="Index_Open", ylim=c(0,50000), 
     main="Stock indices from 2004 to 2024")
lines(SP_USA_cleaned$Open, col='grey', x=SP_USA_cleaned$Date)
lines(NK_JP_cleaned$Open, col='blue', x=NK_JP_cleaned$Date)
legend('topleft', c('DAX30_Germany','S&P500_USA','Nikkei225_Japan'), 
       col=c('black','grey','blue'), lty =c(1,1,1))

##############
# Normalization
# Formula: (x-mean(x))/sqrt(var(x))
JP=(NK_JP_cleaned$Open-mean(NK_JP_cleaned$Open))/sqrt(var(NK_JP_cleaned$Open))
US=(SP_USA_cleaned$Open-mean(SP_USA_cleaned$Open))/sqrt(var(SP_USA_cleaned$Open))
DE=(DAX_DE_cleaned$Open-mean(DAX_DE_cleaned$Open))/sqrt(var(DAX_DE_cleaned$Open))
# Plot one and line them together 
plot(y= DE,type="l", x=DAX_DE_cleaned$Date, 
     xlab="Year", ylab="Index_Open", ylim=c(-2,3), 
     main="Stock indices from 2004 to 2024")
lines(US, col='grey', x=SP_USA_cleaned$Date)
lines(JP, col='blue', x=NK_JP_cleaned$Date)
legend('topleft', c('DAX30_Germany','S&P500_USA','Nikkei225_Japan'), 
       col=c('black','grey','blue'), lty =c(1,1,1))

################################################################################
# Define the optimal garch model 
################################################################################
# Set optimal GARCH model we use the function () here to set up everything
# Using for loop to search the min AIC/BIC 
# Set GARCH(m,s) orders m ∈ {0,1,2}, s ∈ {0,1,2}
# Since we don't do a ARIMA-GARCH model we set ARIMA order as c(0,0)
# Set distributions as: norm., snorm., std., sstd.
# “norm” for the normal distribution
# “std” for the student-t
# “sstd” for the skew-student
# We choose 2000 data points as out-of-sample 
# Models we test are sGARCH; eGARCH; hGARCH; SplineGARCH
################################################################################

optimize_garch_model = function(data, out_sample_n = 2000, model = 'fGARCH', submodel = "HARCH", 
                                garch_orders = list(c(1, 1), c(1, 2), c(2, 1), c(2, 2)), 
                                arma_orders = list(c(0, 0)), 
                                distributions = c("norm", "std", "sstd")) {
  results = list()
  
  # For-loop searching for the optimal model orders and distributions
  for (garch_order in garch_orders) {
    for (arma_order in arma_orders) {
      for (dist in distributions) {
        
        # Define the GARCH model using ugarchspec()
        if (model == 'fGARCH') {
          spec = ugarchspec(
            variance.model = list(model = model, submodel = submodel, garchOrder = garch_order),
            mean.model = list(armaOrder = arma_order, include.mean = TRUE),
            distribution.model = dist
          )
        } else {
          spec = ugarchspec(
            variance.model = list(model = model, garchOrder = garch_order),
            mean.model = list(armaOrder = arma_order, include.mean = TRUE),
            distribution.model = dist
          )
        }
        
        # Fit the model using ugarchfit()
        fit = try(ugarchfit(spec, data, out.sample = out_sample_n), silent = TRUE)
        
        # Skip failed model fits
        if (inherits(fit, "try-error")) next
        
        # Get AIC and BIC information criteria
        aic = infocriteria(fit)["Akaike",]
        bic = infocriteria(fit)["Bayes",]
        
        # Store the results
        model_name = paste("GARCH(", paste(garch_order, collapse = ","), ")", 
                           "ARMA(", paste(arma_order, collapse = ","), ")", 
                           dist, sep = "_")
        print(paste("GARCH(", paste(garch_order, collapse = ","), ")", 
                    "ARMA(", paste(arma_order, collapse = ","), ")", 
                    dist, sep = "_"))
        results[[model_name]] = list(
          model = fit,
          AIC = aic,
          BIC = bic
        )
      }
    }
  }
  
  # Get the best model
  best_model_name = which.min(sapply(results, function(x) x$AIC))
  best_model = results[[names(results)[best_model_name]]]
  
  return(best_model)
}

#############################
# Germany optimal GARCH model 
#############################
# sGARCH model: standard GARCH model
# We call the function we just defined, so it well go back to the loop and 
# find out the optimal GARCH orders and distributions
DAX_DE_best_garch_model = optimize_garch_model(DAX_DE_return,model='sGARCH',
                                               garch_orders = list(c(1, 1), c(1, 2), c(2, 1), c(2, 2)),
                                               arma_orders = list(c(0, 0)),
                                               distributions = c("norm", "std",'sstd'))
print(DAX_DE_best_garch_model$model)
# eGARCH model: exponential GARCH model
DAX_DE_best_egarch_model = optimize_garch_model(DAX_DE_return,model='eGARCH',
                                                garch_orders = list(c(1, 1), c(1, 2), c(2, 1), c(2, 2)),
                                                arma_orders = list(c(0, 0)),
                                                distributions = c("norm", "std",'sstd'))
print(DAX_DE_best_egarch_model$model)
# iGARCH model: integrated GARCH model
DAX_DE_best_ngarch_model = optimize_garch_model(DAX_DE_return,model='fGARCH',,submodel = "NGARCH",
                                                garch_orders = list(c(1, 1), c(1, 2), c(2, 1), c(2, 2)),
                                                arma_orders = list(c(0, 0)),
                                                distributions = c("norm", "std",'sstd'))
print(DAX_DE_best_ngarch_model$model)
# gjrGARCH model: allows a quadratic response of volatility to news with different coefficients for good and bad news.
DAX_DE_best_gjrgarch_model = optimize_garch_model(DAX_DE_return,model='gjrGARCH',
                                                  garch_orders = list(c(1, 1), c(1, 2), c(2, 1), c(2, 2)),
                                                  arma_orders = list(c(0, 0)),
                                                  distributions = c("norm", "std",'sstd'))
print(DAX_DE_best_gjrgarch_model$model)



#########################
# USA optimal GARCH model
#########################
SP_USA_best_garch_model = optimize_garch_model(SP_USA_return,model='sGARCH',
                                               garch_orders = list(c(1, 1), c(1, 2), c(2, 1), c(2, 2)),
                                               arma_orders = list(c(0, 0)), 
                                               distributions = c("norm", "std",'sstd'))
print(SP_USA_best_garch_model$model)

SP_USA_best_egarch_model = optimize_garch_model(SP_USA_return,model='eGARCH',
                                                garch_orders = list(c(1, 1), c(1, 2), c(2, 1), c(2, 2)),
                                                arma_orders = list(c(0, 0)), 
                                                distributions = c("norm", "std",'sstd'))
print(SP_USA_best_egarch_model$model)

SP_USA_best_ngarch_model = optimize_garch_model(SP_USA_return,model='fGARCH',submodel = "NGARCH", 
                                                garch_orders = list(c(1, 1), c(1, 2), c(2, 1), c(2, 2)),
                                                arma_orders = list(c(1, 0)), 
                                                distributions = c("norm", "std",'sstd'))
print(SP_USA_best_ngarch_model$model)

SP_USA_best_gjrgarch_model = optimize_garch_model(SP_USA_return,model='gjrGARCH',
                                                  garch_orders = list(c(1, 1), c(1, 2), c(2, 1), c(2, 2)),
                                                  arma_orders = list(c(0, 0)), 
                                                  distributions = c("norm", "std",'sstd'))
print(SP_USA_best_gjrgarch_model$model)

############################
# Japan optimal GARCH model
############################
NK_JP_best_egarch_model = optimize_garch_model(NK_JP_return,model='eGARCH',
                                               garch_orders = list(c(1, 1), c(1, 2), c(2, 1), c(2, 2)),
                                               arma_orders = list(c(0, 0)),
                                               distributions = c("norm", "std",'sstd'))
print(NK_JP_best_egarch_model$model)

NK_JP_best_garch_model = optimize_garch_model(NK_JP_return,model='sGARCH',
                                              garch_orders = list(c(1, 1), c(1, 2), c(2, 1), c(2, 2)),
                                              arma_orders = list(c(0, 0)),
                                              distributions = c("norm", "std",'sstd'))
print(NK_JP_best_garch_model$model)

NK_JP_best_ngarch_model = optimize_garch_model(NK_JP_return,model='fGARCH',,submodel = "NGARCH",
                                               garch_orders = list(c(1, 1), c(1, 2), c(2, 1), c(2, 2)),
                                               arma_orders = list(c(0, 0)),
                                               distributions = c("norm", "std",'sstd'))
print(NK_JP_best_ngarch_model$model)

NK_JP_best_gjrgarch_model = optimize_garch_model(NK_JP_return,model='gjrGARCH',
                                                 garch_orders = list(c(1, 1), c(1, 2), c(2, 1), c(2, 2)),
                                                 arma_orders = list(c(0, 0)),
                                                 distributions = c("norm", "std",'sstd'))
print(NK_JP_best_gjrgarch_model$model)


#########################
# Variance Model Evaluation
#########################
# Likelihood Ratio Test

# LR Test for DAX_DE

# Function to extract log-likelihood and perform LR Test
lr_test <- function(fit1, fit2) {
  logLik1 <- fit1@fit$LLH  # Log-likelihood of Model 1
  logLik2 <- fit2@fit$LLH  # Log-likelihood of Model 2
  df <- abs(fit1@fit$n.pars - fit2@fit$n.pars)  # Difference in parameters
  test_stat <- -2 * (logLik1 - logLik2)  # LR test statistic
  p_value <- pchisq(test_stat, df = df, lower.tail = FALSE)  # p-value
  return(list(LR_statistic = test_stat, p_value = p_value))
}

# Save log-likelihood values and perform model comparisons
results <- list(
  sGARCH = list(model = DAX_DE_best_garch_model$model, logLik = DAX_DE_best_garch_model$model@fit$LLH),
  eGARCH = list(model = DAX_DE_best_egarch_model$model, logLik = DAX_DE_best_egarch_model$model@fit$LLH),
  nGARCH = list(model = DAX_DE_best_ngarch_model$model, logLik = DAX_DE_best_ngarch_model$model@fit$LLH),
  gjrGARCH = list(model = DAX_DE_best_gjrgarch_model$model, logLik = DAX_DE_best_gjrgarch_model$model@fit$LLH)
)

# Print log-likelihoods
cat("Log-Likelihoods of Models:\n")
sapply(results, function(x) x$logLik)

# Perform LR tests between nested models 
# sGARCH vs gjrGARCH
cat("LR Test: sGARCH vs gjrGARCH\n")
lr_test_sGARCH_vs_gjrGARCH <- lr_test(DAX_DE_best_garch_model$model, DAX_DE_best_gjrgarch_model$model)
print(lr_test_sGARCH_vs_gjrGARCH)

# sGARCH vs nGARCH
cat("LR Test: sGARCH vs nGARCH\n")
lr_test_sGARCH_vs_nGARCH <- lr_test(DAX_DE_best_garch_model$model, DAX_DE_best_ngarch_model$model)
print(lr_test_sGARCH_vs_nGARCH)

# When comparing non-nested models like sGARCH and eGARCH, the LR test might not be strictly valid.

# LR Test for SP_USA 

# Function to extract log-likelihood and perform LR Test
lr_test <- function(fit1, fit2) {
  logLik1 <- fit1@fit$LLH  # Log-likelihood of Model 1
  logLik2 <- fit2@fit$LLH  # Log-likelihood of Model 2
  df <- abs(fit1@fit$n.pars - fit2@fit$n.pars)  # Difference in parameters
  test_stat <- -2 * (logLik1 - logLik2)  # LR test statistic
  p_value <- pchisq(test_stat, df = df, lower.tail = FALSE)  # p-value
  return(list(LR_statistic = test_stat, p_value = p_value))
}

# Save log-likelihood values and perform model comparisons
results_SP_USA <- list(
  sGARCH = list(model = SP_USA_best_garch_model$model, logLik = SP_USA_best_garch_model$model@fit$LLH),
  eGARCH = list(model = SP_USA_best_egarch_model$model, logLik = SP_USA_best_egarch_model$model@fit$LLH),
  nGARCH = list(model = SP_USA_best_ngarch_model$model, logLik = SP_USA_best_ngarch_model$model@fit$LLH),
  gjrGARCH = list(model = SP_USA_best_gjrgarch_model$model, logLik = SP_USA_best_gjrgarch_model$model@fit$LLH)
)

# Print log-likelihoods
cat("Log-Likelihoods of Models (SP_USA):\n")
sapply(results_SP_USA, function(x) x$logLik)

# Perform LR tests between nested models 
# sGARCH vs gjrGARCH
cat("LR Test: sGARCH vs gjrGARCH (SP_USA)\n")
lr_test_sGARCH_vs_gjrGARCH_SP <- lr_test(SP_USA_best_garch_model$model, SP_USA_best_gjrgarch_model$model)
print(lr_test_sGARCH_vs_gjrGARCH_SP)

# sGARCH vs nGARCH
cat("LR Test: sGARCH vs nGARCH (SP_USA)\n")
lr_test_sGARCH_vs_nGARCH_SP <- lr_test(SP_USA_best_garch_model$model, SP_USA_best_ngarch_model$model)
print(lr_test_sGARCH_vs_nGARCH_SP)


# LR Test for NK_JP

# Function to extract log-likelihood and perform LR Test
lr_test <- function(fit1, fit2) {
  logLik1 <- fit1@fit$LLH  # Log-likelihood of Model 1
  logLik2 <- fit2@fit$LLH  # Log-likelihood of Model 2
  df <- abs(fit1@fit$n.pars - fit2@fit$n.pars)  # Difference in parameters
  test_stat <- -2 * (logLik1 - logLik2)  # LR test statistic
  p_value <- pchisq(test_stat, df = df, lower.tail = FALSE)  # p-value
  return(list(LR_statistic = test_stat, p_value = p_value))
}

# Save log-likelihood values and perform model comparisons
results_NK_JP <- list(
  sGARCH = list(model = NK_JP_best_garch_model$model, logLik = NK_JP_best_garch_model$model@fit$LLH),
  eGARCH = list(model = NK_JP_best_egarch_model$model, logLik = NK_JP_best_egarch_model$model@fit$LLH),
  nGARCH = list(model = NK_JP_best_ngarch_model$model, logLik = NK_JP_best_ngarch_model$model@fit$LLH),
  gjrGARCH = list(model = NK_JP_best_gjrgarch_model$model, logLik = NK_JP_best_gjrgarch_model$model@fit$LLH)
)

# Print log-likelihoods
cat("Log-Likelihoods of Models (NK_JP):\n")
sapply(results_NK_JP, function(x) x$logLik)

# Perform LR tests between nested models 
# sGARCH vs gjrGARCH
cat("LR Test: sGARCH vs gjrGARCH (NK_JP)\n")
lr_test_sGARCH_vs_gjrGARCH_NK <- lr_test(NK_JP_best_garch_model$model, NK_JP_best_gjrgarch_model$model)
print(lr_test_sGARCH_vs_gjrGARCH_NK)

# sGARCH vs nGARCH
cat("LR Test: sGARCH vs nGARCH (NK_JP)\n")
lr_test_sGARCH_vs_nGARCH_NK <- lr_test(NK_JP_best_garch_model$model, NK_JP_best_ngarch_model$model)
print(lr_test_sGARCH_vs_nGARCH_NK)



#########################
# Diagnostic Check on the Autocorrelations 
#########################

library(rugarch)

# Bartlett bounds function for confidence intervals
get_bartlett_bounds <- function(data) {
  n_obs <- length(data)
  2 / sqrt(n_obs)
}

# Function to calculate autocorrelations of squared shocks
get_acf_squared_shocks <- function(fit_model, return_data) {
  # Extract conditional variances (sigma^2) from the fitted model
  sigma_squared <- sigma(fit_model)^2
  
  # Compute squared shocks: squared returns divided by GARCH variance
  squared_shocks <- (return_data^2) / sigma_squared
  
  # Compute autocorrelations
  acf(squared_shocks, plot = FALSE)
}

# Function to perform diagnostic checks for a dataset
diagnostic_check <- function(data, country_name) {
  # Bartlett bounds
  bartlett_bounds <- get_bartlett_bounds(data)
  
  # Fit Models
  spec_eGARCH <- ugarchspec(variance.model = list(model = "eGARCH"),
                            mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                            distribution.model = "std")
  fit_eGARCH <- ugarchfit(spec = spec_eGARCH, data = data)
  
  spec_gjrGARCH <- ugarchspec(variance.model = list(model = "gjrGARCH"),
                              mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                              distribution.model = "std")
  fit_gjrGARCH <- ugarchfit(spec = spec_gjrGARCH, data = data)
  
  spec_nGARCH <- ugarchspec(variance.model = list(model = "fGARCH", submodel = "NGARCH"),
                            mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                            distribution.model = "std")
  fit_nGARCH <- ugarchfit(spec = spec_nGARCH, data = data)
  
  # Extract ACF for squared returns
  acf_squared_returns <- acf(data^2, plot = FALSE)
  
  # Get ACF for each model
  acf_eGARCH <- get_acf_squared_shocks(fit_eGARCH, data)
  acf_gjrGARCH <- get_acf_squared_shocks(fit_gjrGARCH, data)
  acf_nGARCH <- get_acf_squared_shocks(fit_nGARCH, data)
  
  # Plot for each model
  par(mfrow = c(2, 2))  # 2x2 layout for comparison
  
  # Plot 1: ACF of Squared Returns
  plot(acf_squared_returns$lag, acf_squared_returns$acf, type = "l", col = "blue",
       lwd = 2, ylim = c(-0.2, 0.5), xlab = "Lag", ylab = "Autocorrelations",
       main = paste(country_name, ": ACF of Squared Returns"))
  abline(h = c(-bartlett_bounds, bartlett_bounds), col = "red", lty = 2)
  
  # Plot for eGARCH
  plot(acf_eGARCH$lag, acf_eGARCH$acf, type = "l", col = "blue",
       lwd = 2, ylim = c(-0.2, 0.5), xlab = "Lag", ylab = "Autocorrelations",
       main = paste(country_name, ": ACF Squared Shocks (eGARCH)"))
  abline(h = c(-bartlett_bounds, bartlett_bounds), col = "red", lty = 2)
  
  # Plot for gjrGARCH
  plot(acf_gjrGARCH$lag, acf_gjrGARCH$acf, type = "l", col = "blue",
       lwd = 2, ylim = c(-0.2, 0.5), xlab = "Lag", ylab = "Autocorrelations",
       main = paste(country_name, ": ACF Squared Shocks (gjrGARCH)"))
  abline(h = c(-bartlett_bounds, bartlett_bounds), col = "red", lty = 2)
  
  # Plot for nGARCH
  plot(acf_nGARCH$lag, acf_nGARCH$acf, type = "l", col = "blue",
       lwd = 2, ylim = c(-0.2, 0.5), xlab = "Lag", ylab = "Autocorrelations",
       main = paste(country_name, ": ACF Squared Shocks (nGARCH)"))
  abline(h = c(-bartlett_bounds, bartlett_bounds), col = "red", lty = 2)
}

# Diagnostic checks for DAX_DE, SP_USA, and NK_JP
diagnostic_check(DAX_DE_return, "DAX_DE")
diagnostic_check(SP_USA_return, "SP_USA")
diagnostic_check(NK_JP_return, "NK_JP")

#########################
# Volatility Forecast Evaluation Using Regression
#########################

library(rugarch)
library(xts)

# Function to perform volatility forecast evaluation using regression
volatility_forecast_evaluation <- function(data, fit_model, country_name) {
  # Extract conditional variances (sigma_t+1^2) from the fitted model
  sigma_squared <- sigma(fit_model)^2
  
  # Compute squared returns (R_t+1^2)
  squared_returns <- lag(data^2, k = -1)  # Shift squared returns for t+1
  
  # Remove NA values caused by lagging
  valid_indices <- !is.na(squared_returns)
  squared_returns <- squared_returns[valid_indices]
  sigma_squared <- sigma_squared[valid_indices]
  
  # Perform regression: R_t+1^2 = b0 + b1 * sigma_t+1^2 + error
  regression_model <- lm(squared_returns ~ sigma_squared)
  
  # Summary of the regression
  regression_summary <- summary(regression_model)
  
  # Output results
  cat("\n-------------------------------------\n")
  cat("Volatility Forecast Evaluation for", country_name, "\n")
  cat("-------------------------------------\n")
  print(regression_summary)
  
  # Return regression model for further use
  return(regression_model)
}

# Germany (DAX_DE)
spec_DAX <- ugarchspec(variance.model = list(model = "sGARCH"),
                       mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                       distribution.model = "std")
fit_DAX <- ugarchfit(spec = spec_DAX, data = DAX_DE_return)

model_Germany <- volatility_forecast_evaluation(DAX_DE_return, fit_DAX, "Germany")

# USA (SP_USA)
spec_SP <- ugarchspec(variance.model = list(model = "sGARCH"),
                      mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                      distribution.model = "std")
fit_SP <- ugarchfit(spec = spec_SP, data = SP_USA_return)

model_USA <- volatility_forecast_evaluation(SP_USA_return, fit_SP, "USA")

# Japan (NK_JP)
spec_NK <- ugarchspec(variance.model = list(model = "sGARCH"),
                      mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                      distribution.model = "std")
fit_NK <- ugarchfit(spec = spec_NK, data = NK_JP_return)

model_Japan <- volatility_forecast_evaluation(NK_JP_return, fit_NK, "Japan")
# Create a data frame to store regression results for all countries
regression_results <- data.frame(
  Country = character(),
  Intercept_Estimate = numeric(),
  Intercept_pValue = numeric(),
  Slope_Estimate = numeric(),
  Slope_pValue = numeric(),
  R_squared = numeric(),
  stringsAsFactors = FALSE
)

# Function to extract regression results
extract_regression_results <- function(regression_model, country_name) {
  intercept <- coef(summary(regression_model))["(Intercept)", "Estimate"]
  intercept_p <- coef(summary(regression_model))["(Intercept)", "Pr(>|t|)"]
  slope <- coef(summary(regression_model))["sigma_squared", "Estimate"]
  slope_p <- round(coef(summary(regression_model))["sigma_squared", "Pr(>|t|)"],3)
  r_squared <- summary(regression_model)$r.squared
  
  return(data.frame(
    Country = country_name,
    Intercept_Estimate = intercept,
    Slope_Estimate = slope,
    pValue = slope_p,
    R_squared = r_squared
  ))
}

# Add DAX_DE's results
germany_results <- extract_regression_results(model_Germany, "DAX_DE")
regression_results <- rbind(regression_results, germany_results)

# Add SP_USA's results
usa_results <- extract_regression_results(model_USA, "SP_USA")
regression_results <- rbind(regression_results, usa_results)

# Add NK_JP's results
japan_results <- extract_regression_results(model_Japan, "NK_JP")
regression_results <- rbind(regression_results, japan_results)

# Display the results in a shared table
library(knitr)
kable(regression_results, format = "markdown", align = "c", caption = "Volatility Forecast Regression Results")

#########################
# The Volatility Forecast Loss Function
#########################

# Load required libraries
library(rugarch)
library(xts)

# Function to compute MSE and QLIKE loss functions
compute_loss_functions <- function(data, fit_model, country_name) {
  # Define a small constant to prevent division by zero or log errors
  epsilon <- 1e-6
  
  # Extract conditional variances (sigma_t+1^2) from the fitted model
  sigma_squared <- sigma(fit_model)^2 + epsilon
  
  # Compute squared returns (R_t+1^2)
  squared_returns <- lag(data^2, k = -1) + epsilon  # Shift squared returns for t+1
  
  # Remove NA values caused by lagging
  valid_indices <- !is.na(squared_returns)
  squared_returns <- squared_returns[valid_indices]
  sigma_squared <- sigma_squared[valid_indices]
  
  # Compute MSE
  mse <- mean((squared_returns - sigma_squared)^2)
  
  # Compute QLIKE
  qlike <- mean((squared_returns / sigma_squared) - log(squared_returns / sigma_squared) - 1)
  
  # Create synthetic data for plotting
  forecast_range <- seq(0, max(sigma_squared, na.rm = TRUE), length.out = 100)
  loss_data <- data.frame(
    Forecast = forecast_range,
    MSE = (forecast_range - 2)^2,
    QLIKE = (forecast_range / 2) - log(forecast_range / 2 + epsilon) - 1
  )
  
  # Remove invalid rows (e.g., Inf, NaN) from loss_data
  loss_data <- loss_data[is.finite(loss_data$MSE) & is.finite(loss_data$QLIKE), ]
  
  # Plot the loss functions 
  plot(loss_data$Forecast, loss_data$MSE, type = "l", col = "blue", lwd = 2,
       ylim = c(0, max(loss_data$MSE, loss_data$QLIKE, na.rm = TRUE)),
       xlab = "Volatility Forecast (true value=2)", ylab = "Forecast Loss",
       main = paste("Volatility Loss Functions -", country_name))
  lines(loss_data$Forecast, loss_data$QLIKE, col = "red", lwd = 2)
  legend("topright", legend = c("MSE", "QLIKE"), col = c("blue", "red"), lty = 1, lwd = 2)
  
  # Return a list of results
  return(list(MSE = mse, QLIKE = qlike))
}

# Germany (DAX_DE)
spec_DAX <- ugarchspec(variance.model = list(model = "sGARCH"),
                       mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                       distribution.model = "std")
fit_DAX <- ugarchfit(spec = spec_DAX, data = DAX_DE_return)

loss_Germany <- compute_loss_functions(DAX_DE_return, fit_DAX, "DAX_DE")

# USA (SP_USA)
spec_SP <- ugarchspec(variance.model = list(model = "sGARCH"),
                      mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                      distribution.model = "std")
fit_SP <- ugarchfit(spec = spec_SP, data = SP_USA_return)

loss_USA <- compute_loss_functions(SP_USA_return, fit_SP, "SP_USA")

# Japan (NK_JP)
spec_NK <- ugarchspec(variance.model = list(model = "sGARCH"),
                      mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                      distribution.model = "std")
fit_NK <- ugarchfit(spec = spec_NK, data = NK_JP_return)

loss_Japan <- compute_loss_functions(NK_JP_return, fit_NK, "NK_JP")

# Combine results into a table
loss_results <- data.frame(
  Country = c("DAX_DE", "SP_USA", "NK_JP"),
  MSE = c(loss_Germany$MSE, loss_USA$MSE, loss_Japan$MSE),
  QLIKE = c(loss_Germany$QLIKE, loss_USA$QLIKE, loss_Japan$QLIKE)
)

# Print the results
print(loss_results)




