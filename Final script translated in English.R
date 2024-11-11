###############################################################################################################

# I) Data Import and Preliminary Analysis

###############################################################################################################


####################### 1: Import Data: ########################################################################

# 1. Import Data:

start_date = as.Date("2022-01-01")
end_date = as.Date("2024-08-31")
library(quantmod)

######################### Euro Dollar #########################################################################

symbol_eur_usd = "EURUSD=X"

eur_usd_data = getSymbols(symbol_eur_usd, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)

# We choose the closing price column as the price series
eur_usd_prices  = eur_usd_data[,4]

########################### Gold USD #########################################################################

symbol_gold = "GC=F"

gold_data = getSymbols(symbol_gold, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)

# We choose the closing price column as the price series
gold_prices = gold_data[,4]

################# DATA CLEANING AND PREPROCESSING #########################################################################

# We choose to remove missing values from the euro-dollar series

gold_prices <- na.omit(gold_prices)

# We choose to remove missing values from the euro-dollar series

eur_usd_prices <- na.omit(eur_usd_prices)

# We convert our data with a frequency of 252 to only include trading days

ts_eur_usd <- ts(eur_usd_prices, start = c(2022, 1), frequency = 252)  
ts_gold <- ts(gold_prices, start = c(2022, 1), frequency = 252)

###############################################################################################################

####################### 2: Plot the Data: #######################################################################

# Plotting both time series:

par(mfrow = c(2, 1)) 

plot(eur_usd_prices, main="EUR/USD Exchange Rate (Close)", xlab="Date", ylab="EUR/USD", col="blue")

grid()

plot(gold_prices, main="Gold Price in USD (Close)", xlab="Date", ylab="Gold Price", col="gold")

grid()

###############################################################################################################

#################### 3. Descriptive Analysis: #######################################################################

######################### Euro Dollar #########################################################################

library(e1071)

summary(ts_eur_usd)

skewness(ts_eur_usd)

kurtosis(ts_eur_usd)

#------------- Results: -------------#

# Index                EURUSD=X.Close  
# Min.   :2022-01-03   Min.   :0.9596  
# 1st Qu.:2022-08-31   1st Qu.:1.0582  
# Median :2023-05-02   Median :1.0791  
# Mean   :2023-05-02   Mean   :1.0719  
# 3rd Qu.:2024-01-01   3rd Qu.:1.0919  
# Max.   :2024-08-29   Max.   :1.1457 

# > skewness(ts_eur_usd)
#[1] -0.8765443

# > kurtosis(ts_eur_usd)
# [1] 0.816749

#------------------------------------#

#---- Interpretation of values: ------------------------------------------------------------------------#

#     skewness = -0.8765443

#     This suggests a slight left skew in the distribution,
#     meaning values below the mean are slightly more common.

#     kurtosis = 0.816749 

#     Indicates fewer extreme values and suggests most values are close to the mean.

############################################################################################################

######################### Gold USD #########################################################################

summary(ts_gold)

skewness(ts_gold)

kurtosis(ts_gold)

#------------- Results: -------------#

# Index                GC=F.Close  
# Min.   :2022-01-03   Min.   :1623  
# 1st Qu.:2022-09-01   1st Qu.:1827  
# Median :2023-05-03   Median :1933  
# Mean   :2023-05-03   Mean   :1970  
# 3rd Qu.:2024-01-01   3rd Qu.:2026  
# Max.   :2024-08-30   Max.   :2526  

# > skewness(ts_gold)
#[1] 0.8796226

# > kurtosis(ts_gold)
# [1] 0.09136757

#------------------------------------#

#---- Interpretation of values: ------------------------------------------------------------------------#

#     skewness = 0.8796226

#     This suggests a slight right skew, meaning values above the mean are more frequent.

#     kurtosis = 0.09136757 

#     Very low, suggesting almost no extreme values, with most values close to the mean.

############################################################################################################

########### Visual Analysis of trends, seasonality, and volatility. ######################################

######################### Euro Dollar ######################################################################

#--------- Trends : --------------------------------------------------------------------------------------#

# Observed a significant downward trend from Jan 2022 to Oct 2023 from 1.13 to about 0.95.
# Strong upward trend from Oct 2023 to Apr 2023 up to 1.10.
# Stabilization from Apr 2023 to Jul 2024 around 1.075, with fluctuations from 1.10 to 1.05.

#------- Seasonality : ------------------------------------------------------------------------------------#

# No clear patterns on the graph, though a recurring pattern from Jan 2022 to Jan 2023 
# reappears from Jul 2023 to 2024 but less pronounced.

#------- Volatility : -------------------------------------------------------------------------------------#

# Volatility was high in 2022 and more moderate in 2023.

######################### Gold USD #######################################################################

#--------- Trends : --------------------------------------------------------------------------------------#

# Overall upward trend from Jan 2023 to Jul 2024, from 1600 to 2500,
# especially noticeable from Sep 2023 to Jul 2024.
# Downward trend throughout 2022.

#------- Seasonality : ------------------------------------------------------------------------------------#

# No clear seasonality or repeating patterns.

#------- Volatility : -------------------------------------------------------------------------------------#

# High volatility throughout, both locally and globally; 
# for example, a price increase from 2000 to 2400 in less than two months (20% increase).

############################################################################################################

# CONCLUSION #############################################################################################

# It is difficult to establish a specific seasonality. The two graphs show a general 
# downward then upward trend. For volatility, the two graphs seem opposite:
# When EUR/USD volatility is high, gold volatility is lower;
# When EUR/USD volatility is stable or decreasing, gold volatility is high.

###############################################################################################################

#################### 4. Stationarity Check: ###################################################################

# Conducting a test to check stationarity of the series.
# Given the non-constant variance and mean, the series is unlikely to be stationary.

library(tseries)

adf.test(ts_eur_usd)

#------------- Results: -------------#

# Augmented Dickey-Fuller Test

# data:  ts_eur_usd
# Dickey-Fuller = -2.6573, Lag order = 8, p-value = 0.3001
# alternative hypothesis: stationary

# Interpretation: High p-value suggests the series is not stationary.

###############################################################################################################

adf.test(ts_gold)

#------------- Results: -------------#

# Augmented Dickey-Fuller Test

# data:  ts_gold
# Dickey-Fuller = -1.5494, Lag order = 8, p-value = 0.769
# alternative hypothesis: stationary

# Interpretation: High p-value suggests the series is not stationary.

###############################################################################################################

##################### Differentiating time series ###############################################################

ts_eur_usd_diff_1 = diff(ts_eur_usd)

adf.test(ts_eur_usd_diff_1)

#------------- Results: -------------#

# Augmented Dickey-Fuller Test

# data:  ts_eur_usd_diff_1
# Dickey-Fuller = -8.8591, Lag order = 8, p-value = 0.01
# alternative hypothesis: stationary

# Low p-value allows us to reject H0, suggesting stationarity.

###############################################################################################################

ts_gold_diff_1 = diff(ts_gold)

adf.test(ts_gold_diff_1)

#------------- Results: -------------#

# Augmented Dickey-Fuller Test

# data:  ts_gold_diff_1
# Dickey-Fuller = -8.4553, Lag order = 8, p-value = 0.01
# alternative hypothesis: stationary

# Low p-value allows us to reject H0, suggesting stationarity.

###############################################################################################################

# Conclusion: Both series gold_usd and eur_usd require first differencing to achieve stationarity.

###############################################################################################################

# II) Modeling and Analysis

##############################################################################################################


########### 1. Autoregressive and Moving Average Models: ###################################################

######################### Euro Dollar ######################################################################

acf(ts_eur_usd_diff_1, main="Differenced ACF of EUR/USD")

pacf(ts_eur_usd_diff_1, main="Differenced PACF of EUR/USD")

###############################################################################################################

acf(ts_gold_diff_1, main="Differenced ACF of GOLD/USD")

pacf(ts_gold_diff_1, main="Differenced PACF of GOLD/USD")

###############################################################################################################

##### Determine the appropriate AR or MA model order for each series using AIC/BIC criteria ###############

library(forecast)

######################### Euro Dollar ######################################################################

fit_arima_010 <- arima(ts_eur_usd, order = c(0, 1, 0))  # ARIMA(0,1,0)
fit_arima_110 <- arima(ts_eur_usd, order = c(1, 1, 0))  # ARIMA(1,1,0)
fit_arima_011 <- arima(ts_eur_usd, order = c(0, 1, 1))  # ARIMA(0,1,1)

# Retrieve AIC and BIC for each model
aic_values <- c(fit_arima_010$aic, fit_arima_110$aic, fit_arima_011$aic)
bic_values <- c(BIC(fit_arima_010), BIC(fit_arima_110), BIC(fit_arima_011))

aic_values
bic_values

# Choosing the model with the lowest BIC and AIC suggests ARIMA (0, 1, 0) for EUR/USD

# Conclusion: EUR/USD should be modeled by an ARIMA(0, 1, 0)

# Testing with auto.arima

auto.arima(ts_eur_usd)

#------------- Results: -------------#

#> auto.arima(eur_usd_prices)
# Series: eur_usd_prices 
# ARIMA(0,1,0)

# sigma^2 = 2.846e-05:  log likelihood = 2647.38
# AIC=-5292.75   AICc=-5292.75   BIC=-5288.21

#------------------------------------#

#---- Interpretation of values: ------------------------------------------------------------------------#

# We observe that our model predicted with the PACF and ACF for eur_usd is consistent with the auto.arima function 

#---------------------------------------------------------------------------------------------------------#

######################### Gold USD ######################################################################

# Manually fitting three ARIMA models for comparison
fit_arima_010_gold <- arima(ts_gold, order = c(0, 1, 0))  # ARIMA(0,1,0)
fit_arima_110_gold <- arima(ts_gold, order = c(1, 1, 0))  # ARIMA(1,1,0)
fit_arima_011_gold <- arima(ts_gold, order = c(0, 1, 1))  # ARIMA(0,1,1)

# Retrieve AIC and BIC for our three models
aic_values <- c(fit_arima_010_gold$aic, fit_arima_110_gold$aic, fit_arima_011_gold$aic)
bic_values <- c(BIC(fit_arima_010_gold), BIC(fit_arima_110_gold), BIC(fit_arima_011_gold))

aic_values
bic_values

#------------- Results: -------------#

# > aic_values
# [1] 5774.333 5773.773 5773.627
# > bic_values
# [1] 5778.838 5782.782 5782.635

#------------------------------------#

#---- Interpretation of values: ------------------------------------------------------------------------#

# The lowest AIC points to the third model ARIMA(0,1,1), while the BIC points to the first model ARIMA(1,1,0)

#---------------------------------------------------------------------------------------------------------#

# Testing with the auto.arima function

auto.arima(ts_gold, stepwise = TRUE, seasonal = FALSE)

#------------- Results: -------------#
# Coefficients:
#   ar1      ar2      ma1     ma2   drift
# 0.4737  -0.9303  -0.5262  0.9319  1.0294
# s.e.  0.0423   0.0382   0.0403  0.0410  0.6715

# sigma^2 = 325.8:  log likelihood = -2878.13
# AIC = 5768.25   AICc = 5768.38   BIC = 5795.28

#------------------------------------#

#---- Interpretation of values: ------------------------------------------------------------------------#

# We find a better AIC but a higher BIC; note that the std.errors are lower

#---------------------------------------------------------------------------------------------------------#

########## Fit AR and ARMA models to the series. ##########################################################

eur_usd_ARIMA_model <- arima(ts_eur_usd, order = c(0, 1, 0))  # ARIMA(0,1,0)

gold_ARIMA_model = arima(ts_gold, order = c(2, 1, 2))  # ARIMA(2,1,2)

############################################################################################################

##################### 2. Residual Analysis:#################################################################

# Perform residual diagnostics (plot residuals, ACF of residuals, Ljung-Box test) to ensure the
# model fits well.

#------ Calculating residuals ---------------#

eur_usd_ARIMA_model_res = residuals(eur_usd_ARIMA_model)

gold_ARIMA_model_res <- residuals(gold_ARIMA_model)

#-----------------------------------------#

#--- Plotting residuals -----------------#

plot(eur_usd_ARIMA_model_res, main="Residuals of ARIMA(0, 1, 0) Model EUR/USD", col="blue")

plot(gold_ARIMA_model_res, main="ACF of ARIMA(2, 1, 2) Residuals GOLD/USD", col='red')

#-----------------------------------------#

#--- Interpretation of results --------#

# We observe that the residual variance is not constant; we will need a model that accounts for heteroscedasticity.

#-----------------------------------------#

#--- ACF/PACF for residuals ---#

acf(eur_usd_ARIMA_model_res)
pacf(eur_usd_ARIMA_model_res)

acf(gold_ARIMA_model_res)
pacf(gold_ARIMA_model_res)

#-----------------------------------------#

#--- Interpretation of results --------#

# An ACF showing only a correlation with itself, indicating no autocorrelation in the residuals series.

# Same interpretation for PACF, which shows no obvious correlation.

# This allows us to conclude that both models are well-specified and adequately represent the data.

#-----------------------------------------#

#--------- Ljung-Box Test ---------------#

Box.test(eur_usd_ARIMA_model_res, lag = 10, type = "Ljung-Box")

Box.test(gold_ARIMA_model_res, lag = 10, type = "Ljung-Box")

#-----------------------------------------#

#------------- Results: -----------------#

# data:  eur_usd_ARIMA_model_res
# X-squared = 14.469, df = 10, p-value = 0.1527

# data:  gold_ARIMA_model_res
# X-squared = 10.1, df = 10, p-value = 0.4318

#-----------------------------------------#

#--- Interpretation of results --------#

# Both models have p-values > 0.05, indicating no autocorrelation in residuals.

#-----------------------------------------#

#------------- Conclusion ----------------#

# Our models:

#.    - ARIMA(2,1,2) for GOLD / USD
#.    - ARIMA(0,1,0) for EUR / USD

# Capture all information in our data and are therefore well-suited.

#-----------------------------------------#

############################################################################################################

################ 3. Heteroscedasticity Testing ############################################################

library(FinTS)

#--- Conducting a heteroscedasticity test ---#

ArchTest(eur_usd_ARIMA_model_res, lags = 10)

ArchTest(gold_ARIMA_model_res, lags = 10)

#------------- Results: -----------------#

# ARCH LM-test; Null hypothesis: no ARCH effects

# data:  eur_usd_ARIMA_model_res
# Chi-squared = 25.363, df = 10, p-value = 0.004699

# data:  gold_ARIMA_model_res
# Chi-squared = 34.62, df = 10, p-value = 0.0001449

#-----------------------------------------#

#--- Interpretation of results --------#

# Both models have p-values < 0.05, indicating heteroscedasticity in residuals, with variance not being constant.
# We can therefore conclude that our residuals exhibit heteroscedasticity.

# The variance of our residuals is not constant, indicating dispersion.

#-----------------------------------------#

## Interpret the results and discuss the presence of volatility clustering ##########################

# Given the presence of heteroscedasticity, it seems highly likely 
# that "volatility clusters" will appear.

# Indeed, as the variance of the residuals is not constant, the variance becomes conditional,
# and because past shocks and their velocity impact future variance, it is reasonable
# to assume there will be periods of high volatility and others of calm.

# This observation is consistent with the macro perspective of this data.
# We often observe periods of crisis and calm in financial markets.

############################################################################################################

# III ARCH and GARCH Model

############################################################################################################

############################################################################################################

# III ARCH and GARCH Model

###########################################################################################################

############### 1. Fitting ARCH Models: ###################################################################

library(rugarch)

######################### Euro Dollar ######################################################################

#------ Specification of the ARCH(1) model for EUR/USD -----------#

arch_spec_eur_usd <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 0)),
                                mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                                distribution.model = "norm")

#---------------------------------------------------------------#

#--- Fitting the ARCH(1) model to the residuals of EUR/USD ---#

arch_fit_eur_usd <- ugarchfit(spec = arch_spec_eur_usd, data = eur_usd_ARIMA_model_res)

#---------------------------------------------------------------#

#------------- Results: -----------------#

print(arch_fit_eur_usd)

#*---------------------------------*
#*          GARCH Model Fit        *
#*---------------------------------*
#  
#  Conditional Variance Dynamics 	
#-----------------------------------
#  GARCH Model	: sGARCH(1,0)
#Mean Model	: ARFIMA(0,0,0)
#Distribution	: norm 
#
#Optimal Parameters
#------------------------------------
#             Estimate  Std. Error  t.  value Pr(>|t|)
#omega   0.000026    0.000002  14.3368 0.000000
#alpha1  0.102523    0.049998   2.0505 0.040313
#
# Information Criteria
# ------------------------------------
#   
# Akaike       -7.6331
# Bayes        -7.6200
# Shibata      -7.6331
# Hannan-Quinn -7.6280

#-----------------------------------------#

#--- Interpretation of Results --------#

# We have an alpha0 / Omega = 0 with a very low std.E
# which makes sense as an exchange rate between two global
# currencies tends to fluctuate very little on average.
# Having an average variance close to 0 is logical.

# Alpha 1 = 0.102523 with Std.E = 0.049998, which is significant.

# Both p-values are low, indicating the significance of 
# the parameters in the model.

#-----------------------------------------#

######################### Gold USD ######################################################################

#--- Specification of the ARCH(1) model for GOLD/USD -------------#

arch_spec_gold <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 0)),
                             mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                             distribution.model = "norm")

#---------------------------------------------------------------#

#--- Fitting the ARCH(1) model to the residuals of GOLD/USD ---#

arch_fit_gold <- ugarchfit(spec = arch_spec_gold, data = gold_ARIMA_model_res)

#---------------------------------------------------------------#

#------------- Results: -----------------#

print(arch_fit_gold)

#*---------------------------------*
#  *          GARCH Model Fit        *
#  *---------------------------------*
#  
#  Conditional Variance Dynamics 	
#-----------------------------------
#  GARCH Model	: sGARCH(1,0)
#Mean Model	: ARFIMA(0,0,0)
#Distribution	: norm 
#
#Optimal Parameters
#------------------------------------
#  Estimate  Std. Error   t value Pr(>|t|)
#omega     323.56   23.147967 13.977948        0
#alpha1      0.00    0.046835  0.000004        1

#Information Criteria
#------------------------------------
#  
#Akaike       8.6227
#Bayes        8.6362
#Shibata      8.6227
#Hannan-Quinn 8.6279

#-----------------------------------------#

#--- Interpretation of Results --------#

# We have an alpha0 / Omega = 323.56 with a std.E of 23.15

# This is logical since gold is an asset that fluctuates 
# significantly on average. Having a high average variance is expected.

# The very low p-value (0) shows the importance of this 
# parameter within the model.

# Alpha 1 = 0.00 with a Std.E = 0.046835

# The p-value of 1 indicates that this parameter does not 
# significantly contribute to the model.

#-----------------------------------------#

###### Verification of volatility capture by ARCH(1) ######################################

#----- We retrieve the residuals and divide by conditional variance -----------#

eur_usd_res_std <- residuals(arch_fit_eur_usd, standardize = TRUE)

gold_res_std <- residuals(arch_fit_gold, standardize = TRUE)

#---- Plotting non-standardized and standardized residuals for comparison ----#

par(mfrow = c(4, 1), mar = c(4, 4, 2, 1))  # adjusted margins

plot(eur_usd_res_std, main = "EUR/USD Standardized Residuals", ylab = "Residuals", xlab = "Time")

plot(eur_usd_ARIMA_model_res, main = "EUR/USD Non-Standardized Residuals", ylab = "Residuals", xlab = "Time")

plot(gold_res_std, main = "Gold Standardized Residuals", ylab = "Residuals", xlab = "Time")

plot(gold_ARIMA_model_res, main = "Gold Non-Standardized Residuals", ylab = "Residuals", xlab = "Time")

#--- Interpretation of Results --------#

# In both time series, we observe that the standardized series from the ARCH(1) model 
# does not have constant variance. The model does not predict the variance 
# of residuals accurately and may not be well-suited.

#-----------------------------------------#

#--- ARCH test on standardized residuals: ---#

ArchTest(eur_usd_res_std, lags = 10)

ArchTest(gold_res_std, lags = 10)

#------------- Results: -----------------#

#ARCH LM-test; Null hypothesis: no ARCH effects

#data:  eur_usd_res_std
#Chi-squared = 20.788, df = 10, p-value = 0.02262

#ARCH LM-test; Null hypothesis: no ARCH effects

#data:  gold_res_std
#Chi-squared = 33.187, df = 10, p-value = 0.0002534

#-----------------------------------------#

#--- Interpretation of Results --------#

# As suggested by the results of the ARCH(1,0) model for the EUR/USD residual series,
# we observe a decrease in heteroscedasticity in the residual series (as the p-value of 
# the ARCH test goes from 0.0046 to 0.022). However, the residual series remains very heteroscedastic 
# since the p-value < 0.05. The ARCH(1,0) model therefore captures very little of the volatility in the 
# EUR/USD residual series. For the gold residual series, we observe almost no improvement with ARCH(1,0); 
# the series remains highly heteroscedastic and does not capture volatility at all.

#-----------------------------------------#

############### 2. Fitting GARCH Models: ###################################################################

# Fit a GARCH(1,1) model to the series.

######################### Euro Dollar ######################################################################

#------ Specification of the GARCH(1,1) model for EUR/USD -----------#

arch_11_spec_eur_usd <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                                   mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                                   distribution.model = "norm")

#---------------------------------------------------------------#

#--- Fitting the GARCH(1,1) model to the residuals of EUR/USD ---#

eur_usd_arch_11_fit <- ugarchfit(spec = arch_11_spec_eur_usd, data = eur_usd_ARIMA_model_res)

#---------------------------------------------------------------#

#------------- Results: -----------------#

print(eur_usd_arch_11_fit)

# *---------------------------------*
# *          GARCH Model Fit        *
# *---------------------------------*
# This section indicates that a GARCH(1,1) model has been fitted to the data.

# Conditional Variance Dynamics
# -----------------------------------
# GARCH Model : sGARCH(1,1)

# Mean Model : ARFIMA(0,0,0)

# Distribution : norm

# Optimal Parameters
# ------------------------------------
#           Estimate   Std. Error   t value   Pr(>|t|)
# omega   0.000000    0.000000    0.13556   0.89217
# alpha1  0.017271    0.002720    6.34978   0.00000
# beta1   0.980262    0.002762  354.91888   0.00000

# Robust Standard Errors:

#           Estimate   Std. Error   t value   Pr(>|t|)
# omega   0.000000    0.000004   0.009273   0.99260
# alpha1  0.017271    0.037250   0.463637   0.64291
# beta1   0.980262    0.035189  27.857013   0.00000
# 

# Information Criteria
# ------------------------------------
# Akaike       -7.6893
# Bayes        -7.6697
# Shibata      -7.6893
# Hannan-Quinn -7.6817
# 

#-----------------------------------------#

#--- Interpretation of Results --------#

# omega: It is very close to zero, indicating that the average variance of errors is extremely low.
# 
# alpha1:  0.017271 means that recent shocks have a low effect on volatility.
# 
# beta1: A high coefficient of 0.980262 indicates a strong persistence in volatility
# (volatility that persists over the long term).

# The p-values:
# omega has a very high p-value (0.89217), which means it is not statistically significant.
# alpha1 and beta1 have very low p-values (0.00000), so they are highly significant in the model.

# With robust errors, omega remains non-significant (p-value of 0.99260), and alpha1 also becomes non-significant (p-value of 0.64291).
# Only beta1 remains significant with a very low p-value, confirming strong volatility persistence.

# The AIC/BIC values are similar

#-----------------------------------------#

######################### Gold USD ######################################################################

#--- Specification of the GARCH(1,1) model for GOLD/USD -------------#

arch_11_spec_gold <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                                mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                                distribution.model = "norm")

#---------------------------------------------------------------#

#--- Fitting the GARCH(1,1) model to the residuals of GOLD/USD ---#

gold_arch_11_fit <- ugarchfit(spec = arch_11_spec_gold, data = gold_ARIMA_model_res)
#---------------------------------------------------------------#

#------------- Results: -----------------#

print(gold_arch_11_fit)

# *---------------------------------*
# *          GARCH Model Fit        *
# *---------------------------------*

# Conditional Variance Dynamics
# -----------------------------------
# GARCH Model : sGARCH(1,1)

# Mean Model : ARFIMA(0,0,0)

# Distribution : norm

# Optimal Parameters
# ------------------------------------
#           Estimate  Std. Error   t value   Pr(>|t|)
# omega   6.864364    3.990052    1.7204    0.085365
# alpha1  0.038324    0.012818    2.9899    0.002791
# beta1   0.941304    0.019975   47.1241    0.000000
#

# Robust Standard Errors:

#           Estimate   Std. Error   t value   Pr(>|t|)
# omega   6.864364    3.619472    1.8965     0.057893
# alpha1  0.038324    0.012325    3.1094     0.001875
# beta1   0.941304    0.016011   58.7910     0.000000
# 
# Information Criteria
# ------------------------------------
# Akaike       8.5877
# Bayes        8.6079
# Shibata      8.5876
# Hannan-Quinn 8.5955
# 
# These information criteria (Akaike, Bayes, Shibata, and Hannan-Quinn) are used to evaluate the trade-off between model fit and complexity (number of parameters). Lower values indicate a better model. Here, the Akaike (AIC) of 8.5877 is relatively low, suggesting a good balance between model fit and simplicity.

#-----------------------------------------#

#--- Interpretation of Results --------#

# omega: is 6.864, meaning that the average variance is relatively high.
# 
# alpha1: a value of 0.038324, meaning past shocks have a moderate effect on conditional variance.
# 
# beta1: a value of 0.941304, showing strong persistence, meaning volatility is long-lasting
# and that shocks impact the series for a long time.

# The p-values:
# omega has a p-value of 0.085365, close to the significance threshold.
# alpha1 has a p-value of 0.002791, indicating statistical significance.
# beta1 has an extremely low p-value (0.000000), showing that 
# persistence is highly significant in the model.

# With robust errors, omega becomes slightly more significant with a p-value of 0.057893, close to the 5% threshold.
# alpha1 and beta1 remain significant with low p-values, confirming the importance of these parameters.

# The AIC/BIC values are also slightly better.

#-----------------------------------------#

#----------- Conclusion ------------------#

# For both series, the GARCH(1,1) model
# seems to be better suited.

#-----------------------------------------#

############### 3: Model Validation: ###################################################################

#----- Retrieve the fitted values from our models -----#

eur_usd_garch_11_fitted_val = fitted(eur_usd_arch_11_fit)

gold_garch_11_fitted_val = fitted(gold_arch_11_fit)


#---- Convert them to time series ------------------#

eur_usd_garch_11_fitted_val_ts = ts(eur_usd_garch_11_fitted_val, start = c(2022, 1), frequency = 252)

gold_garch_11_fitted_val_ts = ts(gold_garch_11_fitted_val, start = c(2022, 1), frequency = 252)

par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))  # adjusted margins
plot(eur_usd_garch_11_fitted_val_ts, main = "EUR/USD fitted values", xlab = "Date", ylab = "Residuals")
plot(eur_usd_ARIMA_model_res, main = "EUR/USD observed values", xlab = "Date", ylab = "Residuals")
plot(gold_garch_11_fitted_val_ts, main = "Gold fitted values", xlab = "Date", ylab = "Residuals")
plot(gold_ARIMA_model_res, main = "Gold observed values", xlab = "Date")

#--- Interpretation of Results --------#

# Strangely, the fitted values of our model are always zero.
# To work around this issue, we will manually generate errors using the
# conditional variances from the GARCH(1,1) model.

#-----------------------------------------#

#--- Extracting conditional sigmas ----#

sigma_garch_gold <- sigma(gold_arch_11_fit)

sigma_garch_eur_usd <- sigma(eur_usd_arch_11_fit)

#-- Simulating errors ------------------#

set.seed(123)  # Fix the seed for reproducibility

simulated_errors_gold <- rnorm(length(sigma_garch_gold), mean = 0, sd = sigma_garch_gold)

simulated_errors_eur_usd <- rnorm(length(sigma_garch_eur_usd), mean = 0, sd = sigma_garch_eur_usd)

#---- Convert to time series ---------------------#

simulated_errors_gold_ts <- ts(simulated_errors_gold, start = start(sigma_garch_gold), frequency = frequency(sigma_garch_gold))

simulated_errors_eur_usd_ts <- ts(simulated_errors_eur_usd, start = start(sigma_garch_eur_usd), frequency = frequency(sigma_garch_eur_usd))

#---- Plot generated errors against observed errors ---# 

par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))  # adjusted margins

plot(simulated_errors_eur_usd_ts, main = "EUR/USD fitted values", xlab = "Date", ylab = "Residuals")
plot(eur_usd_ARIMA_model_res, main = "EUR/USD observed values", xlab = "Date", ylab = "Residuals")
plot(simulated_errors_gold_ts, main = "Gold fitted values", xlab = "Date", ylab = "Residuals")
plot(gold_ARIMA_model_res, main = "Gold observed values", xlab = "Date", ylab = "Residuals")

#------ Results and Analysis ---------------------------#

# We observe that the fitted values follow the same trend and show similar fluctuations
# as the observed residuals, indicating that the GARCH model is effective for modeling volatility.

#--------------------------------------------------------#

# Perform an autocorrelation test on the residuals of the GARCH(1,1) model to validate the model.

# Extract standardized residuals from the GARCH model

eur_usd_garch_res_std <- residuals(eur_usd_arch_11_fit, standardize = TRUE)
gold_garch_res_std <- residuals(gold_arch_11_fit, standardize = TRUE)

# Apply the Ljung-Box test on residuals

Box.test(eur_usd_garch_res_std, lag = 10, type = "Ljung-Box")
Box.test(gold_garch_res_std, lag = 10, type = "Ljung-Box")


#------------- Results: -----------------#

#            Box-Ljung test (EUR/USD)

# data:  eur_usd_garch_res_std
# X-squared = 9.1018, df = 10, p-value = 0.5225

#            Box-Ljung test (Gold)
# 
# data:  gold_garch_res_std
# X-squared = 10.895, df = 10, p-value = 0.3657

#-----------------------------------------#

#--- Interpretation of Results --------#

# The p-values of both tests are well above 0.05 (0.52 for EUR/USD and 0.36 for gold).
# We conclude that the standardized residuals of the GARCH(1,1) model are independent and non-autocorrelated.
# This confirms that the GARCH(1,1) model is suitable and effective for both time series.

#-----------------------------------------#

############################################################################################################

###########################################################################################################

#  IV) Forecasts

###########################################################################################################


############################################################################################################

# 20-point forecast for EUR/USD
forecast_eur_usd <- ugarchforecast(eur_usd_arch_11_fit, n.ahead = 20)

# 20-point forecast for Gold
forecast_gold <- ugarchforecast(gold_arch_11_fit, n.ahead = 20)

# Display results
plot(forecast_eur_usd, which = 1)  # Mean forecast for EUR/USD
plot(forecast_gold, which = 1)     # Mean forecast for Gold

#------------- Results: -----------------#

# The forecasted values are strangely zero
# but the conditional sigmas are not.

# We will therefore manually perform a forecast for both 
# ARIMA values and then errors using GARCH.

#-----------------------------------------#

par(mfrow = c(1, 1))

######################### Gold USD ######################################################################

#--- Convert historical data into a time series --------#

n_obs_gold <- length(gold_prices)
ts_gold_val <- ts(as.numeric(gold_prices), start = c(2022, 1), frequency = 252)

#--- ARIMA forecast for the next 20 periods -------------#

arima_forecast_gold <- predict(gold_ARIMA_model, n.ahead = 20, se.fit = TRUE)

#---- Forecast of conditional variances with GARCH ----------#

garch_forecast_gold <- ugarchforecast(gold_arch_11_fit, n.ahead = 20)

#----- Extract predicted conditional variances (sigma) -----#

garch_sigma_forecast_gold <- sigma(garch_forecast_gold)

#--- Simulate future errors with the predicted conditional variance ---#

set.seed(123)  # For reproducibility
simulated_errors_gold <- rnorm(20, mean = 0, sd = garch_sigma_forecast_gold)

#--- Combine forecasts (ARIMA + GARCH) ----------#

manual_forecast_gold <- arima_forecast_gold$pred + simulated_errors_gold

#--- Calculate confidence intervals for ARIMA --------#

arima_upper_95_gold <- arima_forecast_gold$pred + 1.96 * arima_forecast_gold$se
arima_lower_95_gold <- arima_forecast_gold$pred - 1.96 * arima_forecast_gold$se

#--- Create time series for confidence intervals --------#
# Adjust the start date to match the end of historical data

last_obs_gold <- time(ts_gold_val)[length(ts_gold_val)]  # Get the last date of historical data

ts_arima_upper_95_gold <- ts(arima_upper_95_gold, start = last_obs_gold + 1/252, frequency = 252)
ts_arima_lower_95_gold <- ts(arima_lower_95_gold, start = last_obs_gold + 1/252, frequency = 252)
ts_manual_forecast_gold <- ts(manual_forecast_gold, start = last_obs_gold + 1/252, frequency = 252)
ts_arima_forecast_gold <- ts(arima_forecast_gold$pred, start = last_obs_gold + 1/252, frequency = 252)

#--- Visualize forecasts with historical data on a single plot --------#

# Plot historical data with forecasts
plot(ts_gold_val, type = "l", col = "black", ylab = "Gold Price", 
     main = "Manual Forecast with GARCH (Gold) and 95% Confidence Intervals", 
     xlab = "Time", lwd = 2, xlim = c(2022, 2025))

# Add manual forecasts (ARIMA + GARCH) in red (solid lines)
lines(ts_manual_forecast_gold, type = "l", col = "red", lwd = 2)

# Add ARIMA forecasts only in blue (solid lines)
lines(ts_arima_forecast_gold, type = "l", col = "blue", lwd = 2)

# Add confidence intervals (in light blue)
lines(ts_arima_upper_95_gold, type = "l", col = "lightblue", lwd = 2, lty = 2)
lines(ts_arima_lower_95_gold, type = "l", col = "lightblue", lwd = 2, lty = 2)

# Add a legend to distinguish the lines
legend("bottomright", legend = c("Manual Forecast (ARIMA + GARCH)", "ARIMA Forecast", "Historical Data", "95% CI (ARIMA)"), 
       col = c("red", "blue", "black", "lightblue"), lty = c(1, 1, 1, 2), lwd = 2)

#--- Visualize forecasts only --------#

# Plot ARIMA + GARCH forecasts (red lines)
plot(ts_manual_forecast_gold, type = "l", col = "red", ylab = "Gold Price", 
     main = "Forecast (ARIMA + GARCH) and ARIMA with 95% Confidence Intervals (Gold)", xlab = "Time", lwd = 2)

# Add ARIMA forecasts only in blue
lines(ts_arima_forecast_gold, type = "l", col = "blue", lwd = 2)

# Add a legend to distinguish the lines
legend("topright", legend = c("Manual Forecast (ARIMA + GARCH)", "ARIMA Forecast"), 
       col = c("red", "blue"), lty = c(1, 1, 2), lwd = 2)


######################### EUR USD ######################################################################

#--- Convert historical EUR/USD data into a time series --------#

n_obs_eur <- length(eur_usd_prices)
ts_eur_usd_val <- ts(as.numeric(eur_usd_prices), start = c(2022, 1), frequency = 252)

#--- ARIMA forecast for the next 20 periods EUR/USD -------------#

arima_forecast_eur_usd <- predict(eur_usd_ARIMA_model, n.ahead = 20, se.fit = TRUE)

#---- Forecast of conditional variances with GARCH for EUR/USD -----#

garch_forecast_eur_usd <- ugarchforecast(eur_usd_arch_11_fit, n.ahead = 20)

#----- Extract predicted conditional variances (sigma) for EUR/USD -----#

garch_sigma_forecast_eur_usd <- sigma(garch_forecast_eur_usd)

#--- Simulate future errors with the predicted conditional variance for EUR/USD ---#

set.seed(123)  # For reproducibility
simulated_errors_eur_usd <- rnorm(20, mean = 0, sd = garch_sigma_forecast_eur_usd)

#--- Combine forecasts (ARIMA + GARCH) for EUR/USD ----------#

manual_forecast_eur_usd <- arima_forecast_eur_usd$pred + simulated_errors_eur_usd

#--- Combine forecasts with historical EUR/USD data --------#

# Here, we keep only the forecasts to avoid visual shifts
ts_manual_forecast_eur_usd <- ts(manual_forecast_eur_usd, start = c(2024, 191), frequency = 252)
ts_arima_forecast_eur_usd <- ts(arima_forecast_eur_usd$pred, start = c(2024, 191), frequency = 252)

#--- Calculate confidence intervals for ARIMA --------#

arima_upper_95 <- arima_forecast_eur_usd$pred + 1.96 * arima_forecast_eur_usd$se
arima_lower_95 <- arima_forecast_eur_usd$pred - 1.96 * arima_forecast_eur_usd$se

#--- Create time series for confidence intervals --------#

ts_arima_upper_95 <- ts(arima_upper_95, start = c(2024, 191), frequency = 252)
ts_arima_lower_95 <- ts(arima_lower_95, start = c(2024, 191), frequency = 252)

#--- Visualize forecasts with confidence intervals EUR/USD --------#

# Plot historical data with forecasts
plot(ts_eur_usd_val, type = "l", col = "black", ylab = "EUR/USD Exchange Rate", 
     main = "Manual Forecast with GARCH (EUR/USD) and 95% Confidence Intervals", 
     xlab = "Time", lwd = 2, xlim = c(2022, 2025))

# Add manual forecasts (ARIMA + GARCH) in red (solid lines)
lines(ts_manual_forecast_eur_usd, type = "l", col = "red", lwd = 2)

# Add ARIMA forecasts only in blue (solid lines)
lines(ts_arima_forecast_eur_usd, type = "l", col = "blue", lwd = 2)

# Add confidence intervals (in light blue)
lines(ts_arima_upper_95, type = "l", col = "lightblue", lwd = 2, lty = 2)
lines(ts_arima_lower_95, type = "l", col = "lightblue", lwd = 2, lty = 2)

# Add a legend to distinguish the lines
legend("bottomright", legend = c("Manual Forecast (ARIMA + GARCH)", "ARIMA Forecast", "Historical Data", "95% CI (ARIMA)"), 
       col = c("red", "blue", "black", "lightblue"), lty = c(1, 1, 1, 2), lwd = 2)

#--- Visualization of EUR/USD forecasts only --------#

# Plot ARIMA + GARCH forecasts (red lines)
plot(ts_manual_forecast_eur_usd, type = "l", col = "red", ylab = "EUR/USD Exchange Rate", 
     main = "Forecast (ARIMA + GARCH) and ARIMA with 95% Confidence Intervals (EUR/USD)", xlab = "Time", lwd = 2)

# Add ARIMA forecasts only in blue
lines(ts_arima_forecast_eur_usd, type = "l", col = "blue", lwd = 2)

# Add a legend to distinguish the lines
legend("topright", legend = c("Manual Forecast (ARIMA + GARCH)", "ARIMA Forecast"), 
       col = c("red", "blue"), lty = c(1, 1, 2), lwd = 2)

###############################################################################################################

################## 2. Forecast Evaluation ######################################################################

######  Compare the forecasted values to the actual observations #############################################

# For gold and eur_usd, we have relatively interesting forecasts. Obviously, these depend on the
# seed used to generate the errors.

# The exchange rate shows a relative decline, though it seems to stabilize around 1.10.

# For gold, it also tends to stabilize, though we can see that the 95% confidence intervals
# are quite large.

######  Mean Squared Error (MSE) and Mean Absolute Percentage Error (MAPE) of the forecasts. #################

######################### Gold USD ######################################################################

#--- Extract the last 20 actual observations and align with the forecasts ---#

# Extract the last 20 values from the historical series

observed_gold <- tail(ts_gold_val, 20)

#--- Convert the forecasts to time series aligned with observations ---#

# For GOLD
manual_forecast_gold_ts <- ts(manual_forecast_gold, start = start(observed_gold), frequency = frequency(observed_gold))
arima_forecast_gold_ts <- ts(arima_forecast_gold$pred, start = start(observed_gold), frequency = frequency(observed_gold))

#--- Calculate MSE and MAPE for GOLD ---#

# MSE for Gold (ARIMA + GARCH)
mse_manual_gold <- mean((observed_gold - manual_forecast_gold_ts) ^ 2)

# MSE for Gold (ARIMA)
mse_arima_gold <- mean((observed_gold - arima_forecast_gold_ts) ^ 2)

# MAPE for Gold (ARIMA + GARCH)
mape_manual_gold <- mean(abs((observed_gold - manual_forecast_gold_ts) / observed_gold)) * 100

# MAPE for Gold (ARIMA)
mape_arima_gold <- mean(abs((observed_gold - arima_forecast_gold_ts) / observed_gold)) * 100

# Display results for Gold
cat("MSE (ARIMA + GARCH, Gold):", mse_manual_gold, "\n")
cat("MSE (ARIMA, Gold):", mse_arima_gold, "\n")
cat("MAPE (ARIMA + GARCH, Gold):", mape_manual_gold, "%\n")
cat("MAPE (ARIMA, Gold):", mape_arima_gold, "%\n")

#------------- Résulats: -----------------#

# > cat("MSE (ARIMA + GARCH, Gold):", mse_manual_gold, "\n")
# MSE (ARIMA + GARCH, Gold): 2736.887 
# > cat("MSE (ARIMA, Gold):", mse_arima_gold, "\n")
# MSE (ARIMA, Gold): 2356.171 
# > cat("MAPE (ARIMA + GARCH, Gold):", mape_manual_gold, "%\n")
# MAPE (ARIMA + GARCH, Gold): 1.633064 %
# > cat("MAPE (ARIMA, Gold):", mape_arima_gold, "%\n")
# MAPE (ARIMA, Gold): 1.532201 %

#-----------------------------------------#

######################### EUR USD ######################################################################

# Extract the last 20 values from the historical series

observed_eur_usd <- tail(ts_eur_usd_val, 20)

#--- Convert the forecasts to time series aligned with observations ---#

# For EUR/USD
manual_forecast_eur_usd_ts <- ts(manual_forecast_eur_usd, start = start(observed_eur_usd), frequency = frequency(observed_eur_usd))
arima_forecast_eur_usd_ts <- ts(arima_forecast_eur_usd$pred, start = start(observed_eur_usd), frequency = frequency(observed_eur_usd))

#--- Calculate MSE and MAPE for EUR/USD ---#

# MSE for EUR/USD (ARIMA + GARCH)
mse_manual_eur_usd <- mean((observed_eur_usd - manual_forecast_eur_usd_ts) ^ 2)

# MSE for EUR/USD (ARIMA)
mse_arima_eur_usd <- mean((observed_eur_usd - arima_forecast_eur_usd_ts) ^ 2)

# MAPE for EUR/USD (ARIMA + GARCH)
mape_manual_eur_usd <- mean(abs((observed_eur_usd - manual_forecast_eur_usd_ts) / observed_eur_usd)) * 100

# MAPE for EUR/USD (ARIMA)
mape_arima_eur_usd <- mean(abs((observed_eur_usd - arima_forecast_eur_usd_ts) / observed_eur_usd)) * 100

# Display results for EUR/USD
cat("MSE (ARIMA + GARCH, EUR/USD):", mse_manual_eur_usd, "\n")
cat("MSE (ARIMA, EUR/USD):", mse_arima_eur_usd, "\n")
cat("MAPE (ARIMA + GARCH, EUR/USD):", mape_manual_eur_usd, "%\n")
cat("MAPE (ARIMA, EUR/USD):", mape_arima_eur_usd, "%\n")

#------------- Résulats: -----------------#

# > cat("MSE (ARIMA + GARCH, EUR/USD):", mse_manual_eur_usd, "\n")
# MSE (ARIMA + GARCH, EUR/USD): 0.0001416963 
# > cat("MSE (ARIMA, EUR/USD):", mse_arima_eur_usd, "\n")
# MSE (ARIMA, EUR/USD): 0.000116888 
# > cat("MAPE (ARIMA + GARCH, EUR/USD):", mape_manual_eur_usd, "%\n")
# MAPE (ARIMA + GARCH, EUR/USD): 0.8882546 %
# > cat("MAPE (ARIMA, EUR/USD):", mape_arima_eur_usd, "%\n")
# MAPE (ARIMA, EUR/USD): 0.8610627 %

#-----------------------------------------#

###############################################################################################################

################## 3. Scenario Analysis: ######################################################################

# To change the scenario, we will arbitrarily change the seed and perform a new simulation.

set.seed(10000000)  # For reproducibility

######################### Gold USD ######################################################################

#--- Convert historical data into a time series --------#

n_obs_gold <- length(gold_prices)
ts_gold_val <- ts(as.numeric(gold_prices), start = c(2022, 1), frequency = 252)

#--- ARIMA forecast for the next 20 periods -------------#

arima_forecast_gold <- predict(gold_ARIMA_model, n.ahead = 20, se.fit = TRUE)

#---- Forecast conditional variances with GARCH ----------#

garch_forecast_gold <- ugarchforecast(gold_arch_11_fit, n.ahead = 20)

#----- Extract predicted conditional variances (sigma) -----#

garch_sigma_forecast_gold <- sigma(garch_forecast_gold)

#--- Simulate future errors with the predicted conditional variance ---#

simulated_errors_gold <- rnorm(20, mean = 0, sd = garch_sigma_forecast_gold)

#--- Combine forecasts (ARIMA + GARCH) ----------#

manual_forecast_gold <- arima_forecast_gold$pred + simulated_errors_gold

#--- Calculate confidence intervals for ARIMA --------#

arima_upper_95_gold <- arima_forecast_gold$pred + 1.96 * arima_forecast_gold$se
arima_lower_95_gold <- arima_forecast_gold$pred - 1.96 * arima_forecast_gold$se

#--- Create time series for confidence intervals --------#
# Adjust the start date to match the end of the historical data

last_obs_gold <- time(ts_gold_val)[length(ts_gold_val)]  # Retrieve the last date of historical data

ts_arima_upper_95_gold <- ts(arima_upper_95_gold, start = last_obs_gold + 1/252, frequency = 252)
ts_arima_lower_95_gold <- ts(arima_lower_95_gold, start = last_obs_gold + 1/252, frequency = 252)
ts_manual_forecast_gold <- ts(manual_forecast_gold, start = last_obs_gold + 1/252, frequency = 252)
ts_arima_forecast_gold <- ts(arima_forecast_gold$pred, start = last_obs_gold + 1/252, frequency = 252)


#--- Visualization of forecasts only --------#

######################### Gold USD ######################################################################

# Plot ARIMA + GARCH forecasts (red lines)
plot(ts_manual_forecast_gold, type = "l", col = "red", ylab = "Gold Price", 
     main = "Forecast (ARIMA + GARCH) and ARIMA with 95% Confidence Intervals (Gold)", xlab = "Time", lwd = 2)

# Add ARIMA-only forecasts in blue
lines(ts_arima_forecast_gold, type = "l", col = "blue", lwd = 2)

# Add a legend to distinguish the lines
legend("topright", legend = c("Manual Forecast (ARIMA + GARCH)", "ARIMA Forecast"), 
       col = c("red", "blue"), lty = c(1, 1, 2), lwd = 2)

######################### EUR USD ######################################################################

#--- Convert historical EUR/USD data into a time series --------#

n_obs_eur <- length(eur_usd_prices)
ts_eur_usd_val <- ts(as.numeric(eur_usd_prices), start = c(2022, 1), frequency = 252)

#--- ARIMA forecast for the next 20 periods EUR/USD -------------#

arima_forecast_eur_usd <- predict(eur_usd_ARIMA_model, n.ahead = 20, se.fit = TRUE)

#---- Forecast conditional variances with GARCH for EUR/USD -----#

garch_forecast_eur_usd <- ugarchforecast(eur_usd_arch_11_fit, n.ahead = 20)

#----- Extract predicted conditional variances (sigma) for EUR/USD -----#

garch_sigma_forecast_eur_usd <- sigma(garch_forecast_eur_usd)

#--- Simulate future errors with the predicted conditional variance for EUR/USD ---#

simulated_errors_eur_usd <- rnorm(20, mean = 0, sd = garch_sigma_forecast_eur_usd)

#--- Combine forecasts (ARIMA + GARCH) for EUR/USD ----------#

manual_forecast_eur_usd <- arima_forecast_eur_usd$pred + simulated_errors_eur_usd

#--- Combine forecasts with historical EUR/USD data --------#

# Here, we keep only the forecasts to avoid visual shifts
ts_manual_forecast_eur_usd <- ts(manual_forecast_eur_usd, start = c(2024, 191), frequency = 252)
ts_arima_forecast_eur_usd <- ts(arima_forecast_eur_usd$pred, start = c(2024, 191), frequency = 252)

#--- Calculate confidence intervals for ARIMA --------#

arima_upper_95 <- arima_forecast_eur_usd$pred + 1.96 * arima_forecast_eur_usd$se
arima_lower_95 <- arima_forecast_eur_usd$pred - 1.96 * arima_forecast_eur_usd$se

#--- Create time series for confidence intervals --------#

ts_arima_upper_95 <- ts(arima_upper_95, start = c(2024, 191), frequency = 252)
ts_arima_lower_95 <- ts(arima_lower_95, start = c(2024, 191), frequency = 252)

#--- Visualization of forecasts with confidence intervals EUR/USD --------#

# Plot historical data with forecasts
plot(ts_eur_usd_val, type = "l", col = "black", ylab = "EUR/USD Exchange Rate", 
     main = "Manual Forecast with GARCH (EUR/USD) and 95% Confidence Intervals", 
     xlab = "Time", lwd = 2, xlim = c(2022, 2025))

# Add manual forecasts (ARIMA + GARCH) in red (solid lines)
lines(ts_manual_forecast_eur_usd, type = "l", col = "red", lwd = 2)

# Add ARIMA-only forecasts in blue (solid lines)
lines(ts_arima_forecast_eur_usd, type = "l", col = "blue", lwd = 2)

# Add confidence intervals (in light blue)
lines(ts_arima_upper_95, type = "l", col = "lightblue", lwd = 2, lty = 2)
lines(ts_arima_lower_95, type = "l", col = "lightblue", lwd = 2, lty = 2)

# Add a legend to distinguish the lines
legend("bottomright", legend = c("Manual Forecast (ARIMA + GARCH)", "ARIMA Forecast", "Historical Data", "95% CI (ARIMA)"), 
       col = c("red", "blue", "black", "lightblue"), lty = c(1, 1, 1, 2), lwd = 2)

#--- Visualization of EUR/USD forecasts only --------#

# Plot ARIMA + GARCH forecasts (red lines)
plot(ts_manual_forecast_eur_usd, type = "l", col = "red", ylab = "EUR/USD Exchange Rate", 
     main = "Forecast (ARIMA + GARCH) and ARIMA with 95% Confidence Intervals (EUR/USD)", xlab = "Time", lwd = 2)

# Add ARIMA-only forecasts in blue
lines(ts_arima_forecast_eur_usd, type = "l", col = "blue", lwd = 2)

# Add a legend to distinguish the lines
legend("topright", legend = c("Manual Forecast (ARIMA + GARCH)", "ARIMA Forecast"), 
       col = c("red", "blue"), lty = c(1, 1, 2), lwd = 2)

##################################### Analysis ################################################

# The scenario is more pessimistic with this seed.

###############################################################################################

########## 4. Forecasting Evaluation Without ARCH Model #######################################

# ARIMA prediction for EUR/USD
arima_forecast_eur_usd_only <- predict(eur_usd_ARIMA_model, n.ahead = 20)

# ARIMA prediction for Gold
arima_forecast_gold_only <- predict(gold_ARIMA_model, n.ahead = 20)

#--- Convert forecasts to time series aligned with observations ---#

# Extract the last observation from real series to align forecasts
last_obs_gold <- time(ts_gold_val)[length(ts_gold_val)]
last_obs_eur_usd <- time(ts_eur_usd_val)[length(ts_eur_usd_val)]

# Convert ARIMA forecasts to time series (without ARCH/GARCH)
ts_arima_forecast_eur_usd_only <- ts(arima_forecast_eur_usd_only$pred, start = last_obs_eur_usd + 1/252, frequency = 252)
ts_arima_forecast_gold_only <- ts(arima_forecast_gold_only$pred, start = last_obs_gold + 1/252, frequency = 252)

#--- Visualization of forecasts with previous observations -------------#

# For EUR/USD
plot(ts_eur_usd_val, type = "l", col = "black", ylab = "EUR/USD Exchange Rate", 
     main = "Forecast with ARIMA (EUR/USD) vs Previous Observations", xlab = "Time", lwd = 2, xlim = c(2022, 2025))

lines(ts_arima_forecast_eur_usd_only, col = "blue", lwd = 2)  # ARIMA only forecast
legend("bottomright", legend = c("Previous Observations", "ARIMA Forecast"), col = c("black", "blue"), lty = 1, lwd = 2)

# For Gold
plot(ts_gold_val, type = "l", col = "black", ylab = "Gold Price", 
     main = "Forecast with ARIMA (Gold) vs Previous Observations", xlab = "Time", lwd = 2, xlim = c(2022, 2025))

lines(ts_arima_forecast_gold_only, col = "blue", lwd = 2)  # ARIMA only forecast
legend("bottomright", legend = c("Previous Observations", "ARIMA Forecast"), col = c("black", "blue"), lty = 1, lwd = 2)

##################################### Analysis ################################################

# The results are relatively modest.


