

require(forecast)
require(dplyr)
require(gridExtra)
require(ggplot2)
require(fpp)
require(fpp2)
require(urca)
require(fpp3)
#install.packages("fpp3")

# Consider wmurders, the number of women murdered each year (per 100,000 standard population) in the United States. 
ts_murders <- fpp2::wmurders    #Annual female murder rate (per 100,000 standard population) in the USA. 1950-2004.
    #note fpp has a very similar dataset but it's monthly instead of annual

str(ts_murders)
head(ts_murders)
autoplot(ts_murders, main="Annual rate of females murdered (per 100,000 standard population) in the USA") + xlab('') + ylab('Number of women murdered each year')

# a.	By studying appropriate graphs of the series in R, find an appropriate ARIMA(p,d,q) model for these data.
#can't be seasonal - only have annual data
#long steady trends with sudden shart reversals ==> 1,1,0
ts_murders %>% diff() %>% tsdisplay()
ts_murders %>% diff() %>% diff() %>% tsdisplay()
  #the ACF is sinusoidal, and there is a significant spike at 1 but not really one after that and the PACF tends to decrease
  #    as the lags get bigger and bigger, so this could be a AR(1)


ndiffs(ts_murders)
nsdiffs(ts_murders)  # error, non-seasonal data. 

ts_murders %>% diff() %>% ur.kpss() %>% summary()
ts_murders %>% diff() %>% diff() %>% ur.kpss() %>% summary()
  #almost positive that 
    #a) ur.kpss returns the test statistic, not the pvalue, so we should reject the null hypothesis
    #     when the  test statistic is GREATER than the critical value
    #b) the ndiffs function is using the critical value corresponding to 99% confidence/1% chance of getting a test statistic that extreme


# b.	Should you include a constant in the model? Explain.
#don't think this would have a constant as it doesn't have a trend 

# c.	Write this model in terms of the backshift operator.
#(1-phiB1)(1-B2)yt = et


# d.	Fit the model using R and examine the residuals. Is the model satisfactory?
mdlfit_mrdrs_arima <- Arima(ts_murders, order=c(1,2,2))
summary(mdlfit_mrdrs_arima)

checkresiduals(mdlfit_mrdrs_arima)

checkresiduals(fcst_mrdrs_arima)


#   e.	Forecast three times ahead. Check your forecasts by hand to make sure that you know how they have been calculated.
fcst_mrdrs_arima <- forecast(mdlfit_mrdrs_arima, h=3)



# f.	Create a plot of the series with forecasts and prediction intervals for the next three periods shown.

autoplot(fcst_mrdrs_arima, main="Females Murdered Annually in the US, per 100k population") + 
  ylab('Females Murdered per 100k population') + xlab('') +
  autolayer(fcst_mrdrs_arima$fitted, series="Fitted values from model", linetype='dashed') + 
  labs(subtitle="Forecasted with ARIMA(1,2,2) model")

# g.	Does auto.arima() give the same model you have chosen? If not, which model do you think is better?
mdlfit_mrdrs_autoarima <- auto.arima(ts_murders)
summary(mdlfit_mrdrs_autoarima)
fcst_mrdrs_autoarima <- forecast(mdlfit_mrdrs_autoarima, h=3)

autoplot(fcst_mrdrs_autoarima, main="Females Murdered Annually in the US, per 100k population") + 
  ylab('Females Murdered per 100k population') + xlab('') +
  autolayer(fcst_mrdrs_autoarima$fitted, series="Fitted values from model", linetype='dashed') + 
  labs(subtitle="Forecasted with ARIMA(1,2,1) model")


attributes(mdlfit_mrdrs_autoarima)
mdlfit_mrdrs_autoarima$arma
mdlfit_mrdrs_autoarima$call

mdlfit_mrdrs_autoarima$coef
mdlfit_mrdrs_autoarima$var.coef
mdlfit_mrdrs_autoarima$code
mdlfit_mrdrs_autoarima$series

mdlfit_mrdrs_autoarima$mask
mdlfit_mrdrs_autoarima$n.cond
mdlfit_mrdrs_autoarima$model$Z
mdlfit_mrdrs_autoarima$model$phi


autoplot(fcst_mrdrs_autoarima, main="Females Murdered Annually in the US, per 100k population") + 
  ylab('Females Murdered per 100k population') + xlab('') +
  autolayer(fcst_mrdrs_autoarima$fitted, series="Fitted values from model", linetype='dashed') + 
  labs(subtitle="Forecasted with ARIMA(1,2,1) model")

#Auto.arima gives a slightly different model (1,2,1), which is better as per the lower AICc (-6.39 vs. 0.24) and lower RMSE (0.207 vs 0.227).

checkresiduals(fcst_mrdrs_arima)
checkresiduals(fcst_mrdrs_autoarima)
  # residuals for both show some increasing variance over time.  Might be a tiny bit worse with the ARIMA(1,2,0).  The histogram
#  of residuals is more normally shaped for the ARIMA(1,2,1).  ACF plots for both tests are completely within the threshold lines;
# the Ljung-Box tests are not significant at the 95% confidence level for the ARIMA(1,2,0) but borderline, and the test for the 
# ARIMA(1,2,1) model is well above the significance threshold, even at a 90% confidence level, indicating there is not autocorrelation
# remaining in the residuals.





# For the usgdp series: 
require(expsmooth)

ts_usgdp <- expsmooth::usgdp   #Quarterly US GDP. 1947:1 - 2006.1
str(ts_usgdp)
head(ts_usgdp, 20)
tail(ts_usgdp, 20)
autoplot(usgdp,main="Quarterly US GDP",xlab="Year",ylab="Billions of US Dollars")
 #trend looks kinda quadratic ==> 2nd diffs?

#   a.	if necessary, find a suitable Box-Cox transformation for the data;
ts_usgdp_BoxCox_xformed <- BoxCox(ts_usgdp, lambda="auto")
BoxCox.lambda(ts_usgdp)  #0.366352
autoplot(ts_usgdp_BoxCox_xformed,main="Quarterly US GDP",xlab="Year",ylab="US Dollars")

str(ts_usgdp_BoxCox_xformed)  #note the optimal lambda was stored as an attribute   attr(*, "lambda")= num 0.366
summary(ts_usgdp_BoxCox_xformed)


# b.	fit a suitable ARIMA model to the transformed data using auto.arima();
mdlfit_usgdp_autoarima <- auto.arima(ts_usgdp_BoxCox_xformed)
mdlfit_usgdp_autoarima
summary(mdlfit_usgdp_autoarima)

fcst_usgdp_autoarima <- forecast(mdlfit_usgdp_autoarima)

lambda_usgdp <- attributes(ts_usgdp_BoxCox_xformed)$lambda
fcst_usdgp_autoarima_backXformed <- fcst_usgdp_autoarima$mean %>% InvBoxCox(lambda=lambda_usgdp)
fitted_usdgp_autoarima_backXformed <- fcst_usgdp_autoarima$fitted %>% InvBoxCox(lambda=lambda_usgdp)

fitted_usdgp_autoarima_backXformed

autoplot(fcst_usdgp_autoarima_backXformed, main="US GDP") + 
  ylab('US GDP ($, billions)') + xlab('') +
  autolayer(ts_usgdp, series="Historical Values", linetype='solid') + 
  autolayer(fitted_usdgp_autoarima_backXformed, series="Fitted values from model", linetype='dashed') + 
  labs(subtitle="Forecasted with ARIMA(2,1,0) model")



# c.	try some other plausible models by experimenting with the orders chosen;
#plot makes me think of a random walk with drift.
ndiffs(ts_usgdp_BoxCox_xformed)  #1
nsdiffs(ts_usgdp_BoxCox_xformed)  #0

mdlfit_usgdp_arima110_w_drift <- Arima(ts_usgdp_BoxCox_xformed, order=c(1,1,0), include.drift = TRUE)
summary(mdlfit_usgdp_arima110_w_drift)

fcst_usgdp_arima110_w_drift <- forecast(mdlfit_usgdp_arima110_w_drift)

fcst_usgdp_arima110_w_drift_backXformed <- fcst_usgdp_arima110_w_drift$mean %>% InvBoxCox(lambda=lambda_usgdp)
fttd_usgdp_arima110_w_drift_backXformed <- fcst_usgdp_arima110_w_drift$fitted %>% InvBoxCox(lambda=lambda_usgdp)

autoplot(fcst_usgdp_arima110_w_drift_backXformed, main="US GDP") + 
  ylab('US GDP ($, billions)') + xlab('') +
  autolayer(ts_usgdp, series="Historical Values", linetype='solid') + 
  autolayer(fttd_usgdp_arima110_w_drift_backXformed, series="Fitted values from model", linetype='dashed') + 
  labs(subtitle="Forecasted with ARIMA(1,1,0) model with drift")

checkresiduals(mdlfit_usgdp_arima110_w_drift)
#still appears to be autocorrelation with lag=2  =>  try AR2

ts_usgdp_BoxCox_xformed %>% diff() %>% tsdisplay()
ts_usgdp_BoxCox_xformed %>% diff() %>% diff() %>% tsdisplay()
#AR(1) - b/c it has a big spike at ACF=1 and 2, but spike at PACF2 is not quite over threshold?  
#both ACF and PACF (esp) have something going on at 12, ==> AR(12??)   
#  but that doesnt seem reasonable to look at lag12 - for quarterly data that is 3 yr "seasonality"



mdlfit_usgdp_arima110_no_drift <- Arima(ts_usgdp_BoxCox_xformed, order=c(1,1,0), include.drift = FALSE)
summary(fcst_usgdp_arima110_no_drift)

fcst_usgdp_arima110_no_drift <- forecast(mdlfit_usgdp_arima110_no_drift)

fcst_usgdp_arima110_no_drift_backXformed <- fcst_usgdp_arima110_no_drift$mean %>% InvBoxCox(lambda=lambda_usgdp)
fttd_usgdp_arima110_no_drift_backXformed <- fcst_usgdp_arima110_no_drift$fitted %>% InvBoxCox(lambda=lambda_usgdp)

autoplot(fcst_usgdp_arima110_no_drift_backXformed, main="US GDP") + 
  ylab('US GDP ($, billions)') + xlab('') +
  autolayer(ts_usgdp, series="Historical Values", linetype='solid') + 
  autolayer(fttd_usgdp_arima110_no_drift_backXformed, series="Fitted values from model", linetype='dashed') + 
  labs(subtitle="Forecasted with ARIMA(1,1,0) model - no drift")


checkresiduals(mdlfit_usgdp_arima110_no_drift)
#clearly looks like an issue with lag1 terms, as per ACF plot

nsdiffs(usgdp)

mdlfit_usgdp_arima121_no_drift <- Arima(ts_usgdp_BoxCox_xformed, order=c(1,2,1), include.drift = FALSE)
summary(mdlfit_usgdp_arima121_no_drift)
#RMSE  0.1877815
#MASE  0.1794058

fcst_usgdp_arima121_no_drift <- forecast(mdlfit_usgdp_arima121_no_drift)


# d.	choose what you think is the best model and check the residual diagnostics;
checkresiduals(mdlfit_usgdp_autoarima)



accuracy(fcst_usgdp_arima110_no_drift)
accuracy(fcst_usgdp_arima110_w_drift)

# e.	produce forecasts of your fitted model. Do the forecasts look reasonable?

autoplot(fcst_usdgp_autoarima_backXformed, main="US GDP") + 
  ylab('US GDP ($, billions)') + xlab('') +
  autolayer(ts_usgdp, series="Historical Values", linetype='solid') + 
  autolayer(fitted_usdgp_autoarima_backXformed, series="Fitted values from model", linetype='dashed') + 
  labs(subtitle="Forecasted with ARIMA(2,1,0) model")

#   f.	compare the results with what you would obtain using ets() (with no transformation).
fcst_usgdp_ets <- ts_usgdp %>% ets() %>% forecast()

autoplot(fcst_usgdp_ets, main="US GDP") + 
  ylab('US GDP ($, billions)') + xlab('') +
  autolayer(ts_usgdp, series="Historical Values", linetype='solid') + 
  autolayer(fcst_usgdp_ets$fitted, series="Fitted values from model", linetype='dashed') + 
  labs(subtitle="Forecasted with ETS model - no transformation")







# Consider austourists, the quarterly number of international tourists to Australia for the period 1999?2010. 
ts_austour <- fpp2::austourists

str(ts_austour)
head(ts_austour, 20)
tail(ts_austour, 20)


# a.	Describe the time plot.
plt1 <- autoplot(ts_austour, main="Total Visitor nights spent by int'l tourists to AU") + ylab('Visitor Nights') + xlab('')

#highly seasonal; looks multiplicative;  Q1 is highest quarter, Q2 is lowest; builds in Q3 and Q4 to Q1 
#positive trend
plt2 <- ggseasonplot(ts_austour, main='Seasonal plot - Total visitor nights')
plt3 <- ggsubseriesplot(ts_austour, main='Subseries plot - Total visitor nights by quarter') + ylab('Visitor Nights')

grid.arrange(plt1, plt2, plt3, nrow=2, ncol=2)


tsdisplay(ts_austour, main="Total Visitor nights spent by int'l tourists to AU")


# b.	What can you learn from the ACF graph?
acf(ts_austour)
  # very high spikes at 4 and 8 ==>  annual autocorrelation
  # also above-threshold spikes at 1, 2, 3, ... ==> a lot of autocorrelation    

# c.	What can you learn from the PACF graph?
pacf(ts_austour)
#lag1, lag2, especially lag4 and notably lag5 (strong but negative autocorrelation -- driven by 1 v4 2 yrs ago, 2 vs 1 2 yrs ago,  )


# d.	Produce plots of the seasonally differenced data (1-B4)Yt. What model do these graphs suggest?
ts_austour_ssnldiff <- diff(ts_austour, lag=4)

plt1 <- autoplot(ts_austour_ssnldiff, main="Total Visitor nights spent by int'l tourists visiting AU") + 
    ylab("Seasonally differenced values") + labs(subtitle = 'Seasonal (annual) differences')

ggseasonplot(ts_austour_ssnldiff)

acf(ts_austour_ssnldiff)

pacf(ts_austour_ssnldiff)


#grid.arrange(plt1, plt2, plt3, nrow=2, ncol=2)

tsdisplay(ts_austour_ssnldiff)
ndiffs(ts_austour_ssnldiff) #0
nsdiffs(ts_austour_ssnldiff) #0

nsdiffs(diff(ts_austour, lag=1)) #1
#as shown a few lines above, when you take the seasonal diff and then see if a first diff is recommended, we get a zero (==> no)
#why if we first diff and then check on a seasonal diff do we get 1?   I thought these were transitive.



#since the plot fluctuates from positive to negative, it suggests an AR with phi < 0

ndiffs(ts_austour)  #1
nsdiffs(ts_austour)  #1

# e.	Does auto.arima() give the same model that you chose? If not, which model do you think is better?
mdlfit_autour <- auto.arima(ts_austour)
summary(mdlfit_autour)


# f.	Write the model in terms of the backshift operator, then without using the backshift operator.
    # (1-B1)(1-B4s)yt = et
    # y't = y't-1 = et
    # yt - yt-4 = yt-1 - yt-5 + et
    
      
   










# Before doing this exercise, you will need to install the rdatamarket package in R using
# install.packages("rdatamarket")
#already installed, just load it
require(rdatamarket)
# Select a time series from Datamarket. Then copy its short URL and import the data using
# x <- ts(rdatamarket::dmseries("shorturl")[,1],
#         start=??, frequency=??)
# (Replace ?? with the appropriate values.)
# london_weather <- rdatamarket::dmseries("52kv") #  ("17tm")
#permission error. not sure why as the Qlik website had it under the free series
# remove(london_weather)


sales_lith <- rdatamarket::dmseries("http://bit.ly/2BBxQFe")
str(sales_lith)
head(sales_lith)
tail(sales_lith)

ts_sales_lith <- ts(sales_lith, start=c(1997,1), end=c(2018,6), frequency = 12)


# Plot graphs of the data, and try to identify an appropriate ARIMA model.
autoplot(ts_sales_lith, main="Annual rate of change - sales of personal hygiene and wellness products in Lithuania") +
  ylab('Annual rate of change') + xlab('')

tsdisplay(ts_sales_lith)


ndiffs(ts_sales_lith)  #1
nsdiffs(ts_sales_lith)  #0
# ==>AR(1,1,0)



mdlfit_lith_arima110 <- Arima(ts_sales_lith, order=c(1,1,0))
summary(mdlfit_lith_arima110)
#RMSE 0.8498
#AICc 650


fcst_lith_arima110 <- forecast(mdlfit_lith_arima110)
autoplot(fcst_lith_arima110, main="Annual rate of change - sales of personal hygiene and wellness products in Lithuania") + 
  ylab('Annual rate of change') + xlab('') +
  autolayer(ts_sales_lith, series="Historical Values", linetype='solid') + 
  autolayer(fcst_lith_arima110$fitted, series="Fitted values from model", linetype='dashed') + 
  labs(subtitle="Forecasted with ARIMA(1,1,0) model")

checkresiduals(fcst_lith_arima110)
#really big seasonal spike at 12 on the ACF - try adding a seasonal term??
#residuals aren't too bad but variance seems higher at end

mdlfit_lith_arima110100 <- Arima(ts_sales_lith, order=c(1,1,0), seasonal=c(1,0,0))
summary(mdlfit_lith_arima110100)
#RMSE 0.7539
#AICC 594.

fcst_lith_arima110100 <- forecast(mdlfit_lith_arima110100)
checkresiduals(mdlfit_lith_arima110100)


checkresiduals(fcst_lith_arima110100)

autoplot(fcst_lith_arima110100, main="Annual rate of change - sales of personal hygiene and wellness products in Lithuania") + 
  ylab('Annual rate of change') + xlab('') +
  autolayer(ts_sales_lith, series="Historical Values", linetype='solid') + 
  autolayer(fcst_lith_arima110100$fitted, series="Fitted values from model", linetype='dashed') + 
  labs(subtitle="Forecasted with ARIMA(1,1,0)(1,0,0)[12] model")




mdlfit_lith_autoarima <- auto.arima(ts_sales_lith)
summary(mdlfit_lith_autoarima)


# Do residual diagnostic checking of your ARIMA model. Are the residuals white noise?


#   Use your chosen ARIMA model to forecast the next four years.
fcst2_lith_arima110100 <- forecast(mdlfit_lith_arima110100, h=48)

autoplot(fcst2_lith_arima110100, main="Annual rate of change - sales of personal hygiene and wellness products in Lithuania") + 
  ylab('Annual rate of change') + xlab('') +
  autolayer(ts_sales_lith, series="Historical Values", linetype='solid') + 
  autolayer(fcst2_lith_arima110100$fitted, series="Fitted values from model", linetype='dashed') + 
  labs(subtitle="Forecasted with ARIMA(1,1,0)(1,0,0)[12] model")


# Now try to identify an appropriate ETS model.
fcst_lith_ets <- ts_sales_lith %>% ets() %>% forecast()
summary(ts_sales_lith %>% ets())

# Do residual diagnostic checking of your ETS model. Are the residuals white noise?
checkresiduals(fcst_lith_ets)
accuracy(fcst_lith_ets)
#RMSE is 0.84.  AIC can't be used as this doesn't use diffs


#   Use your chosen ETS model to forecast the next four years.
fcst_lith_ets <- ts_sales_lith %>% ets() %>% forecast(h=48)

autoplot(fcst_lith_ets, main="Annual rate of change - sales of personal hygiene and wellness products in Lithuania") + 
  ylab('Annual rate of change') + xlab('') +
  autolayer(ts_sales_lith, series="Historical Values", linetype='solid') + 
  labs(subtitle="Forecasted with ETS model") +
  autolayer(fcst_lith_ets$fitted, series="Fitted values from model", linetype='dashed') 

# Which of the two models do you prefer?
# The Arima model is preferable as it has lower RMSE on the training set and has tighter prediction intervals.






# Consider monthly sales and advertising data for an automotive parts company (data set advert).
ts_advert <- fma::advert  # Monthly sales and advertising expenditure for an automotive parts company.  (data frame)

# class(df_advert)
# str(advert)
# head(advert)
# str(df_advert)
# head(df_advert)
# autoplot(advert)
  
# a.  Plot the data using autoplot. Why is it useful to set facets=TRUE?
autoplot(advert)
autoplot(advert, facets = TRUE)
# the different series are more clear with the scales being set as needed for each series.


# b.  Fit a standard regression model  
      # yt = a + bxt + ??t
      #     where yt denotes sales and xt denotes advertising 
# using the tslm() function.
lm_advert <- tslm(sales ~ advert, df_advert)
summary(lm_advert)


# tslm(formula = sales ~ advert, data = df_advert)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.8194 -1.1375 -0.2412  0.9123  2.7519 
# 
# Coefficients:
#                 Estimate   Std. Error   t value    Pr(>|t|)    
# (Intercept)     78.73426    0.59735     131.81    < 2e-16 ***
#   advert         0.53426    0.04098     13.04     7.96e-12 ***
#   ---
#   Signif. codes:  0 ?***? 0.001 ?**? 0.01 ?*? 0.05 ?.? 0.1 ? ? 1
# 
# Residual standard error: 1.506 on 22 degrees of freedom
# Multiple R-squared:  0.8854,	Adjusted R-squared:  0.8802 
# F-statistic:   170 on 1 and 22 DF,  p-value: 7.955e-12

accuracy(lm_advert)


# c.  Show that the residuals have significant autocorrelation.
checkresiduals(lm_advert)

acf(lm_advert$residuals)
#Note that a negative residual tends to be preceded by a negative residual, and likewise for positive.
# Also, the ACF function shows strong autocorrelation in lags1 and 2.



# d.  What difference does it make you use the Arima function instead:
#   
#   Arima(advert[,"sales"], xreg=advert[,"advert"],
#         order=c(0,0,0))
mdlfit_advert_arima <- Arima(advert[, 'sales'], xreg=advert[, 'advert'], order=c(0,0,0))
summary(mdlfit_advert_arima)

# The coefficients are the same, but the standard errors are slightly lower with the Arima, leading to tighter prediction intervals.

checkresiduals(mdlfit_advert_arima)

# e.  Refit the model using auto.arima(). How much difference does the error model make to the estimated parameters? 
#What ARIMA model for the errors is selected?
mdlfit_advert_autoarima <- auto.arima(advert[, 'sales'], xreg=advert[, 'advert'])
summary(mdlfit_advert_autoarima)

#The optimal model as per auto.arima is an ARIMA(0,1,0).  RMSE drops from 1.44 in the 000 model to 1.049 in the 010 model.
# the coefficient for the advertising spend changes from 0.5343 to 0.5063.


# f.  Check the residuals of the fitted model.
checkresiduals(mdlfit_advert_autoarima)

# The residuals of the new model, after the first differencing was done, are much more like white noise.  The residual plot
# fluctuates much more randomly and the ACF plt shows that all the lags are within the threshold as desired.



# g.  Assuming the advertising budget for the next six months is exactly 10 units per month, produce 
#and plot sales forecasts with prediction intervals for the next six months.
fcst_advert_autoarima <- forecast(mdlfit_advert_autoarima, xreg=rep(10, 6), h=6)

autoplot(fcst_advert_autoarima, main="Sales") + 
  ylab('Sales') + xlab('') +
  autolayer(advert[, 'sales'], series="Historical Values", linetype='solid') + 
  labs(subtitle="Forecasted with ARIMA(0,1,0) model + external regressor:  advertising") +
  autolayer(fcst_advert_autoarima$fitted, series="Fitted values from model", linetype='dashed') 




plt1 <- autoplot(forecast(mdlfit_advert_arima, xreg=rep(10, 6), h=6), main="Sales") + 
  ylab('Sales') + xlab('') +
  autolayer(advert[, 'sales'], series="Historical Values", linetype='solid') + 
  labs(subtitle="Forecasted with ARIMA(0,0,0) model + external regressor:  advertising") +
  autolayer(fcst_advert_autoarima$fitted, series="Fitted values from model", linetype='dashed') 



plt2 <- autoplot(forecast(lm_advert, newdata=rep(10,6), h=6), main="Sales") + 
  ylab('Sales') + xlab('') +
  autolayer(advert[, 'sales'], series="Historical Values", linetype='solid') + 
  labs(subtitle="Forecasted with tslm model + external regressor:  advertising") +
  autolayer(fcst_advert_autoarima$fitted, series="Fitted values from model", linetype='dashed') 

grid.arrange(plt2, plt1, nrow=1, ncol=2)


# Section 9.7,    Problem 2
# This exercise uses data set huron giving the level of Lake Huron from 1875?1972.
fma::huron    #Level of Lake Huron in feet (reduced by 570 feet): 1875?1972.   (time series data)

autoplot(huron, main="Level of Lake Huron (reduced by 570 feet)") + ylab('level (ft)') + xlab('')


# Fit a piecewise linear trend model to the Lake Huron data with a knot at 1920 and an ARMA error structure.
time_rgrsrs <- cbind(Yrs=seq(from=1875, to=1972),
                    YrCnt=seq(from=1875, to=1972)-1875, 
                    YrCnt_Since_1920=pmax(rep(0,1973-1875), seq(from=1875, to=1972)-1920))

mdlfit_huron_arima <- auto.arima(huron, xreg=time_rgrsrs[,2:3], d=0)  #force d=0 to have ARMA errors;  force d=1 for ARIMA errors
summary(mdlfit_huron_arima)

# Forecast the level for the next 30 years.
b0 = mdlfit_huron_arima$coef[['intercept']]
b1 = mdlfit_huron_arima$coef[['YrCnt']]
b2 = mdlfit_huron_arima$coef[['YrCnt_Since_1920']]

time_rgrsrs_extended <- cbind(Yrs=seq(from=1875, to=2002),
                     YrCnt=seq(from=1875, to=2002)-1875, 
                     YrCnt_Since_1920=pmax(rep(0,2003-1875), seq(from=1875, to=2002)-1920))

lnr_trend_huron <- ts(b0 + 
                      b1*time_rgrsrs_extended[,'YrCnt'] +
                      b2*time_rgrsrs_extended[,'YrCnt_Since_1920'], 
                      start=1875)

autoplot(huron, main="Level of Lake Huron (reduced by 570 feet)") + ylab('level (ft)') + xlab('') +
  autolayer(lnr_trend_huron, series='Piecewise Linear Trend')



# Section 9.7,    Problem 3
# This exercise concerns motel: the total monthly takings from accommodation and the total room nights occupied at 
# hotels, motels, and guest houses in Victoria, Australia, between January 1980 and June 1995. Total monthly takings 
#are in thousands of Australian dollars; total room nights occupied are in thousands.
fma::motel
str(motel)
head(motel)
tail(motel)

autoplot(motel, facets=TRUE, main="Tourism data in Victoria, Australia")


# Use the data to calculate the average cost of a night?s accommodation in Victoria each month.
cost_per_night <- motel[,'Takings']*1000/motel[, 'Roomnights']

ts_motel <- ts(cbind(motel, cost_per_night), start=c(1980,1), frequency=12)
head(ts_motel)

autoplot(ts_motel[,"cost_per_night"], main="Motel room cost per night in Victoria, Australia") + ylab('Avg Cost per night') + xlab('')


# Use cpimel to estimate the monthly CPI.
cpimel   #Quarterly CPI (consumer price index) for Victoria: Q1 1980 to Q2 1995.
tsdisplay(cpimel)

head(cpimel)
tail(cpimel)
# quarterly price chg = (monthly price chg)^3
#  log(q_pr_chg) = 3*log(mo_pr_chg)
#  mo cpi val = last mo's cpi val * (1+mo pr chg)

last_qtr_val <- cpimel %>% stats::lag(-1)
qtly_pr_chg <- cpimel / last_qtr_val - 1
mo_pr_chg <- (1 + qtly_pr_chg)^(1/3)

cpimel[[1]]
cpimel[[2]]
cpimel[[3]]
cpimel[[4]]
cpimel[[5]]

mo_pr_chg[[1]]
mo_pr_chg[[2]]
mo_pr_chg[[3]]
mo_pr_chg[[4]]


df_moCPI <- data.frame(Mo_Nbr=1:3, CPI=rep(cpimel[[1]],3))
for (i in 1:(length(cpimel)-1))
{
  for (j in 1:3)
  {
    df_moCPI <- rbind(df_moCPI, c(3*i+j, cpimel[[i]]*mo_pr_chg[[i]]^j))   
  }
}

df_moCPI$mnth <- seq.Date(from=as.Date("01-01-1980", '%m-%d-%Y'), to=as.Date("06-01-1995", '%m-%d-%Y'), by='month')

df_qtrCPI <- data.frame(cpimel)
df_qtrCPI$mnth <- seq.Date(from=as.Date("02-15-1980", '%m-%d-%Y'), to=as.Date("05-15-1995", '%m-%d-%Y'), by='quarter')





# Produce time series plots of both variables and explain why logarithms of both variables need to be taken before fitting any models.
ggplot(df_moCPI, aes(x=mnth, y=CPI)) + ggtitle("CPI (in Melbourne Australia?)") +
  geom_line() + xlab('') +
  geom_point(mapping=aes(x=mnth, y=cpimel), data=df_qtrCPI) +
  labs(subtitle='The Quarterly CPI series (given) is denoted with dots; the calculated monthly CPI series is denoted with lines.')


# autoplot(cpimel)
# autoplot(ts_motel[,"cost_per_night"], main="Motel room cost per night in Victoria, Australia") + ylab('Avg Cost per night') + xlab('')
# ggtitle(paste(itm_nm, 'at', shp_nm, '.  ComboID', cmbo_id)) + theme(plot.title = element_text(hjust = 0.5)) + 
#   xlab("Month") + ylab("Units Sold") + theme(legend.position = "bottom") +
#   labs(subtitle=paste("Data Aggregation:", dataprep_fn)) 


plt1 <- autoplot(log(ts_motel[,"cost_per_night"]), main="Log of room cost per night in Victoria, Australia") + ylab('Log of avg cost per night') + xlab('')

plt2 <- ggplot(df_moCPI, aes(x=mnth, y=log(CPI))) + ggtitle("logCPI (in Melbourne Australia?)") +
  geom_line() + xlab('') + ylab('Log of CPI') +
  geom_point(mapping=aes(x=mnth, y=log(cpimel)), data=df_qtrCPI) +
  labs(subtitle='The Quarterly CPI series (given) is denoted with dots; the calculated monthly CPI series is denoted with lines.')

grid.arrange(plt1, plt2, nrow=1, ncol=2)


# Fit an appropriate regression model with ARIMA errors. Explain your reasoning in arriving at the final model.
ts_log_cost_per_nite <- ts(log(ts_motel[,"cost_per_night"]), start=c(1980,1), frequency=12)
cpi_rgrsrs <- ts(data.frame(log_cpi=log(df_moCPI$CPI)), start=c(1980,1), frequency=12)

mdlfit_htlcost_arima <- auto.arima(ts_log_cost_per_nite, xreg=cpi_rgrsrs, d=1)  #force d=1 to use ARIMA errors, d=0 to use ARMA errors
summary(mdlfit_htlcost_arima)

# Series: ts_log_cost_per_nite 
# Regression with ARIMA(0,1,1)(0,1,1)[12] errors 
# 
# Coefficients:
#          ma1     sma1  log_cpi
#       -0.5472  -0.6219   0.7502
# s.e.   0.0590   0.0850   0.2608
# 
# sigma^2 estimated as 0.0003825:  log likelihood=433.58
# AIC=-859.16   AICc=-858.93   BIC=-846.55
# 
# Training set error measures:
#   ME       RMSE        MAE          MPE      MAPE      MASE         ACF1
# Training set -0.0003986256 0.01869704 0.01374134 -0.008285523 0.3328377 0.1756718 -0.007903975


checkresiduals(mdlfit_htlcost_arima)

# Forecast the average price per room for the next twelve months using your fitted model. 
#(Hint: You will need to produce forecasts of the CPI figures first.)
#forecast the predictors
fcst_cpi_arima <- auto.arima(cpi_rgrsrs) %>% forecast(h=12)

autoplot(exp(cpi_rgrsrs), main="CPI") +
  autolayer(exp(fcst_cpi_arima$mean), series='Forecasted CPI') +
  ylab("CPI") + xlab("")


#forecast the cost per night
fcst_htlcost_arima <- forecast(mdlfit_htlcost_arima, xreg=fcst_cpi_arima$mean, h=12)


#transform from log back to original units ($/nite)
fcst_origunits_mean <- ts(data.frame(Cost_per_nite_fcstd=exp(fcst_htlcost_arima$mean)), start=c(1995,7), frequency = 12)
fcst_origunits_95upper <- ts(data.frame(Cost_per_nite_fcstd=exp(fcst_htlcost_arima$upper[,2])), start=c(1995,7), frequency = 12)
fcst_origunits_95lower <- ts(data.frame(Cost_per_nite_fcstd=exp(fcst_htlcost_arima$lower[,2])), start=c(1995,7), frequency = 12)


autoplot(ts_motel[,"cost_per_night"], main="Motel room cost per night in Victoria, Australia") + 
    ylab('Avg Cost per night') + xlab('') + 
    autolayer(fcst_origunits_mean, series='Forecast mean') +
    autolayer(fcst_origunits_95upper, series='Upper limit - 95% conf') +
    autolayer(fcst_origunits_95lower, series='Lower limit - 95% conf') +
    labs(subtitle="Regression with ARIMA errors")   




# We fitted a harmonic regression model to part of the gasoline series in Exercise 6 in Section 5.10. We will now revisit this model, and 
# extend it to include more data and ARMA errors.
fpp2::gasoline
str(gasoline)
autoplot(gasoline)
head(gasoline)

start(gasoline)
seq_along(gasoline)
start_dt_gas <- attributes(gasoline)$tsp[[1]]  #1991.1
freq_gas <- attributes(gasoline)$tsp[[3]]      #52.17857
length(gasoline)  #1355

head(tail(gasoline, 400), 20)
# Using tslm(), fit a harmonic regression with a piecewise linear time trend to the full gasoline series. 
# Select the position of the knots in the trend and the appropriate number of Fourier terms to include by minimising the AICc or CV value.

#consider 3 knots - appear to be 3 regimes with a) positive trend, b) negative trend,  c) positive trend.  Use a search to establish the optimal placement
#fit the trend first, then layer on the Fourier series.  A) I think this will be accurate, and B) it will be more straightforward coding and 
#C) lots faster b/c we won't mess with optimizing Fourier series for obviously pointless knot locations.

knot_locn_builder <- function(knot1, knot2)
{
  lngth <- length(gasoline)
  
  trend_mtrx <- cbind(trend_pc1=seq(1:lngth),
                      trend_pc2=pmax( rep(0,lngth), seq(1:lngth)-knot1 ),
                      trend_pc3=pmax( rep(0,lngth), seq(1:lngth)-knot2 )
                      )

}

trend_mtrx_w_knots <- knot_locn_builder(1320, 1335)
tail(trend_mtrx_w_knots, 50)

knot1_start <- 1355-600
knot2_start <- 1355-130

#for speed reasons, create a matrix for AICc scores for various knot locations
df_knotscores <- data.frame(i=rep(1, 200), j=1:200, aicc=rep(9999, 200))
for (i in 2:200)
{
  df_knotscores <- rbind(df_knotscores, data.frame(i=rep(i, 200), j=1:200, aicc=rep(9999, 200)))
}


#systematically search for the best knot 1 & 2 locations - push in from the left towards knot1 locn, looping each time over knot2 locations approached from the right
  # knot1_start + 200  #955
  # knot2_start - 200  #1025    good, these will never cross/pass each other
for (ii in 1:200)
{
  for (jj in 1:200)
  {
    trend_mtrx_w_knots <- knot_locn_builder(knot1_start+ii, knot2_start-jj)
    
    lm_knot_finder <- tslm(gasoline ~ trend_mtrx_w_knots) 
    df_knotscores[df_knotscores$i ==ii & df_knotscores$j == jj, ]$aicc <- CV(lm_knot_finder)[['AICc']]
  }
  if (ii%%10 == 0) {print(paste(ii,'completed'))}
}

#find the top 100 locations with the lowest aicc scores
df_knotscores %>% arrange(aicc) %>% head(100)
#the lowest aicc is for i=73, j=76.   optim. surface is smooth but the values of contenders are in the same neighborhood so no reason not to use these
trend_mtrx_final <- knot_locn_builder(knot1_start+73, knot2_start-76)

0.9494*52.17857

tail(head(gasoline, 828), 1)
tail(head(gasoline, 1149), 1)

#find the optimal number of Fourier terms
df_fourier <- data.frame(k=seq(20), aicc=rep(9999,20))
for (kk in seq(20))
{
  lm_fourier <- tslm(gasoline ~ trend_mtrx_final + fourier(gasoline, K=kk))
  df_fourier[df_fourier$k == kk, ]$aicc <- CV(lm_fourier)[['AICc']]
  print(paste(kk, 'completed'))
}

df_fourier %>% arrange(aicc)
#k=12 is the lowest AICc  (-3808.597)

lm_gas_final <- tslm(gasoline ~ trend_mtrx_final + fourier(gasoline, K=12))
accuracy(lm_gas_final)  #RMSE = 0.23996



# Now refit the model using auto.arima() to allow for correlated errors, keeping the same predictor variables as you used with tslm().
extrnl_rgrsrs <- cbind(trend_mtrx_final, fourier(gasoline, K=12))
head(extrnl_rgrsrs)

ndiffs(gasoline) #1
nsdiffs(gasoline) #0
mdlfit_gas_arima111 <- Arima(gasoline, xreg=extrnl_rgrsrs, order=c(1,1,1))
summary(mdlfit_gas_arima111)
#RMSE = 0.2338994  vs.  accuracy(lm_gas_final)  #RMSE = 0.23996
checkresiduals(mdlfit_gas_arima111)


mdlfit_gas_arima211 <- Arima(gasoline, xreg=extrnl_rgrsrs, order=c(2,1,1))
summary(mdlfit_gas_arima211)
#RMSE =0.2337695  vs  0.2338994  vs.  accuracy(lm_gas_final)  #RMSE = 0.23996
checkresiduals(mdlfit_gas_arima211)

mdlfit_gas_arima211_111 <- Arima(gasoline, xreg=extrnl_rgrsrs, order=c(2,1,1), seasonal=c(1,1,1))
#error in optim...

mdlfit_gas_arima411 <- Arima(gasoline, xreg=extrnl_rgrsrs, order=c(4,1,1))
summary(mdlfit_gas_arima411)
#error in optim...


summary(mdlfit_gas_arima211_111)
#RMSE =0.2337695  vs  0.2338994  vs.  accuracy(lm_gas_final)  #RMSE = 0.23996
checkresiduals(mdlfit_gas_arima211)


# mdlfit_gas_autoarima <- auto.arima(gasoline, xreg=extrnl_rgrsrs)
# summary(mdlfit_gas_autoarima)

# Check the residuals of the final model using the checkresiduals() function. Do they look sufficiently like white noise to continue? If not, try 
# modifying your model, or removing the first few years of data.
checkresiduals(mdlfit_gas_arima111)
#checkresiduals(mdlfit_gas_arima211)
checkresiduals(mdlfit_gas_arima411)

pacf(mdlfit_gas_arima411$residuals)
# Once you have a model with white noise residuals, produce forecasts for the next year.

#first extend the series for the external regressors to be populated for the forecast horizon
fourier_extended <- fourier(gasoline, K=12, h=52)
lst_rw <- trend_mtrx_final[nrow(trend_mtrx_final), ]
trend_mtrx_extended <- data.frame(trend_pc1=lst_rw[['trend_pc1']]+1, 
                                  trend_pc2=lst_rw[['trend_pc2']]+1, 
                                  trend_pc3=lst_rw[['trend_pc3']]+1
                                  )
for (n in 2:52)
{
  trend_mtrx_extended <- rbind(trend_mtrx_extended, c(trend_pc1=lst_rw[['trend_pc1']]+n, 
                                                      trend_pc2=lst_rw[['trend_pc2']]+n, 
                                                      trend_pc3=lst_rw[['trend_pc3']]+n)
                              )
}

extrnl_rgrsrs_extended <- cbind(trend_mtrx_extended, fourier_extended)
head(extrnl_rgrsrs_extended)

# fcst_gas_arima211 <- forecast(mdlfit_gas_arima211, xreg=as.matrix(extrnl_rgrsrs_extended), h=52)
# summary(mdlfit_gas_arima211)

fcst_gas_arima411 <- forecast(mdlfit_gas_arima411, xreg=as.matrix(extrnl_rgrsrs_extended), h=52)
#summary(mdlfit_gas_arima411)


#extend the linear trend
b1 <- 0.0028    
b2 <- -0.0051
b3 <- 0.0037

lnr_trend_gas <- b1*trend_mtrx_final[,'trend_pc1'] + 
                 b2*trend_mtrx_final[,'trend_pc2'] + 
                 b3*trend_mtrx_final[,'trend_pc3']


autoplot(fcst_gas_arima411) +
  ylab('Gasoline supplied (millions of barrels per day)') + xlab('') +
  autolayer(ts(lnr_trend_gas+7.1, start=c(1991.1), frequency = freq_gas ), series="Linear Trend") +
  labs(subtitle='Starting height of linear trend is estimated') 





# Section 9.7,    Problem 5
# Electricity consumption is often modelled as a function of temperature. Temperature is measured by daily heating 
# degrees and cooling degrees. Heating degrees is 18?C minus the average daily temperature when the daily average is below  
# 18?C; otherwise it is zero. This provides a measure of our need to heat ourselves as temperature falls. Cooling degrees 
#measures our need to cool ourselves as the temperature rises. It is defined as the average daily temperature minus 18?C 
#when the daily average is above 18?C; otherwise it is zero. 
#
#Let yt denote the monthly total of kilowatt-hours of electricity used, let x1,t denote the monthly total of heating degrees, 
#and let x2,t denote the monthly total of cooling degrees.
# 
# An analyst fits the following model to a set of such data:
#   y*t = ?1*x1,t  +  ?2*x2,t  +   ??t,
# 
# where
# (
#   1
#   -
#   B
# )
# (
#   1
#   -
#   B
#   12
# )
# ??
# t
# =
#   1
# -
# ??
# 1
# B
# 1
# -
# ??
# 12
# B
# 12
# -
# ??
# 24
# B
# 24
# e
# t
# 
# and  
# y
# *
# t
# =
#   log
# (
#   y
#   t
# )
# ,  
# x
# *
# 1
# ,
# t
# =
#   v
# x
# 1
# ,
# t
# and  
# x
# *
# 2
# ,
# t
# =
#   v
# x
# 2
# ,
# t
# .
# 
# What sort of ARIMA model is identified for ??t ?




#   The estimated coefficients are
# 
# Parameter	
#      Estimate	   s.e.      Z     P-value
# ?1   0.0077   	0.0015  	4.98	  0.000
# ?2   0.0208    	0.0023	  9.23	  0.000
# ??1   0.5830	    0.0720	  8.10	  0.000
# ??12 -0.5373	    0.0856	  -6.27	  0.000
# ??24 -0.4667	    0.0862	  -5.41	  0.000
# Explain what the estimates of ?1 and ?2  tell us about electricity consumption.

# As the number of heating degree days goes up, more electricity is consumed.
# Likewise, as the number of cooling degree days goes up, more electricity is consumed. 
# 
# The amount of electricity consumed per cooling degree day is greater than the amount of electricity consumed
# per heating degree day.  Likely this reflect more (non-electric) modes are used for heating (ex. (natural gas, fuel oil, etc)
# than are available for cooling.
# 

# Write the equation in a form more suitable for forecasting.



# Describe how this model could be used to forecast electricity demand for the next 12 months.



# Explain why the ??t term should be modelled with an ARIMA model rather than modelling the data using a standard 
# regression package. In your discussion, comment on the properties of the estimates, the validity of the standard 
# regression results, and the importance of the ??t model in producing forecasts.







# Section 9.7,    Problem 6
#For the retail time series considered in earlier chapters:

    # (monthly Australian retail data from the book website. These represent retail sales in various categories for different Australian states, and are stored in a MS-Excel file.

setwd("C:/Users/ashle/Documents/Personal Data/Northwestern/2019_01 winter  MSDS413 Time Series/HW2")
retaildata <- readxl::read_excel("retail.xlsx", skip=1)

ts_retail1 <- ts(retaildata$A3349335T, start=c(1982,4,1), frequency= 12)

tail(ts_retail1, 24)
autoplot(ts_retail1, main="Monthly retail sales in AU - category A3349335T") + xlab('') + ylab('Sales')

ts_retail1_xformd <- BoxCox(ts_retail1, lambda='auto')

lambda_auretail <- attributes(ts_retail1_xformd)$lambda  #0.193853

autoplot(ts_retail1_xformd, main="Monthly retail sales in AU - category A3349335T") + xlab('') + ylab('Sales (after Box Cox transform)') +
  labs(subtitle = paste('After Box Cox transformation - optimal lambda =', lambda_auretail))


#   Develop an appropriate dynamic regression model with Fourier terms for the seasonality. Use the AIC to select the 
# number of Fourier terms to include in the model. (You will probably need to use the same Box-Cox transformation you identified previously.)

df_fourier_auretail <- data.frame(k=seq(6), aicc=rep(9999,6))
for (kk in seq(6))
{
  mdlfit_fourier_auretail <- auto.arima(ts_retail1_xformd, xreg=fourier(ts_retail1_xformd, K=kk))
  df_fourier_auretail[df_fourier_auretail$k == kk, ]$aicc <- mdlfit_fourier_auretail$aic
  print(paste(kk, 'completed'))
}



df_fourier_auretail %>% arrange(aicc)
#k=6  is the lowest AIC   (also happens to be the max permissible (period/2))


mdlfit_fourier_auretail_final <- auto.arima(ts_retail1_xformd, xreg=fourier(ts_retail1_xformd, K=6))


fcst_fourier_auretail_final <- forecast(mdlfit_fourier_auretail_final, xreg=fourier(ts_retail1_xformd, K=6, h=12), h=12)
plt1 <- autoplot(fcst_fourier_auretail_final)+ xlab('') + ylab('Sales - after BoxCox transform')

str(fcst_fourier_auretail_final)

# Check the residuals of the fitted model. Does the residual series look like white noise?
checkresiduals(mdlfit_fourier_auretail_final)



#   Compare the forecasts with those you obtained earlier using alternative models.
fcst_ets <- ts_retail1_xformd %>% ets() %>% forecast()
checkresiduals(fcst_ets)


plt2 <- autoplot(fcst_ets) + xlab('') + ylab('Sales - after BoxCox transform')

grid.arrange(plt1, plt2, nrow=1, ncol=2)




#transform back to the original coordinates
#------------------------------------------
#plot the dynamic regression forecast
fcst_fourier_auretail_backXformd__mean <- InvBoxCox(fcst_fourier_auretail_final$mean, lambda_auretail)
fcst_fourier_auretail_backXformd__95PIUp <- InvBoxCox(fcst_fourier_auretail_final$upper[,2], lambda_auretail)
fcst_fourier_auretail_backXformd__95PILow <- InvBoxCox(fcst_fourier_auretail_final$lower[,2], lambda_auretail)



plt1 <- autoplot(ts_retail1, main="Monthly retail sales in AU - category A3349335T") + 
  ylab('Sales') + xlab('') + 
  autolayer(fcst_fourier_auretail_backXformd__mean, series='Forecast mean') +
  autolayer(fcst_fourier_auretail_backXformd__95PIUp, series='Upper limit - 95% conf') +
  autolayer(fcst_fourier_auretail_backXformd__95PILow, series='Lower limit - 95% conf') +
  labs(subtitle="Forecasted with dynamic regression")   



#plot the ETS forecast
fcst_ets_backXformd__mean <- InvBoxCox(fcst_ets$mean, lambda_auretail)
fcst_ets_backXformd__95PIUp <- InvBoxCox(fcst_ets$upper[,2], lambda_auretail)
fcst_ets_backXformd__95PILow <- InvBoxCox(fcst_ets$lower[,2], lambda_auretail)


plt2 <- autoplot(ts_retail1, main="Monthly retail sales in AU - category A3349335T") + 
  ylab('Sales') + xlab('') + 
  autolayer(fcst_fourier_auretail_backXformd__mean, series='Forecast mean') +
  autolayer(fcst_fourier_auretail_backXformd__95PIUp, series='Upper limit - 95% conf') +
  autolayer(fcst_fourier_auretail_backXformd__95PILow, series='Lower limit - 95% conf') +
  labs(subtitle="Forecasted with ETS")   

grid.arrange(plt1, plt2, nrow=1, ncol=2)

