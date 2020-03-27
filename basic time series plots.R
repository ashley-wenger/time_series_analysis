
#install the packages
#install.packages("fpp")
#install.packages("fpp2")

require(fpp)
require(fpp2)
require(gridExtra)

setwd("C:/Users/ashle/Documents/Personal Data/Northwestern/2019_01 winter  MSDS413 Time Series/HW1")

#use the png function in the grDisplay pkg (apparently is loaded by default in the RStudio distro) to export all graphical displays to a file.
#When the png function is called ,it creates an empty image with the given dimensions of width and height in inches.
#?dev.off?? is used to close the opened png.
#All the graphical displays that are created after calling the png function are added to the image until the png is closed using dev.off().


#create the open file.   Apparently this redirects all graphical display to here, b/c it's no longer showing up in the Plots tab
#not thrilled about that, would like to also see it on screen so trying ggsave instead
#png(filename = "Prob2_10__AutoPlot_Output.png", width=480,height=480,bg = "white")



#review the autoplot function
plot1 <- autoplot(hsales, main='hsales')
plot2 <- autoplot(usdeaths, main='usdeaths')
plot3 <- autoplot(bricksq, main='bricksq')
plot4 <- autoplot(sunspotarea, main='Sun Spot Area')
plot5 <- autoplot(gasoline, main='Gasoline')  #note this one is in fpp2
  
tbl_autoplot <- gridExtra::grid.arrange(plot1, plot2, plot3, plot4, plot5, nrow = 3, ncol=2)
#dev.off()    #close the png file   not using png now, using ggsave instead to save file but still get onscreen view 

#not sure why, after saving gridExtra output to tbl_autoplot, just runnin "tbl_autoplot" now gives a table of descriptive info instead of showing the vizn.  To see the vizn on the "plots" tab:
#plot(tbl_autoplot)


ggsave(filename="Prob2_10__AutoPlot_Output.png", plot=tbl_autoplot)





#season plot.  Months are on the X axis, each year is a series
plot1 <- ggseasonplot(hsales, main='hsales')
plot2 <- ggseasonplot(usdeaths, main='usdeaths')
plot3 <- ggseasonplot(bricksq, main='bricksq')
#plot4 <- ggseasonplot(sunspotarea, main='Sun Spot Area')
  # sunspotarea data only has 1 record per year so can't investigate seasonality and seasonplot gives back an error 
plot5 <- ggseasonplot(gasoline, main='Gasoline')  #note this one is in fpp2

tbl_seasonplot <- gridExtra::grid.arrange(plot1, plot2, plot3, plot5, nrow = 2, ncol=2)
ggsave(filename="Prob2_10__SeasonPlot_Output.png", plot=tbl_seasonplot)


#note the seasonality is detected and varies from weekly to monthly to quarterly




#Sub Series plot.  
#I think it's figuring out the optimal? dominant? seasonality, then grouping all the records from the first season (ex.  all the Jan records together, with yr1, yr2, .. as pts?) and giving an avg of that season
plot1 <- ggsubseriesplot(hsales, main='hsales')
plot2 <- ggsubseriesplot(usdeaths, main='usdeaths')
plot3 <- ggsubseriesplot(bricksq, main='bricksq')
#plot4 <- ggsubseriesplot(sunspotarea, main='Sun Spot Area')
#   response:  Error in ggsubseriesplot(sunspotarea, main = "Sun Spot Area") :   Data are not seasonal
#   see note above

#plot5 <- ggsubseriesplot(gasoline, main='Gasoline')  #note this one is in fpp2
#   response:     Error in ggsubseriesplot(gasoline, main = "Gasoline") : 
#                 Each season requires at least 2 observations. This may be caused from specifying a time-series with non-integer frequency.

tbl_subseriesplot <- gridExtra::grid.arrange(plot1, plot2, plot3, nrow = 2, ncol=2)
ggsave(filename="Prob2_10__SubSeriesPlot_Output.png", plot=tbl_subseriesplot)



#Lag plots.  
#?gglagplot? will plot time series against lagged versions of themselves. Helps visualising 'auto-dependence' even when auto-correlations vanish.
#    appears to autodetect seasonality, then create a series for each season (month, qtr, ..)    buckets each data point into the corresponding 
#      season (ex. Aug) and then plots (for lag1:)   the scatterplot of Aug 2 vs. Aug1,   Aug 3 vs Aug2, ...??
#                                      (for lag2:)   the scatterplot of Aug 3 vs. Aug1,   Aug 4 vs Aug2, ...??
plot1 <- gglagplot(hsales, main='hsales')
plot2 <- gglagplot(usdeaths, main='usdeaths')
plot3 <- gglagplot(bricksq, main='bricksq')
plot4 <- gglagplot(sunspotarea, main='Sun Spot Area')
plot5 <- gglagplot(gasoline, main='Gasoline')  #note this data is in fpp2 pkg

#tbl_lagplot <- gridExtra::grid.arrange(plot1, plot2, plot3, plot4, plot5, nrow = 3, ncol=2)
#these get too squished as composite plot.  Save each one separately

ggsave(filename="Prob2_10__LagPlot_Output1.png", plot=plot1)
ggsave(filename="Prob2_10__LagPlot_Output2.png", plot=plot2)
ggsave(filename="Prob2_10__LagPlot_Output3.png", plot=plot3)
ggsave(filename="Prob2_10__LagPlot_Output4.png", plot=plot4)
ggsave(filename="Prob2_10__LagPlot_Output5.png", plot=plot5)





#ACF plot.  
#
plot1 <- ggAcf(hsales, main='hsales')
plot2 <- ggAcf(usdeaths, main='usdeaths')
plot3 <- ggAcf(bricksq, main='bricksq')
plot4 <- ggAcf(sunspotarea, main='Sun Spot Area')
plot5 <- ggAcf(gasoline, main='Gasoline')  #note this data is in fpp2 pkg

tbl_acfplot <- gridExtra::grid.arrange(plot1, plot2, plot3, plot4, plot5, nrow = 3, ncol=2)
ggsave(filename="Prob2_10__ACFPlot_Output.png", plot=tbl_acfplot)





#plot(JPM$Date, JPM$Adj.Close, type="l", lwd=2, col="lightblue", xlab="Date (1=Jan 1, 2017,502=Jan 1, 2019 )",
#     ylab="Daily Adj. Closing Price", main="Daily Adj. Closing by Date (Ascending) with LR Line" )
#lines(model$fitted.values, lwd=2, type="l", lty=2, col="navy")         

# ggplot(HomeSales4, aes(x=fit_Model0_TtlSF, y=SalePrice)) + 
#   geom_point(size=2, color="blue") +
#   ggtitle("Model0a (SalePrice ~ TotalSF)") +
#   xlab("Fitted SalePrice") +
#   theme(plot.title=element_text(lineheight=0.8, size = 16, face="bold", hjust=0.5)) +
#   theme(axis.title = element_text(size = 14)) + 
#   theme(axis.text = element_text(size = 12)) + 
#   geom_smooth(method=lm, se=FALSE, color = "grey")  




#forecast::autoplot()
#beerobj <- fpp::ausbeer
#forecast(beerobj)
#plot(forecast(beerobj))

#describe(beerobj)
#typeof(beerobj)




#visnights contains quarterly visitor nights (in millions) from 1998 to 2016 for twenty regions of Australia.

str(visnights)  # Time-Series [1:76, 1:20] from 1998 to 2017...
tail(visnights,20)   #ends with 2016-Q4, so the 2017 end noted above is NOT included in the time series
summary(visnights)
head(visnights)
plot(visnights[, c('QLDMetro')])
autoplot(visnights)   #plots each column as a series


length(visnights)  #1520    - 20*76=1520
length(visnights[,c('QLDMetro')])

#a.	Use window() to create three training sets for visnights[,"QLDMetro"], omitting the last 1, 2 and 3 years; call these train1, train2, and train3, respectively. For example train1 <- window(visnights[, "QLDMetro"], end = c(2015, 4)).

train1 <- stats::window(visnights[,"QLDMetro"], end=c(2015, 4) )   #note the end point IS included here, as shown below

#tail(train1, 12)   #note that if you don't specify a length, it returns 6 by default.  With a 4 column display for each qtr, this looked like 2014Q1 and 2014Q2 were null.  
#tail(visnights[,"QLDMetro"])
#tail(visnights, 10)
#str(train1)    #Time-Series [1:72] from 1998 to 2016: 12.11 7.79 11.38 9.31 12.67 ...

train2 <- stats::window(visnights[,"QLDMetro"], end=c(2014, 4) )
train3 <- stats::window(visnights[,"QLDMetro"], end=c(2013, 4) )

str(train1)
showMethods("ts")
showClass("ts")

#set the remaining data aside as test sets
test1 <- stats::window(visnights[,"QLDMetro"], start=c(2016,1), end=c(2016,4))
test2 <- stats::window(visnights[,"QLDMetro"], start=c(2015,1), end=c(2016,4))
test3 <- stats::window(visnights[,"QLDMetro"], start=c(2014,1), end=c(2016,4))

#b.	Compute one year of forecasts for each training set using the snaive() method. Call these fc1, fc2 and fc3, respectively.
fc1 <- snaive(train1, h=4)
  #set h to 4 since the frequency of train1 is quarterly (4)   #frequency(train1)  #4

fc2 <- snaive(train2, h=4)
fc3 <- snaive(train3, h=4)


#c.	Use accuracy() to compare the MAPE over the three test sets. Comment on these.
acc1 <- forecast::accuracy(f=fc1, x=test1)
acc1


#?window
#str(acc1)
#attributes(acc1)
#class(acc1)
#acc1[,"MAPE"]


acc2 <- forecast::accuracy(f=fc2, x=test2)
  #acc2b <- forecast::accuracy(f=fc2, x=window(test2, end=c(2015, 4)))
  #acc2 == acc2b     
  #ME RMSE  MAE  MPE MAPE MASE ACF1 Theil's U
  #Training set TRUE TRUE TRUE TRUE TRUE TRUE TRUE        NA
  #Test set     TRUE TRUE TRUE TRUE TRUE TRUE TRUE      TRUE
  
  #they match, so apparently it's ok to give a longer test window than is found in the forecast

#acc2c <- forecast::accuracy(f=fc2, x=window(test2, end=c(2015, 2)))
#shorter test window than is found in the forecast.   no error given.  not sure what it's doing, be careful to give right window or you may get values w/ no indicator that the test window is shorter.
acc2


acc3 <- forecast::accuracy(f=fc3, x=test3)


acc1[, "MAPE"]
acc2[, "MAPE"]
acc3[, "MAPE"]





wkg_dir <- "C:/Users/ashle/Documents/Personal Data/Northwestern/2019_01 winter  MSDS413 Time Series/HW1/"


#Section 3.7, Question 10
#10.	Use the Dow Jones index (data set dowjones) to do the following:

ts_dj <- dowjones   #from the fma package.   help file says it is Dow-Jones index, 28 Aug - 18 Dec 1972

class(ts_dj)   #ts
str(ts_dj)    # Time-Series [1:78] from 1 to 78: 111 111 110 111 111 ...
head(ts_dj)
tail(ts_dj)

frequency(ts_dj)  #1   not sure what that means.  Would have expected 365 for daily data or ~225 for trading days, which is indicated by help file
#time(ts_dj)
#cycle(ts_dj)


#  a.	Produce a time plot of the series.
png(filename = paste(wkg_dir, "Prob3_10__TS_Plot.png", sep=""), width=480,height=480,bg = "white")

ggtsdisplay(ts_dj, main="Dow Jones index from Aug 28, 1972 to Dec 18, 1972", xlab="Days from Aug 28, 1972", ylab="Dow Jones Index")
#produces a list???   help says value is none, but if I assign it, I get a list, which doesn't save with ggsave, so use png() fn

#autoplot(ts_dj)


#class(ts_plot)
#attributes(ts_plot[1]$data)
#plot(ts_plot[1]$data[2])
dev.off()

getwd()



#b.	Produce forecasts using the drift method and plot them.
fcst_dj <- forecast::rwf(ts_dj, h=20, drift=TRUE)
autoplot(fcst_dj)
class(fcst_dj)

#c.	Show that the forecasts are identical to extending the line drawn between the first and last observations.
dj_first_val <- ts_dj[1]
dj_last_val <- ts_dj[length(ts_dj)]
dj_slope <- (dj_last_val - dj_first_val)/(length(ts_dj) - 1)

fcst2_dj <- seq(1, 20) * dj_slope  + dj_last_val

attributes(fcst_dj)
fcst_dj["Forecast"]
fcst_diffs <- fcst_dj$mean - fcst2_dj
#note these are all ~0 to within machine precision

fcst2_dj_all <- seq(0, 97) * dj_slope  + dj_first_val


plot_drift <- autoplot(fcst_dj) +
              autolayer(ts(fcst2_dj_all), color = "red", linetype = "dashed", series="First-Last Extended" ) +
              theme(legend.position = "right")  + 
              ylab("Dow Jones Index") 

ggsave(filename="Prob3_10__DriftPlot.png", plot=plot_drift)




#d.	Try using some of the other benchmark functions to forecast the same data set. 

#naive forecast (next value == last value)
fcst_naive <- naive(ts_dj, h=20)
plot1 <- autoplot(fcst_naive, main="'Naive method' forecast of Dow Jones index", ylab="Dow Jones Index")

#seasonal naive -- use the value from the same season, 1 yr ago
fcst_snaive <- forecast::snaive(ts_dj, h=20)
plot2 <- autoplot(fcst_snaive, main="'Seasonal Naive method' forecast of Dow Jones index", ylab="Dow Jones Index")
#no seasonality to work from!

#use the avg value of all the preceding data
fcst_mean <- forecast::meanf(ts_dj, h=20)
plot3 <- autoplot(fcst_mean, main="'Average method' forecast of Dow Jones index", ylab="Dow Jones Index")


benchmark_plots <- gridExtra::grid.arrange(plot1, plot2, plot3, plot_drift, nrow=2, ncol=2)

ggsave(filename="Prob3_10__BenchmarkPlots.png", plot=benchmark_plots)



#Section 3.7, Question 11. 
#11.	Consider the daily closing IBM stock prices (data set ibmclose).
ts_ibm <- fma::ibmclose    
  # from the description:   Daily closing IBM stock price.  start/end???

#str(ts_ibm)  # Time-Series [1:369] from 1 to 369: 460 457 452 459 462 459 463 479 493 490 ...
#head(ts_ibm)
#class(ts_ibm)  #ts
#attributes(ts_ibm)


#a.	Produce some plots of the data in order to become familiar with it.
tsdisplay(ts_ibm)

plot1 <- autoplot(ts_ibm, main="IBM stock price at end of day", xlab="Day", ylab="Closing price of IBM stock (US$)")
plot1

plot2 <- ggAcf(ts_ibm, main="ACF plot for IBM closing prices")

plot3 <- gglagplot(ts_ibm, main="Lag plot for IBM closing prices")

ibm_eda_plots <- gridExtra::grid.arrange(plot1, plot2, plot3, nrow=2, ncol=2)

ggsave(filename="Prob3_11__IBM_EDA.png", plot=ibm_eda_plots)




ibm_eda_plot2 <- ggtsdisplay(ts_ibm, main="IBM stock price at end of day")
ggsave(paste(wkg_dir, "Prob3_11__IBM_EDA2.png", sep=""), plot=ibm_eda_plot2)

device
#ts_ibm_lag2 <- window(ts_ibm, start=2)
#returns_ibm <- ts_ibm/ts_ibm_lag2
#head(returns_ibm)


#ts_ibm_lag <- lag(ts_ibm, k=1)
#gglagchull(ts_ibm)
#ts_ibm[3]
#ts_ibm_lag[0]


#decompose(ts_ibm, type="additive")
#plot(stl(ts_ibm, s.window="periodic"))
  #no seasonality to decompose!


#b.	Split the data into a training set of 300 observations and a test set of 69 observations.
ts_ibm_train <- window(ts_ibm, end=300)
ts_ibm_test <- window(ts_ibm, start=301)

str(ts_ibm_train)  #Time-Series [1:300] from 1 to 300: 460 457 452 459 462 459 463 479 493 490 ...
str(ts_ibm_test)   #Time-Series [1:69] from 301 to 369: 387 387 376 385 385 380 373 382 377 376 ...


#c.	Try using various benchmark methods to forecast the training set and compare the results on the test set. Which method did best?

#   i)  'naive' method of forecasting
fcst_ibm1 <- forecast::naive(ts_ibm_train, h=69)
acc1_ibm <- accuracy(fcst_ibm1, ts_ibm_test)
acc1_ibm_rmse <- round(acc1_ibm[,"RMSE"][2], 1)
acc1_ibm_mape <- round(acc1_ibm[,"MAPE"][2], 1)

plt1_ibm <- autoplot(fcst_ibm1, main="'Naive' method forecast of IBM's closing stock price", ylab="Closing price of IBM stock") + 
            autolayer(ts_ibm_test, color="red") + 
            annotate("text", label = "Error on test set:", x = 300, y = 600) + 
            annotate("text", label = paste("RMSE =", acc1_ibm_rmse), x = 300, y = 570) + 
            annotate("text", label = paste("MAPE =", acc1_ibm_mape), x = 300, y = 540)

#'average' method of forecasting
fcst_ibm2 <- forecast::meanf(ts_ibm_train, h=69)
acc2_ibm <- accuracy(fcst_ibm2, ts_ibm_test)
acc2_ibm_rmse <- round(acc2_ibm[,"RMSE"][2], 1)
acc2_ibm_mape <- round(acc2_ibm[,"MAPE"][2], 1)

plt2_ibm <- autoplot(fcst_ibm2, main="'Average' method forecast of IBM's closing stock price", ylab="Closing price of IBM stock") + 
            autolayer(ts_ibm_test, color="red") + 
            annotate("text", label = "Error on test set:", x = 240, y = 640) + 
            annotate("text", label = paste("RMSE =", acc2_ibm_rmse), x = 240, y = 612) + 
            annotate("text", label = paste("MAPE =", acc2_ibm_mape), x = 240, y = 584)



#'drift' method of forecasting
fcst_ibm3 <- forecast::rwf(ts_ibm_train, h=69, drift = TRUE)
acc3_ibm <- accuracy(fcst_ibm3, ts_ibm_test)
acc3_ibm_rmse <- round(acc3_ibm[,"RMSE"][2], 1)
acc3_ibm_mape <- round(acc3_ibm[,"MAPE"][2], 1)

plt3_ibm <- autoplot(fcst_ibm3, main="'Drift' method forecast of IBM's closing stock price", ylab="Closing price of IBM stock") + 
            autolayer(ts_ibm_test, color="red") + 
            annotate("text", label = "Error on test set:", x = 300, y = 600) + 
            annotate("text", label = paste("RMSE =", acc3_ibm_rmse), x = 300, y = 570) + 
            annotate("text", label = paste("MAPE =", acc3_ibm_mape), x = 300, y = 540)


#pull together the 4 plots into a single grid
fcst_grid_ibm <- gridExtra::grid.arrange(plt1_ibm, plt2_ibm, plt3_ibm, nrow=2, ncol=2)

#save the image to a file
ggsave(filename="Prob3_11__IBM_Fcsts.png", plot=fcst_grid_ibm)
plot(fcst_grid_ibm)


acc1_ibm
acc2_ibm
acc3_ibm

#  d.	Check the residuals of your preferred method. Do they resemble white noise?
resid_plt <- checkresiduals(fcst_ibm3)  

ggsave(filename="Prob3_11__IBM_Residuals.png", plot=resid_plt)
plot(resid_plt)










  




#Section 3, question 12
#12.	Consider the sales of new one-family houses in the USA, Jan 1973 ? Nov 1995 (data set hsales).
ts_hsales <- fma::hsales
  #description in help:  Monthly sales of new one-family houses sold in the USA since 1973



#str(hsales)    #Time-Series [1:275] from 1973 to 1996: 55 60 68 63 65 61 54 52 46 42 ...
#class(hsales)   #ts
#frequency(hsales)  #12
#head(hsales, 24)
#     Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
#1973  55  60  68  63  65  61  54  52  46  42  37  30
#1974  37  44  55  53  58  50  48  45  41  34  30  24


#a.	Produce some plots of the data in order to become familiar with it.
tsdisplay(ts_hsales)

plot1_hsales <- autoplot(ts_hsales, main="Monthly home sales in the US", xlab="Day", ylab="Homes sold   in '000's??")

plot2_hsales <- autoplot(stl(ts_hsales, s.window="periodic"), main="STL decomposition of hsales")

plot3_hsales <- ggseasonplot(ts_hsales, main="Season plot for monthly home sales", ylab="Homes sold   in '000's??")

plot4_hsales <- ggsubseriesplot(ts_hsales, main="Subseries plot for monthly home sales", ylab="Homes sold   in '000's??")

hsales_eda_plots <- gridExtra::grid.arrange(plot1_hsales, plot2_hsales, plot3_hsales, plot4_hsales, nrow=2, ncol=2)
ggsave(filename="Prob3_12__hsales_EDA.png", plot=hsales_eda_plots)


#autocorrelation investigation
plot1_hsales_AC <- ggAcf(ts_hsales, main="ACF plot for monthly home sales")

plot2_hsales_AC <- gglagplot(ts_hsales, main="Lag plot for monthly home sales")

#hsales_eda_plots_AC <- gridExtra::grid.arrange(plot1_hsales_AC, plot2_hsales_AC, nrow=2, ncol=1)
ggsave(filename="Prob3_12__hsales_EDA_AC1.png", plot=plot1_hsales_AC)
ggsave(filename="Prob3_12__hsales_EDA_AC2.png", plot=plot2_hsales_AC)


#get the partial ACF plot
ggtsdisplay(ts_hsales)


#b.	Split the hsales data set into a training set and a test set, where the test set is the last two years of data.
ts_hsales_train <- window(ts_hsales, end=c(1993, 11))
ts_hsales_test <- window(ts_hsales, start=c(1993, 12))



str(ts_hsales_train)  #Time-Series [1:251] from 1973 to 1994: 55 60 68 63 65 61 54 52 46 42 ...
  #note the end year is an approximation - it really only goes to Nov 1993
tail(ts_hsales_train, 24)
# Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
# 1991                                              36
# 1992  48  55  56  53  52  53  52  56  51  48  42  42
# 1993  44  50  60  66  58  59  55  57  57  56  53

str(ts_hsales_test)   #Time-Series [1:24] from 1994 to 1996: 51 45 58 74 65 65 55 52 59 54 ...
ts_hsales_test



#c.	Try using various benchmark methods to forecast the training set and compare the results on the test set. Which method did best?

#   i)  'naive' method of forecasting
fcst_hsales1 <- forecast::naive(ts_hsales_train, h=24)
acc1_hsales <- accuracy(fcst_hsales1, ts_hsales_test)
acc1_hsales_rmse <- round(acc1_hsales[,"RMSE"][2], 1)
acc1_hsales_mape <- round(acc1_hsales[,"MAPE"][2], 1)

plt1_hsales <- autoplot(fcst_hsales1, main="'Naive' method forecast of monthly home sales", ylab="Homes sold   - in '000's??") + 
                autolayer(ts_hsales_test, color="red") + 
                annotate("text", label = "Error on test set:", x = 1988, y = 108, hjust=0) + 
                annotate("text", label = paste("RMSE =", acc1_hsales_rmse), x = 1988, y = 100, hjust=0) + 
                annotate("text", label = paste("MAPE =", acc1_hsales_mape), x = 1988, y = 92, hjust=0)

#   ii)  'Seasonal naive' method of forecasting
fcst_hsales4 <- forecast::snaive(ts_hsales_train, h=24)
acc4_hsales <- accuracy(fcst_hsales4, ts_hsales_test)
acc4_hsales_rmse <- round(acc4_hsales[,"RMSE"][2], 1)
acc4_hsales_mape <- round(acc4_hsales[,"MAPE"][2], 1)

plt4_hsales <- autoplot(fcst_hsales4, main="'Seasonal Naive' method forecast of monthly home sales", ylab="Homes sold   - in '000's??") + 
                autolayer(ts_hsales_test, color="red") + 
                annotate("text", label = "Error on test set:", x = 1988, y = 108, hjust=0) + 
                annotate("text", label = paste("RMSE =", acc4_hsales_rmse), x = 1988, y = 100, hjust=0) + 
                annotate("text", label = paste("MAPE =", acc4_hsales_mape), x = 1988, y = 92, hjust=0)


#   iii) 'average' method of forecasting
fcst_hsales2 <- forecast::meanf(ts_hsales_train, h=24)
acc2_hsales <- accuracy(fcst_hsales2, ts_hsales_test)
acc2_hsales_rmse <- round(acc2_hsales[,"RMSE"][2], 1)
acc2_hsales_mape <- round(acc2_hsales[,"MAPE"][2], 1)

plt2_hsales <- autoplot(fcst_hsales2, main="'Average' method forecast of monthly home sales", ylab="Homes sold   - in '000's??") + 
                autolayer(ts_hsales_test, color="red") + 
                annotate("text", label = "Error on test set:", x = 1988, y = 100, hjust=0) + 
                annotate("text", label = paste("RMSE =", acc2_hsales_rmse), x = 1988, y = 95, hjust=0) + 
                annotate("text", label = paste("MAPE =", acc2_hsales_mape), x = 1988, y = 90, hjust=0)



#  iv)   'drift' method of forecasting
fcst_hsales3 <- forecast::rwf(ts_hsales_train, h=24, drift = TRUE)
acc3_hsales <- accuracy(fcst_hsales3, ts_hsales_test)
acc3_hsales_rmse <- round(acc3_hsales[,"RMSE"][2], 1)
acc3_hsales_mape <- round(acc3_hsales[,"MAPE"][2], 1)

plt3_hsales <- autoplot(fcst_hsales3, main="'Drift' method forecast of monthly home sales", ylab="Homes sold   - in '000's??") + 
                autolayer(ts_hsales_test, color="red") + 
                annotate("text", label = "Error on test set:", x = 1988, y = 113, hjust=0) + 
                annotate("text", label = paste("RMSE =", acc3_hsales_rmse), x = 1988, y = 104, hjust=0) + 
                annotate("text", label = paste("MAPE =", acc3_hsales_mape), x = 1988, y = 95, hjust=0)


fcst_grid_hsales <- gridExtra::grid.arrange(plt1_hsales, plt4_hsales, plt2_hsales, plt3_hsales, nrow=2, ncol=2)

#save the image to a file
ggsave(filename="Prob3_12__hsales_Fcsts.png", plot=fcst_grid_hsales)
#plot(fcst_grid_hsales)



#  d.	Check the residuals of your preferred method. Do they resemble white noise?
checkresiduals(fcst_hsales4)  


  







#Section 5.10 Exercises, problem 6
#The gasoline series consists of weekly data for supplies of US finished motor gasoline product, 
#from 2 February 1991 to 20 January 2017. The units are in ?million barrels per day?. Consider only the data to the end of 2004.

require(fpp2)
require(forecast)
require(ggplot2)
ts_gas <- window(gasoline, end=2005)

str(ts_gas)
frequency(ts_gas)  #52.17857
head(ts_gas)
tail(ts_gas)
class(ts_gas)
summary(ts_gas)

autoplot(ts_gas, main="weekly volume of US finished motor gasoline", ylab="Volume --  millions of barrels per day")
plot(stl(ts_gas, s.window="periodic"))
autoplot(decompose(ts_gas, type="additive"))
autoplot(decompose(ts_gas, type="multiplicative"))
ggseasonplot(ts_gas)


stl_out <- stl(ts_gas, s.window="periodic")

class(stl_out)
str(stl_out)
attributes(stl_out)

stl_out$time.series
stl_out$weights
stl_out$call
stl_out$win
stl_out$deg   
stl_out$jump      
stl_out$inner   
stl_out$outer

class(stl_out$time.series)
str(stl_out$time.series)
stl_out$time.series[,"mts"]

seasonal(stl_out)
#pretty sure this is the exact same  as the fn above     stl_out$time.series[, "seasonal"]

trendcycle(stl_out)
stl_out$time.series[, "trend"]

remainder(stl_out)
stl_out$time.series[, "remainder"]




#a.	Fit a harmonic regression with trend to the data. Experiment with changing the number Fourier terms. 
#Plot the observed gasoline and fitted values and comment on what you see.

fourier_out <- fourier(ts_gas, K=4)

str(fourier_out)
# num [1:726, 1:8] 0.12 0.239 0.353 0.463 0.566 ...
# - attr(*, "dimnames")=List of 2
# ..$ : NULL
# ..$ : chr [1:8] "S1-52" "C1-52" "S2-52" "C2-52" ...

head(fourier_out)
#         S1-52     C1-52     S2-52     C2-52     S3-52      C3-52     S4-52      C4-52
# [1,] 0.1201262 0.9927586 0.2385126 0.9711394 0.3534447  0.9354554 0.4632579  0.8862235
# [2,] 0.2385126 0.9711394 0.4632579 0.8862235 0.6612635  0.7501537 0.8211001  0.5707842
# [3,] 0.3534447 0.9354554 0.6612635 0.7501537 0.8837203  0.4680153 0.9920985  0.1254612
# [4,] 0.4632579 0.8862235 0.8211001 0.5707842 0.9920985  0.1254612 0.9373419 -0.3484108
# [5,] 0.5663619 0.8241566 0.9335419 0.3584683 0.9724076 -0.2332885 0.6692904 -0.7430009
# [6,] 0.6612635 0.7501537 0.9920985 0.1254612 0.8271893 -0.5619233 0.2489398 -0.9685190

#pretty sure the columns are named S or C for sine or cosine;  1, 2, 3, ...  for the ordinal # of that term;  -M  where M is the frequency -- ex. here we have ~weekly seasonality so M=52

#frequency(ts_gas)  #52.17857
  
#X1,t=1 = sin(2*pi*1/52.17857)
#X1,t=2 = sin(2*pi*2/52.17857)
#X1,t=3 = sin(2*pi*3/52.17857)

#X2,t=1 = cos(2*pi*1/52.17857)
#X2,t=2 = cos(2*pi*2/52.17857)
#X2,t=3 = cos(2*pi*3/52.17857)

#X3,t=1 = sin(4*pi*1/52.17857)
#X3,t=2 = sin(4*pi*2/52.17857)
#X3,t=3 = sin(4*pi*3/52.17857)



# K = 4
# =====
fcst_hmnc1 <- tslm(ts_gas ~ trend + fourier(ts_gas, K=4))
summary(fcst_hmnc1)

attributes(fcst_hmnc1)

str(fcst_hmnc1$fitted.values)

autoplot(ts_gas, main="weekly volume of US finished motor gasoline", ylab="Volume --  millions of barrels per day") +
  autolayer(fcst_hmnc1$fitted.values, linetype="dashed",  color="red", size=2, series="Fitted Values") +
  labs(subtitle="with harmonic regression  (K=4)") + 
  theme(plot.subtitle=element_text(size=12, hjust=0, face="italic", color="black"))  



# K = 8
# =====
fcst_hmnc2 <- tslm(ts_gas ~ trend + fourier(ts_gas, K=8))
summary(fcst_hmnc2)

autoplot(ts_gas, main="weekly volume of US finished motor gasoline", ylab="Volume --  millions of barrels per day") +
  autolayer(fcst_hmnc2$fitted.values, linetype="dashed",  color="red", size=2, series="Fitted Values") +
  labs(subtitle="with harmonic regression  (K=8)") + 
  theme(plot.subtitle=element_text(size=12, hjust=0, face="italic", color="black"))  

# K = 13
# ======
fcst_hmnc3 <- tslm(ts_gas ~ trend + fourier(ts_gas, K=13))
summary(fcst_hmnc3)

autoplot(ts_gas, main="Weekly volume of US finished motor gasoline", ylab="Volume --  millions of barrels per day") +
  autolayer(fcst_hmnc3$fitted.values, linetype="dashed",  color="red", size=2, series="Fitted Values") +
  labs(subtitle="with harmonic regression  (K=13)") + 
  theme(plot.subtitle=element_text(size=12, hjust=0, face="italic", color="black"))  
  

# K = 26  -- this is the max, as we can only go up to M/2
# ======
fcst_hmnc4 <- tslm(ts_gas ~ trend + fourier(ts_gas, K=26))
summary(fcst_hmnc4)

autoplot(ts_gas, main="Weekly volume of US finished motor gasoline", ylab="Volume --  millions of barrels per day") +
  autolayer(fcst_hmnc4$fitted.values, linetype="dashed",  color="red", size=2, series="Fitted Values") +
  labs(subtitle="with harmonic regression  (K=26)") + 
  theme(plot.subtitle=element_text(size=12, hjust=0, face="italic", color="black"))  







#b.	Select the appropriate number of Fourier terms to include by minimising the AICc or CV value.
#accuracy(fcst_hmnc1)
#attributes(CV(fcst_hmnc1))


AICc_rslts <- c(1:26)  #pre-allocate the entire length of the vector - appending to a vector is discouraged b/c of copies
for (kval in 1:26) 
  {
  tslm(ts_gas ~ trend + fourier(ts_gas, K=kval)) %>% CV() -> cv_out
  AICc_rslts[kval] <- cv_out["AICc"]  #overwrite the value in the i'th posn with the 
  next  
  }
plot(AICc_rslts)
#reading off, we see the minimum AICc happens for K=12
AICc_rslts[12]

CV_rslts <- c(1:26)  #pre-allocate the entire length of the vector - appending to a vector is discouraged b/c of copies
for (kval in 1:26) 
{
  tslm(ts_gas ~ trend + fourier(ts_gas, K=kval)) %>% CV() -> cv_out
  CV_rslts[kval] <- cv_out["CV"]  #overwrite the value in the i'th posn with the 
  next  
}


df_cv_rslts <- data.frame(cbind(seq(1:26), CV_rslts))
colnames(df_cv_rslts) <- c("k", "CV")

plot(CV_rslts)
#the minimum CV also happens for K=12
ggplot(data=df_cv_rslts, aes(x=k, y=CV)) +
  geom_line() + 
  xlab("K") + ylab("CV") +
  ggtitle("Optimal K for minimum CV score")

CV_rslts[12]

ggsave(filename="Prob5_10__optimalK.png")


#c.	Check the residuals of the final model using the checkresiduals() function. Even though the residuals fail the 
#correlation tests, the results are probably not severe enough to make much difference to the forecasts and prediction 
#intervals. (Note that the correlations are relatively small, even though they are significant.)

fnl_mdl__ts_gas <- tslm(ts_gas ~ trend + fourier(ts_gas, K=12))

checkresiduals(fnl_mdl__ts_gas)



#d.	To forecast using harmonic regression, you will need to generate the future values of the Fourier terms. This can be done as follows.
#fc <- forecast(fit, newdata=data.frame(fourier(x,K,h)))
#where fit is the fitted model using tslm(), K is the number of Fourier terms used in creating fit, and h is the forecast horizon required.
#Forecast the next year of data.

fc_fnl_mdl <- forecast(fnl_mdl__ts_gas, newdata=data.frame(fourier(ts_gas,K=12,h=52)))

attributes(fc_fnl_mdl)
fc_fnl_mdl$mean


#e.	Plot the forecasts along with the actual data for 2005. What do you find?

autoplot(window(gasoline, end=2006), main="Weekly volume of US finished motor gasoline", ylab="Volume --  millions of barrels per day") +  
  autolayer(fc_fnl_mdl$mean, color="red", size=1, series="Fitted Values") +
  labs(subtitle="2005 forecasted with harmonic regression  (K=12)") + 
  theme(plot.subtitle=element_text(size=12, hjust=0, face="italic", color="black"))  

ggsave(filename="Prob5_10__2005predn.png")







#Section 6.9 Exercises, problem 6
#We will use the bricksq data (Australian quarterly clay brick production. 1956?1994) for this exercise. 

brick_ts <- bricksq

#check it out
str(brick_ts)
head(brick_ts, 24)
frequency(brick_ts)
length(brick_ts)

autoplot(brick_ts)

#a.	Use an STL decomposition to calculate the trend-cycle and seasonal indices. (Experiment with having fixed or changing seasonality.)
brick_stl_out1 <- stl(brick_ts, s.window="periodic")   #using periodic => seasonality is fixed??
autoplot(brick_stl_out1, main="Clay brick production in Australia, by quarter")

#loess

brick_stl_out2 <- stl(brick_ts, s.window=155)   #since the length of the series is 155, is this equiv to using periodic => seasonality is fixed??
autoplot(brick_stl_out2)

#very very similar but not exactly the same - probably just a tiny diff in algorithm or rounding
plot(brick_stl_out2$time.series[,"trend"] - brick_stl_out1$time.series[,"trend"])
plot(brick_stl_out2$time.series[,"seasonal"] - brick_stl_out1$time.series[,"seasonal"])

trend1 <- data.frame(brick_stl_out1$time.series[,"trend"])
colnames(trend1) <- c("trend")
trend1$qtr_nbr <- as.integer(rownames(trend1))

trend2 <- data.frame(brick_stl_out2$time.series[,"trend"])
colnames(trend2) <- c("trend")
trend2$qtr_nbr <- as.integer(rownames(trend2))
attributes(trend2)
str(trend2)

ggplot() +
  geom_line(data=trend2, mapping=aes(x=qtr_nbr, y=trend), color="red") +
  geom_line(data=trend1, mapping=aes(x=qtr_nbr, y=trend), color="blue", series="Periodic") +
  ggtitle("STL decomposition of bricksq") + 
  labs(subtitle="with maximum s.window (155)")  + 
  guides(color=guide_legend("Series")) +
  theme(plot.subtitle=element_text(size=12, hjust=0, face="italic", color="black"))  
  
  
ggsave(filename="Prob6_6__stl_swindow155.png")

autoplot(brick_stl_out2) +
  labs(title="STL decomposition of bricksq") + 
  labs(subtitle="with maximum s.window (155)") + 
  theme(plot.subtitle=element_text(size=12, hjust=0, face="italic", color="black"))  



brick_stl_out3 <- stl(brick_ts, s.window=7)   #7 is the min allowed.  Presumably gives the greatest flexibility for seasonality to vary
autoplot(brick_stl_out3) +
  labs(title="STL decomposition of bricksq") + 
  labs(subtitle="with minimum s.window (7)") + 
  theme(plot.subtitle=element_text(size=12, hjust=0, face="italic", color="black"))  

ggsave(filename="Prob6_6__stl_swindow7.png")





#b.	Compute and plot the seasonally adjusted data.

bricks_ssnly_adjstd <- brick_ts - brick_stl_out3$time.series[,"seasonal"]

autoplot(bricks_ssnly_adjstd, main="Seasonally Adjusted brick production", ylab="Bricks produced")

ggsave(filename="Prob6_6__ssnl_adjstd.png")

bricks_ssnly_adjstd2 <- seasadj(brick_stl_out3)
autoplot(bricks_ssnly_adjstd2, main="Seasonally Adjusted brick production", ylab="Bricks produced")


#comparing, we see they are the same
autoplot(bricks_ssnly_adjstd, main="Seasonally Adjusted brick production", ylab="Bricks produced", color="red") +
  autolayer(bricks_ssnly_adjstd2, color="blue")


#c.	Use a na?ve method to produce forecasts of the seasonally adjusted data.

brick_fcst_naive <- naive(bricks_ssnly_adjstd)
autoplot(brick_fcst_naive, main="Seasonally adjusted brick production", ylab="Bricks produced") +
   labs(subtitle="With naive forecast")

ggsave(filename="Prob6_6__naive_fcst_SA.png")


#d.	Use stlf() to reseasonalise the results, giving forecasts for the original data.
brick_fcst_naive_w_season <- stlf(y=brick_ts, s.window=7, method="naive")
autoplot(brick_fcst_naive_w_season, main="Seasonally adjusted brick production", ylab="Bricks produced") +
  labs(subtitle="With naive forecast of seasonally adjusted series; re-seasonalized")

ggsave(filename="Prob6_6__naive_fcst_resznld.png")


#e.	Do the residuals look uncorrelated?
checkresiduals(brick_fcst_naive_w_season)

#f.	Repeat with a robust STL decomposition. Does it make much difference?
stl(brick_ts, s.window=7, robust=TRUE) %>%
    forecast(method="naive") %>%
      autoplot(main="Seasonally adjusted brick production", ylab="Bricks produced") +
      labs(subtitle="Robust decomposition, then naive forecast of seasonally adjusted series; re-seasonalized")

#AKW note:  prediction intervals are a tiny bit wider with the second method



#g.	Compare forecasts from stlf() with those from snaive(), using a test set comprising the last 2 years of data. Which is better?
  
brick_ts_train <- window(brick_ts, end = c(1992, 3))
brick_ts_test <- window(brick_ts, start = c(1992, 4))

# tail(brick_ts, 24)
# tail(brick_ts_train, 24)
# head(brick_ts_test, 24)

#accuracy on the training data
stlf(brick_ts_train, s.window=7, method="naive", h=8) %>% accuracy()
snaive(brick_ts_train, h=8) %>% accuracy()

#accuracy on the test data
stlf(brick_ts_train, s.window=7, method="naive", h=8) %>% accuracy(x=brick_ts_test)
snaive(brick_ts_train, h=8) %>% accuracy(x=brick_ts_test)

#AKW note:  stlf is better, because seasonal naive ignores the trend ===>  much higher error on the test set



















#Section 7.8,  problem 11
#For this exercise use data set visitors, the monthly Australian short-term overseas visitors data, May 1985?April 2005. 
#Make a time plot of your data and describe the main features of the series.
visitors_ts <- visitors

str(visitors_ts)    #Time-Series [1:240] from 1985 to 2005: 75.7 75.4 83.1 82.9 77.3 ..
head(visitors_ts, 24)
autoplot(visitors_ts, main="Short-term overseas visitors to AU by month", ylab="Visitors") +
  labs(subtitle="May 1985?April 2005")

ggsave(filename="Prob7_11__initial_plot.png")
#AKW note:   strong positive trend.   Seasonality; looks multiplicative as it grows as the trend grows


#b) Split your data into a training set and a test set comprising the last two years of available data. 
#Forecast the test set using Holt-Winters? multiplicative method.
visitors_ts_train <- window(visitors_ts, end=c(2003, 4))
visitors_ts_test <- window(visitors_ts, start=c(2003, 5))

# tail(visitors_ts_train, 24)
# head(visitors_ts_test, 36)

fcst_hw <- forecast::hw(visitors_ts_train, seasonal="multiplicative")
autoplot(fcst_hw, main="Short-term overseas visitors to AU by month", ylab="Visitors") +
  labs(subtitle="with forecast using Holt-Winters multiplicative method")


#c)  Why is multiplicative seasonality necessary here?


#d)  Forecast the two-year test set using each of the following methods: 
#        i. an ETS model;
fittedmdl_ets <- ets(visitors_ts_train, model="ZZZ")
summary(fittedmdl_ets)  #determined the best fit was an ETS(M, Ad, M)

fcst_ets <- forecast(fittedmdl_ets, h=24)

autoplot(fcst_ets, main="Short-term overseas visitors to AU by month", ylab="Visitors") +
  labs(subtitle="with forecast using ETS method (M, Ad, M)") + 
  autolayer(visitors_ts_test, series="Actual", linetype="solid", size=0.9) +
  autolayer(fcst_ets$mean, series="Forecast", linetype="dashed", size=0.8) +
  guides(color=guide_legend("Srs Plttd"))



#        ii. an additive ETS model applied to a Box-Cox transformed series;
fittedmdl_ets2 <- ets(visitors_ts_train, model="ZZZ", additive.only = TRUE, lambda = "auto")
summary(fittedmdl_ets2)
#ETS(A,Ad,A) 
#...
#lambda = "auto")   ==>  find the optimal lambda for a Box cox transformation
#as per summary:   Box-Cox transformation: lambda= 0.3625 

fcst_ets2 <- forecast(fittedmdl_ets2, h=24)

autoplot(fcst_ets2, main="Short-term overseas visitors to AU by month", ylab="Visitors", color="blue", linetype="dotted") +
  labs(subtitle="with forecast using ETS method (A, Ad, A) and BoxCox transformation (lambda = 0.3625)") + 
  autolayer(visitors_ts_test, color="black", linetype="solid", size=0.9) +
  autolayer(visitors_ts_train, color="black", linetype="solid", size=0.9)



#        iii.  a seasonal na?ve method;
fcst_vizs_snaive <- snaive(visitors_ts_train, h=24)
summary(fcst_vizs_snaive)

autoplot(fcst_vizs_snaive, main="Short-term overseas visitors to AU by month", ylab="Visitors", color="blue", linetype="dotted") +
  labs(subtitle="with seasonal naive forecast") + 
  autolayer(visitors_ts_test, color="black", linetype="solid", size=0.9) +
  autolayer(visitors_ts_train, color="black", linetype="solid", size=0.9)

#        iv.  an STL decomposition applied to the Box-Cox transformed data 
#               followed by an ETS model applied to the seasonally adjusted (transformed) data.
vis_train_xformed <- forecast::BoxCox(visitors_ts_train, lambda = "auto")
attributes(vis_train_xformed)
# autoplot(vis_train_xformed, color="blue")
# 
# autoplot(visitors_ts_train) +
#   autolayer(vis_train_xformed, color="blue")


vis_train_xformed_dcmps <- stl(vis_train_xformed, s.window = "periodic")
autoplot(vis_train_xformed_dcmps)

vis_train_xformed_SA <- seasadj(vis_train_xformed_dcmps)
autoplot(vis_train_xformed_SA)

fittedmdl_ets3 <- ets(vis_train_xformed_SA)
summary(fittedmdl_ets3)

# fcst_ets3 <- forecast(fittedmdl_ets3, h=24)
# autoplot(visitors_ts_train) +
#   autolayer(InvBoxCox(fcst_ets3$fitted, lambda=attributes(vis_train_xformed)$lambda[[1]]), color="blue")
  #is missing the seasonality added back!

fcst_ets3b <- stlf(vis_train_xformed, s.window="periodic", method="ets", h=24)
autoplot(visitors_ts) +
  autolayer(InvBoxCox(fcst_ets3b$fitted, lambda=attributes(vis_train_xformed)$lambda[[1]]), color="blue") +
  autolayer(InvBoxCox(fcst_ets3b$mean, lambda=attributes(vis_train_xformed)$lambda[[1]]), color="blue", linetype="dashed", size=0.8)





#e) Which method gives the best forecasts? Does it pass the residual tests?
accuracy(fcst_ets, visitors_ts_test)
accuracy(fcst_ets2, visitors_ts_test)
accuracy(fcst_vizs_snaive, visitors_ts_test)
accuracy(fcst_ets3b, visitors_ts_test)


checkresiduals(fcst_ets3b)


#f) Compare the same four methods using time series cross-validation with the tsCV() function instead of using a training and test set. Do you come to the same conclusions?

fcst_fn_ets <- function(xx, h) {forecast(
                                          ets(xx, model="ZZZ"), 
                                          h=h
                                          )
                                }

tsCV_model1 <- tsCV(visitors_ts_train, forecastfunction = fcst_fn_ets, h=24, window=NULL)

str(tsCV_model1)


fcst_fn_ets2 <- function(xx, h) {forecast(
  ets(xx, model="ZZZ", additive.only = TRUE, lambda = "auto"),
  h=h
)
}

tsCV_model2 <- tsCV(visitors_ts_train, forecastfunction = fcst_fn_ets2, h=24, window=NULL)




fcst_fn_ets3 <- function(xx, h) { snaive(xx, h=h) }
tsCV_model3 <- tsCV(visitors_ts_train, forecastfunction = fcst_fn_ets3, h=24, window=NULL)



fcst_fn_ets4 <- function(xx, h) {
  
  xx_xformed <- forecast::BoxCox(xx, lambda = "auto")
  xx_xformed_dcmps <- stl(xx_xformed, s.window = "periodic")
  xx_xformed_SA <- seasadj(xx_xformed_dcmps)
  #xx_fittedmdl <- ets(xx_xformed_SA)
  fcst_ets3b <- stlf(xx_xformed, s.window="periodic", method="ets", h=h)

  return(InvBoxCox(fcst_ets3b$fitted, lambda=attributes(xx_xformed)$lambda[[1]]))
}



tsCV_model4 <- tsCV(visitors_ts_train, forecastfunction = fcst_fn_ets4, h=24, window=NULL)
str(tsCV_model4)

nrow(tsCV_model3)
print(tsCV_model4)

subset(tsCV_model3, select=c('h=24'))
length(data.frame(tsCV_model3)[,'h.24'])

#take the RMSE of the h=24 column
sqrt(mean(data.frame(tsCV_model3)[,'h.24']))

rmse1 <- sqrt(mean(na.remove(tsCV_model1[,24])))
rmse2 <- sqrt(mean(na.remove(tsCV_model2[,24])))
rmse3 <- sqrt(mean(na.remove(tsCV_model3[,24])))
rmse4 <- sqrt(mean(na.remove(tsCV_model4[,24])))

rmse1
rmse2
rmse3
rmse4

4.430711
2.059565
5.833746

