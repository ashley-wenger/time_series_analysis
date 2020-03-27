

# install.packages('forecast')
# install.packages('ggplot2')
# install.packages('ggplot2')
# install.packages('gridExtra')
# install.packages('urca')
# install.packages('vars')
# install.packages('keras')
# install.packages('fpp')
# install.packages('fpp2')
# install.packages('corrplot')
# install.packages('lubridate')

require(forecast)
require(ggplot2)
require(gridExtra)
require(urca)
require(vars)
require(dplyr)
require(keras)
require(fpp2)
require(corrplot)
require(lubridate)




#read in the training data - features/predictors then the raw data

df_train_features_raw <- read.csv('dengue_features_train.csv', stringsAsFactors = FALSE, header = TRUE)
df_train_features_raw$week_start_date <- as.Date(df_train_features_raw$week_start_date, '%Y-%m-%d')
str(df_train_features_raw)
head(df_train_features_raw)
tail(df_train_features_raw)
nrow(df_train_features_raw)  #1456

df_train_labels_raw <- read.csv('dengue_labels_train.csv', stringsAsFactors = FALSE, header = TRUE)


#checks/investigation:
str(df_train_labels_raw)
nrow(df_train_labels_raw)  #1456
sum(is.na(df_train_labels_raw$total_cases))  #0  ==> they all have a value
#note there are a lot of zeros so log/lambda transforms might be an issue?

sum(df_train_labels_raw$total_cases<0)  #0  ==> they are all 0 or higher


#merge the label field with the predictors
df_train_raw <- merge(df_train_features_raw, df_train_labels_raw, by=c('city', 'year', 'weekofyear'), all.x = TRUE)
str(df_train_raw)

#make sure it is sorted correctly
df_train_raw <- df_train_raw %>% arrange(city, week_start_date)



#check for feature rows w/ missing labels.
sum(is.na(df_train_raw$total_cases))  #good, they all have a value




#read in the test data (which has features/predictors only)
df_test_features_raw <- read.csv('dengue_features_test.csv', stringsAsFactors = FALSE, header=TRUE)
df_test_features_raw$week_start_date <- as.Date(df_test_features_raw$week_start_date, '%Y-%m-%d')

#make sure it is sorted correctly
df_test_features_raw <- df_test_features_raw %>% arrange(city, week_start_date)
str(df_test_features_raw)
head(df_test_features_raw[, 1:4])
tail(df_test_features_raw[, 1:4])
nrow(df_test_features_raw)  #416

#checks
sum(is.na(df_test_features_raw$city))  #0;  good, always populated
sum(is.na(df_test_features_raw$year))  #0;  good, always populated
sum(is.na(df_test_features_raw$weekofyear))  #0;  good, always populated
sum(is.na(df_test_features_raw$week_start_date))  #0;  good, always populated



# =====================================================   
#          EDA
# =====================================================   

#check out the graph of the two time series
ggplot(df_train_raw, aes(x=week_start_date, y=total_cases, group=city)) +
  geom_line(aes(color=city)) + 
  ggtitle('Cases of dengue per week') + theme(plot.title = element_text(hjust=0.5)) + 
  xlab('Week') + theme(axis.title.x = element_text(margin = margin(0.6, 0.1, 0.1, 0.1, "cm"))) +
  ylab('Cases') + theme(axis.title.y = element_text(margin = margin(0.1, 0.6, 0.1, 0.1, "cm"))) +
  theme(legend.position = "right")


#check out the graph of the IQ time series as it gets lost in the combo graph b/c its scale is a lot lower
plt1 <- ggplot( subset(df_train_raw, city=='iq' & year>=2000 & year<2010), 
                aes(x=week_start_date, y=total_cases, group=city)) +
  geom_line(aes(color=city)) + 
  ggtitle('Cases of dengue per week') + theme(plot.title = element_text(hjust=0.5)) + 
  xlim(as.Date('01-01-1990', '%m-%d-%Y'), as.Date('01-01-2010', '%m-%d-%Y')) +
  xlab('Week') + theme(axis.title.x = element_text(margin = margin(0.6, 0.1, 0.1, 0.1, "cm"))) +
  ylab('Cases') + theme(axis.title.y = element_text(margin = margin(0.1, 0.6, 0.1, 0.1, "cm"))) +
  theme(legend.position = "right")

plt2 <- ggplot( subset(df_train_raw, city=='sj' & year>=1990 & year<2010), 
                aes(x=week_start_date, y=total_cases, group=city)) +
  geom_line(aes(color=city)) + 
  ggtitle('Cases of dengue per week') + theme(plot.title = element_text(hjust=0.5)) + 
  xlim(as.Date('01-01-1990', '%m-%d-%Y'), as.Date('01-01-2010', '%m-%d-%Y')) +
  xlab('Week') + theme(axis.title.x = element_text(margin = margin(0.6, 0.1, 0.1, 0.1, "cm"))) +
  ylab('Cases') + theme(axis.title.y = element_text(margin = margin(0.1, 0.6, 0.1, 0.1, "cm"))) +
  theme(legend.position = "right")

grid.arrange(plt1, plt2, nrow=2, ncol=1)


#handle null values
#handle outliers.
#note that IQ is basically flatlined for 2000-2002 so trim that off?  doesn't look well reported



# subset(df_train_raw, city=='iq', select=c('city', 'year', 'weekofyear', 'total_cases')) %>% group_by(city) %>%
#           summarize(minyear=min(year + 0.01*weekofyear))
# tail(subset(df_train_raw, city=='iq', select=c('year', 'weekofyear', 'week_start_date', 'total_cases')) ,20)


#find when the time series start and end -- for training
df_train_raw %>% dplyr::select(c(city, year, weekofyear, week_start_date, total_cases)) %>% 
  filter(city=='iq') %>% arrange(week_start_date) %>%
  mutate(yr_and_wk=year + 0.01*weekofyear) %>%  arrange(week_start_date) %>% 
  summarize(minyear=min(yr_and_wk), maxyear=max(yr_and_wk), minwkstrtdt=min(week_start_date), maxwkstrtdt=max(week_start_date))
#summarize(minyear=min(year + 0.01*weekofyear), maxyear=max(year + 0.01*weekofyear))
#     minyear      maxyear
#iq:   2000.26    2010.25*      2000-07-01     2010-06-25   
# --is 2010.25 when sorting by date (vs. 2010.53 when doing max; for some reason there's a wk 53 at the START of 2010)
#sj:   1990.18    2008.17     1990-04-30    2008-04-22


#find when the time series start and end -- for test
df_test_features_raw %>% dplyr::select(c(city, year, weekofyear, week_start_date)) %>% 
  filter(city=='iq') %>%
  mutate(yr_and_wk=year + 0.01*weekofyear) %>%  arrange(week_start_date) %>% 
  summarize(minyear=min(yr_and_wk), maxyear=max(yr_and_wk), minwkstrtdt=min(week_start_date), maxwkstrtdt=max(week_start_date))
#summarize(minyear=min(year + 0.01*weekofyear), maxyear=max(year + 0.01*weekofyear))
#     minyear      maxyear
#iq:   2010.26    2013.26   2010-07-02   2013-06-25
#sj:   2008.18    2013.17   2008-4-29    2013-04-23 

# the test series for both cities start immediately after the training data ends.  However, 
# 1.  those dates aren't the same  for IQ and SJ (2010-07-02  vs  2008-04-29), and 

#2.  the length of the forecast is not the same either
# nrow(subset(df_test_features_raw, city=='iq'))  #156 == 3 "yrs" at 52wk/yr
# nrow(subset(df_test_features_raw, city=='sj'))  #260 == 5 "yrs" at 52wk/yr

#note that we need a lengthy forecast, so an AR(3+) will not be a good choice due to instability.
#  ditto for GARCH


#look at iq time series individually
#-----------------------------------
ts_iq_raw <- ts(subset(df_train_raw, city=='iq', select=c('year', 'weekofyear', 'week_start_date', 'total_cases')) %>% arrange(week_start_date),
                start=c(2000, 26), frequency = 52)
str(ts_iq_raw)
head(ts_iq_raw)
tail(ts_iq_raw, 20)

#compare graph as a time series w/ freq 52 w/ graph with no freq enforced
plt1 <- autoplot(ts_iq_raw[,'total_cases'])
plt2 <- ggplot(subset(df_train_raw, city=='iq'), aes(x=week_start_date, y=total_cases)) +
  geom_line()  

grid.arrange(plt1, plt2, nrow=2, ncol=1)



#look at sj time series individually
#-----------------------------------
ts_sj_raw <- ts(subset(df_train_raw, city=='sj', select=c('year', 'weekofyear', 'week_start_date', 'total_cases')) %>% arrange(week_start_date),
                start=c(1990, 18), frequency = 52)
str(ts_sj_raw)
head(ts_sj_raw)
tail(ts_sj_raw, 20)


#compare graph as a time series w/ freq 52 w/ graph with no freq enforced
plt1 <- autoplot(ts_sj_raw[,'total_cases'])
plt2 <- ggplot(subset(df_train_raw, city=='sj'), aes(x=week_start_date, y=total_cases)) +
  geom_line()  

grid.arrange(plt1, plt2, nrow=2, ncol=1)






#look at season plots for IQ  (by week)
plt1 <- ggseasonplot(ts_iq_raw[,'total_cases'])
plt2 <- ggplot(subset(df_train_raw, city=='iq'), aes(x=weekofyear, y=total_cases, group=as.factor(year))) +
  geom_line(aes(color=as.factor(year))) + 
  theme(legend.position = 'right') 

grid.arrange(plt1, plt2, nrow=2, ncol=1)




# plt2 <- ggplot(subset(df_train_raw, city=='iq' & year>=2000 & year<2010), aes(x=weekofyear, y=total_cases, group=as.factor(year))) +
#   geom_line() + 
#   theme(legend.position = 'right') +
#   facet_wrap( ~ year, ncol=1, scales="free_y")
# 
# 
# subset(df_train_raw, city=='iq' & year<2004, select=c('week_start_date', 'year', 'weekofyear', 'total_cases'))
# subset(df_train_raw, city=='iq' & year>2003 & year<2008, select=c('week_start_date', 'year', 'weekofyear', 'total_cases'))
# window(ts_iq_raw, start=c(2003,0), end=c(2008,1))


#look at a season plot for San Juan
ggplot(subset(df_train_raw, city=='sj'), aes(x=weekofyear, y=total_cases, group=as.factor(year))) +
  geom_line(aes(color=as.factor(year))) + 
  theme(legend.position = 'right') + 
  ggtitle('Seasonal Plot of Dengue Cases')


plt1 <- ggseasonplot(ts_iq_raw[,'total_cases'], main='Seasonal Plot - Total Cases by Week') + 
  labs(subtitle = 'Iquitos')
plt2 <- ggseasonplot(ts_sj_raw[,'total_cases'], main='') + 
  labs(subtitle = 'San Juan')

grid.arrange(plt1, plt2, nrow=2, ncol=1)




#look at season plots by month
#lubridate::floor_date(df_train_raw$week_start_date, 'month')

df_cases_by_mo <- df_train_raw %>% group_by(city, mo=lubridate::floor_date(week_start_date, 'month')) %>% 
  summarize(cases_this_mo=sum(total_cases))

ggplot(subset(df_cases_by_mo, city=='sj' | city=='iq'), aes(x=mo, y=cases_this_mo, group=city)) +
  geom_col(aes(color=city))


df_cases_by_mo2 <- df_train_raw %>% 
  group_by(city, mo=lubridate::month(week_start_date), yr=lubridate::year(week_start_date)) %>% 
  summarize(cases_this_mo=sum(total_cases))

plt1 <- ggplot(subset(df_cases_by_mo2, city=='iq'), aes(x=mo, y=cases_this_mo, group=yr)) +
  geom_line(aes(color=as.factor(yr))) + xlim(1,12) + scale_x_continuous('Month', breaks=seq(1,12)) + 
  ggtitle('Seasonal Plot - Total Cases by Month', subtitle = 'Iquitos') + labs(color='Year') +
  xlab('Month') + theme(axis.title.x = element_text(margin = margin(0.6, 0.1, 0.1, 0.1, "cm"))) +
  ylab('Cases per month') + theme(axis.title.y = element_text(margin = margin(0.1, 0.6, 0.1, 0.1, "cm"))) 

plt2 <- ggplot(subset(df_cases_by_mo2, city=='sj'), aes(x=mo, y=cases_this_mo, group=yr)) +
  geom_line(aes(color=as.factor(yr))) + xlim(1,12) + scale_x_continuous('Month', breaks=seq(1,12)) + 
  ggtitle('', subtitle = 'San Juan') + labs(color='Year') +
  xlab('Month') + theme(axis.title.x = element_text(margin = margin(0.6, 0.1, 0.1, 0.1, "cm"))) +
  ylab('Cases per month') + theme(axis.title.y = element_text(margin = margin(0.1, 0.6, 0.1, 0.1, "cm"))) 

grid.arrange(plt1, plt2, nrow=2, ncol=1)




#ACF and pACF plots for iq
plt1 <- ggAcf(ts_iq_raw[, 'total_cases'], main='ACF graph of total cases per week - Iquitos') + 
  theme(axis.title.y = element_text(margin = margin(0.1, 0.6, 0.1, 0.1, "cm")))  + theme(axis.title.x = element_text(margin = margin(0.5, 0.1, 0.1, 0.1, "cm")))
plt2 <- ggPacf(ts_iq_raw[, 'total_cases'], main='pACF graph of total cases per week - Iquitos') + 
  theme(axis.title.y = element_text(margin = margin(0.1, 0.6, 0.1, 0.1, "cm")))  + theme(axis.title.x = element_text(margin = margin(0.5, 0.1, 0.1, 0.1, "cm")))
grid.arrange(plt1, plt2, nrow=2, ncol=1)


#ACF and pACF plots for SJ
plt1 <- ggAcf(ts_sj_raw[, 'total_cases'], main='pACF graph of total cases per week - San Juan') + 
  theme(axis.title.y = element_text(margin = margin(0.1, 0.6, 0.1, 0.1, "cm")))  + theme(axis.title.x = element_text(margin = margin(0.5, 0.1, 0.1, 0.1, "cm")))
plt2 <- ggPacf(ts_sj_raw[, 'total_cases'], main='pACF graph of total cases per week - San Juan') +
  theme(axis.title.y = element_text(margin = margin(0.1, 0.6, 0.1, 0.1, "cm")))  + theme(axis.title.x = element_text(margin = margin(0.5, 0.1, 0.1, 0.1, "cm")))
grid.arrange(plt1, plt2, nrow=2, ncol=1)







ndiffs(ts_iq_raw)  #1
nsdiffs(ts_iq_raw)  #error!!

autoplot(ts_iq_raw[, 'total_cases'])
autoplot(diff(ts_iq_raw[,'total_cases'], lag=1))
ggAcf(diff(ts_iq_raw[,'total_cases'], lag=1))
ggPacf(diff(ts_iq_raw[,'total_cases'], lag=1))

autoplot(diff(diff(ts_iq_raw[,'total_cases'], lag=1), lag=1))
ggAcf(diff(diff(ts_iq_raw[,'total_cases'], lag=1), lag=1))
ggPacf(diff(diff(ts_iq_raw[,'total_cases'], lag=1), lag=1))

autoplot(diff(diff(ts_iq_raw[,'total_cases'], lag=1), lag=3))
autoplot(diff(ts_iq_raw[,'total_cases'], lag=52))
autoplot(diff(diff(ts_iq_raw[,'total_cases'], lag=1), lag=52))
autoplot(diff(diff(diff(ts_iq_raw[,'total_cases'], lag=1), lag=52),lag=52))
autoplot(diff(diff(diff(diff(ts_iq_raw[,'total_cases'], lag=1), lag=52),lag=52), lag=52))
autoplot(diff(diff(diff(diff(ts_iq_raw[,'total_cases'], lag=1), lag=52),lag=52), lag=52))

ndiffs(ts_sj_raw)  #1
nsdiffs(ts_sj_raw)  #1

autoplot(ts_sj_raw[, 'total_cases'])
autoplot(diff(ts_sj_raw[,'total_cases'], lag=1))
autoplot(diff(ts_sj_raw[,'total_cases'], lag=3))
autoplot(diff(diff(ts_sj_raw[,'total_cases'], lag=1), lag=1))
autoplot(diff(diff(diff(ts_sj_raw[,'total_cases'], lag=1), lag=1), lag=1))
autoplot(diff(ts_sj_raw[,'total_cases'], lag=52))
autoplot(diff(diff(ts_sj_raw[,'total_cases'], lag=1), lag=156))
autoplot(diff(diff(diff(ts_sj_raw[,'total_cases'], lag=1), lag=52),lag=52))
autoplot(diff(diff(diff(diff(ts_sj_raw[,'total_cases'], lag=1), lag=52),lag=52), lag=52))
autoplot(diff(diff(diff(diff(ts_sj_raw[,'total_cases'], lag=1), lag=52),lag=52), lag=52))



# calls %>% mstl() %>% autoplot()
# str(calls)
# summary(calls)



df_4_msts_iq <- subset(df_train_raw, city=='iq', select=c('total_cases', 'week_start_date')) %>% arrange(week_start_date)
msts_iq_raw <- msts(df_4_msts_iq[,'total_cases'],
                    start=c(2000, 26), seasonal.periods = c(3, 52))  #tried 4, 52 but patterns look silly and residuals have way too much structure
mstl(msts_iq_raw) %>% autoplot(main='MSTL decomposition of cases per week - Iquitos')
#is essentially identical to stl when there's only one seasonalperiod
#stl(ts_iq_raw[,'total_cases'], s.window ='periodic') %>% autoplot()

df_4_msts_sj <- subset(df_train_raw, city=='sj', select=c('year', 'weekofyear', 'week_start_date', 'total_cases')) %>% arrange(week_start_date)
msts_sj_raw <- msts(df_4_msts_sj[,'total_cases'], start=c(1990, 18), seasonal.periods = c(52))
mstl(msts_sj_raw) %>% autoplot(main='MSTL decomposition of cases per week - San Juan')





# ==========================================
#check correlations with external regressors
# ==========================================
# str(df_train_features_raw)
# colnames(df_train_features_raw)
# paste(colnames(df_train_features_raw))
# toString(colnames(df_train_features_raw))
# # "city, year, weekofyear, week_start_date, ndvi_ne, ndvi_nw, ndvi_se, ndvi_sw, precipitation_amt_mm, reanalysis_air_temp_k, reanalysis_avg_temp_k, reanalysis_dew_point_temp_k, reanalysis_max_air_temp_k, reanalysis_min_air_temp_k, reanalysis_precip_amt_kg_per_m2, reanalysis_relative_humidity_percent, reanalysis_sat_precip_amt_mm, reanalysis_specific_humidity_g_per_kg, reanalysis_tdtr_k, station_avg_temp_c, station_diur_temp_rng_c, station_max_temp_c, station_min_temp_c, station_precip_mm"
# # ndvi_ne, ndvi_nw, ndvi_se, ndvi_sw, precipitation_amt_mm, reanalysis_air_temp_k, reanalysis_avg_temp_k, reanalysis_dew_point_temp_k, reanalysis_max_air_temp_k, reanalysis_min_air_temp_k, reanalysis_precip_amt_kg_per_m2, reanalysis_relative_humidity_percent, reanalysis_sat_precip_amt_mm, reanalysis_specific_humidity_g_per_kg, reanalysis_tdtr_k, station_avg_temp_c, station_diur_temp_rng_c, station_max_temp_c, station_min_temp_c, station_precip_mm"
# subset(df_train_features_raw, select=-c('year', 'weekofyear'))
# 

#first by month and year
# ----------------------
#group by mo and year.
xreg_by_mo <- df_train_raw %>% 
  group_by(city, year, mo=lubridate::month(week_start_date)) %>%
  summarize(
    ndvi_ne=mean(ndvi_ne), 
    ndvi_nw=mean(ndvi_nw),
    ndvi_se=mean(ndvi_se),
    ndvi_sw=mean(ndvi_sw),
    precipitation_amt_mm=sum(precipitation_amt_mm),
    reanalysis_air_temp_k=mean(reanalysis_air_temp_k),
    reanalysis_avg_temp_k=mean(reanalysis_avg_temp_k),
    reanalysis_dew_point_temp_k=mean(reanalysis_dew_point_temp_k),
    reanalysis_max_air_temp_k=mean(reanalysis_max_air_temp_k),
    reanalysis_min_air_temp_k=mean(reanalysis_min_air_temp_k),
    reanalysis_precip_amt_kg_per_m2=sum(reanalysis_precip_amt_kg_per_m2),
    reanalysis_relative_humidity_percent=mean(reanalysis_relative_humidity_percent),
    reanalysis_sat_precip_amt_mm=sum(reanalysis_sat_precip_amt_mm),
    reanalysis_specific_humidity_g_per_kg=mean(reanalysis_specific_humidity_g_per_kg),
    reanalysis_tdtr_k=sum(reanalysis_tdtr_k),
    station_avg_temp_c=mean(station_avg_temp_c),
    station_diur_temp_rng_c=mean(station_diur_temp_rng_c),
    station_max_temp_c=mean(station_max_temp_c),
    station_min_temp_c=mean(station_min_temp_c),
    station_precip_mm=sum(station_precip_mm),
    ttl_cases=sum(total_cases)
  )

df_xreg_by_mo <- data.frame(xreg_by_mo)
str(df_xreg_by_mo)


#Iquitos correlations
corr_mtrx_iq <- cor(   subset(df_xreg_by_mo, city=='iq') %>% dplyr::select(c(-city, -year, -mo))  , use = 'complete.obs'  )
corrplot(corr_mtrx_iq)
#total cases is not strongly correlated with anything, and largest abs(corr) are all negative.
#   temp vars are negatively correlated
#   reanalysis_dew_point_temp_k, reanalysis_min_air_temp_k, reanalysis_specific_humidity_g_per_kg,station_min_temp_c 

#rank the correlations in order of magnitude desc
df_corr_iq <- data.frame(corrln_w_ttl_cases=corr_mtrx_iq[,'ttl_cases'])
df_corr_iq$metricnms <- row.names(df_corr_iq)
df_corr_iq <- df_corr_iq %>% select(metricnms, corrln_w_ttl_cases) %>% filter(metricnms!='ttl_cases') %>%
  arrange(desc(abs(corrln_w_ttl_cases)))



corr_mtrx_sj <- cor(   subset(df_xreg_by_mo, city=='sj') %>% dplyr::select(c(-city, -year, -mo))  , use = 'complete.obs'  )
corrplot(corr_mtrx_sj)
#corr_mtrx_sj[,'ttl_cases']
#rank the correlations in order of magnitude desc
df_corr_sj <- data.frame(corrln_w_ttl_cases=corr_mtrx_sj[,'ttl_cases'])
df_corr_sj$metricnms <- row.names(df_corr_sj)
df_corr_sj <- df_corr_sj %>% select(metricnms, corrln_w_ttl_cases) %>% filter(metricnms!='ttl_cases') %>%
  arrange(desc(abs(corrln_w_ttl_cases)))


#San Juan correlations
corr_mtrx_iq_wkly <- cor(   subset(df_train_raw, city=='iq') %>% dplyr::select(c(-city, -year, -weekofyear, -week_start_date))  , use = 'complete.obs'  )
corrplot(corr_mtrx_iq_wkly)
corr_mtrx_iq_wkly[,'total_cases']
#weekly corr is weaker than mo corr

df_corr_iq_wkly <- data.frame(corrln_w_ttl_cases=corr_mtrx_iq_wkly[,'total_cases'])
df_corr_iq_wkly$metricnms <- row.names(df_corr_iq_wkly)
df_corr_iq_wkly <- df_corr_iq_wkly %>% select(metricnms, corrln_w_ttl_cases) %>% filter(metricnms!='total_cases') %>%
  arrange(desc(abs(corrln_w_ttl_cases)))



corr_mtrx_sj_wkly <- cor(   subset(df_train_raw, city=='sj') %>% dplyr::select(c(-city, -year, -weekofyear, -week_start_date))  , use = 'complete.obs'  )
corrplot(corr_mtrx_sj_wkly)
#corr_mtrx_sj_wkly[,'total_cases']

df_corr_sj_wkly <- data.frame(corrln_w_ttl_cases=corr_mtrx_sj_wkly[,'total_cases'])
df_corr_sj_wkly$metricnms <- row.names(df_corr_sj_wkly)
df_corr_sj_wkly <- df_corr_sj_wkly %>% select(metricnms, corrln_w_ttl_cases) %>% filter(metricnms!='total_cases') %>%
  arrange(desc(abs(corrln_w_ttl_cases)))







# look at correlations between the lags of the predictors and ttl cases
# ---------------------------------------------------------------------
# on a monthly basis     seems like 2-3 mos MAX would be sufficient, as mosquitos life cycle is ~2wks and they start to feed in ~28hrs IIRC and the disease
# becomes apparent within days after infection


xreg_by_mo_lags_iq <- xreg_by_mo %>% filter(city=='iq') %>% arrange(city, year, mo) %>% data.frame() %>%
  mutate(
    ndvi_ne_l1=lag(ndvi_ne, n=1),
    ndvi_nw_l1=lag(ndvi_nw, n=1),
    ndvi_se_l1=lag(ndvi_se, n=1),
    ndvi_sw_l1=lag(ndvi_sw, n=1),
    precipitation_amt_mm_l1=lag(precipitation_amt_mm, n=1),
    reanalysis_air_temp_k_l1=lag(reanalysis_air_temp_k, n=1),
    reanalysis_avg_temp_k_l1=lag(reanalysis_avg_temp_k, n=1),
    reanalysis_dew_point_temp_k_l1=lag(reanalysis_dew_point_temp_k, n=1),
    reanalysis_max_air_temp_k_l1=lag(reanalysis_max_air_temp_k, n=1),
    reanalysis_min_air_temp_k_l1=lag(reanalysis_min_air_temp_k, n=1),
    reanalysis_precip_amt_kg_per_m2_l1=lag(reanalysis_precip_amt_kg_per_m2, n=1),
    reanalysis_relative_humidity_percent_l1=lag(reanalysis_relative_humidity_percent, n=1),
    reanalysis_sat_precip_amt_mm_l1=lag(reanalysis_sat_precip_amt_mm, n=1),
    reanalysis_specific_humidity_g_per_kg_l1=lag(reanalysis_specific_humidity_g_per_kg, n=1),
    reanalysis_tdtr_k_l1=lag(reanalysis_tdtr_k, n=1),
    station_avg_temp_c_l1=lag(station_avg_temp_c, n=1),
    station_diur_temp_rng_c_l1=lag(station_diur_temp_rng_c, n=1),
    station_max_temp_c_l1=lag(station_max_temp_c, n=1),
    station_min_temp_c_l1=lag(station_min_temp_c, n=1),
    station_precip_mm_l1=lag(station_precip_mm, n=1),
    
    ndvi_ne_l2=lag(ndvi_ne, n=2),
    ndvi_nw_l2=lag(ndvi_nw, n=2),
    ndvi_se_l2=lag(ndvi_se, n=2),
    ndvi_sw_l2=lag(ndvi_sw, n=2),
    precipitation_amt_mm_l2=lag(precipitation_amt_mm, n=2),
    reanalysis_air_temp_k_l2=lag(reanalysis_air_temp_k, n=2),
    reanalysis_avg_temp_k_l2=lag(reanalysis_avg_temp_k, n=2),
    reanalysis_dew_point_temp_k_l2=lag(reanalysis_dew_point_temp_k, n=2),
    reanalysis_max_air_temp_k_l2=lag(reanalysis_max_air_temp_k, n=2),
    reanalysis_min_air_temp_k_l2=lag(reanalysis_min_air_temp_k, n=2),
    reanalysis_precip_amt_kg_per_m2_l2=lag(reanalysis_precip_amt_kg_per_m2, n=2),
    reanalysis_relative_humidity_percent_l2=lag(reanalysis_relative_humidity_percent, n=2),
    reanalysis_sat_precip_amt_mm_l2=lag(reanalysis_sat_precip_amt_mm, n=2),
    reanalysis_specific_humidity_g_per_kg_l2=lag(reanalysis_specific_humidity_g_per_kg, n=2),
    reanalysis_tdtr_k_l2=lag(reanalysis_tdtr_k, n=2),
    station_avg_temp_c_l2=lag(station_avg_temp_c, n=2),
    station_diur_temp_rng_c_l2=lag(station_diur_temp_rng_c, n=2),
    station_max_temp_c_l2=lag(station_max_temp_c, n=2),
    station_min_temp_c_l2=lag(station_min_temp_c, n=2),
    station_precip_mm_l2=lag(station_precip_mm, n=2)
  )


#look at the correlations w/ predictors lagged by 1 vs ttl cases
corr_mtrx_iq_l1 <- cor(subset(xreg_by_mo_lags_iq, 
                              select=c(ttl_cases, ndvi_ne_l1, ndvi_nw_l1, ndvi_se_l1, ndvi_sw_l1, precipitation_amt_mm_l1, reanalysis_air_temp_k_l1, reanalysis_avg_temp_k_l1, reanalysis_dew_point_temp_k_l1, reanalysis_max_air_temp_k_l1, reanalysis_min_air_temp_k_l1, reanalysis_precip_amt_kg_per_m2_l1, reanalysis_relative_humidity_percent_l1, reanalysis_sat_precip_amt_mm_l1, reanalysis_specific_humidity_g_per_kg_l1, reanalysis_tdtr_k_l1, station_avg_temp_c_l1, station_diur_temp_rng_c_l1, station_max_temp_c_l1, station_min_temp_c_l1, station_precip_mm_l1)),
                       use = "complete.obs" 
)
corrplot(corr_mtrx_iq_l1)
#max(abs(corr_mtrx_iq_l1[1,2:21]))  #0.31 is max abs correlation


#rank the correlations in order of magnitude desc
df_corr_iq_l1 <- data.frame(corrln_w_ttl_cases=corr_mtrx_iq_l1[,'ttl_cases'])
df_corr_iq_l1$metricnms <- row.names(df_corr_iq_l1)
df_corr_iq_l1 <- df_corr_iq_l1 %>% select(metricnms, corrln_w_ttl_cases) %>% filter(metricnms!='ttl_cases') %>%
  arrange(desc(abs(corrln_w_ttl_cases)))
df_corr_iq_l1

#look at the correlations w/ predictors lagged by 1 vs ttl cases
corr_mtrx_iq_l2 <- cor(subset(xreg_by_mo_lags_iq, 
                              select=c(ttl_cases, ndvi_ne_l2, ndvi_nw_l2, ndvi_se_l2, ndvi_sw_l2, precipitation_amt_mm_l2, reanalysis_air_temp_k_l2, reanalysis_avg_temp_k_l2, reanalysis_dew_point_temp_k_l2, reanalysis_max_air_temp_k_l2, reanalysis_min_air_temp_k_l2, reanalysis_precip_amt_kg_per_m2_l2, reanalysis_relative_humidity_percent_l2, reanalysis_sat_precip_amt_mm_l2, reanalysis_specific_humidity_g_per_kg_l2, reanalysis_tdtr_k_l2, station_avg_temp_c_l2, station_diur_temp_rng_c_l2, station_max_temp_c_l2, station_min_temp_c_l2, station_precip_mm_l2)),
                       use = "complete.obs" 
)
corrplot(corr_mtrx_iq_l2)


#rank the correlations in order of magnitude desc
df_corr_iq_l2 <- data.frame(corrln_w_ttl_cases=corr_mtrx_iq_l2[,'ttl_cases'])
df_corr_iq_l2$metricnms <- row.names(df_corr_iq_l2)
df_corr_iq_l2 <- df_corr_iq_l2 %>% select(metricnms, corrln_w_ttl_cases) %>% filter(metricnms!='ttl_cases') %>%
  arrange(desc(abs(corrln_w_ttl_cases)))
df_corr_iq_l2


# ggPacf(subset(xreg_by_mo, city=='iq')$reanalysis_min_air_temp_k)
# ggPacf(subset(xreg_by_mo, city=='iq')$reanalysis_specific_humidity_g_per_kg)
# ggPacf(subset(xreg_by_mo, city=='iq')$reanalysis_dew_point_temp_k)
# 
# ggPacf(xreg_by_mo_lags_iq$reanalysis_min_air_temp_k)
# ggPacf(xreg_by_mo_lags_iq$reanalysis_specific_humidity_g_per_kg)
# ggPacf(xreg_by_mo_lags_iq$reanalysis_dew_point_temp_k)
# 
# ggPacf(diff(subset(xreg_by_mo, city=='iq')$reanalysis_min_air_temp_k, lag=1))
# ggPacf(diff(subset(xreg_by_mo, city=='iq')$reanalysis_specific_humidity_g_per_kg, lag=1))
# ggPacf(diff(subset(xreg_by_mo, city=='iq')$reanalysis_dew_point_temp_k, lag=1))






#loop over the predictors and check out the relationship visually with time series plots
prdctrs <- dimnames(corr_mtrx_iq)[1][[1]][1:20]


#loop over the predictors, print scatterplots of total cases vs predictor
#     check for linear, quadratic, ... 
#look at both cities simultaneously 
for (prdctr in prdctrs)
{
  plt1 <- ggplot(subset(df_train_raw), aes_string(x=prdctr, y='total_cases', group='city')) +
    geom_point(aes(color=city)) + ggtitle(paste('Total cases vs',prdctr))
  print(plt1)
}
#look at IQ by itself
for (prdctr in prdctrs)
{
  plt1 <- ggplot(subset(df_train_raw, city=='iq', select=c('total_cases', prdctr)) %>% na.omit(), 
                 aes_string(x=prdctr, y='total_cases')) +
    geom_point() + ggtitle(paste('Iquitos:  Total cases vs',prdctr)) + xlab(prdctr)
  print(plt1)
}
#look at San Juan by itself
for (prdctr in prdctrs)
{
  plt1 <- ggplot(subset(df_train_raw, city=='sj', select=c('total_cases', prdctr)) %>% na.omit(), 
                 aes_string(x=prdctr, y='total_cases')) +
    geom_point() + ggtitle(paste('San Juan:  Total cases vs',prdctr)) + xlab(prdctr)
  print(plt1)
}

na.omi
# warnings()

# reshape2::melt(subset(df_train_raw, city=='sj'), id.vars=c('city', 'year', 'week_start_date'))


#look at trends over time - any lagged correlations visible? --WEEKLY
#for city=IQ
for (prdctr in prdctrs)
{
  plt1 <- ggplot(subset(df_train_raw, city=='iq')) +
    geom_line(aes_string(x='week_start_date', y=paste('na.interp(', prdctr, ')')), color='red') +
    geom_line(aes_string(x='week_start_date', y='total_cases')) + 
    ggtitle(paste('IQ:  Total cases and ', prdctr, ' over time'))
  print(plt1)
}
#for SJ
for (prdctr in prdctrs)
{
  plt1 <- ggplot(subset(df_train_raw, city=='sj')) +
    geom_line(aes_string(x='week_start_date', y=prdctr), color='red') +
    geom_line(aes_string(x='week_start_date', y='total_cases')) + 
    ggtitle(paste('SJ:  Total cases and ', prdctr, ' over time'))
  print(plt1)
}


#look at monthly values, predicted by lagged (preceding month's) avg/ttl values of predictors
xreg_by_mo_lags_iq$yr_and_mo <- xreg_by_mo_lags_iq$year + 1/12*(xreg_by_mo_lags_iq$mo-1)
for (prdctr in prdctrs)
{
  plt1 <- ggplot(subset(xreg_by_mo_lags_iq, city=='iq')) +
    geom_line(aes_string(x='yr_and_mo', y=paste('na.interp(', prdctr, ')')), color='red') +
    geom_line(aes_string(x='yr_and_mo', y='ttl_cases')) + 
    ggtitle(paste('IQ:  Total cases and ', prdctr, ' over time'))
  print(plt1)
}


#head(xreg_by_mo_lags_iq[,c('year', 'mo', 'yr_and_mo')])
# tsoutliers()
# na.interp
# tsclean()




#clean up outliers and missing values
df_train_iq <- df_train_raw %>% filter(city=='iq') %>% arrange(week_start_date) 


#clean up missing values and outliers       # toString(colnames(df_train_iq))
df_train_iq$ndvi_ne <- tsclean(ts(df_train_iq$ndvi_ne, start=c(2000,26), frequency=52))
df_train_iq$ndvi_nw <- tsclean(ts(df_train_iq$ndvi_nw, start=c(2000,26), frequency=52))
df_train_iq$ndvi_se <- tsclean(ts(df_train_iq$ndvi_se, start=c(2000,26), frequency=52))
df_train_iq$ndvi_sw <- tsclean(ts(df_train_iq$ndvi_sw, start=c(2000,26), frequency=52))
df_train_iq$precipitation_amt_mm <- tsclean(ts(df_train_iq$precipitation_amt_mm, start=c(2000,26), frequency=52))
df_train_iq$reanalysis_air_temp_k <- tsclean(ts(df_train_iq$reanalysis_air_temp_k, start=c(2000,26), frequency=52))
df_train_iq$reanalysis_avg_temp_k <- tsclean(ts(df_train_iq$reanalysis_avg_temp_k, start=c(2000,26), frequency=52))
df_train_iq$reanalysis_dew_point_temp_k <- tsclean(ts(df_train_iq$reanalysis_dew_point_temp_k, start=c(2000,26), frequency=52))
df_train_iq$reanalysis_max_air_temp_k <- tsclean(ts(df_train_iq$reanalysis_max_air_temp_k, start=c(2000,26), frequency=52))
df_train_iq$reanalysis_min_air_temp_k <- tsclean(ts(df_train_iq$reanalysis_min_air_temp_k, start=c(2000,26), frequency=52))
df_train_iq$reanalysis_precip_amt_kg_per_m2 <- tsclean(ts(df_train_iq$reanalysis_precip_amt_kg_per_m2, start=c(2000,26), frequency=52))
df_train_iq$reanalysis_relative_humidity_percent <- tsclean(ts(df_train_iq$reanalysis_relative_humidity_percent, start=c(2000,26), frequency=52))
df_train_iq$reanalysis_sat_precip_amt_mm <- tsclean(ts(df_train_iq$reanalysis_sat_precip_amt_mm, start=c(2000,26), frequency=52))
df_train_iq$reanalysis_specific_humidity_g_per_kg <- tsclean(ts(df_train_iq$reanalysis_specific_humidity_g_per_kg, start=c(2000,26), frequency=52))
df_train_iq$reanalysis_tdtr_k <- tsclean(ts(df_train_iq$reanalysis_tdtr_k, start=c(2000,26), frequency=52))
df_train_iq$station_avg_temp_c <- tsclean(ts(df_train_iq$station_avg_temp_c, start=c(2000,26), frequency=52))
df_train_iq$station_diur_temp_rng_c <- tsclean(ts(df_train_iq$station_diur_temp_rng_c, start=c(2000,26), frequency=52))
df_train_iq$station_max_temp_c <- tsclean(ts(df_train_iq$station_max_temp_c, start=c(2000,26), frequency=52))
df_train_iq$station_min_temp_c <- tsclean(ts(df_train_iq$station_min_temp_c, start=c(2000,26), frequency=52))
df_train_iq$station_precip_mm <- tsclean(ts(df_train_iq$station_precip_mm, start=c(2000,26), frequency=52))

#add moving avgs
df_train_iq$ndvi_ne_4wkRA <-stats::filter(df_train_iq$ndvi_ne,rep(1/4,4), sides=1)
df_train_iq$ndvi_nw_4wkRA <-stats::filter(df_train_iq$ndvi_nw,rep(1/4,4), sides=1)
df_train_iq$ndvi_se_4wkRA <-stats::filter(df_train_iq$ndvi_se,rep(1/4,4), sides=1)
df_train_iq$ndvi_sw_4wkRA <-stats::filter(df_train_iq$ndvi_sw,rep(1/4,4), sides=1)
df_train_iq$precipitation_amt_mm_4wkRA <-stats::filter(df_train_iq$precipitation_amt_mm,rep(1/4,4), sides=1)
df_train_iq$reanalysis_air_temp_k_4wkRA <-stats::filter(df_train_iq$reanalysis_air_temp_k,rep(1/4,4), sides=1)
df_train_iq$reanalysis_avg_temp_k_4wkRA <-stats::filter(df_train_iq$reanalysis_avg_temp_k,rep(1/4,4), sides=1)
df_train_iq$reanalysis_dew_point_temp_k_4wkRA <-stats::filter(df_train_iq$reanalysis_dew_point_temp_k,rep(1/4,4), sides=1)
df_train_iq$reanalysis_max_air_temp_k_4wkRA <-stats::filter(df_train_iq$reanalysis_max_air_temp_k,rep(1/4,4), sides=1)
df_train_iq$reanalysis_min_air_temp_k_4wkRA <-stats::filter(df_train_iq$reanalysis_min_air_temp_k,rep(1/4,4), sides=1)
df_train_iq$reanalysis_precip_amt_kg_per_m2_4wkRA <-stats::filter(df_train_iq$reanalysis_precip_amt_kg_per_m2,rep(1/4,4), sides=1)
df_train_iq$reanalysis_relative_humidity_percent_4wkRA <-stats::filter(df_train_iq$reanalysis_relative_humidity_percent,rep(1/4,4), sides=1)
df_train_iq$reanalysis_sat_precip_amt_mm_4wkRA <-stats::filter(df_train_iq$reanalysis_sat_precip_amt_mm,rep(1/4,4), sides=1)
df_train_iq$reanalysis_specific_humidity_g_per_kg_4wkRA <-stats::filter(df_train_iq$reanalysis_specific_humidity_g_per_kg,rep(1/4,4), sides=1)
df_train_iq$reanalysis_tdtr_k_4wkRA <-stats::filter(df_train_iq$reanalysis_tdtr_k,rep(1/4,4), sides=1)
df_train_iq$station_avg_temp_c_4wkRA <-stats::filter(df_train_iq$station_avg_temp_c,rep(1/4,4), sides=1)
df_train_iq$station_diur_temp_rng_c_4wkRA <-stats::filter(df_train_iq$station_diur_temp_rng_c,rep(1/4,4), sides=1)
df_train_iq$station_max_temp_c_4wkRA <-stats::filter(df_train_iq$station_max_temp_c,rep(1/4,4), sides=1)
df_train_iq$station_min_temp_c_4wkRA <-stats::filter(df_train_iq$station_min_temp_c,rep(1/4,4), sides=1)
df_train_iq$station_precip_mm_4wkRA <-stats::filter(df_train_iq$station_precip_mm,rep(1/4,4), sides=1)

str(df_train_iq)


ts_train_iq <- ts(df_train_iq, start=c(2000,26), frequency = 52) 
str(ts_train_iq)
head(ts_train_iq)



# install.packages('RccpRoll')
# RcppRoll::roll_mean
# rslt <- zoo:rollapply(as.zoo(ts_train_iq[, 'precipitation_amt_mm']), width = 4, FUN = mean, align='right', fill = 'extend')
# df_train_raw$precipitation_amt_mm_4wkRA <- apply(embed(df_train_raw$precipitation_amt_mm, 4), 1, mean)



df_train_sj <- df_train_raw %>% filter(city=='sj') %>% arrange(week_start_date) 

#clean up missing values and outliers       # toString(colnames(df_train_sj))
df_train_sj$ndvi_ne <- tsclean(ts(df_train_sj$ndvi_ne, start=c(1990, 18), frequency=52))
df_train_sj$ndvi_nw <- tsclean(ts(df_train_sj$ndvi_nw, start=c(1990, 18), frequency=52))
df_train_sj$ndvi_se <- tsclean(ts(df_train_sj$ndvi_se, start=c(1990, 18), frequency=52))
df_train_sj$ndvi_sw <- tsclean(ts(df_train_sj$ndvi_sw, start=c(1990, 18), frequency=52))
df_train_sj$precipitation_amt_mm <- tsclean(ts(df_train_sj$precipitation_amt_mm, start=c(1990, 18), frequency=52))
df_train_sj$reanalysis_air_temp_k <- tsclean(ts(df_train_sj$reanalysis_air_temp_k, start=c(1990, 18), frequency=52))
df_train_sj$reanalysis_avg_temp_k <- tsclean(ts(df_train_sj$reanalysis_avg_temp_k, start=c(1990, 18), frequency=52))
df_train_sj$reanalysis_dew_point_temp_k <- tsclean(ts(df_train_sj$reanalysis_dew_point_temp_k, start=c(1990, 18), frequency=52))
df_train_sj$reanalysis_max_air_temp_k <- tsclean(ts(df_train_sj$reanalysis_max_air_temp_k, start=c(1990, 18), frequency=52))
df_train_sj$reanalysis_min_air_temp_k <- tsclean(ts(df_train_sj$reanalysis_min_air_temp_k, start=c(1990, 18), frequency=52))
df_train_sj$reanalysis_precip_amt_kg_per_m2 <- tsclean(ts(df_train_sj$reanalysis_precip_amt_kg_per_m2, start=c(1990, 18), frequency=52))
df_train_sj$reanalysis_relative_humidity_percent <- tsclean(ts(df_train_sj$reanalysis_relative_humidity_percent, start=c(1990, 18), frequency=52))
df_train_sj$reanalysis_sat_precip_amt_mm <- tsclean(ts(df_train_sj$reanalysis_sat_precip_amt_mm, start=c(1990, 18), frequency=52))
df_train_sj$reanalysis_specific_humidity_g_per_kg <- tsclean(ts(df_train_sj$reanalysis_specific_humidity_g_per_kg, start=c(1990, 18), frequency=52))
df_train_sj$reanalysis_tdtr_k <- tsclean(ts(df_train_sj$reanalysis_tdtr_k, start=c(1990, 18), frequency=52))
df_train_sj$station_avg_temp_c <- tsclean(ts(df_train_sj$station_avg_temp_c, start=c(1990, 18), frequency=52))
df_train_sj$station_diur_temp_rng_c <- tsclean(ts(df_train_sj$station_diur_temp_rng_c, start=c(1990, 18), frequency=52))
df_train_sj$station_max_temp_c <- tsclean(ts(df_train_sj$station_max_temp_c, start=c(1990, 18), frequency=52))
df_train_sj$station_min_temp_c <- tsclean(ts(df_train_sj$station_min_temp_c, start=c(1990, 18), frequency=52))
df_train_sj$station_precip_mm <- tsclean(ts(df_train_sj$station_precip_mm, start=c(1990, 18), frequency=52))

#add moving avgs
df_train_sj$ndvi_ne_4wkRA <-stats::filter(df_train_sj$ndvi_ne,rep(1/4,4), sides=1)
df_train_sj$ndvi_nw_4wkRA <-stats::filter(df_train_sj$ndvi_nw,rep(1/4,4), sides=1)
df_train_sj$ndvi_se_4wkRA <-stats::filter(df_train_sj$ndvi_se,rep(1/4,4), sides=1)
df_train_sj$ndvi_sw_4wkRA <-stats::filter(df_train_sj$ndvi_sw,rep(1/4,4), sides=1)
df_train_sj$precipitation_amt_mm_4wkRA <-stats::filter(df_train_sj$precipitation_amt_mm,rep(1/4,4), sides=1)
df_train_sj$reanalysis_air_temp_k_4wkRA <-stats::filter(df_train_sj$reanalysis_air_temp_k,rep(1/4,4), sides=1)
df_train_sj$reanalysis_avg_temp_k_4wkRA <-stats::filter(df_train_sj$reanalysis_avg_temp_k,rep(1/4,4), sides=1)
df_train_sj$reanalysis_dew_point_temp_k_4wkRA <-stats::filter(df_train_sj$reanalysis_dew_point_temp_k,rep(1/4,4), sides=1)
df_train_sj$reanalysis_max_air_temp_k_4wkRA <-stats::filter(df_train_sj$reanalysis_max_air_temp_k,rep(1/4,4), sides=1)
df_train_sj$reanalysis_min_air_temp_k_4wkRA <-stats::filter(df_train_sj$reanalysis_min_air_temp_k,rep(1/4,4), sides=1)
df_train_sj$reanalysis_precip_amt_kg_per_m2_4wkRA <-stats::filter(df_train_sj$reanalysis_precip_amt_kg_per_m2,rep(1/4,4), sides=1)
df_train_sj$reanalysis_relative_humidity_percent_4wkRA <-stats::filter(df_train_sj$reanalysis_relative_humidity_percent,rep(1/4,4), sides=1)
df_train_sj$reanalysis_sat_precip_amt_mm_4wkRA <-stats::filter(df_train_sj$reanalysis_sat_precip_amt_mm,rep(1/4,4), sides=1)
df_train_sj$reanalysis_specific_humidity_g_per_kg_4wkRA <-stats::filter(df_train_sj$reanalysis_specific_humidity_g_per_kg,rep(1/4,4), sides=1)
df_train_sj$reanalysis_tdtr_k_4wkRA <-stats::filter(df_train_sj$reanalysis_tdtr_k,rep(1/4,4), sides=1)
df_train_sj$station_avg_temp_c_4wkRA <-stats::filter(df_train_sj$station_avg_temp_c,rep(1/4,4), sides=1)
df_train_sj$station_diur_temp_rng_c_4wkRA <-stats::filter(df_train_sj$station_diur_temp_rng_c,rep(1/4,4), sides=1)
df_train_sj$station_max_temp_c_4wkRA <-stats::filter(df_train_sj$station_max_temp_c,rep(1/4,4), sides=1)
df_train_sj$station_min_temp_c_4wkRA <-stats::filter(df_train_sj$station_min_temp_c,rep(1/4,4), sides=1)
df_train_sj$station_precip_mm_4wkRA <-stats::filter(df_train_sj$station_precip_mm,rep(1/4,4), sides=1)

str(df_train_sj)


ts_train_sj <- ts(df_train_sj, start=c(1990, 18), frequency = 52) 
str(ts_train_sj)
head(ts_train_sj)









# split the data into test and eval set
# =====================================
# nrow(subset(df_train_features_raw, city=='iq'))  #520 == 10 "yrs" at 52wk/yr
# nrow(subset(df_train_features_raw, city=='sj'))  #936 == 18 "yrs" at 52wk/yr
# ==>  hold out 2 yrs of data for eval set on sj, and 2 yr on iq
#their patterns look very similar, so hopefully we will get consistent messages and the same model will
#be preferable for both of them

# autoplot(window(ts_train_iq[, 'total_cases'], start=c(2001,50)))
# length(window(ts_train_iq[, 'total_cases'], end=c(2001,50)))  #77

#iq:   2000.26    2010.25*      2000-07-01     2010-06-25   
# --is 2010.25 when sorting by date (vs. 2010.53 when doing max; for some reason there's a wk 53 at the START of 2010)
#sj:   1990.18    2008.17     1990-04-30    2008-04-22


#split iq series
ts_iq_train <- window(ts_train_iq, start=c(2001,50), end=c(2008,25))   
#trim off the initial part because it looks way too flatlined.  Not diagnosed, not reported, data tracking issues, etc - doesn't 
# seem very unlikely it would have been zero most weeks and max of ONE the other weeks when in 2002 it had dozens of cases

#looks confusing when you look at the weekyr in the time series (ends INCLUDING 26 of 2008) b/c the #s are off from how the ts index thinks about them
#I think it's ok b/c we used the same logic to start the eval series that we used to end the train series
ts_iq_eval <- window(ts_train_iq, start=c(2008,26))
autoplot(ts_iq_train[, 'total_cases']) + autolayer(ts_iq_eval[, 'total_cases'], series='Eval set')


#split sj series
# 0.327*52+1  #18th pt  (17/52)
ts_sj_train <- window(ts_train_sj, end=c(2006, 17))   
ts_sj_eval <- window(ts_train_sj, start=c(2006,18))  #has 104 rows, as desired
autoplot(ts_sj_train[, 'total_cases']) + autolayer(ts_sj_eval[, 'total_cases'], series='Eval set')




#A.  create a collection of models
# ===================================================
#         San Juan
# ===================================================

#ets      #didn't like frequency over 24

mdlfit_sj_stlf <- stlf(ts_sj_train[, 'total_cases'], h=104)
checkresiduals(mdlfit_sj_stlf)
fcst_sj_stlf <- forecast(mdlfit_sj_stlf, h=104)
# summary(fcst_sj_stlf)
# fcst_sj_stlf$mean

# autoplot(ts_sj_eval[, 'total_cases'])
autoplot(fcst_sj_stlf) + 
  autolayer(fcst_sj_stlf$fitted, series='Fitted vals', color='red') + 
  autolayer(ts_sj_eval[,'total_cases'], series='Actuals', color='purple') +
  theme(legend.position = 'right')

autoplot(fcst_sj_stlf$fitted, series='Fitted vals', color='red') + 
  autolayer(fcst_sj_stlf$mean) + 
  autolayer(ts_sj_eval[,'total_cases'], series='Actuals', color='purple')

accuracy(fcst_sj_stlf, ts_sj_eval[,'total_cases'])    

Box.test(fcst_sj_stlf$residuals, type='Ljung-Box', lag=104)
#testing independence (vs autocorrelation) of time series - null hypothesis is that they are independent,
#   so we fail to reject if the p value is GREATER than 0.05.  Reject null hypothesis if p<0.05 ==> autocorrelated
#pretty sure for a series of residuals, you want to test lags up to the length of the residual series
#  pretty sure that this is cumulative, and is called a portmanteau test b/c you are testing that ALL the lags between 1 and LAG= param are independent

# min(fcst_sj_stlf$mean) #3.64 - they are all above 0

#arima
mdlfit_sj_autoarima <- auto.arima(ts_sj_train[, 'total_cases'])
checkresiduals(mdlfit_sj_autoarima)
#this recreates the result above.  Note the use of Box.test(fcst_sj_autoarima$residuals, type='Ljung-Box', lag = 104, fitdf = 3)

fcst_sj_autoarima <- forecast(mdlfit_sj_autoarima, h=104)
summary(fcst_sj_autoarima)

autoplot(fcst_sj_autoarima) + 
  autolayer(fcst_sj_autoarima$fitted, series='Fitted vals', color='purple') + 
  autolayer(ts_sj_eval[,'total_cases'], series='Actuals', color='red') +
  theme(legend.position = 'right')

accuracy(fcst_sj_autoarima, ts_sj_eval[,'total_cases'])    




#neural net
mdlfit_sj_nn <- nnetar(ts_sj_train[, 'total_cases'])
checkresiduals(mdlfit_sj_nn)

fcst_sj_nn <- forecast(mdlfit_sj_nn, h=104)
summary(fcst_sj_nn)

autoplot(fcst_sj_nn) + 
  autolayer(fcst_sj_nn$fitted, series='Fitted vals', color='purple') + 
  autolayer(ts_sj_eval[,'total_cases'], series='Actuals', color='red') +
  theme(legend.position = 'right')

accuracy(fcst_sj_nn, ts_sj_eval[,'total_cases'])    


#tried BoxCox transform - errors out.  Not sure why that's a big help anyway as the series isn't increasing over time and doesn't show increasing variance
# mdlfit_sj_nn_bc <- nnetar(ts_sj_train[, 'total_cases'], lambda='auto')
# checkresiduals(mdlfit_sj_nn_bc)
# 
# fcst_sj_nn_bc <- forecast(mdlfit_sj_nn_bc, h=104)
# # summary(fcst_sj_nn)
# 
# autoplot(fcst_sj_nn_bc) + 
#   autolayer(fcst_sj_nn_bc$fitted, series='Fitted vals', color='purple') + 
#   autolayer(ts_sj_eval[,'total_cases'], series='Actuals', color='red') +
#   theme(legend.position = 'right')


#   #try var  
# VARselect(ts_iq_train[, 'total_cases'])
#   
# unit_root_test__iq <- ur.kpss(ts_iq_train[, 'total_cases'])
# summary(unit_root_test__iq)
# # Value of test-statistic is: 0.08 
# # 
# # Critical value for a significance level of: 
# #   10pct  5pct 2.5pct  1pct
# # critical values 0.347 0.463  0.574 0.739
#   # ==> FAIL to reject the null (stationarity)  ==> DO NOT need to take a diff
# 
# 
# unit_root_test__sj <- ur.kpss(ts_sj_train[, 'total_cases'])
# summary(unit_root_test__sj)
# 
# 
# ts_iq_train_diffs <- diff(ts_iq_train[,'total_cases'])
# ts_sj_train_diffs <- diff(ts_sj_train[,'total_cases'])
# 
# head(ts_iq_train_diffs)
# 
# cmbnd_diffs_var <- cbind(  iq_cases=window(ts_iq_train_diffs, end=c(2006,17)),   sj_cases=window(ts_sj_train_diffs, start=c(2001, 51))   )
# str(cmbnd_diffs_var)
# head(cmbnd_diffs_var)
# tail(cmbnd_diffs_var)
# 
# 
# mdlft_var <- vars::VAR(cmbnd_diffs_var[,1:2], p=1, type="const")
# mdlft_var
# 
# str(mdlft_var)
# 
# 
# serial.test(mdlft_var, lags.pt=10, type="PT.asymptotic")
# 
# 
# fcst_var <- forecast(mdlft_var, h=52)
# autoplot(fcst_var, main='VAR(1) forecasts')
# 


#tbats
#algorithm for multiple seasonality?  or changing seasonality?

#b) snaive

# gradient boosting



# ==========================================
#    IQ:   create models
# ==========================================
#A.  
#ets -  didn't like frequency over 24

#stlf
mdlfit_iq_stlf <- stlf(ts_iq_train[, 'total_cases'], h=104)
checkresiduals(mdlfit_iq_stlf)
fcst_iq_stlf <- forecast(mdlfit_iq_stlf, h=104)

# summary(fcst_iq_stlf)
accuracy(fcst_iq_stlf, ts_iq_eval[,'total_cases'])    

autoplot(fcst_iq_stlf, PI=FALSE) + 
  autolayer(fcst_iq_stlf$fitted, series='Fitted vals', color='red') + 
  autolayer(ts_iq_eval[,'total_cases'], series='Actuals', color='purple') +
  theme(legend.position = 'right')



Box.test(fcst_iq_stlf$residuals, type='Ljung-Box', lag=104)
#testing independence (vs autocorrelation) of time series - null hypothesis is that they are independent,
#   so we fail to reject if the p value is GREATER than 0.05.  Reject null hypothesis if p<0.05 ==> autocorrelated
#pretty sure for a series of residuals, you want to test lags up to the length of the residual series
#  pretty sure that this is cumulative, and is called a portmanteau test b/c you are testing that ALL the lags between 1 and LAG= param are independent

#arima
mdlfit_iq_autoarima <- auto.arima(ts_iq_train[, 'total_cases'])
checkresiduals(mdlfit_iq_autoarima)

fcst_iq_autoarima <- forecast(mdlfit_iq_autoarima, h=104)
accuracy(fcst_iq_autoarima, ts_iq_eval[,'total_cases'])    

autoplot(fcst_iq_autoarima, PI=FALSE) + 
  autolayer(fcst_iq_autoarima$fitted, series='Fitted vals', color='purple') + 
  autolayer(ts_iq_eval[,'total_cases'], series='Actuals', color='red') +
  theme(legend.position = 'right')


#neural net
mdlfit_iq_nn <- nnetar(ts_iq_train[, 'total_cases'])
checkresiduals(mdlfit_iq_nn)

fcst_iq_nn <- forecast(mdlfit_iq_nn, h=104)
accuracy(fcst_iq_nn, ts_iq_eval[,'total_cases'])    

autoplot(fcst_iq_nn) + 
  autolayer(fcst_iq_nn$fitted, series='Fitted vals', color='purple') + 
  autolayer(ts_iq_eval[,'total_cases'], series='Actuals', color='red') +
  theme(legend.position = 'right')


#with xreg
#---------


#we trimmed some periods off the start of the iq series b/c they appeared to be uninformative.  
#also set aside the last 2 yrs for eval purposes      do the same for the regressors
ts_xreg_iq_train <- window(ts_xreg_iq, start=c(2001, 50), end=c(2008,25))
ts_xreg_iq_eval <- window(ts_xreg_iq, start=c(2008, 26), end=c(2010,25))
# head(ts_iq_train)
# head(ts_xreg_iq_train)
# tail(ts_iq_train)
# tail(ts_xreg_iq_train)
# ts_iq_eval


#stlf:   doesn't support xreg        

#arima
#-----
mdlfit_iq_autoarima_xr <- auto.arima(ts_iq_train[, 'total_cases'], xreg=ts_xreg_iq_train)
checkresiduals(mdlfit_iq_autoarima_xr)
mdlfit_iq_autoarima_xr    #has worse (higher) AIC, AICc, and BIC than model w/o xr
#mdlfit_iq_autoarima  

fcst_iq_autoarima_xr <- forecast(mdlfit_iq_autoarima_xr, xreg=ts_xreg_iq_eval, h=104)
accuracy(fcst_iq_autoarima_xr, ts_iq_eval[,'total_cases'])    
#summary(fcst_iq_autoarima)

autoplot(fcst_iq_autoarima_xr, PI=FALSE) + 
  autolayer(fcst_iq_autoarima_xr$fitted, series='Fitted vals', color='purple') + 
  autolayer(ts_iq_eval[,'total_cases'], series='Actuals', color='red') +
  theme(legend.position = 'right')


#neural net
#----------
mdlfit_iq_nn_xr <- nnetar(ts_iq_train[, 'total_cases'], xreg=ts_xreg_iq_train)
checkresiduals(mdlfit_iq_nn_xr)

fcst_iq_nn_xr <- forecast(mdlfit_iq_nn_xr, xreg=ts_xreg_iq_eval, h=104)
#summary(fcst_iq_nn_xr)
accuracy(fcst_iq_nn_xr, ts_iq_eval[,'total_cases'])    

autoplot(fcst_iq_nn_xr) + 
  autolayer(fcst_iq_nn_xr$fitted, series='Fitted vals', color='purple') + 
  autolayer(ts_iq_eval[,'total_cases'], series='Actuals', color='red') +
  theme(legend.position = 'right')


# accuracy(fcst_iq_nn_xr, ts_iq_eval[,'total_cases'])    
# accuracy(fcst_iq_nn, ts_iq_eval[,'total_cases'])    
# accuracy(fcst_iq_autoarima, ts_iq_eval[,'total_cases'])    
# accuracy(fcst_iq_autoarima_xr, ts_iq_eval[,'total_cases'])    
# accuracy(fcst_iq_stlf, ts_iq_eval[,'total_cases'])    


#looks like there might be benefit in ensembling the autoarima and neural net results



# ===================        
#    SJ, with xreg
# ===================        
df_corr_iq_l2


xreg_colllist <- c('')    
ndvi_ne, ndvi_nw, ndvi_se, ndvi_sw, precipitation_amt_mm, reanalysis_air_temp_k, reanalysis_avg_temp_k, reanalysis_dew_point_temp_k, reanalysis_max_air_temp_k, reanalysis_min_air_temp_k, reanalysis_precip_amt_kg_per_m2, reanalysis_relative_humidity_percent, reanalysis_sat_precip_amt_mm, reanalysis_specific_humidity_g_per_kg, reanalysis_tdtr_k, station_avg_temp_c, station_diur_temp_rng_c, station_max_temp_c, station_min_temp_c, station_precip_mm, total_cases, ndvi_ne_4wkRA, ndvi_nw_4wkRA, ndvi_se_4wkRA, ndvi_sw_4wkRA, precipitation_amt_mm_4wkRA, reanalysis_air_temp_k_4wkRA, reanalysis_avg_temp_k_4wkRA, reanalysis_dew_point_temp_k_4wkRA, reanalysis_max_air_temp_k_4wkRA, reanalysis_min_air_temp_k_4wkRA, reanalysis_precip_amt_kg_per_m2_4wkRA, reanalysis_relative_humidity_percent_4wkRA, reanalysis_sat_precip_amt_mm_4wkRA, reanalysis_specific_humidity_g_per_kg_4wkRA, reanalysis_tdtr_k_4wkRA, station_avg_temp_c_4wkRA, station_diur_temp_rng_c_4wkRA, station_max_temp_c_4wkRA, station_min_temp_c_4wkRA, station_precip_mm_4wkRA

toString(colnames(df_train_iq))
#we set aside the last 2 yrs for eval purposes      do the same for the regressors
ts_xreg_sj_train <- window(ts_sj_train, end=c(2006, 17))[,collist]
ts_xreg_sj_eval <- window(ts_xreg_sj, start=c(2006, 18), end=c(2008,17))





#stlf:   doesn't support xreg        
#arima
#-----
mdlfit_sj_autoarima_xr <- auto.arima(ts_sj_train[, 'total_cases'], xreg=ts_xreg_sj_train)
checkresiduals(mdlfit_sj_autoarima_xr)
mdlfit_sj_autoarima_xr    #has worse (higher) AIC, AICc, and BIC than model w/o xr
#mdlfit_sj_autoarima  

fcst_sj_autoarima_xr <- forecast(mdlfit_sj_autoarima_xr, xreg=ts_xreg_sj_eval, h=104)
accuracy(fcst_sj_autoarima_xr, ts_sj_eval[,'total_cases'])    
#summary(fcst_sj_autoarima)
# fcst_sj_autoarima$mean

autoplot(fcst_sj_autoarima_xr, PI=FALSE) + 
  autolayer(fcst_sj_autoarima_xr$fitted, series='Fitted vals', color='purple') + 
  autolayer(ts_sj_eval[,'total_cases'], series='Actuals', color='red') +
  theme(legend.position = 'right')


#neural net
#----------
mdlfit_sj_nn_xr <- nnetar(ts_sj_train[, 'total_cases'], xreg=ts_xreg_sj_train)
checkresiduals(mdlfit_sj_nn_xr)

fcst_sj_nn_xr <- forecast(mdlfit_sj_nn_xr, xreg=ts_xreg_sj_eval, h=104)
#summary(fcst_sj_nn_xr)
accuracy(fcst_sj_nn_xr, ts_sj_eval[,'total_cases'])    

autoplot(fcst_sj_nn_xr) + 
  autolayer(fcst_sj_nn_xr$fitted, series='Fitted vals', color='purple') + 
  autolayer(ts_sj_eval[,'total_cases'], series='Actuals', color='red') +
  theme(legend.position = 'right')


accuracy(fcst_sj_autoarima_xr, ts_sj_eval[,'total_cases'])    
accuracy(fcst_sj_autoarima, ts_sj_eval[,'total_cases'])    


accuracy(fcst_sj_nn_xr, ts_sj_eval[,'total_cases'])    
accuracy(fcst_sj_nn, ts_sj_eval[,'total_cases'])    

accuracy(fcst_sj_stlf, ts_sj_eval[,'total_cases'])    









# ====================================================
#submission 1 -  autoarima with xr for IQ, stlf for SJ
# ====================================================
mdlfit_iq_sub1 <- auto.arima(window(ts_iq_raw, start=c(2001,50))[, 'total_cases'], xreg=window(ts_xreg_iq, start=c(2001, 50)))
checkresiduals(mdlfit_iq_sub1)
mdlfit_iq_sub1
# mdlfit_iq_autoarima_xr  
# mdlfit_iq_autoarima  


#prep the external regressors during the test period
df_xreg_iq_test <- df_test_features_raw %>% filter(city=='iq') %>% arrange(week_start_date) %>%
  select(reanalysis_specific_humidity_g_per_kg, reanalysis_dew_point_temp_k, reanalysis_min_air_temp_k, station_min_temp_c, reanalysis_tdtr_k, reanalysis_relative_humidity_percent, station_avg_temp_c, ndvi_se, reanalysis_air_temp_k, reanalysis_precip_amt_kg_per_m2, ndvi_nw)

ts_xreg_iq_test <- ts(df_xreg_iq_test, start=c(2010,26), frequency = 52) 

nrow(df_xreg_iq_test)

#clean up missing values and outliers
ts_xreg_iq_test[, 'reanalysis_specific_humidity_g_per_kg'] <- tsclean(ts_xreg_iq_test[, 'reanalysis_specific_humidity_g_per_kg'])
ts_xreg_iq_test[, 'reanalysis_dew_point_temp_k'] <- tsclean(ts_xreg_iq_test[, 'reanalysis_dew_point_temp_k'])
ts_xreg_iq_test[, 'reanalysis_min_air_temp_k'] <- tsclean(ts_xreg_iq_test[, 'reanalysis_min_air_temp_k'])
ts_xreg_iq_test[, 'station_min_temp_c'] <- tsclean(ts_xreg_iq_test[, 'station_min_temp_c'])
ts_xreg_iq_test[, 'reanalysis_tdtr_k'] <- tsclean(ts_xreg_iq_test[, 'reanalysis_tdtr_k'])
ts_xreg_iq_test[, 'reanalysis_relative_humidity_percent'] <- tsclean(ts_xreg_iq_test[, 'reanalysis_relative_humidity_percent'])
ts_xreg_iq_test[, 'station_avg_temp_c'] <- tsclean(ts_xreg_iq_test[, 'station_avg_temp_c'])
ts_xreg_iq_test[, 'ndvi_se'] <- tsclean(ts_xreg_iq_test[, 'ndvi_se'])
ts_xreg_iq_test[, 'reanalysis_air_temp_k'] <- tsclean(ts_xreg_iq_test[, 'reanalysis_air_temp_k'])
ts_xreg_iq_test[, 'reanalysis_precip_amt_kg_per_m2'] <- tsclean(ts_xreg_iq_test[, 'reanalysis_precip_amt_kg_per_m2'])
ts_xreg_iq_test[, 'ndvi_nw'] <- tsclean(ts_xreg_iq_test[, 'ndvi_nw'])


fcst_iq_sub1 <- forecast(mdlfit_iq_sub1, xreg=ts_xreg_iq_test, h=156)
accuracy(fcst_iq_sub1)    

#accuracy(fcst_iq_autoarima_xr, ts_iq_eval[,'total_cases'])    
#summary(fcst_iq_autoarima)
# fcst_iq_autoarima$mean

autoplot(fcst_iq_sub1, PI=FALSE) + 
  autolayer(fcst_iq_sub1$fitted, series='Fitted vals', color='purple') + 
  theme(legend.position = 'right')


df_sub1_iq <- data.frame(iq=round(fcst_iq_sub1$mean))



#sum(df_test_features_raw$city=='sj')


mdlfit_sj_sub1 <- stlf(ts_sj_raw[, 'total_cases'], h=260)
checkresiduals(mdlfit_sj_sub1)
fcst_sj_sub1 <- forecast(mdlfit_sj_sub1, h=260)
accuracy(fcst_sj_sub1)    

autoplot(fcst_sj_sub1) + 
  autolayer(fcst_sj_sub1$fitted, series='Fitted vals', color='purple') + 
  theme(legend.position = 'right')


df_sub1_sj <- data.frame(sj=round(fcst_sj_sub1$mean))





# ==============================
# try a recurrent neural network
# ==============================
#https://blogs.rstudio.com/tensorflow/posts/2017-12-20-time-series-forecasting-with-recurrent-neural-networks/




# head(window(ts_xreg_iq, start=c(2001, 50)))
# head(ts_iq_train)
# 
# tail(window(ts_xreg_iq, start=c(2001, 50)))
# tail(ts_iq_train)


#prep the matrix of training data
#--------------------------------
rnn_data_iq_train <- data.matrix(cbind(data.frame(ts_iq_train), 
                                       data.frame(window(ts_xreg_iq, start=c(2001, 50), end=c(2008,25)))
)
)
rnn_data_iq_train <- rnn_data_iq_train[,-2]     #drop weekofyear as it doesn't seem logical in how it sequences
str(rnn_data_iq_train)



#prep the matrix of validation data
#--------------------------------
rnn_data_iq_eval <- data.matrix(cbind(data.frame(ts_iq_eval), 
                                      data.frame(window(ts_xreg_iq, start=c(2008, 26), end=c(2010,25)))
)
)
rnn_data_iq_eval <- rnn_data_iq_eval[,-2]    #drop weekofyear as it doesn't seem logical in how it sequences
str(rnn_data_iq_eval)




#standardize the data by subtracting the mean of each time series and dividing by the standard deviation. You’re going to use the first 200,000 timesteps as training data, so compute the mean and standard deviation for normalization only on this fraction of the data.
mean_iq <- apply(rnn_data_iq_train, 2, mean)
std_iq <- apply(rnn_data_iq_train, 2, sd)
rnn_data_iq_train <- scale(rnn_data_iq_train, center=mean_iq, scale=std_iq)
rnn_data_iq_eval <- scale(rnn_data_iq_eval, center=mean_iq, scale=std_iq)




#define a generator function which will return sequences of data
generator <- function(data, lookback, delay, min_index, max_index,
                      shuffle = FALSE, batch_size = 128, step = 6) {
  if (is.null(max_index))
    max_index <- nrow(data) - delay - 1
  i <- min_index + lookback
  function() {
    if (shuffle) {
      rows <- sample(c((min_index+lookback):max_index), size = batch_size)
    } else {
      if (i + batch_size >= max_index)
        i <<- min_index + lookback
      rows <- c(i:min(i+batch_size-1, max_index))
      i <<- i + length(rows)
    }
    
    samples <- array(0, dim = c(length(rows), 
                                lookback / step,
                                dim(data)[[-1]]))
    targets <- array(0, dim = c(length(rows)))
    
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]]-1, 
                     length.out = dim(samples)[[2]])
      samples[j,,] <- data[indices,]
      targets[[j]] <- data[rows[[j]] + delay,2]
    }            
    
    list(samples, targets)
  }
}

nrow(rnn_data_iq_eval)
rnn_data_iq <- rbind(rnn_data_iq_train, rnn_data_iq_eval)

lookback <- 52 # Observations (sequence length used for predicting) will go back 5 yrs for IQ -- seems akin to # of lags in time series 
steps <- 1     #Observations will be sampled every 1 (4=~1 per mo?  6?) data point
delay <- 4   #targets to predict will be the next 4 weeks in the future.
batch_size <- 24

#prep generator fns for training and eval
train_gen <- generator(
  rnn_data_iq,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = 340,
  shuffle = TRUE,
  step = steps, 
  batch_size = batch_size
)

val_gen <- generator(
  rnn_data_iq,
  lookback = lookback,
  delay = delay,
  min_index = 341,
  max_index = (444 - delay - 1),   
  step = steps,
  batch_size = batch_size
)


# How many steps to draw from val_gen in order to see the entire validation set
val_steps <- (104 - delay - lookback) / batch_size




evaluate_naive_method <- function() {
  batch_maes <- c()
  for (step in 1:val_steps) {
    c(samples, targets) %<-% val_gen()
    preds <- samples[,dim(samples)[[2]],2]
    mae <- mean(abs(preds - targets))
    batch_maes <- c(batch_maes, mae)
  }
  print(mean(batch_maes))
}

evaluate_naive_method()



#Model1 (starter):  flattened model, so info about sequence and order is NOT utilized
# model_iq <- keras_model_sequential() %>% 
#   layer_flatten(input_shape = c(lookback / steps, dim(rnn_data_iq)[-1])) %>% 
#   layer_dense(units = 32, activation = "relu") %>% 
#   layer_dense(units = 1)
# 
# model_iq %>% compile(
#   optimizer = optimizer_rmsprop(),
#   loss = "mae"
# )
# 
# history <- model_iq %>% fit_generator(
#   train_gen,
#   steps_per_epoch = 500,
#   epochs = 20,
#   validation_data = val_gen,
#   validation_steps = val_steps
# )



#Model 2:  recurrent neural network so that info about sequence and order is utilized - critical for time series
model_iq2 <- keras_model_sequential() %>% 
  layer_gru(units = 32, input_shape = list(NULL, dim(rnn_data_iq)[[-1]])) %>% 
  layer_dense(units = 1)

model_iq2 %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae"
)

history_iq2 <- model_iq2 %>% fit_generator(
  train_gen,
  steps_per_epoch = 500,
  epochs = 20,
  validation_data = val_gen,
  validation_steps = val_steps
)






#Model 3:   2-layer recurrent neural network, with dropout for regularization
model_iq3 <- keras_model_sequential() %>% 
  layer_gru(units = 32, 
            dropout = 0.1, 
            recurrent_dropout = 0.5,
            return_sequences = TRUE,
            input_shape = list(NULL, dim(rnn_data_iq)[[-1]])) %>% 
  layer_gru(units = 64, activation = "relu",
            dropout = 0.1,
            recurrent_dropout = 0.5) %>% 
  layer_dense(units = 1)

model_iq3 %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae"
)

history_iq3 <- model_iq3 %>% fit_generator(
  train_gen,
  steps_per_epoch = 500,
  epochs = 40,
  validation_data = val_gen,
  validation_steps = val_steps
)






































#c) ensemble these results





#evaluate on the eval set



#select the optimal model - score using MAPE b/c that's the contest's specified metric




#retrain with the entire training set of data



#forecast the test set of data



#upload predictions to site





