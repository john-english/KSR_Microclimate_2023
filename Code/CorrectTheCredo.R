library(here)
library(forecast)
library(ggplot2)
library(stringr)
library(data.table)

#wd = 'C:/Users/Nicholas/Desktop/data_macivor_csv_recovered/data_macivor_csv_recovered/'
#wd = 'C:/Users/joeng/Documents/University of Toronto/Project/KSR/RData/data_macivor_csv_recovered/john-english-KSR_Microcliate_2022'
#setwd(wd)

here::i_am("Data/RawSensorData")

path_input = paste0(wd,grep('raw_CS', dir(),value=T))

####################################################################################
# remove anomalous data points with ARIMA  
manage_dat = function(x){
  # set DT
  x$Date_Time = as.POSIXct(strptime(x$Date_Time, format='%Y-%m-%d %H:%M:%S'))
  x$Temperature_c = as.numeric(x$Temperature_c)
  # 12 hour shift
  x$dt = as.numeric(x$Date_Time) - min(as.numeric(x$Date_Time))
  x$dt = x$dt + 60*60*12
  x$dt = as.POSIXct(x$dt, origin = min(x$Date_Time))
  # remove days from start and end of series
  x$day = as.POSIXct(str_extract(x$dt,'..........'), origin = min(x$Date_Time))
  x = x[x$dt > sort(unique(x$day))[3] & x$dt < sort(unique(x$day),decreasing=T)[2],]
  # remove NA values 
  x = x[!is.na(x$Temperature_c),]
  # 
  x[,c("dt", "Temperature_c", "RH.")]
}
# 
filter_dat = function(x){
  # fit ARIMA 
  mod = auto.arima(x$Temperature_c)
  x$residual = mod$residuals
  x$fitted = mod$fitted
  x$z_cat = cut(x$residual,breaks=quantile(x$residual,c(0,0.01,.975,1),na.rm=T),
                include.lowest = T, labels = c(1,2,3))
  # remove 99 percentile residuals from bottom 50% of data
  x = x[(x$z_cat != 3 & (x$Temperature_c < median(x$Temperature_c))) | x$Temperature_c > median(x$Temperature_c),]
  
  # 
  x[,c("dt", "Temperature_c", "RH.")]
  }

# lapply across data time series 
x = lapply(path_input,read.csv)
y = lapply(x,manage_dat)
y2 = lapply(y,filter_dat)

# assign sensor IDs
names(y2) = str_extract(grep('raw_CS', dir(),value=T), 'CS[0-9]+')


####################################################################################
# Tmin Tmax

# MINUTE 
climate_minute = y2
climate_minute = rbindlist(climate_minute, idcol = T)

# HOUR 
summary_stat = function(x){
  x$dt_hour = as.POSIXct(strptime(paste0(str_extract(x$dt,'.............'),":00:00"),format='%Y-%m-%d %H:%M:%S'))
  setDT(x)
  x$RH. = as.numeric(x$RH.)
  x = x[,.(max_temp = max(Temperature_c), mean_temp = mean(Temperature_c), min_temp = min(Temperature_c),
           max_rh=max(RH.), mean_rh=mean(RH.), min_rh=min(RH.)),
        by = 'dt_hour']
  x
}
climate_hour = lapply(y2,summary_stat)
climate_hour = rbindlist(climate_hour, idcol = T)

# DAY 
summary_stat = function(x){
  x$dt_hour = as.POSIXct(strptime(paste0(str_extract(x$dt,'.............'),":00:00"),format='%Y-%m-%d %H:%M:%S'))
  setDT(x)
  x$RH. = as.numeric(x$RH.)
  x = x[,.(mean_temp = mean(Temperature_c), mean_rh=mean(RH.)), by = dt_hour]
  x$dt_day = as.POSIXct(strptime(paste0(str_extract(x$dt,'...........'),"00:00:00"),format='%Y-%m-%d %H:%M:%S'))
  x = x[,.(
    maxT=max(mean_temp), meanT = mean(mean_temp), minT=min(mean_temp),
    maxRH=max(mean_rh,na.rm=T), meanRH = mean(mean_rh,na.rm=T), minRH=min(mean_rh,na.rm=T)),
    by = dt_day]
  x
}
climate_day = lapply(y2,summary_stat)
climate_day = rbindlist(climate_day, idcol = T)


save(list=c('climate_minute','climate_hour','climate_day'), 
     file='C:/Users/Nicholas/Desktop/data_macivor_csv_recovered/climate_dat.RData')



####################################################################################
# MERGE SENSOR ID INFO 
sensor = read.csv('C:/Users/Nicholas/Desktop/data_macivor_csv_recovered/KSR_Sensor_List.csv')

climate_day = merge(sensor,climate_day,by.x='CredoSense',by.y='.id')
climate_hour = merge(sensor,climate_hour,by.x='CredoSense',by.y='.id')
climate_minute = merge(sensor,climate_minute,by.x='CredoSense',by.y='.id')

save(list=c('climate_minute','climate_hour','climate_day'), 
     file='C:/Users/Nicholas/Desktop/data_macivor_csv_recovered/climate_dat.RData')



####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
# SENSOR
yy = rbindlist(y3,idcol='sensor_id')
yy = yy[,.(
  maxT = mean(maxT), meanT = mean(meanT), minT = mean(minT),
  maxRH = mean(maxRH), meanRH = mean(meanRH), minRH = mean(minRH)),
  by='sensor_id']


save(list=c('climate_minute','climate_hour','climate_day','sensor'), 
     file='C:/Users/Nicholas/Desktop/data_macivor_csv_recovered/dat_credo.RData')




##########################################################################################

ggplot(yy,aes(x=sensor_id,y=meanT)) + geom_point() + geom_errorbar(aes(ymin=minT,ymax=maxT))
ggplot(yy,aes(x=sensor_id,y=meanRH)) + geom_point() + geom_errorbar(aes(ymin=minRH,ymax=maxRH))




# Tmin Tmax
x = y2$CS1
x$dt_hour = as.POSIXct(strptime(paste0(str_extract(x$dt,'.............'),":00:00"),format='%Y-%m-%d %H:%M:%S'))
setDT(x)
x$RH. = as.numeric(x$RH.)
x = x[,.(mean_temp = mean(Temperature_c), mean_rh=RH.), by = dt_hour]
x$dt_day = as.POSIXct(strptime(paste0(str_extract(x$dt,'...........'),"00:00:00"),format='%Y-%m-%d %H:%M:%S'))
x = x[,.(
  maxT=max(mean_temp), meanT = mean(mean_temp), minT=min(mean_temp),
         maxRH=max(mean_rh,na.rm=T), meanRH = mean(mean_rh,na.rm=T), minRH=min(mean_rh,na.rm=T)),
  by = dt_day]

?geom_ribbon
ggplot(x,aes(x=dt_day,y=meanRH)) + geom_line() + geom_ribbon(aes(ymin=minRH,ymax=maxRH),alpha=0.2)
ggplot(x,aes(x=dt_day,y=meanT)) + geom_line() + geom_ribbon(aes(ymin=minT,ymax=maxT),alpha=0.2)



plot(x$dt_day,x$maxRH,type='l')
plot(x$dt_day,x$minRH,type='l')



