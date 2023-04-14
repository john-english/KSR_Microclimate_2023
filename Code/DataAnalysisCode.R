library(lme4)
library(nlme)
library(ggplot2)
library(gridExtra)

# Load "climate_dat.RData"

# Subset data into Bottom / Top sensors
# Day
day_top_sensor = climate_day[climate_day$Location=="Top",]
day_bot_sensor = climate_day[climate_day$Location=="Bottom",]

# Hour
hour_top_sensor = climate_hour[climate_hour$Location=="Top",]
hour_bot_sensor = climate_hour[climate_hour$Location=="Bottom",]

# Minute
minute_top_sensor = climate_minute[climate_minute$Location=="Top",]
minute_bot_sensor = climate_minute[climate_minute$Location=="Bottom",]



################## Day
# fit the ANCOVA model with random effect for block
model_day <- lme(meanT ~ Treatment, random=~1|Block, data = climate_day)
summary(model_day)
plot(model_day)
anova(model_day)


################## Hour
# fit the ANCOVA model with random effect for block
model_hour <- lme(mean_temp ~ Treatment, random=~1|Block, data = climate_hour)
summary(model_hour)
plot(model_hour)
anova(model_hour)


################## Minute
# fit the ANCOVA model with random effect for block
model_minute <- lme(Temperature_c ~ Treatment, random=list(Block = ~1, dt = ~1), data = climate_minute)
summary(model_minute)
plot(model_minute)
anova(model_minute)


##############################################################
#################### PLOTS ###################################
##############################################################


library(ggplot2)

############## Day 
# Bar graph for meanT by treatment and location
ggplot(data=climate_day, aes(x=Treatment, y=meanT, fill=Location)) +
  geom_bar(stat="identity",
           position=position_dodge()) 

# Bar graph for minT by treatment and location
ggplot(data=climate_day, aes(x=Treatment, y=minT, fill=Location)) +
  geom_bar(stat="identity",
           position=position_dodge()) 

# Bar graph for maxT by treatment and location
ggplot(data=climate_day, aes(x=Treatment, y=maxT, fill=Location)) +
  geom_bar(stat="identity",
           position=position_dodge()) 

# Line Graph
ggplot(climate_day, aes(x = dt_day, y = meanT, colour = Treatment)) + geom_line()

# Line graph of top and bot sensors
require(gridExtra)
plot1 = ggplot(day_top_sensor, aes(x=dt_day,y=meanT, colour=Treatment))+geom_point()+geom_smooth()+ggtitle("Top")
plot2 = ggplot(day_bot_sensor, aes(x=dt_day,y=meanT, colour=Treatment)) +  geom_point()+geom_smooth()+ggtitle("Bot")
plot3 = ggplot(day_top_sensor, aes(x=dt_day,y=minT, colour=Treatment))+geom_point()+geom_smooth()+ggtitle("Top")
plot4 = ggplot(day_bot_sensor, aes(x=dt_day,y=minT, colour=Treatment)) +  geom_point()+geom_smooth()+ggtitle("Bot")
plot5 = ggplot(day_top_sensor, aes(x=dt_day,y=maxT, colour=Treatment))+geom_point()+geom_smooth()+ggtitle("Top")
plot6 = ggplot(day_bot_sensor, aes(x=dt_day,y=maxT, colour=Treatment)) +  geom_point()+geom_smooth()+ggtitle("Bot")
grid.arrange(plot1,plot2,plot3,plot4,plot5,plot6,nrow=3,ncol=2)

############## Hour
# Line Graph
ggplot(climate_minute, aes(x = dt, y = Temperature_c, colour = Treatment)) + geom_line()




############## Minute
# Bar graph for meanT by treatment and location
ggplot(data=climate_day, aes(x=Treatment, y=meanT, fill=Location)) +
  geom_bar(stat="identity",
           position=position_dodge()) 

# Bar graph for minT by treatment and location
ggplot(data=climate_day, aes(x=Treatment, y=minT, fill=Location)) +
  geom_bar(stat="identity",
           position=position_dodge()) 

# Bar graph for maxT by treatment and location
ggplot(data=climate_day, aes(x=Treatment, y=maxT, fill=Location)) +
  geom_bar(stat="identity",
           position=position_dodge()) 

# Line Graph
ggplot(climate_day, aes(x = dt_day, y = meanT, colour = Treatment)) + geom_line()
