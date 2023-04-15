library(tidyverse)
library(chron)

# Source function from files
source("functions_extra.R")
source("G-Extra-Functions.R")

# Set preferences
Sys.setlocale("LC_TIME","C");
options(stringsAsFactors=FALSE)
options(chron.year.abb=FALSE)
theme_set(theme_bw()) # just my preference for plots

# Get working directory
getwd()

# Get the data frame
df <- readRDS("MAGLAUS/MAG-LAU.rds")
# Into long format
lf <- df %>%
  gather(variable, value,
         -c(site, datetime, season, year, month, day, hour, dayofwk, daytype))
# Assign datetime column
lf$date <- dates(lf$datetime)

# Visualize the data frame
head(lf)

# Plot the variation of precipitation throughout the season (bad plot)
lf %>%
  filter(variable=="PREC" & !is.na(value)) %>%
  group_by(date, site, season, variable) %>%
  summarise(value = sum(value)) %>%
  group_by(months(date), years(date), site, season, variable) %>%
  summarise(mean_val = mean(value),
            rainy_days = sum(value>0),
            sum_val = sum(value),
            h_rainy_days = sum(value > 1)) %>%
  ggplot +
  facet_grid(variable ~ season, scale = "free_y") +
  geom_dotplot(aes(x=site, y=rainy_days, group=site), binaxis='y', stackdir='center', binwidth=0.3)+
  stat_summary(aes(x=site, y=rainy_days), fun=mean, geom="point", shape=18,
               size=3, color="red")

# Compute the daily precipitation
dailyPrec <- lf %>%
  filter(variable=="PREC" & !is.na(value)) %>% # select variables
  mutate(date = as.character(dates(datetime))) %>%# get the date value
  mutate(rain.hour = value>0) %>%
  select(-c(datetime, date, hour, daytype, variable))%>%
  group_by(site, year, month, day, season) %>%
  summarise(prec.mean = mean(value),
            prec.sum = sum(value),
            rain.hours = sum(rain.hour),
            rain.day = sum(value)>0,
            recovery.percent = length(value)*100/24,
            recovery.numbers = length(value)) %>%
  ungroup()

# Visualize the daily precipitation dataframe
head(dailyPrec)


## Monthly precipitation stats data frame
monthlyPrec <- dailyPrec %>%
  group_by(site, year, month, season) %>%
  summarize(daily.prec.mean = mean(prec.sum),
            daily.hours.prec.mean = mean(rain.hours),
            monthly.rain = sum(prec.sum),
            monthly.hours.prec = sum(rain.hours),
            recovery.percent.mean = mean(recovery.percent),
            days.rain = sum(rain.day))

# + visualize dataframe
head(monthlyPrec)

## Seasonal precipitation stats
seasonalPrec <- monthlyPrec %>%
  group_by(site, season) %>%
  summarise(monthly.prec.mean = mean(monthly.rain),
            seasonal.prec.sum = sum(monthly.rain),
            days.rain.sum = sum(days.rain),
            days.rain.mean = mean(days.rain),
            hours.rain.mean = mean(monthly.hours.prec),
            hours.rain.total = mean(monthly.hours.prec),
            recovery.percent.mean = mean(recovery.percent.mean),
            daily.prec.mean = mean(daily.prec.mean))
# + visualize
head(seasonalPrec)

## Monthly cumulative precipitation bar plot
monthlyPrec %>%
  ggplot + 
  geom_col(aes(x=month, y=monthly.rain, fill=site), position = "dodge")+
  labs(x = "Month", y = "Cumulative precipitation [mm]")+
  theme(text = element_text(size = 17))               # all text size

dev.print(device=png,
          file="figures/monthly_cumulative_rain.png",
          width=20,height=10,
          units="cm",res=150)

## Monthly days of rain 
monthlyPrec %>%
  ggplot + 
  geom_col(aes(x=month, y=days.rain, fill=site), position = "dodge")+
  labs(x = "Month", y = "Days with rain [#]")+
  theme(text = element_text(size = 17))               # Axis text size

dev.print(device=png,
          file="figures/monthly_rain_days.png",
          width=20,height=10,
          units="cm",res=150)

## Seasonal Cumulative precipitation amount
seasonalPrec %>%
  ggplot + 
  facet_grid(col= vars(season))+
  geom_col(aes(x=site, y=seasonal.prec.sum, fill=site), position = "dodge")+
  labs(x = "Season", y = "Cumulative precipitation [mm]")+
  theme(text = element_text(size = 17))  

dev.print(device=png,
          file="figures/seasonal_cumulative_rain.png",
          width=15,height=15,
          units="cm",res=200)

## seasonal total days of precipitation
seasonalPrec %>%
  ggplot + 
  facet_grid(col= vars(season))+
  geom_col(aes(x=site, y=days.rain.sum, fill=site), position = "dodge")+
  labs(x = "Season", y = "Days with precipitation [#]")+
  theme(text = element_text(size = 17))  

dev.print(device=png,
          file="figures/seasonal_rain_days.png",
          width=15,height=15,
          units="cm",res=200)

## seasonal total cumulative precipitation as POINTRANGE
monthlyPrec %>%
  ggplot + 
  facet_grid(col= vars(season))+
  geom_pointrange(aes(x=site, y=monthly.rain, color=site),
                  stat="summary",
                  fun.min=min,
                  fun.max=max,
                  fun = mean,
                  linewidth=1.5,
                  size=0.8)+
  labs(x = "Season", y = "Cumulative precipitation [mm]")+
  theme(text = element_text(size = 17))  

dev.print(device=png,
          file="figures/seasonal_cumulative_rain_pointrange.png",
          width=20,height=15,
          units="cm",res=200)

## seasonal total days of precipitation as POINTRANGE
monthlyPrec %>%
  ggplot + 
  facet_grid(col= vars(season))+
  geom_pointrange(aes(x=site, y=days.rain, color=site),
                stat="summary",
                fun.min=min,
                fun.max=max,
                fun = mean,
                linewidth=1.5,
                size=0.8)+
  labs(x = "Season", y = "Days with precipitation [month^-1]")+
  theme(text = element_text(size = 17))

dev.print(device=png,
          file="figures/seasonal_rain_days_pointrange.png",
          width=20,height=15,
          units="cm",res=400)


