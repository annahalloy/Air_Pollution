# Get necessary librarys
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

# focus on RADiation and TEMPerature
lf %>%
  filter(!is.na(value) & variable==c("RAD", "TEMP")) %>%
  ggplot(aes(x=hour, y=value, group=site, color=site)) +
  facet_grid(variable ~ season, scale = "free_y", drop=TRUE) +
  geom_line(stat = "summary", fun = "mean", linewidth=1.1, alpha=0.8) +
  geom_errorbar(
    stat = "summary",
    fun.min = function(x) quantile(x, 0.25),
    fun.max = function(x) quantile(x, 0.75),
    width = 1,
    linewidth=0.5
  )+  
  scale_fill_manual(values = c("#C35C5C", "#43682F"))+
  scale_color_manual(values = c("#C35C5C", "#43682F"))

dev.print(device=png,
          file="figures/RADandTEMP_seasonal_daily_variatons.png",
          width=20,height=8,
          units="cm",res=300)

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
  theme(text = element_text(size = 17),
        axis.text.x = element_text(angle = 90),
        legend.position = "bottom")+               # all text size
  scale_fill_manual(values = c("#C35C5C", "#43682F"))+
  scale_color_manual(values = c("#C35C5C", "#43682F"))

dev.print(device=png,
          file="figures/monthly_cumulative_rain.png",
          width=12,height=12,
          units="cm",res=200)

## Monthly days of rain 
monthlyPrec %>%
  ggplot(aes(x=month, y=days.rain, fill=site)) + 
  geom_col( position = "dodge")+
  labs(x = "Month", y = "Days with rain [#]")+
  theme(text = element_text(size = 17),
        axis.text.x = element_text(angle = 90),
        legend.position = "bottom")+               # Axis text size
  scale_fill_manual(values = c("#C35C5C", "#43682F"))+
  scale_color_manual(values = c("#C35C5C", "#43682F"))


dev.print(device=png,
          file="figures/monthly_rain_days.png",
          width=12,height=12,
          units="cm",res=200)

## Seasonal Cumulative precipitation amount
seasonalPrec %>%
  ggplot(aes(x=site, y=seasonal.prec.sum, fill=site))+ 
  facet_grid(col= vars(season))+
  geom_col(position = "dodge")+
  labs(x = "Season", y = "Cumulative precipitation [mm]")+
  theme(text = element_text(size = 17))+
  scale_fill_manual(values = c("#C35C5C", "#43682F"))+
  scale_color_manual(values = c("#C35C5C", "#43682F"))


dev.print(device=png,
          file="figures/seasonal_total_cumulative_rain.png",
          width=15,height=15,
          units="cm",res=200)

## seasonal total days of precipitation
seasonalPrec %>%
  ggplot + 
  facet_grid(col= vars(season))+
  geom_col(aes(x=site, y=days.rain.sum, fill=site), position = "dodge")+
  
  labs(x = "Season", y = "Days with precipitation [#]")+
  theme(text = element_text(size = 17))+
  scale_fill_manual(values = c("#C35C5C", "#43682F"))+
  scale_color_manual(values = c("#C35C5C", "#43682F"))


dev.print(device=png,
          file="figures/seasonal_total_rain_days.png",
          width=15,height=15,
          units="cm",res=200)

## seasonal mean cumulative precipitation as POINTRANGE
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
  labs(x = "Site", y = expression(paste("Cumulative precipitation ","[","mm ","month"^-1,"]")))+
  theme(legend.position="none")+
  scale_fill_manual(values = c("#C35C5C", "#43682F"))+
  scale_color_manual(values = c("#C35C5C", "#43682F"))


dev.print(device=png,
          file="figures/seasonal_cumulative_rain_pointrange.png",
          width=10,height=7,
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
  labs(x = "Site", y = expression(paste("Days with precipitation ","[","month"^-1,"]")))+
  theme(legend.position="none")+
  scale_fill_manual(values = c("#C35C5C", "#43682F"))+
  scale_color_manual(values = c("#C35C5C", "#43682F"))


dev.print(device=png,
          file="figures/seasonal_rain_days_pointrange.png",
          width=10,height=7,
          units="cm",res=200)


