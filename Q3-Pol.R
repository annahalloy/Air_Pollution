library(tidyverse)
library(chron)

# Get extra functions

source("functions_extra.R")
source("G-Extra-Functions.R")

# Set preferences
Sys.setlocale("LC_TIME","C");
options(stringsAsFactors=FALSE)
options(chron.year.abb=FALSE)
theme_set(theme_bw()) # just my preference for plots

# Get working directory
getwd()

# Get Data
df <- readRDS("MAGLAUS/MAG-LAU.rds")
lf <- df %>%
  gather(variable, value,
         -c(site, datetime, season, year, month, day, hour, dayofwk, daytype))
lf$date <- dates(lf$datetime)

head(lf)

# Datasets with Dropped lines of NA values
lfNona <- drop_na(lf, "value")
lfNona$site <- as.factor(lfNona$site)
lfNona$season <- as.factor(lfNona$season)
lfNona$variable <- as.factor(lfNona$variable)
head(lfNona)

# List of variables of interest
pollutants <- c("CO", "NO2", "NOX", "O3", "PM10", "PM2.5")
chemicals <- c("CO", "NO2", "NOX", "O3")
particles <- c("PM10","PM2.5")
meteo <- c(	"TEMP","PREC","RAD")

# View variability in pollutant concentrations

ggplot(lf)+ # `lf` is the data frame
  facet_grid(variable~site, scale="free_y")+ # panels created out of these variables
  geom_line(aes(datetime, value, color=site))+ # plot `value` vs. `time` as lines
  scale_x_chron()+ # format x-axis labels (time units)
  theme(axis.text.x=element_text(angle=30, hjust=1)) # rotate x-axis labels

# Seasonal variations

ggplot(lf) +
  facet_grid(variable ~ site, scale = "free_y") +
  geom_boxplot(aes(month, value), outlier.size = 0.5, outlier.shape = 3)

# By season and station
lf %>%
  filter(!is.na(value)) %>%
  ggplot +
  facet_grid(variable ~ season, scale = "free_y") +
  geom_boxplot(aes(site, value), outlier.size = 0.5, outlier.shape = 3)


## Focus on PM10 variation by season & station with line range
lf %>%
  filter(!is.na(value) & variable==c("PM10")) %>%
  ggplot+ 
  facet_grid(variable ~ season, scale = "free_y") +
  stat_summary(
    mapping = aes(x = site, y = value),
    fun.min = min,
    fun.max = max,
    fun = mean )

# Focus on PM10, season and site, with CROSSBAR and mean + sdl 
lf %>%
  filter(!is.na(value) & variable==c("PM10")) %>%
  ggplot(aes(x = site, y = value))+ 
  facet_grid(variable ~ season, scale = "free_y") +
  geom_point()+
  stat_summary(
    fun.data = "mean_sdl",
    geom = "crossbar",
    colour = "red", width = 0.5
  ) +
  xlab("Site")

# Focus on PM10, with variation as DOTPLOT season & site
lf %>%
  filter(!is.na(value) & variable==c("PM10")) %>%
  ggplot(aes(x = site, y = value))+ 
  facet_grid(variable ~ season, scale = "free_y") +
  geom_dotplot(binaxis='y', stackdir='center', binwidth=0.3)+
  stat_summary(fun=mean, geom="point", shape=18,
               size=3, color="red")

# Focus on pollutants 
lf %>%
  filter(!is.na(value) & variable==c('NO2', 'NOX', 'O3')) %>%
  ggplot +
  facet_grid(variable ~ season, scale = "free_y") +
  geom_dotplot(aes(x=site, y=value, color=site), stackdir='center', binaxis='y',
               binwidth = 1.1)+
  stat_summary(aes(x=site, y=value), fun=mean, geom="point", shape=95,
               size=22, color="black")+
  theme(legend.position="none")

dev.print(device=png,
          file="seasonal_mean_chemicals.png",
          width=20,height=10,
          units="cm",res=200)

# Focus on CHEMICALS, VIOLIN PLOTS, and Black line
lf %>%
  filter(!is.na(value) & variable==chemicals) %>%
  ggplot +
  facet_grid(variable ~ season, scale = "free_y") +
  geom_violin(aes(x=site, y=value, fill=site, color=site))+
  stat_summary(aes(x=site, y=value), fun=mean, geom="point", shape=95,
               size=20, color="black", alpha=0.9)+
  theme(legend.position="none")

dev.print(device=png,
          file="figures/chemicals_seasonal_mean_violinPlot.png",
          width=20,height=20,
          units="cm",res=200)

# FOCUS ON PARTICLES 
lf %>%
  filter(!is.na(value) & variable==particles) %>%
  ggplot +
  facet_grid(variable ~ season, scale = "free_y") +
  geom_violin(aes(x=site, y=value, fill=site))+
  stat_summary(aes(x=site, y=value), fun=mean, geom="point", shape=95,
               size=24, color="black")+
  theme(legend.position="none")

dev.print(device=png,
          file="figures/particles_seasonal_mean_violinPlot.png",
          width=20,height=15,
          units="cm",res=200)

# focus on RADiation and TEMPerature
lf %>%
  filter(!is.na(value) & variable==c("RAD", "TEMP")) %>%
  ggplot(aes(x=hour, y=value, group=site, color=site)) +
  facet_grid(variable ~ season, scale = "free_y", drop=TRUE) +
  geom_line(stat="summary", fun="mean", linewidth=0.9)+
  geom_errorbar(stat="summary",
                fun.min=Percentile(25),
                fun.max=Percentile(75),
                linewidth=0.7)

dev.print(device=png,
          file="figures/RADandTEMP_seasonal_daily_variatons.png",
          width=20,height=10,
          units="cm",res=400)


# Focus on RAD 
lf %>%
  filter(!is.na(value) & variable==c("RAD")) %>%
  ggplot(aes(x=hour, y=value, group=date, color=site)) +
  facet_grid(~ season, scale = "free_y", drop=TRUE) +
  geom_line(alpha=0.3)


