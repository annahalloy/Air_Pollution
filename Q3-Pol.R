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

# List of variables of interest
pollutants <- c("CO", "NO2", "NOX", "O3", "PM10", "PM2.5")
chemicals <- c("CO", "NO2", "NOX", "O3")
particles <- c("PM10","PM2.5")
meteo <- c(	"TEMP","PREC","RAD")

# View variability in pollutant concentrations OVER TIME

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
  filter(!is.na(value) & variable==c("PM10", "PM2.5")) %>%
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
  )


dev.print(device=png,
          file="seasonal_mean_chemicals.png",
          width=20,height=10,
          units="cm",res=200)

# Focus on CHEMICALS, VIOLIN PLOTS, and Black line
lf %>%
  filter(!is.na(value) & variable==c("CO", "SO2", "NO2", "O3")) %>%
  ggplot +
  facet_grid(variable ~ season, scale = "free") +
  geom_violin(aes(x=site, y=value, fill=site, color=site))+
  stat_summary(aes(x=site, y=value), fun=mean, geom="point", shape=95,
               size=12, color="black")+
  theme(legend.position="None")+
  scale_fill_manual(values = c("#C35C5C", "#43682F"))+
  scale_color_manual(values = c("#C35C5C", "#43682F"))+
  labs(y=expression(paste("Concentration (", mu, "g ", m^-3, ")")))

dev.print(device=png,
          file="figures/chemicals_seasonal_mean_violinPlot.png",
          width=15,height=15,
          units="cm",res=200)

# ALL HISTOGRAMS

lf %>%
  filter(!is.na(value) & variable==c("CO", "SO2","NO2", "O3", "PM10", "PM2.5")) %>%
  ggplot +
  facet_grid(season~variable, scale = "free_x") +
  geom_histogram(aes(x=value, fill=site), bins=30, alpha=0.75, position="identity")+
  stat_summary(aes(x=0.1, y = value, xintercept = stat(y), linetype = site, color=site), 
               fun = mean, geom = "vline", linewidth=1)+
  labs(x=expression(paste("Concentration (", mu, "g ", m^-3, ")")), y="count")+
  scale_fill_manual(values = c("#C35C5C", "#43682F"))+
  scale_color_manual(values = c("#C35C5C", "#43682F"))+
  scale_linetype_manual(values=c("dotted", "twodash"))+
  theme(legend.position="bottom")+
  ylim(0,150)

dev.print(device=png,
          file="figures/all_season_hist.png",
          width=20,height=15,
          units="cm",res=300)


# HISTOGRAMS FOR CHEMICALS

lf %>%
  filter(!is.na(value) & variable==c("CO", "SO2", "NO2", "O3")) %>%
  ggplot +
  facet_grid(season~variable, scale = "free_x") +
  geom_histogram(aes(x=value, fill=site), bins=30, alpha=0.75, position="identity")+
  stat_summary(aes(x=0.1, y = value, xintercept = stat(y), linetype = site, color=site), 
               fun = mean, geom = "vline", linewidth=1)+
  labs(title="Concentrations histograms", x="concentrations", y="count")+
  scale_fill_manual(values = c("#C35C5C", "#43682F"))+
  scale_color_manual(values = c("#C35C5C", "#43682F"))+
  scale_linetype_manual(values=c("dotted", "twodash"))+
  theme(legend.position="bottom")

dev.print(device=png,
          file="figures/chemicals_season_hist.png",
          width=17,height=12,
          units="cm",res=200)

# Histograms for PM10, PM2.5

lf %>%
  filter(!is.na(value) & variable==particles) %>%
  ggplot +
  facet_grid(season~variable, scale = "free_x") +
  geom_histogram(aes(x=value, fill=site), bins=30, alpha=0.75, position="identity")+
  stat_summary(aes(x=0.1, y = value, xintercept = stat(y), linetype = site, color=site), 
               fun = mean, geom = "vline", linewidth=1)+
  labs(title="Concentrations histograms", x="concentrations", y="count")+
  scale_fill_manual(values = c("#C35C5C", "#43682F"))+
  scale_color_manual(values = c("#C35C5C", "#43682F"))+
  scale_linetype_manual(values=c("dotted", "twodash"))+
  theme(legend.position="bottom")

dev.print(device=png,
          file="figures/particles_season_hist.png",
          width=10,height=12,
          units="cm",res=200)

# FOCUS ON PARTICLES 
lf %>%
  filter(!is.na(value) & variable==particles) %>%
  ggplot +
  facet_grid(variable ~ season, scale = "free_y") +
  geom_violin(aes(x=site, y=value, fill=site))+
  stat_summary(aes(x=site, y=value), fun=mean, geom="point", shape=95,
               size=15, color="black")+
  theme(legend.position="none")+
  scale_fill_manual(values = c("#C35C5C", "#43682F"))+
  scale_color_manual(values = c("#C35C5C", "#43682F"))dev.print(device=png,
          file="figures/particles_seasonal_mean_violinPlot.png",
          width=20,height=10,
          units="cm",res=200)


# All POLLUTANTS MEAN VARIATIONS

lf %>%
  filter(!is.na(value) & variable %in% c("CO", "SO2", "NO2", "O3", "PM10", "PM2.5")) %>%
  ggplot(aes(month, value, fill = site)) +
  facet_grid(variable ~ ., scale = "free_y") +
  geom_bar(stat = "summary", fun = "mean", position = position_dodge()) +
  geom_errorbar(
    stat = "summary",
    fun.min = function(x) quantile(x, 0.25),
    fun.max = function(x) quantile(x, 0.75),
    width = 0.2,
    position = position_dodge(0.9),
    color = "#A8A8D2"
  ) +
  geom_text(
    stat = "summary",
    aes(label = sprintf("%.2f", after_stat(y))),
    fun = function(x) mean(x, na.rm = TRUE),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size=2.5
  ) +
  scale_fill_manual(values = c("#C35C5C", "#43682F"))+
  scale_color_manual(values = c("#C35C5C", "#43682F"))+
  labs(y=expression(paste("Concentration (", mu, "g ", m^-3, ")")))

dev.print(device=png,
          file="figures/all_errorBars_Txt_monthly_variations.png",
          width=25,height=18,
          units="cm",res=400)

lf %>%
  filter(!is.na(value) & variable %in% c("CO", "SO2", "NO2", "O3", "PM10", "PM2.5")) %>%
  ggplot(aes(month, value, fill = site)) +
  facet_grid(variable ~ ., scale = "free_y") +
  geom_bar(stat = "summary", fun = "mean", position = position_dodge()) +
  geom_errorbar(
    stat = "summary",
    fun.min = function(x) quantile(x, 0.25),
    fun.max = function(x) quantile(x, 0.75),
    width = 0.3,
    position = position_dodge(0.9)
  ) +
  scale_fill_manual(values = c("#C35C5C", "#43682F"))+
  scale_color_manual(values = c("#C35C5C", "#43682F"))+
  labs(y=expression(paste("Concentration (", mu, "g ", m^-3, ")")))


dev.print(device=png,
          file="figures/all_errorBars_monthly_variations.png",
          width=25,height=18,
          units="cm",res=400)


lf %>%
  filter(!is.na(value) & variable==c("CO", "SO2", "NO2", "O3", "PM10", "PM2.5")) %>%
  ggplot(aes(month, value, fill=site)) +
    facet_grid(variable~., scale="free_y") +
    geom_boxplot()+
  scale_fill_manual(values = c("#C35C5C", "#43682F"))+
  scale_color_manual(values = c("#C35C5C", "#43682F"))

dev.print(device=png,
          file="figures/all_boxplot_month_variations.png",
          width=20,height=20,
          units="cm",res=200)
