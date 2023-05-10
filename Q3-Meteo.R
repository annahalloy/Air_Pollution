# Get librarys
library(tidyverse)
library(chron)

# GET EXTRA FUNCTIONS
source("functions_extra.R")
source("G-Extra-Functions.R")
source("WindRose.R")

# SET PARAMETERS
Sys.setlocale("LC_TIME","C")
options(stringsAsFactors=FALSE)
options(chron.year.abb=FALSE)
theme_set(theme_bw()) # just my preference for plots

## GET THE DATA 
getwd()
datapath <- "MAGLAUS"
met <- full_join(cbind(site="LAU", ReadMet(file.path(datapath, "LAU_Wind_MM10_22.txt"))),
                 cbind(site="MAG", ReadMet(file.path(datapath, "MAG_Wind_MM10_22.txt"))))

conc <- readRDS("MAGLAUS/mag-lau.rds")

# MERGE DATA

df <- left_join(conc,
                met %>% select(-datetime) %>% mutate(year=as.character(year)))
# conc[["year"]] is otherwise incompatible with met[["year"]]
tail(df)


# WIND INTENSITY PLOTS
df %>%
  filter(!is.na(WIGE)) %>%
  ggplot+
  facet_grid(~season, scale="free_y")+
  geom_pointrange(aes(x=site, y=WIGE, color=site),
                  stat = "summary",
                  fun = mean,
                  fun.min = Percentile(25),
                  fun.max = Percentile(75),
                  linewidth=1.1)+
  scale_fill_manual(values = c("#C35C5C", "#43682F"))+
  scale_color_manual(values = c("#C35C5C", "#43682F"))

df %>%
  filter(!is.na(WIGE)) %>%
  ggplot+
  facet_grid(~season, scale="free_y")+
  geom_boxplot(aes(x=site, y=WIGE, fill=site))+
  theme(legend.position="none")+
  scale_fill_manual(values = c("#C35C5C", "#43682F"))+
  scale_color_manual(values = c("#C35C5C", "#43682F"))

dev.print(device=png,
          file="figures/WIGE_season_boxplot.png",
          width=20, height=7, units="cm", res=300)

# CHECK FOR THE INFLUENCE OF HOURS ON WIND INTENSITY
df %>%
  filter(!is.na(WIGE)) %>%
  ggplot(aes(x=hour, y=WIGE, group=site, color=site)) +
  facet_grid(~ season, scale = "free_y", drop=TRUE) +
  geom_line(stat="summary", fun="mean", linewidth=1, alpha=0.9)+
  geom_errorbar(stat="summary",
                fun.min=Percentile(25),
                fun.max=Percentile(75),
                linewidth=1,
                alpha=0.6)+
  ylab("wind velocity [m/s]")+
  xlab("hour")+
  scale_fill_manual(values = c("#C35C5C", "#43682F"))+
  scale_color_manual(values = c("#C35C5C", "#43682F"))

dev.print(device=png,
          file="figures/WIGE_season_daily.png",
          width=20,height=6,
          units="cm",res=300)


# WINDROSE PLOT
plotWindrose(df, spd = "WIGE", dir = "WIRI") +
  facet_grid(site~season)+
  theme(legend.position = "bottom")

dev.print(device=png,
          file="figures/windrose.png",
          width=30,height=25,
          units="cm",res=400)


