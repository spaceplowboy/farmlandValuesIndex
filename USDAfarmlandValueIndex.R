
setwd("~/GitHub/")

library(tidyverse)
library(dplyr)

myURL<-c("https://quickstats.nass.usda.gov/api/api_GET/
?key=53D17844-D92E-3CF5-8520-6E1E45CA2A49
&format=CSV
&year__GE=1940
&commodity_desc=AG LAND
&short_desc=AG LAND, INCL BUILDINGS - ASSET VALUE, MEASURED IN $ / ACRE
&freq_desc=ANNUAL")

download.file(myURL, destfile="landValues.csv")
NASSland<-read.csv("landValues.csv")

NASSland<-subset(NASSland, state_fips_code!=0)
NASSland<-subset(NASSland, state_fips_code!=99)
NASSland$LandValue<-as.numeric(gsub(",","",NASSland$Value))

attach(NASSland)
NASSland <- NASSland[order(state_alpha, year),]
NASSland <- NASSland %>% group_by(state_alpha) %>% mutate(lvar = lag(LandValue))
NASSland$percChange<- (NASSland$LandValue - NASSland$lvar) / NASSland$lvar

# makes a map
NASSland2014<-subset(NASSland, year==2018)

dat4mapping<-NASSland2014[,c("state_name","Value"), drop=FALSE]
dat4mapping$Value<-as.numeric(gsub(",","",dat4mapping$Value))
dat4mapping$state_name<-tolower(dat4mapping$state_name)
colnames(dat4mapping)<-c("region", "value")

library(choroplethr)
library(choroplethrMaps)
library(spdep)

state_choropleth(dat4mapping,  title ="        2018 Farmland Values", legend = "Values in 2018 ($/ac)", num_colors = 5)

keeps <- c("year","state_alpha", "Value")
usda.dat1<-NASSland[keeps]

usda.dat3 <- reshape(usda.dat1, 
                     timevar = "year",
                     idvar = c("state_alpha"),
                     direction = "wide")

dat$values<-as.numeric(as.character(dat$Value))

library(ggplot2)

###########
# makes a graph

dat2<-  NASSland[ NASSland$state_name %in% c("KANSAS","OKLAHOMA", "COLORADO", "NEBRASKA", "MISSOURI"), ] 

ggplot(data=dat2, aes(x=year, y=LandValue, group=state_name, color=factor(state_name))) +
  geom_line(size=1.1) + ylab("Farmland value ($ per acre)")  + xlab(NULL) + theme_bw() +
  scale_color_manual(name="State", values=c("green", "purple", "black", "red", "orange"), labels=c("Colorado", "Kansas", "Missouri", "Nebraska", "Oklahoma")) +
  annotate("text", x = 2010, y = 25, label = "Source: USDA NASS") +
  theme(
    legend.position = c(.2, .9),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))
 
ggsave("2018FarmLandGraph.png", width=7, height=5)
                     

