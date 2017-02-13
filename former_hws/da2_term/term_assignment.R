#TERM PROJECT DA2 - 2016
#Direct impacts of tropical and sub-tropical de-forestation
library(arm)
library(readr)
library(dplyr)
library(ggplot2)
library("pastecs")
library(e1071)
library(stargazer)
library(sandwich)
library(splines)

library(lmtest)  
library(splines) 

rm(list=ls())

# CHECK WORKING DIRECTORY
setwd("..\\temp\\rWorkDir")
getwd()
#import lookup data for continents, source: https://datahub.io/dataset/countries-continents
coco_lookup <-read.csv("..\\temp\\rWorkDir\\Countries-Continents.csv")
#import raw data from world data bank
#messy data caused a lot of problems, needed to eliminate .. and similar things
wd_raw <- read.csv("..\\temp\\rWorkDir\\cleaned_raw_Data.txt", sep="\t")

wd_raw$CountryName = wd_raw$ï..Country.Name

#join the 2 frames, to see, whether we can place all countries
#let's look on one world bank series only to filter missing lookups
wd_indicator <-subset(wd_raw,wd_raw$Series.Name=="Forest area (sq. km)")
#it looks i will need to normalize names, so instead of inner join i will use full join
geo_wd_indicator <-merge(coco_lookup, wd_indicator, by.x = "Country", by.y = "Country.Name", all = TRUE)
#i have eliminated naming errors, and add the countries missing, but do not deatl with European, North American countries, 
#islands on the Pacific Ocean, North Korea, Yemen and Islamic State 
problem_set <- subset(geo_wd_indicator,is.na(geo_wd_indicator$Continent))

coco_lookup <-read.csv("..\\temp\\rWorkDir\\Countries-Continents2.csv")


#using inner join, to deal with country level data only
geo_wd <-merge(coco_lookup, wd_raw, by.x = "Country", by.y = "Country.Name")
#i may need to exclude more countries, due to the environmental factors
subt_wd <- subset(geo_wd,
                  (geo_wd$Continent=='Africa'&geo_wd$Country!='South Sudan')|
                    geo_wd$Continent=='South America'|
                    geo_wd$Continent=='Oceania'|
                    (geo_wd$Continent=='North America' &
                       geo_wd$Country!='United States' &
                       geo_wd$Country!='Mexico' &
                       geo_wd$Country!='Canada')|
                    (geo_wd$Continent=='Asia' &
                       geo_wd$Country!='Japan'&  
                       geo_wd$Country!='China'&
                       geo_wd$Country!='Mongolia'&
                       geo_wd$Country!='Russian Federation'&
                       geo_wd$Country!='Saudi Arabia'&
                       geo_wd$Country!='United Arab Emirates'&
                       geo_wd$Country!='Kazakhstan'&
                       geo_wd$Country!='Uzbekistan'&
                       geo_wd$Country!='Singapore'&
                       geo_wd$Country!='Bahrain'&
                       geo_wd$Country!='Maldives'&
                       geo_wd$Country!='Korea- Rep.'
                     )
                  )

#preview for analysis

forestarea_pct <- subset(subt_wd, subt_wd$Series.Name=="Forest area (% of land area)")
forestarea_sqkm <- subset(subt_wd, subt_wd$Series.Name=="Forest area (sq. km)")
aggricarea_pct <- subset(subt_wd, subt_wd$Series.Name=="Agricultural land (% of land area)")
aggricarea_sqkm <- subset(subt_wd, subt_wd$Series.Name=="Agricultural land (sq. km)")
arrable_happrs <- subset(subt_wd, subt_wd$Series.Name=="Arable land (hectares per person)")
rural_pop_pct <- subset(subt_wd, subt_wd$Series.Name=="Rural population (% of total population)")
urban_pop_pct <- subset(subt_wd, subt_wd$Series.Name=="Urban population (% of total)")
slums_pop_pct <- subset(subt_wd, subt_wd$Series.Name=="Population living in slums- (% of urban population)")
surface_area_sqkm <- subset(subt_wd, subt_wd$Series.Name=="Surface area (sq. km)")
gdppc <- subset(subt_wd, subt_wd$Series.Name=="GDP per capita (constant 2010 US$)")
pop_dens_ppha <- subset(subt_wd, subt_wd$Series.Name=="Population density (people per sq. km of land area)")



#no data
annual_aggri_water_pct <- subset(subt_wd, subt_wd$Series.Name=="Annual freshwater withdrawals- agriculture (% of total freshwater withdrawal)")
dfex_pct_pop <- subset(subt_wd, subt_wd$Series.Name=="Droughts- floods- extreme temperatures (% of population- average 1990-2009)")
rural_pov_pct <- subset(subt_wd, subt_wd$Series.Name=="Rural poverty gap at national poverty lines (%)") 
annual_aggri_water_pct$c2000_2010 <- annual_aggri_water_pct$YR2010 - annual_aggri_water_pct$YR2000
summary(annual_aggri_water_pct$c2000_2010)

#this may work
agg_add_oct_growth <- subset(subt_wd, subt_wd$Series.Name=="Agriculture- value added (annual % growth)")


#main x
forestarea_pct$c2000_2010 <- forestarea_pct$YR2010 - forestarea_pct$YR2000
summary(forestarea_pct$c2000_2010)
ggplot(forestarea_pct, aes(x = c2000_2010)) + 
  geom_histogram(stat = "bin", binwidth=0.5,
                 fill="seagreen3", colour="black") +
  labs(x="Binwidth: 0.5", y="Forest area percentage change between 2000 and 2010")

#control x: arrable ha per person
arrable_happrs$c2000_2010 <- arrable_happrs$YR2010 - arrable_happrs$YR2000
summary(arrable_happrs$c2000_2010)
ggplot(arrable_happrs, aes(x = c2000_2010)) + 
  geom_histogram(stat = "bin", binwidth=0.005,
                 fill="seagreen3", colour="black") +
  labs(x="Binwidth: 0.005", y="Arrable ha per person change between 2000 and 2010")

#control x
aggricarea_pct$c2000_2010 <- aggricarea_pct$YR2010 - aggricarea_pct$YR2000
summary(aggricarea_pct$c2000_2010)
ggplot(aggricarea_pct, aes(x = c2000_2010)) + 
  geom_histogram(stat = "bin", binwidth=0.5,
                 fill="seagreen3", colour="black") +
  labs(x="Binwidth: 0.5", y="Aggricultural area change between 2000 and 2010")



#25% NA, may not be good control
agg_add_oct_growth$c2000_2010 <- agg_add_oct_growth$YR2010 - agg_add_oct_growth$YR2000 
summary(agg_add_oct_growth$c2000_2010)

#control x: gdppc
gdppc$c2000_2010 <- gdppc$YR2010 - gdppc$YR2000
summary(gdppc$c2000_2010)

ggplot(gdppc, aes(x = c2000_2010)) + 
  geom_histogram(stat = "bin", binwidth=100,
                 fill="gold", colour="black") +
  labs(x="Binwidth: 100", y="GDP per capita change between 2000 and 2010")

#control x: population density per ha
pop_dens_ppha$c2000_2010 <- pop_dens_ppha$YR2010 - pop_dens_ppha$YR2000
summary(pop_dens_ppha$c2000_2010)

ggplot(pop_dens_ppha, aes(x = c2000_2010)) + 
  geom_histogram(stat = "bin", binwidth=100,
                 fill="firebrick4", colour="black") +
  labs(x="Binwidth: 100", y="Population desnity per ha change between 2000 and 2010")

#y-var
rural_pop_pct$c2000_2010 <- rural_pop_pct$YR2010 - rural_pop_pct$YR2000
summary(rural_pop_pct$c2000_2010)
ggplot(rural_pop_pct, aes(x = c2000_2010)) + 
  geom_histogram(stat = "bin", binwidth=0.5,
                 fill="tomato2", colour="black") +
  labs(x="Binwidth: 0.5", y="Rural population % change between 2000 and 2010")

workset1 <-merge(forestarea_pct, rural_pop_pct, by = "Country")
workset1$Continent <- workset1$Continent.x
workset1$CountryCode <- workset1$Country.Code.x
workset1$rural_pop_pct_2000 <- workset1$YR2000.y
workset1$rural_pop_pct_2010 <- workset1$YR2010.y
workset1$forestarea_pct_2010 <- workset1$YR2010.x
workset1$forestarea_pct_2000 <- workset1$YR2000.x

#later on  i will need to figure out something how to alias the fields or do the drops before the merge
workset1$Continent.x <- NULL
workset1$YR1990.x <- NULL
workset1$YR1995.x <- NULL
workset1$YR2000.x <- NULL
workset1$YR2005.x <- NULL
workset1$YR2010.x <- NULL
workset1$YR2015.x <- NULL
workset1$Continent.y <- NULL
workset1$YR1990.y <- NULL
workset1$YR1995.y <- NULL
workset1$YR2000.y <- NULL
workset1$YR2005.y <- NULL
workset1$YR2010.y <- NULL
workset1$YR2015.y <- NULL
workset1$Series.Name.x <- NULL
workset1$Series.Code.x <- NULL
workset1$Country.Code.x <- NULL
workset1$Series.Name.y <- NULL
workset1$Series.Code.y <- NULL
workset1$Country.Code.y <- NULL

workset2 <- merge(workset1,arrable_happrs, by = "Country")
workset2$arrable_happrs_2010 <- workset2$YR2010
workset2$arrable_happrs_2000 <- workset2$YR2000

workset2$YR1990 <- NULL
workset2$YR1995 <- NULL
workset2$YR2000 <- NULL
workset2$YR2005 <- NULL
workset2$YR2010 <- NULL
workset2$YR2015 <- NULL
workset2$Series.Name <- NULL
workset2$Series.Code <- NULL
workset2$Country.Code <- NULL
workset2$Continent <- workset2$Continent.x
workset2$Continent.y <- NULL
workset2$Continent.x <- NULL
workset2$c2000_2010.x <- NULL
workset2$c2000_2010.y <- NULL
workset2$c2000_2010 <- NULL
workset2$c2005_2010 <- NULL


workset3 <- merge(workset2,aggricarea_pct, by = "Country")
workset3$aggricarea_pct_2000 <- workset3$YR2000
workset3$aggricarea_pct_2010 <- workset3$YR2010

workset3$YR1990 <- NULL
workset3$YR1995 <- NULL
workset3$YR2000 <- NULL
workset3$YR2005 <- NULL
workset3$YR2010 <- NULL
workset3$YR2015 <- NULL
workset3$Series.Name <- NULL
workset3$Series.Code <- NULL
workset3$Country.Code <- NULL
workset3$Continent <- workset3$Continent.x
workset3$Continent.y <- NULL
workset3$Continent.x <- NULL
workset3$c2000_2010 <- NULL


workset4 <- merge(workset3,surface_area_sqkm, by = "Country")
workset4$surface_area_sqkm <- workset4$YR2000
workset4$YR1990 <- NULL
workset4$YR1995 <- NULL
workset4$YR2000 <- NULL
workset4$YR2005 <- NULL
workset4$YR2010 <- NULL
workset4$YR2015 <- NULL
workset4$Series.Name <- NULL
workset4$Series.Code <- NULL
workset4$Country.Code <- NULL
workset4$Continent <- workset4$Continent.x
workset4$Continent.y <- NULL
workset4$Continent.x <- NULL

workset5 <- merge(workset4,gdppc, by="Country")
workset5$gdppc_2000 <- gdppc$YR2000
workset5$gdppc_2010 <- gdppc$YR2010
workset5$YR1990 <- NULL
workset5$YR1995 <- NULL
workset5$YR2000 <- NULL
workset5$YR2005 <- NULL
workset5$YR2010 <- NULL
workset5$YR2015 <- NULL
workset5$Series.Name <- NULL
workset5$Series.Code <- NULL
workset5$Country.Code <- NULL
workset5$Continent <- workset5$Continent.x
workset5$Continent.y <- NULL
workset5$Continent.x <- NULL
workset5$c2000_2010 <- NULL

workset6 <-  merge(workset5,pop_dens_ppha, by="Country")
workset6$pop_dens_pha_2000 <- pop_dens_ppha$YR2000
workset6$pop_dens_pha_2010 <- pop_dens_ppha$YR2010
workset6$YR1990 <- NULL
workset6$YR1995 <- NULL
workset6$YR2000 <- NULL
workset6$YR2005 <- NULL
workset6$YR2010 <- NULL
workset6$YR2015 <- NULL
workset6$Series.Name <- NULL
workset6$Series.Code <- NULL
workset6$Country.Code <- NULL
workset6$Continent <- workset6$Continent.x
workset6$Continent.y <- NULL
workset6$Continent.x <- NULL
workset6$c2000_2010 <- NULL

workset <- workset6
workset1 <- NULL
workset2 <- NULL
workset3 <- NULL
workset4 <- NULL
workset5 <- NULL
workset6 <- NULL
geo_wd_indicator <- NULL
problem_set <- NULL
wd_raw <- NULL
geo_wd <-NULL
wd_indicator <-NULL
coco_lookup <-NULL
arrable_happrs <- NULL
agg_add_oct_growth <- NULL
aggricarea_pct <- NULL
aggricarea_sqkm <- NULL
forestarea_pct <- NULL
forestarea_sqkm <- NULL
gdppc <- NULL
rural_pov_pct <- NULL
rural_pop_pct <- NULL
slums_pop_pct <- NULL
subt_wd <- NULL
surface_area_sqkm <- NULL
urban_pop_pct <- NULL

###########################################################
#checking the changes in the usable data sets
workset$d_rural_pop_pct <- workset$rural_pop_pct_2010 - workset$rural_pop_pct_2000
workset$d_forest_area_pct <- workset$forestarea_pct_2010 - workset$forestarea_pct_2000
workset$d_aggricarea_pct <- workset$aggricarea_pct_2010 - workset$aggricarea_pct_2000
workset$d_arrable_happrs <- workset$arrable_happrs_2010 - workset$arrable_happrs_2000
workset$log_d_arrable_happrs <- log(workset$d_arrable_happrs)
total_area<-sum(workset$surface_area_sqkm)
workset$weight <- workset$surface_area_sqkm / total_area
workset$d_gdppc <- workset$gdppc_2010 - workset$gdppc_2000
workset$log_d_gdppc <- log(workset$d_gdppc)
workset$d_pop_dens_pha <- workset$pop_dens_pha_2010 - workset$pop_dens_pha_2000
workset$log_d_pop_dens_pha <- log(workset$d_pop_dens_pha)
###########################################################
ggplot(data = workset, aes(x=d_forest_area_pct, y=d_rural_pop_pct)) +
  geom_point(size=1.5, colour="orange") +
  geom_smooth(method="loess", colour="black") +
  geom_smooth(method="lm", colour="forestgreen")+
  geom_text(aes(label=workset$Country), size=3)+
  labs(x="Forest area pp change between 2000 and 2010", y="Rural population pp change between 2000 and 2010")

linreg_deforest <- lm(d_rural_pop_pct ~ d_forest_area_pct, data=workset)
summary(linreg_deforest)
stargazer(linreg_deforest, digits=2, out="deforestation_ruralpopchg01.html")

#weight does not help
ggplot(data = workset, aes(x=d_forest_area_pct*weight, y=d_rural_pop_pct)) +
  geom_point(size=1.5, colour="orange") +
  geom_smooth(method="loess", colour="black") +
  geom_smooth(method="lm", colour="deepskyblue3")+
  geom_text(aes(label=workset$Country), size=3)+
  labs(x="Forest area pp change between 2000 and 2010", y="Rural population pp change between 2000 and 2010")

#control var: aggricultural area
ggplot(data = workset, aes(x=d_aggricarea_pct, y=d_rural_pop_pct)) +
  geom_point(size=1.5, colour="orange") +
  geom_smooth(method="loess", colour="black") +
  geom_smooth(method="lm", colour="brown")+
  geom_text(aes(label=workset$Country), size=3)+
  labs(x="Aggricultural area pp change between 2000 and 2010", y="Rural population pp change between 2000 and 2010")

linreg_aggarea <- lm(d_rural_pop_pct ~ d_aggricarea_pct, data=workset)
summary(linreg_aggarea)

#control var: arrable area per person
ggplot(data = workset, aes(x=d_arrable_happrs, y=d_rural_pop_pct)) +
  geom_point(size=1.5, colour="orange") +
  geom_smooth(method="loess", colour="black") +
  geom_smooth(method="lm", colour="darkgoldenrod4")+
  geom_text(aes(label=workset$Country), size=3)+
  labs(x="Arrable ha per person change between 2000 and 2010", y="Rural population pp change between 2000 and 2010")

linreg_d_arrarea <- lm(d_rural_pop_pct ~ d_arrable_happrs, data=workset)
summary(linreg_d_arrarea)

#arrable land per person does not really change, log is useless, what if absolute size matters
ggplot(data = workset, aes(x=arrable_happrs_2000, y=d_rural_pop_pct)) +
  geom_point(size=1.5, colour="orange") +
  geom_smooth(method="loess", colour="black") +
  geom_smooth(method="lm", colour="darkgoldenrod3")+
  geom_text(aes(label=workset$Country), size=3)+
  labs(x="Arrable ha per person change between 2000 and 2010", y="Rural population pp change between 2000 and 2010")

linreg_abs_arrarea <- lm(d_rural_pop_pct ~ arrable_happrs_2000, data=workset)
summary(linreg_abs_arrarea)





#gdppc change analysis does not help
ggplot(data = workset, aes(x=d_gdppc, y=d_rural_pop_pct)) +
  geom_point(size=1.5, colour="orange") +
  geom_smooth(method="loess", colour="black") +
  geom_smooth(method="lm", colour="gold")+
  geom_text(aes(label=workset$Country), size=3)+
  labs(x="GDP per capita (constant 2010 US$) change between 2000 and 2010", y="Rural population pp change between 2000 and 2010")

linreg_d_gdppc <- lm(d_rural_pop_pct ~ d_gdppc, data=workset)
summary(linreg_d_gdppc)

#log gdppc
ggplot(data = workset, aes(x=log_d_gdppc, y=d_rural_pop_pct)) +
  geom_point(size=1.5, colour="orange") +
  geom_smooth(method="loess", colour="black") +
  geom_smooth(method="lm", colour="gold")+
  geom_text(aes(label=workset$Country), size=3)+
  labs(x="Log GDP per capita (constant 2010 US$) change between 2000 and 2010", y="Rural population pp change between 2000 and 2010")

linreg_log_d_gdppc <- lm(d_rural_pop_pct ~ log_d_gdppc, data=workset)
summary(linreg_log_d_gdppc)

#population density
ggplot(data = workset, aes(x=d_pop_dens_pha, y=d_rural_pop_pct)) +
  geom_point(size=1.5, colour="orange") +
  geom_smooth(method="loess", colour="black") +
  geom_smooth(method="lm", colour="firebrick4")+
  geom_text(aes(label=workset$Country), size=3)+
  labs(x="Change of population density per ha between 2000 and 2010", y="Rural population pp change between 2000 and 2010")

linreg_d_pop_dens_pha <- lm(d_rural_pop_pct ~ d_pop_dens_pha, data=workset)
summary(linreg_d_pop_dens_pha)

#log population density
ggplot(data = workset, aes(x=log_d_pop_dens_pha, y=d_rural_pop_pct)) +
  geom_point(size=1.5, colour="orange") +
  geom_smooth(method="loess", colour="black") +
  geom_smooth(method="lm", colour="firebrick4")+
  geom_text(aes(label=workset$Country), size=3)+
  labs(x="Log Change of population density per ha between 2000 and 2010", y="Rural population pp change between 2000 and 2010")

linreg_log_d_pop_dens_pha <- lm(d_rural_pop_pct ~ log_d_pop_dens_pha, data=workset)
summary(linreg_log_d_pop_dens_pha)

stargazer(list(linreg_deforest, 
               linreg_aggarea,
               linreg_d_arrarea,
               linreg_abs_arrarea,
               linreg_log_d_gdppc,
               linreg_log_d_pop_dens_pha), digits=3, out="ruralpop02.html")

mreg_deforest_aggarea <- lm(d_rural_pop_pct ~ d_forest_area_pct + d_aggricarea_pct, data=workset)
summary(mreg_deforest_aggarea)

mreg_deforest_aggarea_lngdppc <- lm(d_rural_pop_pct ~ d_forest_area_pct + 
                                      d_aggricarea_pct + 
                                      log_d_gdppc, data=workset)
summary(mreg_deforest_aggarea_lngdppc)

mreg_deforest_aggarea_lngdppc_logpopdens <- lm(d_rural_pop_pct ~ d_forest_area_pct + 
                                      d_aggricarea_pct + 
                                      log_d_gdppc+
                                      log_d_pop_dens_pha, data=workset)
summary(mreg_deforest_aggarea_lngdppc_logpopdens)

workset$isAfrica <- as.numeric(workset$Continent=="Africa")
workset$isAsia <- as.numeric(workset$Continent=="Asia")
workset$isOceania <-as.numeric(workset$Continent=="Oceania") 
workset$isCentralAmerica <-as.numeric(workset$Continent=="North America")
workset$isSouthAmerica <- as.numeric(workset$Continent=="South America")

mreg_deforest_continentdummy <- lm(d_rural_pop_pct ~ d_forest_area_pct + 
                                     isOceania + 
                                     isAsia + 
                                     isCentralAmerica + 
                                     isSouthAmerica,data=workset)
summary(mreg_deforest_continentdummy)

mreg_deforest_aggarea_lngdppc2 <- lm(d_rural_pop_pct ~ d_forest_area_pct + 
                                      d_aggricarea_pct + 
                                      log_d_gdppc+
                                      isOceania + 
                                      isAsia + 
                                      isCentralAmerica + 
                                      isSouthAmerica, data=workset)
summary(mreg_deforest_aggarea_lngdppc2)

mreg_deforest_aggarea_lngdppc3 <- lm(d_rural_pop_pct ~ d_forest_area_pct + 
                                       d_aggricarea_pct + 
                                       log_d_gdppc+
                                       d_aggricarea_pct * d_forest_area_pct + 
                                       log_d_gdppc * d_forest_area_pct
                                       , data=workset)
summary(mreg_deforest_aggarea_lngdppc3)


stargazer(list(linreg_deforest, 
               mreg_deforest_aggarea,
               mreg_deforest_aggarea_lngdppc), digits=3, out="ruralpop03multi.html")
