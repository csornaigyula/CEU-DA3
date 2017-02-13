#install.packages('WDI')
rm(list = ls())
library(WDI)
library(data.table)
#look for GDP related data I can potentially use
gdp_inds <- WDIsearch('gdp')
grep("2005", gdp_inds, value = TRUE )
#identify unique identified
gdpppCode <- gdp_inds[match("GDP per capita, PPP (constant 2005 international $)",gdp_inds[,2]),1]
#getting data
dat = WDI(
  indicator=gdpppCode, 
  start=1992, end=2016)
#full of regional aggregated garbage data
dt <- data.table(dat)
#filtering down the 47 non-country data from the beginning of the data frame
exclusionList <-dt[,.(itemCnt=.N),by=.(code = dt$iso2c)][1:47,1]
gdpData <- subset(dt, !(dt$iso2c %in%  exclusionList$code))

#collecting CO2 data
co2_inds <- WDIsearch('co2')
#CO2 emissions (metric tons per capita)
#CO2 emissions (kt)
co2code <- co2_inds[match("CO2 emissions (metric tons per capita)", co2_inds[,2]),1]
co2dat = WDI(
  indicator=co2code, 
  start=1992, end=2016)
co2dt <- data.table(co2dat)
co2Data <- subset(co2dt, !(co2dt$iso2c %in%  exclusionList$code))

#merging, with multiple key match
panelData <-merge(gdpData, co2Data, 
                  by.x = c("iso2c", "year","country"), 
                  by.y = c("iso2c", "year", "country") )

#renaming columns
names(panelData)[names(panelData) == "NY.GDP.PCAP.PP.KD"] = "GDPPC2005"
names(panelData)[names(panelData) == "EN.ATM.CO2E.PC"] = "CO2EM"

#checking for countries with almost full coverage
panelCompleteness <- panelData[!is.na(panelData$CO2EM) & !is.na(panelData$GDPPC2005),
                               .(cntY = .N),
                               by=country]
#definition of almost full
countryInclusion <- panelCompleteness[cntY>21]
#panelDataOriginal <- panelData
panelData <- subset(panelData,(panelData$country %in% countryInclusion$country))

#descriptive statistics
summary(panelData)

#plot 2 main variables
library(ggplot2)
ggplot(panelData)+aes(x=GDPPC2005)+
  geom_histogram(fill='dodgerblue4')+
  labs(
    x="GDP per capita levels in all years constant 2005 prices",
    title="Histogram of GDP per capita distribution in years 1992-2016"
  )+
  theme_bw()

ggplot(panelData)+aes(x=CO2EM)+
  geom_histogram(fill='indianred')+
  labs(
    x="CO2 emission levels in all years",
    title="Histogram of CO2 emission in metric kilotons 1992-2016"
  )+
  theme_bw()
#transformations - both skewed, need logs
#metric kilotons will produce more than 25% of values below 1, use metric tons instead
panelData$CO2EMT <- panelData$CO2EM*1000
panelData$lnco2em <- log(panelData$CO2EMT)
panelData$lngdppc <- log(panelData$GDPPC2005)

ggplot(panelData)+aes(x=lngdppc)+
  geom_histogram(fill='dodgerblue4')+
  labs(
    x="GDP per capita levels in all years constant 2005 prices",
    title="Histogram of lnGDP per capita distribution in years 1992-2016"
  )+
  theme_bw()

ggplot(panelData)+aes(x=lnco2em)+
  geom_histogram(fill='indianred')+
  labs(
    x="CO2 emission levels in all years",
    title="Histogram of ln of CO2 emission in metric tons 1992-2016"
  )+
  theme_bw()
#ordered list of country data by country code first and then year
panelData[order(panelData$iso2c,panelData$year)]

#to use Mr Baksa's code
#install.packages('tidyr')
#lag gives the previous instance only, not the difference, from dplyr package
library(tidyr)
library(dplyr)
#install.packages('plm')
#this needs no megic on a sorted set
library(plm)
fd1 <- plm(log(CO2EMT) ~ log(GDPPC2005), data = panelData, model = 'fd')
#?plm
myFDFunction <- function(lagSize){
  fd_panel <- panelData %>%
    group_by(country) %>%
    mutate(
      lag_gdp = lag(GDPPC2005,lagSize),
      lag_co2 = lag(CO2EM,lagSize)) %>%    
    filter(!is.na(lag_gdp)) %>%
    mutate(
      dlgdp = log(GDPPC2005)-log(lag_gdp),
      dlco2 = log(CO2EM)-log(lag_co2))
  return(fd_panel)
}
fd1_panel <- myFDFunction(1)
fd2_panel <- myFDFunction(2)
fd3_panel <- myFDFunction(3)
fd6_panel <- myFDFunction(6)

fd1A <-  lm(dlco2 ~ dlgdp, data = fd1_panel)
fd2 <-  lm(dlco2 ~ dlgdp, data = fd2_panel)
fd3 <-  lm(dlco2 ~ dlgdp, data = fd3_panel)
fd6 <-  lm(dlco2 ~ dlgdp, data = fd6_panel)

library(stargazer)
stargazer(
  list(fd1, fd1A, fd2, fd3, fd6), digits = 2, 
column.labels = c('FD (lag=1)', 'FD (lag=1, crosscheck)', 'FD (lag=2)', 'FD (lag=3)', 'FD (lag=6)'),
model.names = FALSE, dep.var.labels.include = FALSE, 
dep.var.caption = 'Dependent variable: log(CO2)',omit = "Constant",
out="firstDifferenceModels.html"
)


  


?lag


