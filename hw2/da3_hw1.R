#install.packages('WDI')
rm(list = ls())
library(WDI)
library(data.table)

#TASK1
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

fullPanelData <- panelData
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
ggplot(panelData)+aes(x=year)+geom_bar()
#plot 2 main variables
library(ggplot2)
ggplot(panelData)+aes(x=GDPPC2005)+
  geom_histogram(fill='dodgerblue4')+
  labs(
    x="GDP per capita levels in all years constant 2005 prices",
    title="Histogram of GDP per capita distribution in years 1992-2016"
  )+
  theme_bw()

ggplot(panelData)+aes(x=CO2EMT)+
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


pan <- subset(panelData, panelData$year > 2003 & panelData$year < 2014)
ggplot(pan)+aes(x=GDPPC2005, y=CO2EMT)+
  geom_point(size=1, col='orange')+
  geom_smooth(method='lm')+
  facet_grid(year~.)

lpan <- subset(panelData, panelData$year > 2003 & panelData$year < 2014)
ggplot(lpan)+aes(x=lngdppc, y=lnco2em)+
  geom_point(size=1, col='orange')+
  geom_smooth(method='lm')+
  facet_grid(year~.)

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


#TASK2
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
      lag_co2 = lag(CO2EMT,lagSize)) %>%    
    filter(!is.na(lag_gdp)) %>%
    mutate(
      dlgdp = log(GDPPC2005)-log(lag_gdp),
      dlco2 = log(CO2EMT)-log(lag_co2))
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
single.row = TRUE,
out="Table1.html"
)

#TASK3
olsPanel1995 <- subset(panelData, panelData$year == 1995)
ols1995 <- lm(lnco2em ~ lngdppc,data = olsPanel1995)
olsPanel2007 <- subset(panelData, panelData$year == 2007)
ols2007 <- lm(lnco2em ~ lngdppc,data = olsPanel2007)
olsPanel2013 <- subset(panelData, panelData$year == 2013)
ols2013 <- lm(lnco2em ~ lngdppc,data = olsPanel2013)

#within is fixed effect estimation
fe <- plm(log(CO2EMT) ~ log(GDPPC2005), data = panelData, model = 'within')
#add year dummies to FE model
fewyd <- plm(log(CO2EMT) ~ log(GDPPC2005) + year, data = panelData, model = 'within')

#we have good data until 2013 only
ld_panel <- panelData %>%
  filter(panelData$year==1992 | panelData$year==2013) %>%  
  group_by(country) %>%
    mutate(
      lag_gdp = lag(GDPPC2005),
      lag_co2 = lag(CO2EMT)) %>%    
    filter(!is.na(lag_gdp)) %>%
    mutate(
      dlgdp = log(GDPPC2005)-log(lag_gdp),
      dlco2 = log(CO2EMT)-log(lag_co2))

ld <-  lm(dlco2 ~ dlgdp, data = ld_panel)

stargazer(
  list(fd3, ols1995, ols2007, ols2013, fe, fewyd, ld), digits = 2, 
  column.labels = c('FD (lag=3)', 'OLS1995', 'OLS2007', 'OLS2013','FE', 'FE w/ year dummies', 'LD'),
  model.names = FALSE, dep.var.labels.include = FALSE, 
  dep.var.caption = 'Dependent variable: log(CO2)',omit = "Constant",
  single.row = TRUE,
  out="Table2.html"
)

#TASK4 interpret
#TASK5 pick and justify

#TASK6
fullPanelData$CO2EMT <- fullPanelData$CO2EM*1000
fullPanelData$lnco2em <- log(fullPanelData$CO2EMT)
fullPanelData$lngdppc <- log(fullPanelData$GDPPC2005)
olsFullPanel2007 <- subset(fullPanelData, fullPanelData$year == 2007)

olsFull2007 <- lm(lnco2em ~ lngdppc,data = olsPanel2007)
feFull <- plm(log(CO2EMT) ~ log(GDPPC2005), data = fullPanelData, model = 'within')
fd3Full_panel <- fullPanelData %>%
  group_by(country) %>%
  mutate(
    lag_gdp = lag(GDPPC2005,3),
    lag_co2 = lag(CO2EMT,3)) %>%    
  filter(!is.na(lag_gdp)) %>%
  mutate(
    dlgdp = log(GDPPC2005)-log(lag_gdp),
    dlco2 = log(CO2EMT)-log(lag_co2))
fd3Full <-  lm(dlco2 ~ dlgdp, data = fd3Full_panel)

ldFull_panel <- fullPanelData %>%
  filter(fullPanelData$year==1992 | fullPanelData$year==2013) %>%  
  group_by(country) %>%
  mutate(
    lag_gdp = lag(GDPPC2005),
    lag_co2 = lag(CO2EMT)) %>%    
  filter(!is.na(lag_gdp)) %>%
  mutate(
    dlgdp = log(GDPPC2005)-log(lag_gdp),
    dlco2 = log(CO2EMT)-log(lag_co2))

ldFull <-  lm(dlco2 ~ dlgdp, data = ldFull_panel)


stargazer(
  list(olsFull2007, feFull, fd3Full, ldFull ), digits = 2, 
  column.labels = c('OLS2007', 'FE', 'FD(3)', 'LD'),
  model.names = FALSE, dep.var.labels.include = FALSE, 
  dep.var.caption = 'Dependent variable: log(CO2)',omit = "Constant",
  single.row = TRUE,
  out="Table3.html"
)

#compare, discuss

#TASK7 confounder
#Industry, value added (% of GDP) (NV.IND.TOTL.ZS)
industryPctCode <- "NV.IND.TOTL.ZS"
inddat = WDI(
  indicator=industryPctCode, 
  start=1992, end=2016)
indDT <- data.table(inddat)
indGDPPctData <- subset(indDT, !(indDT$iso2c %in%  exclusionList$code))
summary(indGDPPctData)
names(indGDPPctData)[names(indGDPPctData) == industryPctCode] = "IndPct"
extendedFullPanelData2 <-merge(fullPanelData, indGDPPctData, 
                              by.x = c("iso2c", "year","country"), 
                              by.y = c("iso2c", "year", "country") )

head(extendedFullPanelData2)
panelCompleteness3 <- extendedFullPanelData2[!is.na(extendedFullPanelData2$CO2EM) & 
                                              !is.na(extendedFullPanelData2$GDPPC2005) &
                                              !is.na(extendedFullPanelData2$IndPct),
                                            .(cntY = .N),
                                            by=country]

countryInclusion3 <- panelCompleteness3[cntY>20]
panelData2 <- subset(extendedFullPanelData2,(extendedFullPanelData2$country %in% countryInclusion$country))


panelData2$CO2EMT <- panelData2$CO2EM*1000
panelData2$lnco2em <- log(panelData2$CO2EMT)
panelData2$lngdppc <- log(panelData2$GDPPC2005)
olsConfPanel2007 <- subset(panelData2, panelData2$year == 2007)

olsConf2007 <- lm(lnco2em ~ lngdppc + IndPct,data = olsConfPanel2007)
feConf <- plm(log(CO2EMT) ~ log(GDPPC2005) + IndPct, data = panelData2, model = 'within')
fd3ConfPanel <- panelData2 %>%
  group_by(country) %>%
  mutate(
    lag_gdp = lag(GDPPC2005,3),
    lag_co2 = lag(CO2EMT,3),
    lag_indpct = lag(IndPct,3)) %>%    
  filter(!is.na(lag_gdp)) %>%
  mutate(
    dlgdp = log(GDPPC2005)-log(lag_gdp),
    dlco2 = log(CO2EMT)-log(lag_co2),
    dindpct = IndPct - lag_indpct)
fd3Conf <-  lm(dlco2 ~ dlgdp + dindpct, data = fd3ConfPanel)

ldConfPanel <- panelData2 %>%
  filter(panelData2$year==1992 | panelData2$year==2013) %>%  
  group_by(country) %>%
  mutate(
    lag_gdp = lag(GDPPC2005),
    lag_co2 = lag(CO2EMT),
    lag_indpct = lag(IndPct)) %>%    
  filter(!is.na(lag_gdp)) %>%
  mutate(
    dlgdp = log(GDPPC2005)-log(lag_gdp),
    dlco2 = log(CO2EMT)-log(lag_co2),
    dindpct = IndPct - lag_indpct)

ldConf <-  lm(dlco2 ~ dlgdp + dindpct, data = ldConfPanel)


stargazer(
  list(olsConf2007, feConf , fd3Conf, ldConf ), digits = 2, 
  column.labels = c('OLS2007', 'FE', 'FD(3)', 'LD'),
  model.names = FALSE, dep.var.labels.include = FALSE, 
  dep.var.caption = 'Dependent variable: log(CO2)',omit = "Constant",
  single.row = TRUE,
  out="Table4.html"
)

#TASK8
#2007 groups
panel2007 <- subset(panelData,panelData$year==2007)
ggplot(panel2007)+
  aes(x=GDPPC2005, y=CO2EMT)+
  geom_point()+
  geom_smooth(method='lm')+
  geom_smooth(method='loess', col='indianred')+
  theme_bw()
meanGDPPC2007 <- mean(panel2007$GDPPC2005)
panel2007$ccat <- ifelse(panel2007$GDPPC2005 < 30000,0,1)

sgPanelData <- merge(panelData, panel2007,
                     by.x = c("iso2c","country"), 
                     by.y = c("iso2c", "country"))

sgPanelfDf <- data.frame(sgPanelData)
head(sgPanelData)
sgPanelData$year.y <- NULL
sgPanelData$GDPPC2005.y <- NULL
sgPanelData$CO2EM.y <- NULL
sgPanelData$CO2EM.x <- NULL
sgPanelData$CO2EMT.y <- NULL
sgPanelData$lnco2em.y <- NULL
sgPanelData$lngdppc.y <- NULL
head(sgPanelData)
names(sgPanelData)[names(sgPanelData) == "year.x"] = "year"
names(sgPanelData)[names(sgPanelData) == "GDPPC2005.x"] = "GDPPC2005"
names(sgPanelData)[names(sgPanelData) == "CO2EMT.x"] = "CO2EMT"
names(sgPanelData)[names(sgPanelData) == "lnco2em.x"] = "lnco2em"
names(sgPanelData)[names(sgPanelData) == "lngdppc.x"] = "lngdppc"
head(sgPanelData)

sgPanelData0 <- subset(sgPanelData,sgPanelData$ccat==0)
sgPanelData0$ccat <- NULL
sgPanelData1 <- subset(sgPanelData,sgPanelData$ccat==1)
sgPanelData1$ccat <- NULL


head(sgPanelData0)
head(panelData)
olsSG0 <- lm(lnco2em ~ lngdppc,data = sgPanelData0)
#feSG0 <- plm(log(CO2EMT) ~ log(GDPPC2005), data = panelData, model = 'within')
#throws error, i have no idea why
panelfd3SG0 <- sgPanelData0 %>%
  group_by(country) %>%
  mutate(
    lag_gdp = lag(GDPPC2005,3),
    lag_co2 = lag(CO2EMT,3)) %>%    
  filter(!is.na(lag_gdp)) %>%
  mutate(
    dlgdp = log(GDPPC2005)-log(lag_gdp),
    dlco2 = log(CO2EMT)-log(lag_co2))
fd3SG0 <-  lm(dlco2 ~ dlgdp, data = panelfd3SG0)

panelldSG0 <- sgPanelData0 %>%
  filter(sgPanelData0$year==1992 | sgPanelData0$year==2013) %>%  
  group_by(country) %>%
  mutate(
    lag_gdp = lag(GDPPC2005),
    lag_co2 = lag(CO2EMT)) %>%    
  filter(!is.na(lag_gdp)) %>%
  mutate(
    dlgdp = log(GDPPC2005)-log(lag_gdp),
    dlco2 = log(CO2EMT)-log(lag_co2))

ldSG0 <-  lm(dlco2 ~ dlgdp, data = panelldSG0)
################
olsSG0 <- lm(lnco2em ~ lngdppc,data = sgPanelData0)
#feSG0 <- plm(log(CO2EMT) ~ log(GDPPC2005), data = panelData, model = 'within')
#throws error, i have no idea why
panelfd3SG1 <- sgPanelData1 %>%
  group_by(country) %>%
  mutate(
    lag_gdp = lag(GDPPC2005,3),
    lag_co2 = lag(CO2EMT,3)) %>%    
  filter(!is.na(lag_gdp)) %>%
  mutate(
    dlgdp = log(GDPPC2005)-log(lag_gdp),
    dlco2 = log(CO2EMT)-log(lag_co2))
fd3SG1 <-  lm(dlco2 ~ dlgdp, data = panelfd3SG1)

panelldSG1 <- sgPanelData1 %>%
  filter(sgPanelData1$year==1992 | sgPanelData1$year==2013) %>%  
  group_by(country) %>%
  mutate(
    lag_gdp = lag(GDPPC2005),
    lag_co2 = lag(CO2EMT)) %>%    
  filter(!is.na(lag_gdp)) %>%
  mutate(
    dlgdp = log(GDPPC2005)-log(lag_gdp),
    dlco2 = log(CO2EMT)-log(lag_co2))

ldSG1 <-  lm(dlco2 ~ dlgdp, data = panelldSG1)

stargazer(
  list(olsSG0, olsSG1, fd3SG0, fd3SG1 , ldSG0, ldSG1 ), digits = 2, 
  column.labels = c('OLS2007poor', 'OLS2007rich', 'FD3poor', 'FD3rich', 'LDpoor', 'LDrich'),
  model.names = FALSE, dep.var.labels.include = FALSE, 
  dep.var.caption = 'Dependent variable: log(CO2)',omit = "Constant",
  single.row = TRUE,
  out="Table5.html"
)