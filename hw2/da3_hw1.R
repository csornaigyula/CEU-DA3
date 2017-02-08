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


