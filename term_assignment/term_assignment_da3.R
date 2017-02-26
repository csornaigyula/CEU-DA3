rm(list = ls())
library(WDI)
library(data.table)
library(ggplot2)
library(pander)
library(stargazer)
library(tidyr)
library(dplyr)
library(plm)

###########################################################################
# Does higher expenditure on education result higher high-tech export?
############################################################################

## STEP1: Acquiring data
## High-technology exports (% of manufactured exports) (TX.VAL.TECH.MF.ZS)
## Getting high tech data
higtechCode <- "TX.VAL.TECH.MF.ZS"
htdat0 = WDI(country='all',
  indicator=higtechCode, 
  start=2016, end=2016)
notcountryVector <- htdat0[1:47,1]
countryVector <- htdat0[48:264,1]

htdat = WDI(country=countryVector,
             indicator=higtechCode, 
             start=1992, end=2016)
names(htdat)[names(htdat) == "TX.VAL.TECH.MF.ZS"] = "hitech_exp_pct"

hdt <- data.table(htdat)

## Government expenditure on education, total (% of GDP) (SE.XPD.TOTL.GD.ZS)
eduCode <- 'SE.XPD.TOTL.GD.ZS'
edudat <- WDI(country=countryVector,
              indicator=eduCode, 
              start=1992, end=2016)
names(edudat)[names(edudat) == "SE.XPD.TOTL.GD.ZS"] = "edu_exp_gdppct"
edt <- data.table(edudat)

## STEP2: Data munging

## creating panel data only from the 2 variables
panel2var <- merge(edt, hdt, 
                   by.x = c("iso2c", "year","country"), 
                   by.y = c("iso2c", "year", "country") )

## checking for countries with almost full coverage
panelCompleteness <- panel2var[!is.na(panel2var$edu_exp_gdppct) & !is.na(panel2var$hitech_exp_pct),
                               .(cntY = .N),
                               by=iso2c]

ggplot(panelCompleteness)+
  aes(x=iso2c, y=cntY)+
  geom_col(fill='goldenrod1', col='goldenrod3')+
  labs(
    x='Counrty codes',
    y='Number of years\nwhere both data elements are available',
    title='Panel completeness - countries',
    subtitle='Analysis on how balanced the data is between 1992 and 2016 looking on all countries'
  )+
  geom_hline(aes(yintercept=mean(panelCompleteness$cntY)),col='red')+
  geom_label(
    aes(x=0 , y=mean(panelCompleteness$cntY)), 
             label=
               paste(
                 'Average number of\navailable years =',
                 as.character(floor(mean(panelCompleteness$cntY))),
                 sep=' '
                 ),
             nudge_x = nrow(panelCompleteness)*0.2, 
             nudge_y=0, size=5)+
  theme_bw()+
  theme(axis.text.x=element_text(angle=-90, hjust=0.5, size=5))

ggplot(panelCompleteness)+
  aes(x=cntY)+
  geom_histogram(bins=10, fill='forestgreen', col='darkgreen')+
  labs(
    x='Number of years available',
    y='Number of countries',
    title='Panel balance histogram for years 1992-2016'
  )+
  theme_bw()

panelCompletenessY <- panel2var[!is.na(panel2var$edu_exp_gdppct) & !is.na(panel2var$hitech_exp_pct),
                               .(cntC = .N),
                               by=year]
panelCompletenessY$year <- as.factor(panelCompletenessY$year)

ggplot(panelCompletenessY)+
  aes(x=year, y=cntC)+
  geom_col(fill='goldenrod1', col='goldenrod3')+
  labs(
    x='Years',
    y='Number of countries\nwhere both data elements are available',
    title='Panel balance - years'
  )+
  geom_hline(aes(yintercept=mean(panelCompletenessY$cntC)),col='red')+
  geom_label(
    aes(x=0 , y=mean(panelCompletenessY$cntC)), 
    label=
      paste(
        'Average number of\navailable countries =',
        as.character(floor(mean(panelCompletenessY$cntC))),
        sep=' '
      ),
    nudge_x = nrow(panelCompletenessY)*0.2, 
    nudge_y=0, size=5)+
  theme_bw()+
  theme(axis.text.x=element_text(angle=-90, hjust=0.5, size=8))

panel2var2k <- subset(panel2var, 
                      panel2var$year >1998 & panel2var$year < 2013)

panelCompleteness2k <- panel2var2k[!is.na(panel2var2k$edu_exp_gdppct) & !is.na(panel2var2k$hitech_exp_pct),
                               .(cntY = .N),
                               by=iso2c]

ggplot(panelCompleteness2k)+
  aes(x=iso2c, y=cntY)+
  geom_col(fill='goldenrod1', col='goldenrod3')+
  labs(
    x='Counrty codes',
    y='Number of years\nwhere both data elements are available',
    title='Panel completeness - countries\nbetween 1999 and 2012'
  )+
  geom_hline(aes(yintercept=mean(panelCompleteness2k$cntY)),col='red')+
  geom_label(
    aes(x=0 , y=mean(panelCompleteness2k$cntY)), 
    label=
      paste(
        'Average number of\navailable years =',
        as.character(floor(mean(panelCompleteness2k$cntY))),
        sep=' '
      ),
    nudge_x = nrow(panelCompleteness2k)*0.1, 
    nudge_y=0, size=3)+
  theme_bw()+
  theme(axis.text.x=element_text(angle=-90, hjust=0.5, size=4))

length(panelCompleteness2k[panelCompleteness2k$cntY < 10]$iso2c)
length(panelCompleteness2k[panelCompleteness2k$cntY < 9]$iso2c)

panelCompleteness2k[panelCompleteness2k$cntY >= 10]$iso2c 

panelData <- subset(panel2var2k, 
                    panel2var2k$iso2c %in% 
                      panelCompleteness2k[panelCompleteness2k$cntY >= 10]$iso2c  )

panelCompletenessFinal  <- panelData[!is.na(panelData$edu_exp_gdppct) & !is.na(panelData$hitech_exp_pct),
                                                             .(cntY = .N),
                                                             by=country]

ggplot(panelCompletenessFinal)+
  aes(x=country, y=cntY)+
  geom_col(fill='forestgreen', col='darkgreen')+
  labs(
    x='Years',
    y='Number of countries\nwhere both data elements are available',
    title='Panel completeness in years 1999-2012 in countries where we have at least 10 observations'
  )+
  geom_hline(aes(yintercept=mean(panelCompletenessFinal$cntY)),col='red')+
  geom_label(
    aes(x=0 , y=mean(panelCompletenessFinal$cntY)), 
    label=
      paste(
        'Average number of\navailable years =',
        as.character(floor(mean(panelCompletenessFinal$cntY))),
        sep=' '
      ),
    nudge_x = nrow(panelCompletenessFinal)*0.5, 
    nudge_y=0, size=5)+
  theme_bw()+
  theme(axis.text.x=element_text(angle=-90, hjust=0.5, size=8))

## 2.1 trending

## 2.1.1 Global trends

aggData <- panelData[,
                     .(annualAvgEdu = mean(edu_exp_gdppct, na.rm = TRUE),
                       annualAvgHTexp = mean(hitech_exp_pct, na.rm = TRUE)),
                     by=year]



ggplot()+
  geom_line(data=aggData, aes(x=year, y=annualAvgEdu), col='indianred')+
  geom_line(data=aggData, aes(x=year, y=annualAvgHTexp), col='dodgerblue4')+
  labs(
    x='Years',
    y='Percentage of marker',
    title='Trends of high-tech export %  and Government expenditure on education (GDP%) levels'
  )+
  geom_vline(aes(xintercept=2006),col='red',linetype='dotted')+
  geom_vline(aes(xintercept=2008),col='red',linetype='dotted')+
  geom_vline(aes(xintercept=2009),col='red',linetype='dotted')+
  geom_label(
    aes(x=min(aggData$year) , y=mean(aggData$annualAvgEdu)), 
    label='Average government expenditure\non education (GDP%)\nacross selected 81 countries',
    nudge_x = nrow(aggData)*0.5, 
    nudge_y=-2, size=4)+
  geom_label(
    aes(x=min(aggData$year) , y=mean(aggData$annualAvgHTexp)), 
    label='Average high-tech export %\nacross selected 81 countries',
    nudge_x = nrow(aggData)*0.5, 
    nudge_y=-2, size=4)+
  geom_label(
    aes(x=2006, y=max(aggData$annualAvgHTexp +3)),
    label='2006',
    size=3)+
  geom_label(
    aes(x=2008, y=max(aggData$annualAvgHTexp +3)),
    label='2008',
    size=3)+
  geom_label(
    aes(x=2009, y=max(aggData$annualAvgHTexp +3)),
    label='2009',
    size=3)+
  xlim(min(aggData$year), max(aggData$year))+
  ylim(0, max(aggData$annualAvgHTexp +3))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=-90, hjust=0.5, size=8))

## 2.1.2 Trends in cherry-picked countries
cherryData <- subset(panelData, 
                     panelData$iso2c %in% c('CY', 'BY', 'SK', 'HU', 
                                            'US', 'KR', 'FI', 'SG', 'JP'))
str(cherryData)

ggplot()+
  geom_line(data=cherryData, aes(x=year, y=edu_exp_gdppct), 
            col='indianred', na.rm = TRUE)+
  geom_line(data=cherryData, aes(x=year, y=hitech_exp_pct), 
            col='dodgerblue4', na.rm = TRUE)+
  facet_wrap(~cherryData$country)+
  labs(
    x='Years',
    y='Percentage of marker',
    title='Trends of high-tech export %  and Government expenditure on education (GDP%) levels',
    subtitle='RED: Government expenditure on education (GDP%) across selected 81 countries\nBLUE: High-tech export %'
  )+
  xlim(min(cherryData$year), max(cherryData$year))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=-90, hjust=0.5, size=8))

## 3 change analysis
 