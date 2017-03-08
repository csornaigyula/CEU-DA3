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
    x='Countries',
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
myFDFunction <- function(lagSize){
  fd_panel <- panelData %>%
    group_by(country) %>%
    mutate(
      lag_hitechexp = lag(hitech_exp_pct,lagSize),
      lag_eduexp = lag(edu_exp_gdppct,lagSize)) %>%    
    filter(!is.na(lag_hitechexp)) %>%
    filter(!is.na(lag_eduexp)) %>%
    mutate(
      dhtech = hitech_exp_pct-lag_hitechexp,
      deduex = edu_exp_gdppct - lag_eduexp
    )
  return(fd_panel)
}

fd1_panel <- myFDFunction(1)
fd2_panel <- myFDFunction(2)
fd3_panel <- myFDFunction(3)
fd6_panel <- myFDFunction(6)

fd1 <-  lm(dhtech ~ deduex, data = fd1_panel)
fd2 <-  lm(dhtech ~ deduex, data = fd2_panel)
fd3 <-  lm(dhtech ~ deduex, data = fd3_panel)
fd6 <-  lm(dhtech ~ deduex, data = fd6_panel)

fewyd <- plm(hitech_exp_pct ~ edu_exp_gdppct + year, data = panelData, model = 'within')
getwd()
setwd("..\\temp\\rWorkDir\\da3")
stargazer(
  list(fd1, fd2, fd3, fd6,fewyd), digits = 2, 
  column.labels = c('FD', 'FD (lag=2)', 'FD (lag=3)', 'FD (lag=6)','FE w/ year dummies'),
  model.names = FALSE, dep.var.labels.include = FALSE, 
  dep.var.caption = 'Dependent variable: High-tech export% of all exports',
  single.row = TRUE,
  out="Models.html"
)

diff2 <- plm(
  diff(hitech_exp_pct) ~ diff(edu_exp_gdppct) + stats::lag(diff(edu_exp_gdppct), 1:2),
  data = panelData, model = 'pooling'
)

diff3 <- plm(
  diff(hitech_exp_pct) ~ diff(edu_exp_gdppct) + stats::lag(diff(edu_exp_gdppct), 1:3),
  data = panelData, model = 'pooling'
)

diff6 <- plm(
  diff(hitech_exp_pct) ~ diff(edu_exp_gdppct) + stats::lag(diff(edu_exp_gdppct), 1:6),
  data = panelData, model = 'pooling'
)


stargazer(
  list(fd1, diff2, diff3, diff6), type = 'text',
  column.labels = c('FD', 'FD with 2 lags', 'FD w/ 3 lags', 'FD w/ 6 lags'),
  omit = 'year', digits = 2,
  model.names = FALSE, dep.var.labels.include = FALSE, 
  dep.var.caption = 'Dependent variable: High-tech export% of all exports',
  single.row = TRUE,
  out="Models2.html"
)

stargazer(
  list(fd1, fd2, diff2, fd3, diff3, fd6, diff6), type = 'text',
  column.labels = c('FD', 'FD w/ 2 lags cum', 'FD w/ 2 lags', 'FD w/ 3 lags cum', 'FD w/ 3 lags', 'FD w/ 6 lags cum', 'FD w/ 6 lags'),
  omit = 'year', digits = 2,
  model.names = FALSE, dep.var.labels.include = FALSE, 
  dep.var.caption = 'Dependent variable: High-tech export% of all exports',
  single.row = TRUE,
  out="Models2B.html"
)


## 4- Confounders 


panelCountryVector<-unique(panelData$iso2c)
## Labor force with secondary education (% of total) (SL.TLF.SECO.ZS)
labfSE = WDI(country=panelCountryVector,
            indicator='SL.TLF.SECO.ZS', 
            start=1999, end=2012)
names(labfSE)[names(labfSE) == "SL.TLF.SECO.ZS"] = "labf_pct_edus"
dtLFSE <- data.table(labfSE)
panelCompletenessLSE <- dtLFSE[!is.na(dtLFSE$labf_pct_edus),
                                .(cntC = .N),
                                by=year]
## Only the half is usable

## Labor force with tertiary education (% of total) (SL.TLF.TERT.ZS)
labfTE = WDI(country=panelCountryVector,
             indicator='SL.TLF.TERT.ZS', 
             start=1999, end=2012)
names(labfTE)[names(labfTE) == "SL.TLF.TERT.ZS"] = "labf_pct_edut"
dtLFTE <- data.table(labfTE)
panelCompletenessLTE <- dtLFTE[!is.na(dtLFTE$labf_pct_edut),
                               .(cntC = .N),
                               by=year]
## Only the half is usable 

## Research and development expenditure (% of GDP) (GB.XPD.RSDV.GD.ZS)
## ~60 usable
rndExp = WDI(country=panelCountryVector,
             indicator='GB.XPD.RSDV.GD.ZS', 
             start=1999, end=2012)
names(rndExp)[names(rndExp) == "GB.XPD.RSDV.GD.ZS"] = "gdp_rnd_pct"
dtRNDP <- data.table(rndExp)
panelCompletenessRNDP <- dtRNDP[!is.na(dtRNDP$gdp_rnd_pct),
                               .(cntC = .N),
                               by=year]

## Researchers in R&D (per million people) (SP.POP.SCIE.RD.P6)
rndResPmp = WDI(country=panelCountryVector,
             indicator='SP.POP.SCIE.RD.P6', 
             start=1999, end=2012)
names(rndResPmp)[names(rndResPmp) == "SP.POP.SCIE.RD.P6"] = "rnd_res_pmp"
dtRndResPmp <- data.table(rndResPmp)
panelCompletenessRndResPmp <- dtRndResPmp[!is.na(dtRndResPmp$rnd_res_pmp),
                                .(cntC = .N),
                                by=year]

## Technicians in R&D (per million people) (SP.POP.TECH.RD.P6)
rndTechPmp = WDI(country=panelCountryVector,
                indicator='SP.POP.TECH.RD.P6', 
                start=1999, end=2012)
names(rndTechPmp)[names(rndTechPmp) == "SP.POP.TECH.RD.P6"] = "rnd_tech_pmp"
dtRndTechPmp <- data.table(rndTechPmp)
panelCompletenessRndTechPmp <- dtRndTechPmp[!is.na(dtRndTechPmp$rnd_tech_pmp),
                                          .(cntC = .N),
                                          by=year]


gigapanelRaw <- merge(panelData,dtLFSE,
                      by.x = c("iso2c", "year","country"), 
                      by.y = c("iso2c", "year", "country") )

gigapanelRaw <- merge(gigapanelRaw,dtLFTE,
                      by.x = c("iso2c", "year","country"), 
                      by.y = c("iso2c", "year", "country") ) 

gigapanelRaw <- merge(gigapanelRaw,dtRNDP,
                      by.x = c("iso2c", "year","country"), 
                      by.y = c("iso2c", "year", "country") )

gigapanelRaw <- merge(gigapanelRaw,rndResPmp,
                      by.x = c("iso2c", "year","country"), 
                      by.y = c("iso2c", "year", "country") )

gigapanelRaw <- merge(gigapanelRaw,rndTechPmp,
                      by.x = c("iso2c", "year","country"), 
                      by.y = c("iso2c", "year", "country") )
 
str(gigapanelRaw)

panelCompletenessGigaPanel <- gigapanelRaw[!is.na(gigapanelRaw$rnd_tech_pmp)&
                                              !is.na(gigapanelRaw$edu_exp_gdppct)&
                                              !is.na(gigapanelRaw$hitech_exp_pct)&
                                              !is.na(gigapanelRaw$labf_pct_edus)&
                                              !is.na(gigapanelRaw$labf_pct_edut)&
                                              !is.na(gigapanelRaw$gdp_rnd_pct)&
                                              !is.na(gigapanelRaw$rnd_res_pmp),
                                            .(cntC = .N),
                                            by=year] 

panelCompletenessGigaPanel2 <- gigapanelRaw[!is.na(gigapanelRaw$rnd_tech_pmp)&
                                             !is.na(gigapanelRaw$edu_exp_gdppct)&
                                             !is.na(gigapanelRaw$hitech_exp_pct)&
                                             !is.na(gigapanelRaw$labf_pct_edus)&
                                             !is.na(gigapanelRaw$labf_pct_edut)&
                                             !is.na(gigapanelRaw$gdp_rnd_pct)&
                                             !is.na(gigapanelRaw$rnd_res_pmp),
                                           .(cntY = .N),
                                           by=iso2c ]

gigaPanel <- subset(gigapanelRaw, 
                    gigapanelRaw$iso2c %in% 
                      panelCompletenessGigaPanel2[panelCompletenessGigaPanel2$cntY >= 10]$iso2c  )

myFDFunctionG <- function(lagSize){
  fd_panel <- gigaPanel %>%
    group_by(country) %>%
    mutate(
      lag_hitechexp = lag(hitech_exp_pct,lagSize),
      lag_rnd_tech_pmp = lag(rnd_tech_pmp,lagSize),
      lag_rnd_res_pmp = lag(rnd_res_pmp,lagSize),
      lag_labf_pct_edus = lag(labf_pct_edus,lagSize),
      lag_labf_pct_edut = lag(labf_pct_edut,lagSize),
      lag_gdp_rnd_pct = lag(gdp_rnd_pct,lagSize),
      lag_edu_exp_gdppct = lag(edu_exp_gdppct,lagSize)) %>%    
    filter(!is.na(lag_hitechexp)) %>%
    filter(!is.na(lag_rnd_tech_pmp)) %>%
    filter(!is.na(lag_rnd_res_pmp)) %>%
    filter(!is.na(lag_labf_pct_edus)) %>%
    filter(!is.na(lag_labf_pct_edut)) %>%
    filter(!is.na(lag_gdp_rnd_pct)) %>%
    filter(!is.na(lag_edu_exp_gdppct)) %>%
    mutate(
      dhitechexp = hitech_exp_pct-lag_hitechexp,
      dedu_exp_gdppct = edu_exp_gdppct - lag_edu_exp_gdppct,
      drnd_tech_pmp = rnd_tech_pmp - lag_rnd_tech_pmp,
      drnd_res_pmp = rnd_res_pmp - lag_rnd_tech_pmp,
      dlabf_pct_edus = labf_pct_edus - lag_labf_pct_edus,
      dlabf_pct_edut = labf_pct_edut - lag_labf_pct_edut,
      dgdp_rnd_pct = gdp_rnd_pct - lag_gdp_rnd_pct
    )
  return(fd_panel)
}

fd1_gpanel <- myFDFunctionG(1)
fd2_gpanel <- myFDFunctionG(2)
fd3_gpanel <- myFDFunctionG(3)
fd6_gpanel <- myFDFunctionG(6)

fd1g <-  lm(dhitechexp ~ dedu_exp_gdppct + drnd_tech_pmp + drnd_res_pmp +
              dlabf_pct_edus + dlabf_pct_edut + dgdp_rnd_pct
            , data = fd1_gpanel)
fd2g <-  lm(dhitechexp ~ dedu_exp_gdppct + drnd_tech_pmp + drnd_res_pmp +
              dlabf_pct_edus + dlabf_pct_edut + dgdp_rnd_pct
            , data = fd2_gpanel)

fd3g <-  lm(dhitechexp ~ dedu_exp_gdppct + drnd_tech_pmp + drnd_res_pmp +
              dlabf_pct_edus + dlabf_pct_edut + dgdp_rnd_pct
            , data = fd3_gpanel)

fd6g <-  lm(dhitechexp ~ dedu_exp_gdppct + drnd_tech_pmp + drnd_res_pmp +
              dlabf_pct_edus + dlabf_pct_edut + dgdp_rnd_pct
            , data = fd6_gpanel)

fewydg <- plm(hitech_exp_pct ~ edu_exp_gdppct + labf_pct_edus + labf_pct_edut +
                gdp_rnd_pct + rnd_res_pmp + rnd_tech_pmp + year, 
              data = gigaPanel, model = 'within')

stargazer(
  list(fd1g, fd2g, fd3g,  fd6g, fewydg), type = 'text',
  column.labels = c('FD', 'FD w/ 2 lags cum', 'FD w/ 3 lags cum',  'FD w/ 6 lags cum', 'FE'),
  digits = 2,
  model.names = FALSE, dep.var.labels.include = FALSE, 
  dep.var.caption = 'Dependent variable: High-tech export% of all exports',
  single.row = TRUE,
  out="Models3.html"
)


## High tech export volume USD
htexpUSD = WDI(country=panelCountryVector,
               indicator='TX.VAL.TECH.CD', 
               start=1999, end=2012)
names(htexpUSD)[names(htexpUSD) == "TX.VAL.TECH.CD"] = "ht_usd_level"
dtHTEUSD <- data.table(htexpUSD)
panelCompletenessHteUsd <- dtHTEUSD[!is.na(dtHTEUSD$ht_usd_level),
                                    .(cntC = .N),
                                    by=year]

panelData2 <-  merge(panelData, htexpUSD, 
                     by.x = c("iso2c", "year","country"), 
                     by.y = c("iso2c", "year", "country") )

panel2Completeness <- panelData2[ !is.na(panelData2$edu_exp_gdppct) & 
                                    !is.na(panelData2$ht_usd_level) ,
                                  .(cntY = .N),
                                  by=iso2c]

panelData2Final <- subset(panelData2, 
                          panelData2$iso2c %in% 
                            panel2Completeness[panel2Completeness$cntY >= 10]$iso2c  )
panelData2Final <- subset(panelData2Final, panelData2Final$ht_usd_level > 0)
panelData2Final$log_htusd = log(panelData2Final$ht_usd_level)

myFDFunctionNew <- function(lagSize){
  fd_panel <- panelData2Final %>%
    group_by(country) %>%
    mutate(
      lag_eduexp = lag(edu_exp_gdppct,lagSize), 
      lag_log_htusd = lag (log_htusd, lagSize))%>%    
    filter(!is.na(lag_log_htusd)) %>%
    filter(!is.na(lag_eduexp)) %>%
    mutate(
      deduex = edu_exp_gdppct - lag_eduexp,
      dloghtusd = log_htusd - lag_log_htusd
    )
  return(fd_panel)
}

fd1panel_new <- myFDFunctionNew(1)
fd1panel_new <- subset(fd1panel_new, !is.na(fd1panel_new$deduex) & !is.na(fd1panel_new$dloghtusd))
fd2panel_new <- myFDFunctionNew(2)
fd3panel_new <- myFDFunctionNew(3)
fd4panel_new <- myFDFunctionNew(4)
fd5panel_new <- myFDFunctionNew(5)
fd6panel_new <- myFDFunctionNew(6)

fdm1_new <-  lm(dloghtusd ~ deduex , data = fd1panel_new)
fdm2_new <-  lm(dloghtusd ~ deduex , data = fd2panel_new)
fdm3_new <-  lm(dloghtusd ~ deduex , data = fd3panel_new)
fdm4_new <-  lm(dloghtusd ~ deduex , data = fd4panel_new)
fdm5_new <-  lm(dloghtusd ~ deduex , data = fd5panel_new)
fdm6_new <-  lm(dloghtusd ~ deduex , data = fd6panel_new)

fewyd_new <- plm(log_htusd ~ edu_exp_gdppct +  year, 
              data = panelData2Final, model = 'within')

stargazer(fdm1_new,fdm2_new,fdm3_new,fdm4_new,fdm5_new,fdm6_new, fewyd_new, type = 'text',
            column.labels = c('FD', 'FD w/ 2 lags', 'FD w/ 3 lags', 'FD w/ 4 lags', 'FD w/ 5 lags', 'FD w/ 6 lags', 'FE'),
            digits = 2,
            model.names = FALSE, dep.var.labels.include = FALSE, 
            dep.var.caption = 'Dependent variable: Log(High-tech export value USD)',
            single.row = TRUE,
            out="Models5.html"
)



## High tech export USD TX.VAL.TECH.CD
## Expenditure on tertiary education (% of government expenditure on education) (SE.XPD.TERT.ZS)
## Expenditure on secondary education (% of government expenditure on education) (SE.XPD.SECO.ZS)
## Unemployment with secondary education (% of total unemployment) (SL.UEM.SECO.ZS)
## Unemployment with tertiary education (% of total unemployment) (SL.UEM.TERT.ZS)
## Trained teachers in upper secondary education (% of total teachers) (SE.SEC.TCAQ.UP.ZS) - No data

aggData2 <- panelData2Final[,
                     .(annualAvgEdu = mean(edu_exp_gdppct, na.rm = TRUE),
                       annualAvgHTexp = mean(log_htusd, na.rm = TRUE)),
                     by=year]




ggplot()+
  geom_line(data=aggData2, aes(x=year, y=annualAvgEdu), col='indianred')+
  geom_line(data=aggData2, aes(x=year, y=annualAvgHTexp), col='dodgerblue4')+
  labs(
    x='Years',
    y='Average of value',
    title='Trends of log high-tech export USD  and Government expenditure on education (GDP%) levels'
  )+
  geom_vline(aes(xintercept=2006),col='red',linetype='dotted')+
  geom_vline(aes(xintercept=2008),col='red',linetype='dotted')+
  geom_vline(aes(xintercept=2009),col='red',linetype='dotted')+
  geom_label(
    aes(x=min(aggData2$year) , y=mean(aggData2$annualAvgEdu)), 
    label='Average government expenditure\non education (GDP%)\nacross countries where data is available',
    nudge_x = nrow(aggData2)*0.5, 
    nudge_y=-2, size=4)+
  geom_label(
    aes(x=min(aggData2$year) , y=mean(aggData2$annualAvgHTexp)), 
    label='Average log high-tech export USD\nacross countries where data is available',
    nudge_x = nrow(aggData2)*0.5, 
    nudge_y=-2, size=4)+
  geom_label(
    aes(x=2006, y=max(aggData2$annualAvgHTexp +3)),
    label='2006',
    size=3)+
  geom_label(
    aes(x=2008, y=max(aggData2$annualAvgHTexp +3)),
    label='2008',
    size=3)+
  geom_label(
    aes(x=2009, y=max(aggData2$annualAvgHTexp +3)),
    label='2009',
    size=3)+
  xlim(min(aggData2$year), max(aggData2$year))+
  ylim(0, max(aggData2$annualAvgHTexp +3))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=-90, hjust=0.5, size=8))
aed <- mean(aggData2$annualAvgEdu)
aex <- mean(aggData2$annualAvgHTexp)
aggData2$annualAvgEdu <- aggData2$annualAvgEdu - aed
aggData2$annualAvgHTexp <- aggData2$annualAvgHTexp - aex

ggplot()+
  geom_line(data=aggData2, aes(x=year, y=annualAvgEdu), col='indianred')+
  geom_line(data=aggData2, aes(x=year, y=annualAvgHTexp), col='dodgerblue4')+
  labs(
    x='Years',
    y='Difference from the average',
    title='Trends of log high-tech export USD  and Government expenditure on education (GDP%)\ndifferences from average'
  )+
  geom_vline(aes(xintercept=2006),col='red',linetype='dotted')+
  geom_vline(aes(xintercept=2008),col='red',linetype='dotted')+
  geom_vline(aes(xintercept=2009),col='red',linetype='dotted')+
  geom_label(
    aes(x=min(aggData2$year) , y=mean(aggData2$annualAvgEdu)), 
    label='Average government expenditure\non education (GDP%)\nacross countries where data is available',
    nudge_x = nrow(aggData2)*0.5, 
    nudge_y=-0.5, size=3)+
  geom_label(
    aes(x=min(aggData2$year) , y=max(aggData2$annualAvgHTexp)), 
    label='Average log high-tech export USD\nacross countries where data is available',
    nudge_x = nrow(aggData2)*0.5, 
    nudge_y=0.5, size=3)+
  geom_label(
    aes(x=2006, y=max(aggData2$annualAvgHTexp +1)),
    label='2006',
    size=3)+
  geom_label(
    aes(x=2008, y=max(aggData2$annualAvgHTexp +1)),
    label='2008',
    size=3)+
  geom_label(
    aes(x=2009, y=max(aggData2$annualAvgHTexp +1)),
    label='2009',
    size=3)+
  xlim(min(aggData2$year), max(aggData2$year))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=-90, hjust=0.5, size=8))