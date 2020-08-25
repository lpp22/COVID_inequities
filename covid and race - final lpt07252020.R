rm(list = ls(all = T))

#####################################
##################################### obtaining covid, census, brfss, testing, mortality, hosptial beds data 
#####################################
# source of preprocessing code: Preprocessing.R data from https://github.com/wxwx1993/PM_COVID
library(dplyr)
library(stringr)
library(RCurl)
library(tigris)
library(spdep)   
library(rgdal)   
library(maptools)
library(sp) 
library(arm)
library(raster)
library(rgeos)
library(RColorBrewer)
library(classInt)
library(INLA)
library(spdep)
library(usdm)
library(ggplot2)
library(usmap)

# Historical data
covid_hist = read.csv(text=getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-30-2020.csv"))
covid_us_hist = subset(covid_hist, Country_Region == "US" & is.na(FIPS)==F)

# Import outcome data from JHU CSSE
covid = read.csv(text=getURL(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/04-30-2020.csv")))
covid_us = subset(covid,Country_Region == "US" & is.na(FIPS)!=T)
covid_us = rbind(covid_us,subset(covid_us_hist, (!(FIPS %in% covid_us$FIPS))  & Confirmed == 0 & Deaths == 0 & is.na(FIPS)==F))

# Import census and mortality data as potential confounders
county_census = read.csv(text=getURL("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/census_county_interpolated.csv"))

county_base_mortality = read.table(text=getURL("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/county_base_mortality.txt"), sep = "",header = T)
county_old_mortality = read.table(text=getURL("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/county_old_mortality.txt"), sep = "",header = T)
colnames(county_old_mortality)[4] = c("older_Population")
county_base_mortality = merge(county_base_mortality,county_old_mortality[,c(2,4)] ,by = "County.Code")
county_base_mortality$older_pecent = county_base_mortality$older_Population/county_base_mortality$Population


county_census_aggregated2 = subset(county_census, year==2018)
county_census_aggregated2$q_popdensity = 1
quantile_popdensity = quantile(county_census_aggregated2$popdensity,c(0.25,0.5,0.75))
county_census_aggregated2$q_popdensity[county_census_aggregated2$popdensity<=quantile_popdensity[1]] = 1
county_census_aggregated2$q_popdensity[county_census_aggregated2$popdensity>quantile_popdensity[1] &
                                         county_census_aggregated2$popdensity<=quantile_popdensity[2]] = 2
county_census_aggregated2$q_popdensity[county_census_aggregated2$popdensity>quantile_popdensity[2] &
                                         county_census_aggregated2$popdensity<=quantile_popdensity[3]] = 3
county_census_aggregated2$q_popdensity[county_census_aggregated2$popdensity>quantile_popdensity[3]] = 4


# merge covid data with county census data
covid_census = merge(covid_us,county_census_aggregated2,by.x="FIPS",by.y = "fips")

# merge in county base mortality
covid_census_cdc = merge(covid_census,county_base_mortality[,c(1,4:5,8:9)],by.x = "FIPS",by.y = "County.Code",all.x = T)

# take out regions without a fips code
covid_census_cdc = covid_census_cdc[is.na(covid_census_cdc$FIPS) == F,]

names(covid_census_cdc)
dim(covid_census_cdc)

# creating a NY only covid dataset
covid_census_cdc.ny <- covid_census_cdc[covid_census_cdc$Province_State == "New York",]

# creating a dataset without NY
covid_census_cdc.exclude.ny <- covid_census_cdc[covid_census_cdc$Province_State != "New York",]

#####################################
##################################### merging covid data with shapefile for US counties (and for NY only)
#####################################
# census county shapefile
# source: https://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2018_us_county_5m.zip
usb <- st_read("~/Dropbox/Research/COVID/COVID-19/Data/cb_2018_us_county_5m/cb_2018_us_county_5m.shp")
usb$FIPS <- as.numeric(paste(usb$STATEFP,usb$COUNTYFP,sep=''))
usb <- usb[(usb$FIPS %in% covid_census_cdc$FIPS), ]

#usb.ny <- counties(state = "NY") # FIPS code is 36
#usb.ny$FIPS <- as.numeric(paste(usb.ny$STATEFP,usb.ny$COUNTYFP,sep=''))
usb.ny <- usb[usb$STATEFP == "36", ]

usb.exclude.ny <- usb[(usb$STATEFP != "36"), ]

# various projections: https://www.r-bloggers.com/map-projections-for-r-shapefiles-united-states/

# Albers Equal Area Projection
#us <- spTransform(usb, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
#us.ny <- spTransform(usb.ny, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
#us.exclude.ny <- spTransform(usb.exclude.ny, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
#plot(us,axes=TRUE)
#plot(us.ny,axes=TRUE)
#plot(usb.exclude.ny,axes=TRUE)

# order both the shapefile and the covid dataset by the FIPS code
us <- usb[order(usb$FIPS),]
us.ny <- usb.ny[order(usb.ny$FIPS),]
us.exclude.ny <- usb.exclude.ny[order(usb.exclude.ny$FIPS),]

covid_census_cdc <- covid_census_cdc[order(covid_census_cdc$FIPS),]
covid_census_cdc.ny <- covid_census_cdc.ny[order(covid_census_cdc.ny$FIPS),]
covid_census_cdc.exclude.ny <- covid_census_cdc.exclude.ny[order(covid_census_cdc.exclude.ny$FIPS),]

# merging county shapefile with covid dataset
us.covid_census_cdc <- left_join(us, covid_census_cdc, by = c("FIPS", "FIPS"))
us.ny.covid_census_cdc <- left_join(us.ny, covid_census_cdc.ny, by = c("FIPS", "FIPS"))
us.exclude.ny.covid_census_cdc <- left_join(us.exclude.ny, covid_census_cdc.exclude.ny, by = c("FIPS", "FIPS"))


#####################################
##################################### checking for multicollinearity (race + confounders)
#####################################
# entire US
M0 <- as.data.frame(x = cbind(pct_blk = covid_census_cdc$pct_blk, hispanic=covid_census_cdc$hispanic, poverty=covid_census_cdc$poverty, popdensity = covid_census_cdc$popdensity, medianhousevalue=covid_census_cdc$medianhousevalue,
                              medhouseholdincome = covid_census_cdc$medhouseholdincome, education = covid_census_cdc$education, older_pecent = covid_census_cdc$older_pecent, population = covid_census_cdc$population))
vif.table <- vif(M0)
vif.table
# note: all VIF < 6

# NY only
M1 <- as.data.frame(x = cbind(pct_blk = covid_census_cdc.ny$pct_blk, hispanic=covid_census_cdc.ny$hispanic, poverty=covid_census_cdc.ny$poverty, popdensity = covid_census_cdc.ny$popdensity, medianhousevalue=covid_census_cdc.ny$medianhousevalue,
                              medhouseholdincome = covid_census_cdc.ny$medhouseholdincome, education = covid_census_cdc.ny$education, older_pecent = covid_census_cdc.ny$older_pecent, population = covid_census_cdc.ny$population))
vif.table <- vif(M1)
vif.table
# note: medianhousevalue removed due to how large it is

M1.a <- as.data.frame(x = cbind(pct_blk = covid_census_cdc.ny$pct_blk, hispanic=covid_census_cdc.ny$hispanic, poverty=covid_census_cdc.ny$poverty, popdensity = covid_census_cdc.ny$popdensity, 
                                medhouseholdincome = covid_census_cdc.ny$medhouseholdincome, education = covid_census_cdc.ny$education, older_pecent = covid_census_cdc.ny$older_pecent, population = covid_census_cdc.ny$population))
vif.table <- vif(M1.a)
vif.table
# note: all VIF < 7

# US excluding NY
M2 <- as.data.frame(x = cbind(pct_blk = covid_census_cdc.exclude.ny$pct_blk, hispanic=covid_census_cdc.exclude.ny$hispanic, poverty=covid_census_cdc.exclude.ny$poverty, popdensity = covid_census_cdc.exclude.ny$popdensity, medianhousevalue=covid_census_cdc.exclude.ny$medianhousevalue,
                              medhouseholdincome = covid_census_cdc.exclude.ny$medhouseholdincome, education = covid_census_cdc.exclude.ny$education, older_pecent = covid_census_cdc.exclude.ny$older_pecent, population = covid_census_cdc.exclude.ny$population))
vif.table <- vif(M2)
vif.table
# note: all VIF < 6

#####################################
##################################### descriptive analyses
#####################################
# entire US
# outcomes
cases.summary <- summary(covid_census_cdc$Confirmed)
deaths.summary <- summary(covid_census_cdc$Deaths)
sum(covid_census_cdc$Confirmed)
sum(covid_census_cdc$Deaths)

# race
pct_blk.summary <- summary(covid_census_cdc$pct_blk)
hispanic.summary <- summary(covid_census_cdc$hispanic)

# confounders
poverty.summary <- summary(covid_census_cdc$poverty)
popdensity.summary <- summary(covid_census_cdc$popdensity)
medianhousevalue.summary <- summary(covid_census_cdc$medianhousevalue)
medianhouseholdincome.summary <- summary(covid_census_cdc$medhouseholdincome)
education.summary <- summary(covid_census_cdc$education)
older_percent.summary <- summary(covid_census_cdc$older_pecent) # missing in 1 county
population.summary <- summary(covid_census_cdc$population)

US.summary.stats <- round(rbind(cases.summary, deaths.summary, 
                                pct_blk.summary, hispanic.summary,
                                poverty.summary, popdensity.summary, medianhousevalue.summary, medianhouseholdincome.summary, education.summary, older_percent.summary, population.summary), 1)
write.csv(x = US.summary.stats, file = "/Users/lpp22/Dropbox/Research/COVID/COVID-19/Results/US.summary.stats.csv")

# outcomes
cases.sd <- sd(covid_census_cdc$Confirmed)
deaths.sd <- sd(covid_census_cdc$Deaths)
sum(covid_census_cdc$Confirmed)
sum(covid_census_cdc$Deaths)

# race
pct_blk.sd <- sd(covid_census_cdc$pct_blk)
hispanic.sd <- sd(covid_census_cdc$hispanic)

# confounders
poverty.sd <- sd(covid_census_cdc$poverty)
popdensity.sd <- sd(covid_census_cdc$popdensity)
medianhousevalue.sd <- sd(covid_census_cdc$medianhousevalue)
medianhouseholdincome.sd <- sd(covid_census_cdc$medhouseholdincome)
education.sd <- sd(covid_census_cdc$education)
older_percent.sd <- sd(covid_census_cdc$older_pecent, na.rm = T) # missing in 1 county
population.sd <- sd(covid_census_cdc$population)

US.sd.stats <- round(rbind(cases.sd, deaths.sd, 
                           pct_blk.sd, hispanic.sd,
                           poverty.sd, popdensity.sd, medianhousevalue.sd, medianhouseholdincome.sd, education.sd, older_percent.sd, population.sd), 1)
write.csv(x = US.sd.stats, file = "/Users/lpp22/Dropbox/Research/COVID/COVID-19/Results/US.sd.stats.csv")


# NY only 
# outcomes
cases.summary.ny <- summary(covid_census_cdc.ny$Confirmed)
deaths.summary.ny <- summary(covid_census_cdc.ny$Deaths)
sum(covid_census_cdc.ny$Confirmed)
sum(covid_census_cdc.ny$Deaths)

# race
pct_blk.summary.ny <- summary(covid_census_cdc.ny$pct_blk)
hispanic.summary.ny <- summary(covid_census_cdc.ny$hispanic)

# confounders
poverty.summary.ny <- summary(covid_census_cdc.ny$poverty)
popdensity.summary.ny <- summary(covid_census_cdc.ny$popdensity)
medianhousevalue.summary.ny <- summary(covid_census_cdc.ny$medianhousevalue)
medianhouseholdincome.summary.ny <- summary(covid_census_cdc.ny$medhouseholdincome)
education.summary.ny <- summary(covid_census_cdc.ny$education)
older_percent.summary.ny <- summary(covid_census_cdc.ny$older_pecent, na.rm = T) # missing in 1 county
population.summary.ny <- summary(covid_census_cdc.ny$population)

NY.summary.stats <- round(rbind(cases.summary.ny, deaths.summary.ny, 
                                pct_blk.summary.ny, hispanic.summary.ny,
                                poverty.summary.ny, popdensity.summary.ny, medianhousevalue.summary.ny, medianhouseholdincome.summary.ny, education.summary.ny, older_percent.summary.ny, population.summary.ny), 1)
write.csv(x = NY.summary.stats, file = "/Users/lpp22/Dropbox/Research/COVID/COVID-19/Results/NY.summary.stats.csv")

# US excluding NY
# outcomes
cases.summary.exclude.ny <- summary(covid_census_cdc.exclude.ny$Confirmed)
deaths.summary.exclude.ny <- summary(covid_census_cdc.exclude.ny$Deaths)

# race
pct_blk.summary.exclude.ny <- summary(covid_census_cdc.exclude.ny$pct_blk)
hispanic.summary.exclude.ny <- summary(covid_census_cdc.exclude.ny$hispanic)

# confounders
poverty.summary.exclude.ny <- summary(covid_census_cdc.exclude.ny$poverty)
popdensity.summary.exclude.ny <- summary(covid_census_cdc.exclude.ny$popdensity)
medianhousevalue.summary.exclude.ny <- summary(covid_census_cdc.exclude.ny$medianhousevalue)
medianhouseholdincome.summary.exclude.ny <- summary(covid_census_cdc.exclude.ny$medhouseholdincome)
education.summary.exclude.ny <- summary(covid_census_cdc.exclude.ny$education)
older_percent.summary.exclude.ny <- summary(covid_census_cdc.exclude.ny$older_pecent, na.rm = T) # missing in 1 county
population.summary.exclude.ny <- summary(covid_census_cdc.exclude.ny$population)

US.exclude.NY.summary.stats <- round(rbind(cases.summary.exclude.ny, deaths.summary.exclude.ny, 
                                           pct_blk.summary.exclude.ny, hispanic.summary.exclude.ny,
                                           poverty.summary.exclude.ny, popdensity.summary.exclude.ny, medianhousevalue.summary.exclude.ny, medianhouseholdincome.summary.exclude.ny, education.summary.exclude.ny, older_percent.summary.exclude.ny, population.summary.exclude.ny), 1)

write.csv(x = US.exclude.NY.summary.stats, file = "/Users/lpp22/Dropbox/Research/COVID/COVID-19/Results/US.exclude.NY.summary.stats.csv")

# outcomes
cases.sd.ny <- sd(covid_census_cdc.ny$Confirmed)
deaths.sd.ny <- sd(covid_census_cdc.ny$Deaths)
sum(covid_census_cdc.ny$Confirmed)
sum(covid_census_cdc.ny$Deaths)

# race
pct_blk.sd.ny <- sd(covid_census_cdc.ny$pct_blk)
hispanic.sd.ny <- sd(covid_census_cdc.ny$hispanic)

# confounders
poverty.sd.ny <- sd(covid_census_cdc.ny$poverty)
popdensity.sd.ny <- sd(covid_census_cdc.ny$popdensity)
medianhousevalue.sd.ny <- sd(covid_census_cdc.ny$medianhousevalue)
medianhouseholdincome.sd.ny <- sd(covid_census_cdc.ny$medhouseholdincome)
education.sd.ny <- sd(covid_census_cdc.ny$education)
older_percent.sd.ny <- sd(covid_census_cdc.ny$older_pecent) # missing in 1 county
population.sd.ny <- sd(covid_census_cdc.ny$population)

NY.sd.stats <- round(rbind(cases.sd.ny, deaths.sd.ny, 
                           pct_blk.sd.ny, hispanic.sd.ny,
                           poverty.sd.ny, popdensity.sd.ny, medianhousevalue.sd.ny, medianhouseholdincome.sd.ny, education.sd.ny, older_percent.sd.ny, population.sd.ny), 1)
write.csv(x = NY.sd.stats, file = "/Users/lpp22/Dropbox/Research/COVID/COVID-19/Results/NY.sd.stats.csv")

# US excluding NY
# outcomes
cases.sd.exclude.ny <- sd(covid_census_cdc.exclude.ny$Confirmed)
deaths.sd.exclude.ny <- sd(covid_census_cdc.exclude.ny$Deaths)

# race
pct_blk.sd.exclude.ny <- sd(covid_census_cdc.exclude.ny$pct_blk)
hispanic.sd.exclude.ny <- sd(covid_census_cdc.exclude.ny$hispanic)

# confounders
poverty.sd.exclude.ny <- sd(covid_census_cdc.exclude.ny$poverty)
popdensity.sd.exclude.ny <- sd(covid_census_cdc.exclude.ny$popdensity)
medianhousevalue.sd.exclude.ny <- sd(covid_census_cdc.exclude.ny$medianhousevalue)
medianhouseholdincome.sd.exclude.ny <- sd(covid_census_cdc.exclude.ny$medhouseholdincome)
education.sd.exclude.ny <- sd(covid_census_cdc.exclude.ny$education)
older_percent.sd.exclude.ny <- sd(covid_census_cdc.exclude.ny$older_pecent, na.rm = T) # missing in 1 county
population.sd.exclude.ny <- sd(covid_census_cdc.exclude.ny$population)

US.exclude.NY.sd.stats <- round(rbind(cases.sd.exclude.ny, deaths.sd.exclude.ny, 
                                      pct_blk.sd.exclude.ny, hispanic.sd.exclude.ny,
                                      poverty.sd.exclude.ny, popdensity.sd.exclude.ny, medianhousevalue.sd.exclude.ny, medianhouseholdincome.sd.exclude.ny, education.sd.exclude.ny, older_percent.sd.exclude.ny, population.sd.exclude.ny), 1)

write.csv(x = US.exclude.NY.sd.stats, file = "/Users/lpp22/Dropbox/Research/COVID/COVID-19/Results/US.exclude.NY.sd.stats.csv")


#####################################
##################################### exploratory spatial data analyses
#####################################
# Reference Code - Figure.R data from https://github.com/wxwx1993/PM_COVID

# entire US
us.covid_census_cdc$cases = us.covid_census_cdc$Confirmed/us.covid_census_cdc$population*10^6
us.covid_census_cdc$logcases = log10(us.covid_census_cdc$Confirmed/us.covid_census_cdc$population*10^6)
us.covid_census_cdc$logcases[us.covid_census_cdc$logcases < 0] <- -1
us.covid_census_cdc$logcases[is.na(us.covid_census_cdc$logcases)] <- -1
summary(us.covid_census_cdc$logcases)
us.covid_census_cdc$mortality = us.covid_census_cdc$Deaths/us.covid_census_cdc$population*10^6
us.covid_census_cdc$logmortality = log10(us.covid_census_cdc$Deaths/us.covid_census_cdc$population*10^6)
us.covid_census_cdc$logmortality[us.covid_census_cdc$logmortality < 0] <- -1
us.covid_census_cdc$logmortality[is.na(us.covid_census_cdc$logmortality)] <- -1
summary(us.covid_census_cdc$logmortality)

# NY
us.ny.covid_census_cdc$cases = us.ny.covid_census_cdc$Confirmed/us.ny.covid_census_cdc$population*10^6
us.ny.covid_census_cdc$logcases = log10(us.ny.covid_census_cdc$Confirmed/us.ny.covid_census_cdc$population*10^6)
us.ny.covid_census_cdc$logcases[us.ny.covid_census_cdc$logcases < 0] <- -1
us.ny.covid_census_cdc$logcases[is.na(us.ny.covid_census_cdc$logcases)] <- -1
summary(us.ny.covid_census_cdc$logcases)
us.ny.covid_census_cdc$mortality = us.ny.covid_census_cdc$Deaths/us.ny.covid_census_cdc$population*10^6
us.ny.covid_census_cdc$logmortality = log10(us.ny.covid_census_cdc$Deaths/us.ny.covid_census_cdc$population*10^6)
us.ny.covid_census_cdc$logmortality[us.ny.covid_census_cdc$logmortality < 0] <- -1
us.ny.covid_census_cdc$logmortality[is.na(us.ny.covid_census_cdc$logmortality)] <- -1
summary(us.ny.covid_census_cdc$logmortality)

# US excluding NY
us.exclude.ny.covid_census_cdc$cases = us.exclude.ny.covid_census_cdc$Confirmed/us.exclude.ny.covid_census_cdc$population*10^6
us.exclude.ny.covid_census_cdc$logcases = log10(us.exclude.ny.covid_census_cdc$Deaths/us.exclude.ny.covid_census_cdc$population*10^6)
us.exclude.ny.covid_census_cdc$logcases[us.exclude.ny.covid_census_cdc$logcases < 0] <- -1
us.exclude.ny.covid_census_cdc$logcases[is.na(us.exclude.ny.covid_census_cdc$logcases)] <- -1
summary(us.exclude.ny.covid_census_cdc$logcases)
us.exclude.ny.covid_census_cdc$mortality = us.exclude.ny.covid_census_cdc$Deaths/us.exclude.ny.covid_census_cdc$population*10^6
us.exclude.ny.covid_census_cdc$logmortality = log10(us.exclude.ny.covid_census_cdc$Deaths/us.exclude.ny.covid_census_cdc$population*10^6)
us.exclude.ny.covid_census_cdc$logmortality[us.exclude.ny.covid_census_cdc$logmortality < 0] <- -1
us.exclude.ny.covid_census_cdc$logmortality[is.na(us.exclude.ny.covid_census_cdc$logmortality)] <- -1
summary(us.exclude.ny.covid_census_cdc$logmortality)


# reference: https://liuyanguu.github.io/post/2019/04/17/ggplot-heatmap-us-50-states-map-and-china-province-map/
#51 states including Alaska and Hawaii
suppressPackageStartupMessages({
  library(ggplot2)
  library(maps)
  library(usmap)
  library(data.table)
  library(ggsn) # for scale bar `scalebar`
  library(ggrepel) # if need to repel labels 
})


dt1 <- as.data.table(copy(us.covid_census_cdc))

dt1$fips <- us.covid_census_cdc$FIPS

dt1 <- dt1[,.(fips, logmortality)]
# only need state name and variable to plot in the input file:
str(dt1) 

us_map <- usmap::us_map(regions = "counties") # used to add map scale


g1 <- usmap::plot_usmap(regions = "counties", data = dt1, values = "logmortality", labels = F, color = "transparent") +
  #  geom_sf(aes(fill = PD_p),color=NA,size=0.025)+
  #geom_sf(aes(fill = logmortality),color='grey',size=0.005)+
  #  scale_fill_viridis_c(option="magma",begin=0.4)+
  scale_fill_gradient2(expression(paste("# COVID-19 Deaths per 1 Million")),low  = "#1e90ff", mid="#ffffba", high = "#8b0000",midpoint = 1,
                       breaks = c(-1,0,1,2,4),
                       labels = c("0","1","10","100","1000+"),limits = c(-1,4.1) , na.value = "white") +
  # labs(title = expression(paste("Cumulative Deaths Related to COVID-19 until March 30, 2020"))) +
  theme_minimal() +
  theme(plot.title = element_text(size = 24*2,hjust = 0.5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        line = element_blank(),
        axis.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal", 
        #        legend.text = element_text(angle = 60,  size = 20*2),
        legend.text = element_text(angle = 60,  size = 20*1),
        legend.text.align = 0.75,
        #        legend.title = element_text(size = 18*2),
        legend.title = element_text(size = 18*1.5),
        legend.key.width = unit(150*2, "points"),
        panel.grid.major = element_line(color = "transparent"))
g1


dt2 <- as.data.table(copy(us.covid_census_cdc))

dt2$fips <- us.covid_census_cdc$FIPS

dt2 <- dt2[,.(fips, logcases)]
# only need state name and variable to plot in the input file:
str(dt2) 
summary(us.covid_census_cdc$logcases)

us_map <- usmap::us_map(regions = "counties") # used to add map scale


g2 <- usmap::plot_usmap(regions = "counties", data = dt2, values = "logcases", labels = F, color = "transparent") +
  #  geom_sf(aes(fill = PD_p),color=NA,size=0.025)+
  #geom_sf(aes(fill = logmortality),color='grey',size=0.005)+
  #  scale_fill_viridis_c(option="magma",begin=0.4)+
  scale_fill_gradient2(expression(paste("# COVID-19 Cases per 1 Million")),low  = "#1e90ff", mid="#ffffba", high = "#8b0000",midpoint = 1,
                       breaks = c(-1,0,1,2,5.1),
                       labels = c("0","1","10","100","1000+"),limits = c(-1,5.1) , na.value = "white") +
  # labs(title = expression(paste("Cumulative Deaths Related to COVID-19 until March 30, 2020"))) +
  theme_minimal() +
  theme(plot.title = element_text(size = 24*2,hjust = 0.5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        line = element_blank(),
        axis.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal", 
        #        legend.text = element_text(angle = 60,  size = 20*2),
        legend.text = element_text(angle = 60,  size = 20*1),
        legend.text.align = 0.75,
        #        legend.title = element_text(size = 18*2),
        legend.title = element_text(size = 18*1.5),
        legend.key.width = unit(150*2, "points"),
        panel.grid.major = element_line(color = "transparent"))
g2


png("/Users/lpp22/Dropbox/Research/COVID/COVID-19/Maps/county_covid_deaths_us.jpeg", height = 1024*0.6*2, width = 1024*2)
g1
dev.off()

png("/Users/lpp22/Dropbox/Research/COVID/COVID-19/Maps/county_covid_cases_us.jpeg", height = 1024*0.6*2, width = 1024*2)
g2
dev.off()


dt3 <- as.data.table(copy(us.ny.covid_census_cdc))

dt3$fips <- us.ny.covid_census_cdc$FIPS

dt3 <- dt3[,.(fips, logmortality)]
# only need state name and variable to plot in the input file:
str(dt3) 
summary(dt3$logmortality)

us_map <- usmap::us_map(regions = "counties") # used to add map scale


g3 <- usmap::plot_usmap(regions = "counties", data = dt3, values = "logmortality", labels = F, color = "transparent", include = c("NY")) +
  #  geom_sf(aes(fill = PD_p),color=NA,size=0.025)+
  #geom_sf(aes(fill = logmortality),color='grey',size=0.005)+
  #  scale_fill_viridis_c(option="magma",begin=0.4)+
  scale_fill_gradient2(expression(paste("# COVID-19 Deaths per 1 Million")),low  = "#1e90ff", mid="#ffffba", high = "#8b0000",midpoint = 1,
                       breaks = c(-1,0,1,2,4),
                       labels = c("0","1","10","100","1000+"),limits = c(-1,4.1) , na.value = "white") +
  # labs(title = expression(paste("Cumulative Deaths Related to COVID-19 until March 30, 2020"))) +
  theme_minimal() +
  theme(plot.title = element_text(size = 24*2,hjust = 0.5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        line = element_blank(),
        axis.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal", 
        #        legend.text = element_text(angle = 60,  size = 20*2),
        legend.text = element_text(angle = 60,  size = 20*1),
        legend.text.align = 0.75,
        #        legend.title = element_text(size = 18*2),
        legend.title = element_text(size = 18*1.5),
        legend.key.width = unit(150*2, "points"),
        panel.grid.major = element_line(color = "transparent"))
g3


dt4 <- as.data.table(copy(us.ny.covid_census_cdc))

dt4$fips <- us.ny.covid_census_cdc$FIPS

dt4 <- dt4[,.(fips, logcases)]
# only need state name and variable to plot in the input file:
str(dt4) 
summary(us.ny.covid_census_cdc$logcases)

us_map <- usmap::us_map(regions = "counties") # used to add map scale


g4 <- usmap::plot_usmap(regions = "counties", data = dt4, values = "logcases", labels = F, color = "transparent", include = c("NY")) +
  #xlim(-125,-65)+ylim(25,50)+
  #  geom_sf(aes(fill = PD_p),color=NA,size=0.025)+
  #geom_sf(aes(fill = logmortality),color='grey',size=0.005)+
  #  scale_fill_viridis_c(option="magma",begin=0.4)+
  scale_fill_gradient2(expression(paste("# COVID-19 Cases per 1 Million")),low  = "#1e90ff", mid="#ffffba", high = "#8b0000",midpoint = 1,
                       breaks = c(-1,0,1,2,5.1),
                       labels = c("0","1","10","100","1000+"),limits = c(-1,5.1) , na.value = "white") +
  # labs(title = expression(paste("Cumulative Deaths Related to COVID-19 until March 30, 2020"))) +
  theme_minimal() +
  theme(plot.title = element_text(size = 24*2,hjust = 0.5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        line = element_blank(),
        axis.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal", 
        #        legend.text = element_text(angle = 60,  size = 20*2),
        legend.text = element_text(angle = 60,  size = 20*1),
        legend.text.align = 0.75,
        #        legend.title = element_text(size = 18*2),
        legend.title = element_text(size = 18*1.5),
        legend.key.width = unit(150*2, "points"),
        panel.grid.major = element_line(color = "transparent"))
g4


png("/Users/lpp22/Dropbox/Research/COVID/COVID-19/Maps/county_covid_deaths_ny.jpeg", height = 1024*0.6*2, width = 1024*2)
g3
dev.off()

png("/Users/lpp22/Dropbox/Research/COVID/COVID-19/Maps/county_covid_cases_ny.jpeg", height = 1024*0.6*2, width = 1024*2)
g4
dev.off()



# moran's I test for spatial autocorrelation
# create adjacency matrix
# based on Queen adjacency 
sa.nb <- poly2nb(us, queen = T)
sa.nb.ny <- poly2nb(us.ny, queen = T)
sa.nb.exclude.ny <- poly2nb(us.exclude.ny, queen = T)
#summary(sa.nb)
#sa.wt <- nb2listw(neighbours = sa.nb, style = "B")
sa.wt <- nb2listw(neighbours = sa.nb, style = "W", zero.policy = T) # use row-standardized; sums over all links to n
sa.wt.ny <- nb2listw(neighbours = sa.nb.ny, style = "W", zero.policy = T) # use row-standardized; sums over all links to n
sa.wt.exclude.ny <- nb2listw(neighbours = sa.nb.exclude.ny, style = "W", zero.policy = T) # use row-standardized; sums over all links to n
#plot(geodat.us)
#plot(sa.nb, coordinates(geodat.us), add = T, col = "red")

# assess spatial correlation via Moran's I test
set.seed(1234)
moran.cases.us <- moran.mc(x = us.covid_census_cdc$Confirmed, listw = sa.wt, nsim = 99, zero.policy = T) # statistic = 0.16557, observed rank = 100, p-value = 0.01
moran.deaths.us <- moran.mc(x = us.covid_census_cdc$Deaths, listw = sa.wt, nsim = 99, zero.policy = T) # statistic = 0.067308, observed rank = 100, p-value = 0.01
moran.cases.ny <- moran.mc(x = us.ny.covid_census_cdc$Confirmed, listw = sa.wt.ny, nsim = 99, zero.policy = T) # statistic = 0.020023, observed rank = 86, p-value = 0.14
moran.deaths.ny <- moran.mc(x = us.ny.covid_census_cdc$Deaths, listw = sa.wt.ny, nsim = 99, zero.policy = T) # statistic = -0.019072, observed rank = 54, p-value = 0.46
moran.cases.us.exclude.ny <- moran.mc(x = us.exclude.ny.covid_census_cdc$Confirmed, listw = sa.wt.exclude.ny, nsim = 99, zero.policy = T) # statistic = 0.41989, observed rank = 100, p-value = 0.01
moran.deaths.us.exclude.ny <- moran.mc(x = us.exclude.ny.covid_census_cdc$Deaths, listw = sa.wt.exclude.ny, nsim = 99, zero.policy = T) # statistic = 0.46965, observed rank = 100, p-value = 0.01

#####################################
##################################### inla analyses
#####################################

# create ID variables for inla
covid_census_cdc <- transform(covid_census_cdc, ID.county = as.numeric(factor(covid_census_cdc$FIPS)))
covid_census_cdc <- transform(covid_census_cdc, ID.county1 = as.numeric(factor(covid_census_cdc$FIPS)))
covid_census_cdc <- transform(covid_census_cdc, ID.county2 = as.numeric(factor(covid_census_cdc$FIPS)))
covid_census_cdc.ny <- transform(covid_census_cdc.ny, ID.county = as.numeric(factor(covid_census_cdc.ny$FIPS)))
covid_census_cdc.ny <- transform(covid_census_cdc.ny, ID.county1 = as.numeric(factor(covid_census_cdc.ny$FIPS)))
covid_census_cdc.ny <- transform(covid_census_cdc.ny, ID.county2 = as.numeric(factor(covid_census_cdc.ny$FIPS)))
covid_census_cdc.exclude.ny <- transform(covid_census_cdc.exclude.ny, ID.county = as.numeric(factor(covid_census_cdc.exclude.ny$FIPS)))
covid_census_cdc.exclude.ny <- transform(covid_census_cdc.exclude.ny, ID.county1 = as.numeric(factor(covid_census_cdc.exclude.ny$FIPS)))
covid_census_cdc.exclude.ny <- transform(covid_census_cdc.exclude.ny, ID.county2 = as.numeric(factor(covid_census_cdc.exclude.ny$FIPS)))

# neighborhood structure (i.e. counties that share a border are neighbors)
setwd("/Users/lpp22/Dropbox/Research/COVID/COVID-19/Code")

# entire US
nb.us <- poly2nb(us)
#head(nb.us)
nb2INLA("map.us.adj", nb.us)
g.us <- inla.read.graph(filename = "map.us.adj")

# NY
nb.us.ny <- poly2nb(us.ny)
#head(nb.us.ny)
nb2INLA("map.us.ny.adj", nb.us.ny)
g.us.ny <- inla.read.graph(filename = "map.us.ny.adj")


# US Excluding NY
nb.us.exclude.ny <- poly2nb(us.exclude.ny)
#head(nb.us.exclude.ny)
nb2INLA("map.us.exclude.ny.adj", nb.us.exclude.ny)
g.us.exclude.ny <- inla.read.graph(filename = "map.us.exclude.ny.adj")


#### formulas
# negative binomial settings with bym state level random effects
# priors
prior <- list(
  prec = list(
    prior = "pc.prec",
    param = c(0.5 / 0.31, 0.01)),
  phi = list(
    prior = "pc",
    param = c(0.5, 2 / 3))
)
hyper = list(theta=list(prior="loggamma", param=c(1, 0.5)))

# leroux random effects - creation of diagonal matrix
# entire US
Q <- Diagonal(x = sapply(nb.us, length))
for(i in 2:nrow(us)) {
  Q[i - 1, i] <- -1
  Q[i, i - 1] <- -1
}

C <- Diagonal(x = 1, n = nrow(us)) - Q


# NY only
Q.ny <- Diagonal(x = sapply(nb.us.ny, length))
for(i in 2:nrow(us.ny)) {
  Q.ny[i - 1, i] <- -1
  Q.ny[i, i - 1] <- -1
}

C.ny <- Diagonal(x = 1, n = nrow(us.ny)) - Q.ny


# Excluding NY
Q.exclude.ny <- Diagonal(x = sapply(nb.us.exclude.ny, length))
for(i in 2:nrow(us.exclude.ny)) {
  Q.exclude.ny[i - 1, i] <- -1
  Q.exclude.ny[i, i - 1] <- -1
}

C.exclude.ny <- Diagonal(x = 1, n = nrow(us.exclude.ny)) - Q.exclude.ny


#### inla formulas
# NY only
cases.fixed.ny.leroux <- Confirmed ~ scale(pct_blk) + scale(hispanic) + scale(poverty) + scale(popdensity) + scale(medhouseholdincome) + scale(education) + scale(older_pecent) + offset(log(population)) + f(ID.county, model = "generic1", Cmatrix = C.ny, hyper = hyper)
deaths.fixed.ny.leroux <- Deaths ~ scale(pct_blk) + scale(hispanic) + scale(poverty) + scale(popdensity) + scale(medhouseholdincome) + scale(education) + scale(older_pecent) + offset(log(population)) + f(ID.county, model = "generic1", Cmatrix = C.ny, hyper = hyper)
cases.svc.ny.leroux <- Confirmed ~ scale(pct_blk) + scale(hispanic) + scale(poverty) + scale(popdensity) + scale(medhouseholdincome) + scale(education) + scale(older_pecent) + offset(log(population)) + f(ID.county, model = "generic1", Cmatrix = C.ny, hyper = hyper) + f(ID.county1, scale(pct_blk), model = "generic1", Cmatrix = C.ny, hyper = hyper) + f(ID.county2, scale(hispanic), model = "generic1", Cmatrix = C.ny, hyper = hyper)
deaths.svc.ny.leroux <- Deaths ~ scale(pct_blk) + scale(hispanic) + scale(poverty) + scale(popdensity) + scale(medhouseholdincome) + scale(education) + scale(older_pecent) + offset(log(population)) + f(ID.county, model = "generic1", Cmatrix = C.ny, hyper = hyper) + f(ID.county1, scale(pct_blk), model = "generic1", Cmatrix = C.ny, hyper = hyper) + f(ID.county2, scale(hispanic), model = "generic1", Cmatrix = C.ny, hyper = hyper)

# entire US
cases.fixed.leroux <- Confirmed ~ scale(pct_blk) + scale(hispanic) + scale(poverty) + scale(popdensity) + scale(medianhousevalue) + scale(medhouseholdincome) + scale(education) + scale(older_pecent) + offset(log(population)) + f(ID.county, model = "generic1", Cmatrix = C, hyper = hyper)
deaths.fixed.leroux <- Deaths ~ scale(pct_blk) + scale(hispanic) + scale(poverty) + scale(popdensity) + scale(medianhousevalue) + scale(medhouseholdincome) + scale(education) + scale(older_pecent) + offset(log(population)) + f(ID.county, model = "generic1", Cmatrix = C, hyper = hyper)
cases.svc.leroux <- Confirmed ~ scale(pct_blk) + scale(hispanic) + scale(poverty) + scale(popdensity) + scale(medianhousevalue) + scale(medhouseholdincome) + scale(education) + scale(older_pecent) + offset(log(population)) + f(ID.county, model = "generic1", Cmatrix = C, hyper = hyper) + f(ID.county1, scale(pct_blk), model = "generic1", Cmatrix = C, hyper = hyper) + f(ID.county2, scale(hispanic), model = "generic1", Cmatrix = C, hyper = hyper)
deaths.svc.leroux <- Deaths ~ scale(pct_blk) + scale(hispanic) + scale(poverty) + scale(popdensity) + scale(medianhousevalue) + scale(medhouseholdincome) + scale(education) + scale(older_pecent) + offset(log(population)) + f(ID.county, model = "generic1", Cmatrix = C, hyper = hyper) + f(ID.county1, scale(pct_blk), model = "generic1", Cmatrix = C, hyper = hyper) + f(ID.county2, scale(hispanic), model = "generic1", Cmatrix = C, hyper = hyper)

# Excluding NY
cases.fixed.exclude.ny.leroux <- Confirmed ~ scale(pct_blk) + scale(hispanic) + scale(poverty) + scale(popdensity) + scale(medianhousevalue) + scale(medhouseholdincome) + scale(education) + scale(older_pecent) + offset(log(population)) + f(ID.county, model = "generic1", Cmatrix = C.exclude.ny, hyper = hyper)
deaths.fixed.exclude.ny.leroux <- Deaths ~ scale(pct_blk) + scale(hispanic) + scale(poverty) + scale(popdensity) + scale(medianhousevalue) + scale(medhouseholdincome) + scale(education) + scale(older_pecent) + offset(log(population)) + f(ID.county, model = "generic1", Cmatrix = C.exclude.ny, hyper = hyper)
cases.svc.exclude.ny.leroux <- Confirmed ~ scale(pct_blk) + scale(hispanic) + scale(poverty) + scale(popdensity) + scale(medianhousevalue) + scale(medhouseholdincome) + scale(education) + scale(older_pecent) + offset(log(population)) + f(ID.county, model = "generic1", Cmatrix = C.exclude.ny, hyper = hyper) + f(ID.county1, scale(pct_blk), model = "generic1", Cmatrix = C.exclude.ny, hyper = hyper) + f(ID.county2, scale(hispanic), model = "generic1", Cmatrix = C.exclude.ny, hyper = hyper)
deaths.svc.exclude.ny.leroux <- Deaths ~ scale(pct_blk) + scale(hispanic) + scale(poverty) + scale(popdensity) + scale(medianhousevalue) + scale(medhouseholdincome) + scale(education) + scale(older_pecent) + offset(log(population)) + f(ID.county, model = "generic1", Cmatrix = C.exclude.ny, hyper = hyper) + f(ID.county1, scale(pct_blk), model = "generic1", Cmatrix = C.exclude.ny, hyper = hyper) + f(ID.county2, scale(hispanic), model = "generic1", Cmatrix = C.exclude.ny, hyper = hyper)


####################################### inla models: negative binomial
# NY only
date()
#starting.value <- inla(cases.fixed.ny.leroux, family = "nbinomial", data = covid_census_cdc.ny, control.inla = list(diagonal = 100, strategy = "gaussian", int.strategy = "eb"), control.predictor=list(A=covid_census_cdc.ny$A), verbose=F)
#inla.cases.fixed.ny.leroux <- inla(cases.fixed.ny.leroux, family = "nbinomial", data = covid_census_cdc.ny, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE), control.inla = list(strategy="simplified.laplace", cmin=0), control.mode = list(result = starting.value, restart=T))
inla.cases.fixed.ny.leroux <- inla(cases.fixed.ny.leroux, family = "nbinomial", data = covid_census_cdc.ny, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE))
date()
#starting.value <- inla(deaths.fixed.ny.leroux, family = "nbinomial", data = covid_census_cdc.ny, control.inla = list(diagonal = 100, strategy = "gaussian", int.strategy = "eb"), control.predictor=list(A=covid_census_cdc.ny$A), verbose=F)
#inla.deaths.fixed.ny.leroux <- inla(deaths.fixed.ny.leroux, family = "nbinomial", data = covid_census_cdc.ny, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE), control.inla = list(strategy="simplified.laplace", cmin=0), control.mode = list(result = starting.value, restart=T))
inla.deaths.fixed.ny.leroux <- inla(deaths.fixed.ny.leroux, family = "nbinomial", data = covid_census_cdc.ny, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE))
date()
#starting.value <- inla(cases.svc.ny.leroux, family = "nbinomial", data = covid_census_cdc.ny, control.inla = list(diagonal = 100, strategy = "gaussian", int.strategy = "eb"), control.predictor=list(A=covid_census_cdc.ny$A), verbose=F)
#inla.cases.svc.ny.leroux <- inla(cases.svc.ny.leroux, family = "nbinomial", data = covid_census_cdc.ny, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE), control.inla = list(strategy="simplified.laplace", cmin=0), control.mode = list(result = starting.value, restart=T))
inla.cases.svc.ny.leroux <- inla(cases.svc.ny.leroux, family = "nbinomial", data = covid_census_cdc.ny, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE))
date()
#starting.value <- inla(deaths.svc.ny.leroux, family = "nbinomial", data = covid_census_cdc.ny, control.inla = list(diagonal = 100, strategy = "gaussian", int.strategy = "eb"), control.predictor=list(A=covid_census_cdc.ny$A), verbose=F)
#inla.deaths.svc.ny.leroux <- inla(deaths.svc.ny.leroux, family = "nbinomial", data = covid_census_cdc.ny, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE), control.inla = list(strategy="simplified.laplace",cmin=0), control.mode = list(result = starting.value, restart=T))
inla.deaths.svc.ny.leroux <- inla(deaths.svc.ny.leroux, family = "nbinomial", data = covid_census_cdc.ny, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE))
date()
save.image("~/Dropbox/Research/COVID/COVID-19/Code/R Workspaces/COVID and RACE - final lpt07252020 NB NY.RData")

# entire US
date()
starting.value <- inla(cases.fixed.leroux, family = "nbinomial", data = covid_census_cdc, control.inla = list(diagonal = 100, strategy = "gaussian", int.strategy = "eb"), control.predictor=list(A=covid_census_cdc$A), verbose=F)
inla.cases.fixed.leroux <- inla(cases.fixed.leroux, family = "nbinomial", data = covid_census_cdc, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE), control.inla = list(strategy="simplified.laplace",cmin=0), control.mode = list(result = starting.value, restart=T))
#inla.cases.fixed.leroux <- inla(cases.fixed.leroux, family = "nbinomial", data = covid_census_cdc, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE))
date()
starting.value <- inla(deaths.fixed.leroux, family = "nbinomial", data = covid_census_cdc, control.inla = list(diagonal = 100, strategy = "gaussian", int.strategy = "eb"), control.predictor=list(A=covid_census_cdc$A), verbose=F)
inla.deaths.fixed.leroux <- inla(deaths.fixed.leroux, family = "nbinomial", data = covid_census_cdc, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE), control.inla = list(strategy="simplified.laplace",cmin=0), control.mode = list(result = starting.value, restart=T))
#inla.deaths.fixed.leroux <- inla(deaths.fixed.leroux, family = "nbinomial", data = covid_census_cdc, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE))
date()
starting.value <- inla(cases.svc.leroux, family = "nbinomial", data = covid_census_cdc, control.inla = list(diagonal = 100, strategy = "gaussian", int.strategy = "eb"), control.predictor=list(A=covid_census_cdc$A), verbose=F)
inla.cases.svc.leroux <- inla(cases.svc.leroux, family = "nbinomial", data = covid_census_cdc, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE), control.inla = list(strategy="simplified.laplace",cmin=0), control.mode = list(result = starting.value, restart=T))
#inla.cases.svc.leroux <- inla(cases.svc.leroux, family = "nbinomial", data = covid_census_cdc, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE))
date()
starting.value <- inla(deaths.svc.leroux, family = "nbinomial", data = covid_census_cdc, control.inla = list(diagonal = 100, strategy = "gaussian", int.strategy = "eb"), control.predictor=list(A=covid_census_cdc$A), verbose=F)
inla.deaths.svc.leroux <- inla(deaths.svc.leroux, family = "nbinomial", data = covid_census_cdc, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE), control.inla = list(strategy="simplified.laplace",cmin=0), control.mode = list(result = starting.value, restart=T))
#inla.deaths.svc.leroux <- inla(deaths.svc.leroux, family = "nbinomial", data = covid_census_cdc, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE))
date()
save.image("~/Dropbox/Research/COVID/COVID-19/Code/R Workspaces/COVID and RACE - final lpt07252020 NB US.RData")

# Excluding NY
date()
starting.value <- inla(cases.fixed.exclude.ny.leroux, family = "nbinomial", data = covid_census_cdc.exclude.ny, control.inla = list(diagonal = 100, strategy = "gaussian", int.strategy = "eb"), control.predictor=list(A=covid_census_cdc.exclude.ny$A), verbose=F)
inla.cases.fixed.exclude.ny.leroux <- inla(cases.fixed.exclude.ny.leroux, family = "nbinomial", data = covid_census_cdc.exclude.ny, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE), control.inla = list(strategy="simplified.laplace",cmin=0), control.mode = list(result = starting.value, restart=T))
#inla.cases.fixed.exclude.ny.leroux <- inla(cases.fixed.exclude.ny.leroux, family = "nbinomial", data = covid_census_cdc.exclude.ny, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE))
date()
starting.value <- inla(deaths.fixed.exclude.ny.leroux, family = "nbinomial", data = covid_census_cdc.exclude.ny, control.inla = list(diagonal = 100, strategy = "gaussian", int.strategy = "eb"), control.predictor=list(A=covid_census_cdc.exclude.ny$A), verbose=F)
inla.deaths.fixed.exclude.ny.leroux <- inla(deaths.fixed.exclude.ny.leroux, family = "nbinomial", data = covid_census_cdc.exclude.ny, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE), control.inla = list(strategy="simplified.laplace",cmin=0), control.mode = list(result = starting.value, restart=T))
#inla.deaths.fixed.exclude.ny.leroux <- inla(deaths.fixed.exclude.ny.leroux, family = "nbinomial", data = covid_census_cdc.exclude.ny, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE))
date()
starting.value <- inla(cases.svc.exclude.ny.leroux, family = "nbinomial", data = covid_census_cdc.exclude.ny, control.inla = list(diagonal = 100, strategy = "gaussian", int.strategy = "eb"), control.predictor=list(A=covid_census_cdc.exclude.ny$A), verbose=F)
inla.cases.svc.exclude.ny.leroux <- inla(cases.svc.exclude.ny.leroux, family = "nbinomial", data = covid_census_cdc.exclude.ny, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE), control.inla = list(strategy="simplified.laplace",cmin=0), control.mode = list(result = starting.value, restart=T))
#inla.cases.svc.exclude.ny.leroux <- inla(cases.svc.exclude.ny.leroux, family = "nbinomial", data = covid_census_cdc.exclude.ny, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE))
date()
starting.value <- inla(deaths.svc.exclude.ny.leroux, family = "nbinomial", data = covid_census_cdc.exclude.ny, control.inla = list(diagonal = 100, strategy = "gaussian", int.strategy = "eb"), control.predictor=list(A=covid_census_cdc.exclude.ny$A), verbose=F)
inla.deaths.svc.exclude.ny.leroux <- inla(deaths.svc.exclude.ny.leroux, family = "nbinomial", data = covid_census_cdc.exclude.ny, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE), control.inla = list(strategy="simplified.laplace",cmin=0), control.mode = list(result = starting.value, restart=T))
#inla.deaths.svc.exclude.ny.leroux <- inla(deaths.svc.exclude.ny.leroux, family = "nbinomial", data = covid_census_cdc.exclude.ny, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE))
date()
save.image("~/Dropbox/Research/COVID/COVID-19/Code/R Workspaces/COVID and RACE - final lpt07252020 NB excl NY.RData")

####################################### inla models: zero inflated negative binomial
# NY only
date()
#starting.value <- inla(cases.fixed.ny.leroux, family = "zeroinflatednbinomial1", data = covid_census_cdc.ny, control.inla = list(diagonal = 100, strategy = "gaussian", int.strategy = "eb"), control.predictor=list(A=covid_census_cdc.ny$A), verbose=F)
#inla.cases.fixed.ny.leroux.zinb <- inla(cases.fixed.ny.leroux, family = "zeroinflatednbinomial1", data = covid_census_cdc.ny, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE), control.inla = list(strategy="simplified.laplace", cmin=0), control.mode = list(result = starting.value, restart=T))
inla.cases.fixed.ny.leroux.zinb <- inla(cases.fixed.ny.leroux, family = "zeroinflatednbinomial1", data = covid_census_cdc.ny, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE))
date()
#starting.value <- inla(deaths.fixed.ny.leroux, family = "zeroinflatednbinomial1", data = covid_census_cdc.ny, control.inla = list(diagonal = 100, strategy = "gaussian", int.strategy = "eb"), control.predictor=list(A=covid_census_cdc.ny$A), verbose=F)
#inla.deaths.fixed.ny.leroux.zinb <- inla(deaths.fixed.ny.leroux, family = "zeroinflatednbinomial1", data = covid_census_cdc.ny, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE), control.inla = list(strategy="simplified.laplace", cmin=0), control.mode = list(result = starting.value, restart=T))
inla.deaths.fixed.ny.leroux.zinb <- inla(deaths.fixed.ny.leroux, family = "zeroinflatednbinomial1", data = covid_census_cdc.ny, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE))
date()
#starting.value <- inla(cases.svc.ny.leroux, family = "zeroinflatednbinomial1", data = covid_census_cdc.ny, control.inla = list(diagonal = 100, strategy = "gaussian", int.strategy = "eb"), control.predictor=list(A=covid_census_cdc.ny$A), verbose=F)
#inla.cases.svc.ny.leroux.zinb <- inla(cases.svc.ny.leroux, family = "zeroinflatednbinomial1", data = covid_census_cdc.ny, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE), control.inla = list(strategy="simplified.laplace", cmin=0), control.mode = list(result = starting.value, restart=T))
inla.cases.svc.ny.leroux.zinb <- inla(cases.svc.ny.leroux, family = "zeroinflatednbinomial1", data = covid_census_cdc.ny, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE))
date()
#starting.value <- inla(deaths.svc.ny.leroux, family = "zeroinflatednbinomial1", data = covid_census_cdc.ny, control.inla = list(diagonal = 100, strategy = "gaussian", int.strategy = "eb"), control.predictor=list(A=covid_census_cdc.ny$A), verbose=F)
#inla.deaths.svc.ny.leroux.zinb <- inla(deaths.svc.ny.leroux, family = "zeroinflatednbinomial1", data = covid_census_cdc.ny, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE), control.inla = list(strategy="simplified.laplace",cmin=0), control.mode = list(result = starting.value, restart=T))
inla.deaths.svc.ny.leroux.zinb <- inla(deaths.svc.ny.leroux, family = "zeroinflatednbinomial1", data = covid_census_cdc.ny, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE))
date()
save.image("~/Dropbox/Research/COVID/COVID-19/Code/R Workspaces/COVID and RACE - final lpt07252020 ZINB NY.RData")

# entire US
date()
starting.value <- inla(cases.fixed.leroux, family = "zeroinflatednbinomial1", data = covid_census_cdc, control.inla = list(diagonal = 100, strategy = "gaussian", int.strategy = "eb"), control.predictor=list(A=covid_census_cdc$A), verbose=F)
inla.cases.fixed.leroux.zinb <- inla(cases.fixed.leroux, family = "zeroinflatednbinomial1", data = covid_census_cdc, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE), control.inla = list(strategy="simplified.laplace",cmin=0), control.mode = list(result = starting.value, restart=T))
#inla.cases.fixed.leroux.zinb <- inla(cases.fixed.leroux, family = "zeroinflatednbinomial1", data = covid_census_cdc, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE))
date()
starting.value <- inla(deaths.fixed.leroux, family = "zeroinflatednbinomial1", data = covid_census_cdc, control.inla = list(diagonal = 100, strategy = "gaussian", int.strategy = "eb"), control.predictor=list(A=covid_census_cdc$A), verbose=F)
inla.deaths.fixed.leroux.zinb <- inla(deaths.fixed.leroux, family = "zeroinflatednbinomial1", data = covid_census_cdc, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE), control.inla = list(strategy="simplified.laplace",cmin=0), control.mode = list(result = starting.value, restart=T))
#inla.deaths.fixed.leroux.zinb <- inla(deaths.fixed.leroux, family = "zeroinflatednbinomial1", data = covid_census_cdc, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE))
date()
starting.value <- inla(cases.svc.leroux, family = "zeroinflatednbinomial1", data = covid_census_cdc, control.inla = list(diagonal = 100, strategy = "gaussian", int.strategy = "eb"), control.predictor=list(A=covid_census_cdc$A), verbose=F)
inla.cases.svc.leroux.zinb <- inla(cases.svc.leroux, family = "zeroinflatednbinomial1", data = covid_census_cdc, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE), control.inla = list(strategy="simplified.laplace",cmin=0), control.mode = list(result = starting.value, restart=T))
#inla.cases.svc.leroux.zinb <- inla(cases.svc.leroux, family = "zeroinflatednbinomial1", data = covid_census_cdc, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE))
date()
starting.value <- inla(deaths.svc.leroux, family = "zeroinflatednbinomial1", data = covid_census_cdc, control.inla = list(diagonal = 100, strategy = "gaussian", int.strategy = "eb"), control.predictor=list(A=covid_census_cdc$A), verbose=F)
inla.deaths.svc.leroux.zinb <- inla(deaths.svc.leroux, family = "zeroinflatednbinomial1", data = covid_census_cdc, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE), control.inla = list(strategy="simplified.laplace",cmin=0), control.mode = list(result = starting.value, restart=T))
#inla.deaths.svc.leroux.zinb <- inla(deaths.svc.leroux, family = "zeroinflatednbinomial1", data = covid_census_cdc, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE))
date()
save.image("~/Dropbox/Research/COVID/COVID-19/Code/R Workspaces/COVID and RACE - final lpt07252020 ZINB US.RData")

# Excluding NY
date()
starting.value <- inla(cases.fixed.exclude.ny.leroux, family = "zeroinflatednbinomial1", data = covid_census_cdc.exclude.ny, control.inla = list(diagonal = 100, strategy = "gaussian", int.strategy = "eb"), control.predictor=list(A=covid_census_cdc.exclude.ny$A), verbose=F)
inla.cases.fixed.exclude.ny.leroux.zinb <- inla(cases.fixed.exclude.ny.leroux, family = "zeroinflatednbinomial1", data = covid_census_cdc.exclude.ny, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE), control.inla = list(strategy="simplified.laplace",cmin=0), control.mode = list(result = starting.value, restart=T))
#inla.cases.fixed.exclude.ny.leroux.zinb <- inla(cases.fixed.exclude.ny.leroux, family = "zeroinflatednbinomial1", data = covid_census_cdc.exclude.ny, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE))
date()
starting.value <- inla(deaths.fixed.exclude.ny.leroux, family = "zeroinflatednbinomial1", data = covid_census_cdc.exclude.ny, control.inla = list(diagonal = 100, strategy = "gaussian", int.strategy = "eb"), control.predictor=list(A=covid_census_cdc.exclude.ny$A), verbose=F)
inla.deaths.fixed.exclude.ny.leroux.zinb <- inla(deaths.fixed.exclude.ny.leroux, family = "zeroinflatednbinomial1", data = covid_census_cdc.exclude.ny, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE), control.inla = list(strategy="simplified.laplace",cmin=0), control.mode = list(result = starting.value, restart=T))
#inla.deaths.fixed.exclude.ny.leroux.zinb <- inla(deaths.fixed.exclude.ny.leroux, family = "zeroinflatednbinomial1", data = covid_census_cdc.exclude.ny, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE))
date()
starting.value <- inla(cases.svc.exclude.ny.leroux, family = "zeroinflatednbinomial1", data = covid_census_cdc.exclude.ny, control.inla = list(diagonal = 100, strategy = "gaussian", int.strategy = "eb"), control.predictor=list(A=covid_census_cdc.exclude.ny$A), verbose=F)
inla.cases.svc.exclude.ny.leroux.zinb <- inla(cases.svc.exclude.ny.leroux, family = "zeroinflatednbinomial1", data = covid_census_cdc.exclude.ny, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE), control.inla = list(strategy="simplified.laplace",cmin=0), control.mode = list(result = starting.value, restart=T))
#inla.cases.svc.exclude.ny.leroux.zinb <- inla(cases.svc.exclude.ny.leroux, family = "zeroinflatednbinomial1", data = covid_census_cdc.exclude.ny, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE))
date()
starting.value <- inla(deaths.svc.exclude.ny.leroux, family = "zeroinflatednbinomial1", data = covid_census_cdc.exclude.ny, control.inla = list(diagonal = 100, strategy = "gaussian", int.strategy = "eb"), control.predictor=list(A=covid_census_cdc.exclude.ny$A), verbose=F)
inla.deaths.svc.exclude.ny.leroux.zinb <- inla(deaths.svc.exclude.ny.leroux, family = "zeroinflatednbinomial1", data = covid_census_cdc.exclude.ny, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE), control.inla = list(strategy="simplified.laplace",cmin=0), control.mode = list(result = starting.value, restart=T))
#inla.deaths.svc.exclude.ny.leroux.zinb <- inla(deaths.svc.exclude.ny.leroux, family = "zeroinflatednbinomial1", data = covid_census_cdc.exclude.ny, control.compute = list(dic = T, waic = T, cpo = T), control.predictor = list(compute = TRUE))
date()
save.image("~/Dropbox/Research/COVID/COVID-19/Code/R Workspaces/COVID and RACE - final lpt07252020 ZINB excl NY.RData")

#####################################
##################################### inla results
#####################################

#### summary of fit statistics
inla.cases.deaths.leroux.dic.us <- c(inla.cases.fixed.leroux$dic$dic, inla.cases.svc.leroux$dic$dic, inla.deaths.fixed.leroux$dic$dic, inla.deaths.svc.leroux$dic$dic)
inla.cases.deaths.leroux.dic.ny <- c(inla.cases.fixed.ny.leroux$dic$dic, inla.cases.svc.ny.leroux$dic$dic, inla.deaths.fixed.ny.leroux$dic$dic, inla.deaths.svc.ny.leroux$dic$dic)
inla.cases.deaths.leroux.dic.us.exclude.ny <- c(inla.cases.fixed.exclude.ny.leroux$dic$dic, inla.cases.svc.exclude.ny.leroux$dic$dic, inla.deaths.fixed.exclude.ny.leroux$dic$dic, inla.deaths.svc.exclude.ny.leroux$dic$dic)

inla.cases.deaths.leroux.dic.us.zinb <- c(inla.cases.fixed.leroux.zinb$dic$dic, inla.cases.svc.leroux.zinb$dic$dic, inla.deaths.fixed.leroux.zinb$dic$dic, inla.deaths.svc.leroux.zinb$dic$dic)
inla.cases.deaths.leroux.dic.ny.zinb <- c(inla.cases.fixed.ny.leroux.zinb$dic$dic, inla.cases.svc.ny.leroux.zinb$dic$dic, inla.deaths.fixed.ny.leroux.zinb$dic$dic, inla.deaths.svc.ny.leroux.zinb$dic$dic)
inla.cases.deaths.leroux.dic.us.exclude.ny.zinb <- c(inla.cases.fixed.exclude.ny.leroux.zinb$dic$dic, inla.cases.svc.exclude.ny.leroux.zinb$dic$dic, inla.deaths.fixed.exclude.ny.leroux.zinb$dic$dic, inla.deaths.svc.exclude.ny.leroux.zinb$dic$dic)

inla.cases.deaths.leroux.dic <- rbind(inla.cases.deaths.leroux.dic.us, inla.cases.deaths.leroux.dic.us.zinb,
                                      inla.cases.deaths.leroux.dic.ny, inla.cases.deaths.leroux.dic.ny.zinb,
                                      inla.cases.deaths.leroux.dic.us.exclude.ny, inla.cases.deaths.leroux.dic.us.exclude.ny.zinb)
colnames(inla.cases.deaths.leroux.dic) <- c("Cases - Fixed Race", "Cases - SVC Race", "Deaths - Fixed Race", "Deaths - SVC Race")

write.csv(inla.cases.deaths.leroux.dic, "~/Dropbox/Research/COVID/COVID-19/Results/fit.statistics.csv")

#### summary of inla models fixed effects
# negative binomial
# entire US
inla.cases.fixed.leroux.est <- inla.cases.fixed.leroux$summary.fixed
inla.deaths.fixed.leroux.est <- inla.deaths.fixed.leroux$summary.fixed
inla.cases.svc.leroux.est <- inla.cases.svc.leroux$summary.fixed
inla.deaths.svc.leroux.est <- inla.deaths.svc.leroux$summary.fixed

inla.est.us <- cbind(inla.cases.fixed.leroux.est, inla.cases.svc.leroux.est, inla.deaths.fixed.leroux.est, inla.deaths.svc.leroux.est)
#colnames(inla.est.us) <- c(rep("Cases - Fixed",7), rep("Cases - SVC",7), rep("Deaths - Fixed",7), rep("Deaths - SVC",7))
write.csv(inla.est.us, "/Users/lpp22/Dropbox/Research/COVID/COVID-19/Results/inla.est.us.nb.coefficients.csv")

# NY only
inla.cases.fixed.ny.leroux.est <- inla.cases.fixed.ny.leroux$summary.fixed
inla.deaths.fixed.ny.leroux.est <- inla.deaths.fixed.ny.leroux$summary.fixed
inla.cases.svc.ny.leroux.est <- inla.cases.svc.ny.leroux$summary.fixed
inla.deaths.svc.ny.leroux.est <- inla.deaths.svc.ny.leroux$summary.fixed

inla.est.ny <- cbind(inla.cases.fixed.ny.leroux.est, inla.cases.svc.ny.leroux.est, inla.deaths.fixed.ny.leroux.est, inla.deaths.svc.ny.leroux.est)
write.csv(inla.est.ny, "/Users/lpp22/Dropbox/Research/COVID/COVID-19/Results/inla.est.ny.nb.coefficients.csv")

# Excluding NY
inla.cases.fixed.exclude.ny.leroux.est <- inla.cases.fixed.exclude.ny.leroux$summary.fixed
inla.deaths.fixed.exclude.ny.leroux.est <- inla.deaths.fixed.exclude.ny.leroux$summary.fixed
inla.cases.svc.exclude.ny.leroux.est <- inla.cases.svc.exclude.ny.leroux$summary.fixed
inla.deaths.svc.exclude.ny.leroux.est <- inla.deaths.svc.exclude.ny.leroux$summary.fixed

inla.est.us.exclude.ny <- cbind(inla.cases.fixed.exclude.ny.leroux.est, inla.cases.svc.exclude.ny.leroux.est, inla.deaths.fixed.exclude.ny.leroux.est, inla.deaths.svc.exclude.ny.leroux.est)
write.csv(inla.est.us.exclude.ny, "/Users/lpp22/Dropbox/Research/COVID/COVID-19/Results/inla.est.us.exclude.ny.nb.coefficients.csv")


# zero-inflated negative binomial
# entire US
inla.cases.fixed.leroux.est.zinb <- inla.cases.fixed.leroux.zinb$summary.fixed
inla.deaths.fixed.leroux.est.zinb <- inla.deaths.fixed.leroux.zinb$summary.fixed
inla.cases.svc.leroux.est.zinb <- inla.cases.svc.leroux.zinb$summary.fixed
inla.deaths.svc.leroux.est.zinb <- inla.deaths.svc.leroux.zinb$summary.fixed

inla.est.us.zinb <- cbind(inla.cases.fixed.leroux.est.zinb, inla.cases.svc.leroux.est.zinb, inla.deaths.fixed.leroux.est.zinb, inla.deaths.svc.leroux.est.zinb)
#colnames(inla.est.us) <- c(rep("Cases - Fixed",7), rep("Cases - SVC",7), rep("Deaths - Fixed",7), rep("Deaths - SVC",7))
write.csv(inla.est.us.zinb, "/Users/lpp22/Dropbox/Research/COVID/COVID-19/Results/inla.est.us.zinb.coefficients.csv")

# NY only
inla.cases.fixed.ny.leroux.est.zinb <- inla.cases.fixed.ny.leroux.zinb$summary.fixed
inla.deaths.fixed.ny.leroux.est.zinb <- inla.deaths.fixed.ny.leroux.zinb$summary.fixed
inla.cases.svc.ny.leroux.est.zinb <- inla.cases.svc.ny.leroux.zinb$summary.fixed
inla.deaths.svc.ny.leroux.est.zinb <- inla.deaths.svc.ny.leroux.zinb$summary.fixed

inla.est.ny.zinb <- cbind(inla.cases.fixed.ny.leroux.est.zinb, inla.cases.svc.ny.leroux.est.zinb, inla.deaths.fixed.ny.leroux.est.zinb, inla.deaths.svc.ny.leroux.est.zinb)
write.csv(inla.est.ny.zinb, "/Users/lpp22/Dropbox/Research/COVID/COVID-19/Results/inla.est.ny.zinb.coefficients.csv")

# Excluding NY
inla.cases.fixed.exclude.ny.leroux.est.zinb <- inla.cases.fixed.exclude.ny.leroux.zinb$summary.fixed
inla.deaths.fixed.exclude.ny.leroux.est.zinb <- inla.deaths.fixed.exclude.ny.leroux.zinb$summary.fixed
inla.cases.svc.exclude.ny.leroux.est.zinb <- inla.cases.svc.exclude.ny.leroux.zinb$summary.fixed
inla.deaths.svc.exclude.ny.leroux.est.zinb <- inla.deaths.svc.exclude.ny.leroux.zinb$summary.fixed

inla.est.us.exclude.ny.zinb <- cbind(inla.cases.fixed.exclude.ny.leroux.est.zinb, inla.cases.svc.exclude.ny.leroux.est.zinb, inla.deaths.fixed.exclude.ny.leroux.est.zinb, inla.deaths.svc.exclude.ny.leroux.est.zinb)
write.csv(inla.est.us.exclude.ny.zinb, "/Users/lpp22/Dropbox/Research/COVID/COVID-19/Results/inla.est.us.exclude.ny.zinb.coefficients.csv")


# exponentiating coefficients and 95% CIs
exp.b0.mean <- inla.emarginal(exp,inla.cases.fixed.exclude.ny.leroux.zinb$marginals.fixed[[1]])
exp.b0.mean
exp.b0.95CI <- inla.qmarginal(c(0.025,0.975),
                              inla.tmarginal(exp,inla.cases.fixed.exclude.ny.leroux.zinb$marginals.fixed[[1]]))
exp.b0.95CI

exp(inla.cases.fixed.exclude.ny.leroux.zinb$summary.fixed$mean[1])
exp(inla.cases.fixed.exclude.ny.leroux.zinb$summary.fixed$`0.025quant`[1])
exp(inla.cases.fixed.exclude.ny.leroux.zinb$summary.fixed$`0.975quant`[1])




save.image("~/Dropbox/Research/COVID/COVID-19/Code/COVID and RACE - final lpt07252020.RData")

#load("~/Dropbox/Research/COVID/COVID-19/Code/R Workspaces/COVID and RACE - final lpt07252020.RData")



# obtain betas for deaths
inla.cases.svc.leroux.est.zinb.beta.black.RR <- exp(inla.cases.svc.leroux.zinb$summary.fixed[2,1])
inla.deaths.svc.leroux.est.zinb.beta.black.RR <- exp(inla.deaths.svc.leroux.zinb$summary.fixed[2,1])

inla.cases.svc.ny.leroux.est.zinb.beta.black.RR <- exp(inla.cases.svc.ny.leroux.zinb$summary.fixed[2,1])
inla.deaths.svc.ny.leroux.est.zinb.beta.black.RR <- exp(inla.deaths.svc.ny.leroux.zinb$summary.fixed[2,1])

inla.cases.svc.exclude.ny.leroux.est.zinb.beta.black.RR <- exp(inla.cases.svc.exclude.ny.leroux.zinb$summary.fixed[2,1])
inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.black.RR <- exp(inla.deaths.svc.exclude.ny.leroux.zinb$summary.fixed[2,1])


# obtain 95% CIs
# lower bound
inla.cases.svc.leroux.est.zinb.beta.black.RR.low <- exp(inla.cases.svc.leroux.zinb$summary.fixed[2,3])
inla.deaths.svc.leroux.est.zinb.beta.black.RR.low <- exp(inla.deaths.svc.leroux.zinb$summary.fixed[2,3])

inla.cases.svc.ny.leroux.est.zinb.beta.black.RR.low <- exp(inla.cases.svc.ny.leroux.zinb$summary.fixed[2,3])
inla.deaths.svc.ny.leroux.est.zinb.beta.black.RR.low <- exp(inla.deaths.svc.ny.leroux.zinb$summary.fixed[2,3])

inla.cases.svc.exclude.ny.leroux.est.zinb.beta.black.RR.low <- exp(inla.cases.svc.exclude.ny.leroux.zinb$summary.fixed[2,3])
inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.black.RR.low <- exp(inla.deaths.svc.exclude.ny.leroux.zinb$summary.fixed[2,3])

# upper bound
inla.cases.svc.leroux.est.zinb.beta.black.RR.upper <- exp(inla.cases.svc.leroux.zinb$summary.fixed[2,5])
inla.deaths.svc.leroux.est.zinb.beta.black.RR.upper <- exp(inla.deaths.svc.leroux.zinb$summary.fixed[2,5])

inla.cases.svc.ny.leroux.est.zinb.beta.black.RR.upper <- exp(inla.cases.svc.ny.leroux.zinb$summary.fixed[2,5])
inla.deaths.svc.ny.leroux.est.zinb.beta.black.RR.upper <- exp(inla.deaths.svc.ny.leroux.zinb$summary.fixed[2,5])

inla.cases.svc.exclude.ny.leroux.est.zinb.beta.black.RR.upper <- exp(inla.cases.svc.exclude.ny.leroux.zinb$summary.fixed[2,5])
inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.black.RR.upper <- exp(inla.deaths.svc.exclude.ny.leroux.zinb$summary.fixed[2,5])

RRs.cases.blacks <- c(inla.cases.svc.leroux.est.zinb.beta.black.RR, inla.cases.svc.ny.leroux.est.zinb.beta.black.RR, inla.cases.svc.exclude.ny.leroux.est.zinb.beta.black.RR)
low.RRs.cases.blacks <- c(inla.cases.svc.leroux.est.zinb.beta.black.RR.low, inla.cases.svc.ny.leroux.est.zinb.beta.black.RR.low, inla.cases.svc.exclude.ny.leroux.est.zinb.beta.black.RR.low)
upper.RRs.cases.blacks <- c(inla.cases.svc.leroux.est.zinb.beta.black.RR.upper, inla.cases.svc.ny.leroux.est.zinb.beta.black.RR.upper, inla.cases.svc.exclude.ny.leroux.est.zinb.beta.black.RR.upper)
betas.low.upper.cases.blacks <- cbind(RRs.cases.blacks, low.RRs.cases.blacks, upper.RRs.cases.blacks)
betas.low.upper.cases.blacks

RRs.deaths.blacks <- c(inla.deaths.svc.leroux.est.zinb.beta.black.RR, inla.deaths.svc.ny.leroux.est.zinb.beta.black.RR, inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.black.RR)
low.RRs.deaths.blacks <- c(inla.deaths.svc.leroux.est.zinb.beta.black.RR.low, inla.deaths.svc.ny.leroux.est.zinb.beta.black.RR.low, inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.black.RR.low)
upper.RRs.deaths.blacks <- c(inla.deaths.svc.leroux.est.zinb.beta.black.RR.upper, inla.deaths.svc.ny.leroux.est.zinb.beta.black.RR.upper, inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.black.RR.upper)
betas.low.upper.deaths.blacks <- cbind(RRs.deaths.blacks, low.RRs.deaths.blacks, upper.RRs.deaths.blacks)
betas.low.upper.deaths.blacks

##################
##################
# figure: stacked plots
##################
##################
library(ggplot2)

# create labels
boxLabels <- c("US", "NY","US - excluding NY")

# Enter summary data. boxOdds are the odds ratios (calculated elsewhere), boxCILow is the lower bound of the CI, boxCIHigh is the upper bound.
df <- data.frame(
  yAxis = length(boxLabels):1,
  boxOdds = c(inla.cases.svc.leroux.est.zinb.beta.black.RR, inla.cases.svc.ny.leroux.est.zinb.beta.black.RR, inla.cases.svc.exclude.ny.leroux.est.zinb.beta.black.RR),
  boxCILow = c(inla.cases.svc.leroux.est.zinb.beta.black.RR.low, inla.cases.svc.ny.leroux.est.zinb.beta.black.RR.low, inla.cases.svc.exclude.ny.leroux.est.zinb.beta.black.RR.low),
  boxCIHigh = c(inla.cases.svc.leroux.est.zinb.beta.black.RR.upper, inla.cases.svc.ny.leroux.est.zinb.beta.black.RR.upper, inla.cases.svc.exclude.ny.leroux.est.zinb.beta.black.RR.upper)
)

# Plot
pdf("/Users/lpp22/Dropbox/Research/COVID/COVID-19/Figures/bar_plots_cases_black.pdf")
yAxis = length(boxLabels):1
p <- ggplot(df, aes(x = boxOdds, y = yAxis))
p + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "black") +
  geom_point(size = 2.5, color = "red") +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = yAxis, labels = boxLabels) +
  scale_x_continuous(breaks = seq(0,7,1) ) +
  coord_trans(x = "identity") +
  ylab("COVID-19 Cases") +
  xlab("Estimated Relative Risks (95% Confience Intervals)") +
  annotate(geom = "text", y =1.1, x = 1.5, label ="", size = 3.5, hjust = 0) + ggtitle(" ")
dev.off()


# create labels
boxLabels <- c("US", "NY","US - excluding NY")

# Enter summary data. boxOdds are the odds ratios (calculated elsewhere), boxCILow is the lower bound of the CI, boxCIHigh is the upper bound.
df <- data.frame(
  yAxis = length(boxLabels):1,
  boxOdds = c(inla.deaths.svc.leroux.est.zinb.beta.black.RR, inla.deaths.svc.ny.leroux.est.zinb.beta.black.RR, inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.black.RR),
  boxCILow = c(inla.deaths.svc.leroux.est.zinb.beta.black.RR.low, inla.deaths.svc.ny.leroux.est.zinb.beta.black.RR.low, inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.black.RR.low),
  boxCIHigh = c(inla.deaths.svc.leroux.est.zinb.beta.black.RR.upper, inla.deaths.svc.ny.leroux.est.zinb.beta.black.RR.upper, inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.black.RR.upper)
)

# Plot
pdf("/Users/lpp22/Dropbox/Research/COVID/COVID-19/Figures/bar_plots_deaths_black.pdf")
yAxis = length(boxLabels):1
p <- ggplot(df, aes(x = boxOdds, y = yAxis))
p + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "black") +
  geom_point(size = 2.5, color = "red") +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = yAxis, labels = boxLabels) +
  scale_x_continuous(breaks = seq(0,7,1) ) +
  coord_trans(x = "identity") +
  ylab("COVID-19 Deaths") +
  xlab("Estimated Relative Risks (95% Confience Intervals)") +
  annotate(geom = "text", y =1.1, x = 1.5, label ="", size = 3.5, hjust = 0) + ggtitle(" ")
dev.off()

# obtain betas for all measures
inla.cases.svc.leroux.est.zinb.beta.black.RR <- exp(inla.cases.svc.leroux.zinb$summary.fixed[2,1])
inla.cases.svc.leroux.est.zinb.beta.hispanic.RR <- exp(inla.cases.svc.leroux.zinb$summary.fixed[3,1])
inla.cases.svc.leroux.est.zinb.beta.poverty.RR <- exp(inla.cases.svc.leroux.zinb$summary.fixed[4,1])
inla.cases.svc.leroux.est.zinb.beta.popden.RR <- exp(inla.cases.svc.leroux.zinb$summary.fixed[5,1])
inla.cases.svc.leroux.est.zinb.beta.medhouseval.RR <- exp(inla.cases.svc.leroux.zinb$summary.fixed[6,1])
inla.cases.svc.leroux.est.zinb.beta.medhouseinc.RR <- exp(inla.cases.svc.leroux.zinb$summary.fixed[7,1])
inla.cases.svc.leroux.est.zinb.beta.edu.RR <- exp(inla.cases.svc.leroux.zinb$summary.fixed[8,1])
inla.cases.svc.leroux.est.zinb.beta.old.RR <- exp(inla.cases.svc.leroux.zinb$summary.fixed[9,1])

inla.deaths.svc.leroux.est.zinb.beta.black.RR <- exp(inla.deaths.svc.leroux.zinb$summary.fixed[2,1])
inla.deaths.svc.leroux.est.zinb.beta.hispanic.RR <- exp(inla.deaths.svc.leroux.zinb$summary.fixed[3,1])
inla.deaths.svc.leroux.est.zinb.beta.poverty.RR <- exp(inla.deaths.svc.leroux.zinb$summary.fixed[4,1])
inla.deaths.svc.leroux.est.zinb.beta.popden.RR <- exp(inla.deaths.svc.leroux.zinb$summary.fixed[5,1])
inla.deaths.svc.leroux.est.zinb.beta.medhouseval.RR <- exp(inla.deaths.svc.leroux.zinb$summary.fixed[6,1])
inla.deaths.svc.leroux.est.zinb.beta.medhouseinc.RR <- exp(inla.deaths.svc.leroux.zinb$summary.fixed[7,1])
inla.deaths.svc.leroux.est.zinb.beta.edu.RR <- exp(inla.deaths.svc.leroux.zinb$summary.fixed[8,1])
inla.deaths.svc.leroux.est.zinb.beta.old.RR <- exp(inla.deaths.svc.leroux.zinb$summary.fixed[9,1])


inla.cases.svc.ny.leroux.est.zinb.beta.black.RR <- exp(inla.cases.svc.ny.leroux.zinb$summary.fixed[2,1])
inla.cases.svc.ny.leroux.est.zinb.beta.hispanic.RR <- exp(inla.cases.svc.ny.leroux.zinb$summary.fixed[3,1])
inla.cases.svc.ny.leroux.est.zinb.beta.poverty.RR <- exp(inla.cases.svc.ny.leroux.zinb$summary.fixed[4,1])
inla.cases.svc.ny.leroux.est.zinb.beta.popden.RR <- exp(inla.cases.svc.ny.leroux.zinb$summary.fixed[5,1])
#inla.cases.svc.ny.leroux.est.zinb.beta.medhouseval.RR <- exp(inla.cases.svc.ny.leroux.zinb$summary.fixed[6,1])
inla.cases.svc.ny.leroux.est.zinb.beta.medhouseinc.RR <- exp(inla.cases.svc.ny.leroux.zinb$summary.fixed[6,1])
inla.cases.svc.ny.leroux.est.zinb.beta.edu.RR <- exp(inla.cases.svc.ny.leroux.zinb$summary.fixed[7,1])
inla.cases.svc.ny.leroux.est.zinb.beta.old.RR <- exp(inla.cases.svc.ny.leroux.zinb$summary.fixed[8,1])

inla.deaths.svc.ny.leroux.est.zinb.beta.black.RR <- exp(inla.deaths.svc.ny.leroux.zinb$summary.fixed[2,1])
inla.deaths.svc.ny.leroux.est.zinb.beta.hispanic.RR <- exp(inla.deaths.svc.ny.leroux.zinb$summary.fixed[3,1])
inla.deaths.svc.ny.leroux.est.zinb.beta.poverty.RR <- exp(inla.deaths.svc.ny.leroux.zinb$summary.fixed[4,1])
inla.deaths.svc.ny.leroux.est.zinb.beta.popden.RR <- exp(inla.deaths.svc.ny.leroux.zinb$summary.fixed[5,1])
#inla.deaths.svc.ny.leroux.est.zinb.beta.medhouseval.RR <- exp(inla.deaths.svc.ny.leroux.zinb$summary.fixed[6,1])
inla.deaths.svc.ny.leroux.est.zinb.beta.medhouseinc.RR <- exp(inla.deaths.svc.ny.leroux.zinb$summary.fixed[6,1])
inla.deaths.svc.ny.leroux.est.zinb.beta.edu.RR <- exp(inla.deaths.svc.ny.leroux.zinb$summary.fixed[7,1])
inla.deaths.svc.ny.leroux.est.zinb.beta.old.RR <- exp(inla.deaths.svc.ny.leroux.zinb$summary.fixed[8,1])


inla.cases.svc.exclude.ny.leroux.est.zinb.beta.black.RR <- exp(inla.cases.svc.exclude.ny.leroux.zinb$summary.fixed[2,1])
inla.cases.svc.exclude.ny.leroux.est.zinb.beta.hispanic.RR <- exp(inla.cases.svc.exclude.ny.leroux.zinb$summary.fixed[3,1])
inla.cases.svc.exclude.ny.leroux.est.zinb.beta.poverty.RR <- exp(inla.cases.svc.exclude.ny.leroux.zinb$summary.fixed[4,1])
inla.cases.svc.exclude.ny.leroux.est.zinb.beta.popden.RR <- exp(inla.cases.svc.exclude.ny.leroux.zinb$summary.fixed[5,1])
inla.cases.svc.exclude.ny.leroux.est.zinb.beta.medhouseval.RR <- exp(inla.cases.svc.exclude.ny.leroux.zinb$summary.fixed[6,1])
inla.cases.svc.exclude.ny.leroux.est.zinb.beta.medhouseinc.RR <- exp(inla.cases.svc.exclude.ny.leroux.zinb$summary.fixed[7,1])
inla.cases.svc.exclude.ny.leroux.est.zinb.beta.edu.RR <- exp(inla.cases.svc.exclude.ny.leroux.zinb$summary.fixed[8,1])
inla.cases.svc.exclude.ny.leroux.est.zinb.beta.old.RR <- exp(inla.cases.svc.exclude.ny.leroux.zinb$summary.fixed[9,1])

inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.black.RR <- exp(inla.deaths.svc.exclude.ny.leroux.zinb$summary.fixed[2,1])
inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.hispanic.RR <- exp(inla.deaths.svc.exclude.ny.leroux.zinb$summary.fixed[3,1])
inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.poverty.RR <- exp(inla.deaths.svc.exclude.ny.leroux.zinb$summary.fixed[4,1])
inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.popden.RR <- exp(inla.deaths.svc.exclude.ny.leroux.zinb$summary.fixed[5,1])
inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.medhouseval.RR <- exp(inla.deaths.svc.exclude.ny.leroux.zinb$summary.fixed[6,1])
inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.medhouseinc.RR <- exp(inla.deaths.svc.exclude.ny.leroux.zinb$summary.fixed[7,1])
inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.edu.RR <- exp(inla.deaths.svc.exclude.ny.leroux.zinb$summary.fixed[8,1])
inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.old.RR <- exp(inla.deaths.svc.exclude.ny.leroux.zinb$summary.fixed[9,1])



# obtain 95% CIs
# lower bound
inla.cases.svc.leroux.est.zinb.beta.black.RR.low <- exp(inla.cases.svc.leroux.zinb$summary.fixed[2,3])
inla.cases.svc.leroux.est.zinb.beta.hispanic.RR.low <- exp(inla.cases.svc.leroux.zinb$summary.fixed[3,3])
inla.cases.svc.leroux.est.zinb.beta.poverty.RR.low <- exp(inla.cases.svc.leroux.zinb$summary.fixed[4,3])
inla.cases.svc.leroux.est.zinb.beta.popden.RR.low <- exp(inla.cases.svc.leroux.zinb$summary.fixed[5,3])
inla.cases.svc.leroux.est.zinb.beta.medhouseval.RR.low <- exp(inla.cases.svc.leroux.zinb$summary.fixed[6,3])
inla.cases.svc.leroux.est.zinb.beta.medhouseinc.RR.low <- exp(inla.cases.svc.leroux.zinb$summary.fixed[7,3])
inla.cases.svc.leroux.est.zinb.beta.edu.RR.low <- exp(inla.cases.svc.leroux.zinb$summary.fixed[8,3])
inla.cases.svc.leroux.est.zinb.beta.old.RR.low <- exp(inla.cases.svc.leroux.zinb$summary.fixed[9,3])

inla.deaths.svc.leroux.est.zinb.beta.black.RR.low <- exp(inla.deaths.svc.leroux.zinb$summary.fixed[2,3])
inla.deaths.svc.leroux.est.zinb.beta.hispanic.RR.low <- exp(inla.deaths.svc.leroux.zinb$summary.fixed[3,3])
inla.deaths.svc.leroux.est.zinb.beta.poverty.RR.low <- exp(inla.deaths.svc.leroux.zinb$summary.fixed[4,3])
inla.deaths.svc.leroux.est.zinb.beta.popden.RR.low <- exp(inla.deaths.svc.leroux.zinb$summary.fixed[5,3])
inla.deaths.svc.leroux.est.zinb.beta.medhouseval.RR.low <- exp(inla.deaths.svc.leroux.zinb$summary.fixed[6,3])
inla.deaths.svc.leroux.est.zinb.beta.medhouseinc.RR.low <- exp(inla.deaths.svc.leroux.zinb$summary.fixed[7,3])
inla.deaths.svc.leroux.est.zinb.beta.edu.RR.low <- exp(inla.deaths.svc.leroux.zinb$summary.fixed[8,3])
inla.deaths.svc.leroux.est.zinb.beta.old.RR.low <- exp(inla.deaths.svc.leroux.zinb$summary.fixed[9,3])


inla.cases.svc.ny.leroux.est.zinb.beta.black.RR.low <- exp(inla.cases.svc.ny.leroux.zinb$summary.fixed[2,3])
inla.cases.svc.ny.leroux.est.zinb.beta.hispanic.RR.low <- exp(inla.cases.svc.ny.leroux.zinb$summary.fixed[3,3])
inla.cases.svc.ny.leroux.est.zinb.beta.poverty.RR.low <- exp(inla.cases.svc.ny.leroux.zinb$summary.fixed[4,3])
inla.cases.svc.ny.leroux.est.zinb.beta.popden.RR.low <- exp(inla.cases.svc.ny.leroux.zinb$summary.fixed[5,3])
#inla.cases.svc.ny.leroux.est.zinb.beta.medhouseval.RR.low <- exp(inla.cases.svc.ny.leroux.zinb$summary.fixed[6,3])
inla.cases.svc.ny.leroux.est.zinb.beta.medhouseinc.RR.low <- exp(inla.cases.svc.ny.leroux.zinb$summary.fixed[6,3])
inla.cases.svc.ny.leroux.est.zinb.beta.edu.RR.low <- exp(inla.cases.svc.ny.leroux.zinb$summary.fixed[7,3])
inla.cases.svc.ny.leroux.est.zinb.beta.old.RR.low <- exp(inla.cases.svc.ny.leroux.zinb$summary.fixed[8,3])

inla.deaths.svc.ny.leroux.est.zinb.beta.black.RR.low <- exp(inla.deaths.svc.ny.leroux.zinb$summary.fixed[2,3])
inla.deaths.svc.ny.leroux.est.zinb.beta.hispanic.RR.low <- exp(inla.deaths.svc.ny.leroux.zinb$summary.fixed[3,3])
inla.deaths.svc.ny.leroux.est.zinb.beta.poverty.RR.low <- exp(inla.deaths.svc.ny.leroux.zinb$summary.fixed[4,3])
inla.deaths.svc.ny.leroux.est.zinb.beta.popden.RR.low <- exp(inla.deaths.svc.ny.leroux.zinb$summary.fixed[5,3])
#inla.deaths.svc.ny.leroux.est.zinb.beta.medhouseval.RR.low <- exp(inla.deaths.svc.ny.leroux.zinb$summary.fixed[6,3])
inla.deaths.svc.ny.leroux.est.zinb.beta.medhouseinc.RR.low <- exp(inla.deaths.svc.ny.leroux.zinb$summary.fixed[6,3])
inla.deaths.svc.ny.leroux.est.zinb.beta.edu.RR.low <- exp(inla.deaths.svc.ny.leroux.zinb$summary.fixed[7,3])
inla.deaths.svc.ny.leroux.est.zinb.beta.old.RR.low <- exp(inla.deaths.svc.ny.leroux.zinb$summary.fixed[8,3])


inla.cases.svc.exclude.ny.leroux.est.zinb.beta.black.RR.low <- exp(inla.cases.svc.exclude.ny.leroux.zinb$summary.fixed[2,3])
inla.cases.svc.exclude.ny.leroux.est.zinb.beta.hispanic.RR.low <- exp(inla.cases.svc.exclude.ny.leroux.zinb$summary.fixed[3,3])
inla.cases.svc.exclude.ny.leroux.est.zinb.beta.poverty.RR.low <- exp(inla.cases.svc.exclude.ny.leroux.zinb$summary.fixed[4,3])
inla.cases.svc.exclude.ny.leroux.est.zinb.beta.popden.RR.low <- exp(inla.cases.svc.exclude.ny.leroux.zinb$summary.fixed[5,3])
inla.cases.svc.exclude.ny.leroux.est.zinb.beta.medhouseval.RR.low <- exp(inla.cases.svc.exclude.ny.leroux.zinb$summary.fixed[6,3])
inla.cases.svc.exclude.ny.leroux.est.zinb.beta.medhouseinc.RR.low <- exp(inla.cases.svc.exclude.ny.leroux.zinb$summary.fixed[7,3])
inla.cases.svc.exclude.ny.leroux.est.zinb.beta.edu.RR.low <- exp(inla.cases.svc.exclude.ny.leroux.zinb$summary.fixed[8,3])
inla.cases.svc.exclude.ny.leroux.est.zinb.beta.old.RR.low <- exp(inla.cases.svc.exclude.ny.leroux.zinb$summary.fixed[9,3])

inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.black.RR.low <- exp(inla.deaths.svc.exclude.ny.leroux.zinb$summary.fixed[2,3])
inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.hispanic.RR.low <- exp(inla.deaths.svc.exclude.ny.leroux.zinb$summary.fixed[3,3])
inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.poverty.RR.low <- exp(inla.deaths.svc.exclude.ny.leroux.zinb$summary.fixed[4,3])
inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.popden.RR.low <- exp(inla.deaths.svc.exclude.ny.leroux.zinb$summary.fixed[5,3])
inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.medhouseval.RR.low <- exp(inla.deaths.svc.exclude.ny.leroux.zinb$summary.fixed[6,3])
inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.medhouseinc.RR.low <- exp(inla.deaths.svc.exclude.ny.leroux.zinb$summary.fixed[7,3])
inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.edu.RR.low <- exp(inla.deaths.svc.exclude.ny.leroux.zinb$summary.fixed[8,3])
inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.old.RR.low <- exp(inla.deaths.svc.exclude.ny.leroux.zinb$summary.fixed[9,3])


# upper bound
inla.cases.svc.leroux.est.zinb.beta.black.RR.upper <- exp(inla.cases.svc.leroux.zinb$summary.fixed[2,5])
inla.cases.svc.leroux.est.zinb.beta.hispanic.RR.upper <- exp(inla.cases.svc.leroux.zinb$summary.fixed[3,5])
inla.cases.svc.leroux.est.zinb.beta.poverty.RR.upper <- exp(inla.cases.svc.leroux.zinb$summary.fixed[4,5])
inla.cases.svc.leroux.est.zinb.beta.popden.RR.upper <- exp(inla.cases.svc.leroux.zinb$summary.fixed[5,5])
inla.cases.svc.leroux.est.zinb.beta.medhouseval.RR.upper <- exp(inla.cases.svc.leroux.zinb$summary.fixed[6,5])
inla.cases.svc.leroux.est.zinb.beta.medhouseinc.RR.upper <- exp(inla.cases.svc.leroux.zinb$summary.fixed[7,5])
inla.cases.svc.leroux.est.zinb.beta.edu.RR.upper <- exp(inla.cases.svc.leroux.zinb$summary.fixed[8,5])
inla.cases.svc.leroux.est.zinb.beta.old.RR.upper <- exp(inla.cases.svc.leroux.zinb$summary.fixed[9,5])

inla.deaths.svc.leroux.est.zinb.beta.black.RR.upper <- exp(inla.deaths.svc.leroux.zinb$summary.fixed[2,5])
inla.deaths.svc.leroux.est.zinb.beta.hispanic.RR.upper <- exp(inla.deaths.svc.leroux.zinb$summary.fixed[3,5])
inla.deaths.svc.leroux.est.zinb.beta.poverty.RR.upper <- exp(inla.deaths.svc.leroux.zinb$summary.fixed[4,5])
inla.deaths.svc.leroux.est.zinb.beta.popden.RR.upper <- exp(inla.deaths.svc.leroux.zinb$summary.fixed[5,5])
inla.deaths.svc.leroux.est.zinb.beta.medhouseval.RR.upper <- exp(inla.deaths.svc.leroux.zinb$summary.fixed[6,5])
inla.deaths.svc.leroux.est.zinb.beta.medhouseinc.RR.upper <- exp(inla.deaths.svc.leroux.zinb$summary.fixed[7,5])
inla.deaths.svc.leroux.est.zinb.beta.edu.RR.upper <- exp(inla.deaths.svc.leroux.zinb$summary.fixed[8,5])
inla.deaths.svc.leroux.est.zinb.beta.old.RR.upper <- exp(inla.deaths.svc.leroux.zinb$summary.fixed[9,5])


inla.cases.svc.ny.leroux.est.zinb.beta.black.RR.upper <- exp(inla.cases.svc.ny.leroux.zinb$summary.fixed[2,5])
inla.cases.svc.ny.leroux.est.zinb.beta.hispanic.RR.upper <- exp(inla.cases.svc.ny.leroux.zinb$summary.fixed[3,5])
inla.cases.svc.ny.leroux.est.zinb.beta.poverty.RR.upper <- exp(inla.cases.svc.ny.leroux.zinb$summary.fixed[4,5])
inla.cases.svc.ny.leroux.est.zinb.beta.popden.RR.upper <- exp(inla.cases.svc.ny.leroux.zinb$summary.fixed[5,5])
#inla.cases.svc.ny.leroux.est.zinb.beta.medhouseval.RR.upper <- exp(inla.cases.svc.ny.leroux.zinb$summary.fixed[6,5])
inla.cases.svc.ny.leroux.est.zinb.beta.medhouseinc.RR.upper <- exp(inla.cases.svc.ny.leroux.zinb$summary.fixed[6,5])
inla.cases.svc.ny.leroux.est.zinb.beta.edu.RR.upper <- exp(inla.cases.svc.ny.leroux.zinb$summary.fixed[7,5])
inla.cases.svc.ny.leroux.est.zinb.beta.old.RR.upper <- exp(inla.cases.svc.ny.leroux.zinb$summary.fixed[8,5])

inla.deaths.svc.ny.leroux.est.zinb.beta.black.RR.upper <- exp(inla.deaths.svc.ny.leroux.zinb$summary.fixed[2,5])
inla.deaths.svc.ny.leroux.est.zinb.beta.hispanic.RR.upper <- exp(inla.deaths.svc.ny.leroux.zinb$summary.fixed[3,5])
inla.deaths.svc.ny.leroux.est.zinb.beta.poverty.RR.upper <- exp(inla.deaths.svc.ny.leroux.zinb$summary.fixed[4,5])
inla.deaths.svc.ny.leroux.est.zinb.beta.popden.RR.upper <- exp(inla.deaths.svc.ny.leroux.zinb$summary.fixed[5,5])
#inla.deaths.svc.ny.leroux.est.zinb.beta.medhouseval.RR.upper <- exp(inla.deaths.svc.ny.leroux.zinb$summary.fixed[6,5])
inla.deaths.svc.ny.leroux.est.zinb.beta.medhouseinc.RR.upper <- exp(inla.deaths.svc.ny.leroux.zinb$summary.fixed[6,5])
inla.deaths.svc.ny.leroux.est.zinb.beta.edu.RR.upper <- exp(inla.deaths.svc.ny.leroux.zinb$summary.fixed[7,5])
inla.deaths.svc.ny.leroux.est.zinb.beta.old.RR.upper <- exp(inla.deaths.svc.ny.leroux.zinb$summary.fixed[8,5])


inla.cases.svc.exclude.ny.leroux.est.zinb.beta.black.RR.upper <- exp(inla.cases.svc.exclude.ny.leroux.zinb$summary.fixed[2,5])
inla.cases.svc.exclude.ny.leroux.est.zinb.beta.hispanic.RR.upper <- exp(inla.cases.svc.exclude.ny.leroux.zinb$summary.fixed[3,5])
inla.cases.svc.exclude.ny.leroux.est.zinb.beta.poverty.RR.upper <- exp(inla.cases.svc.exclude.ny.leroux.zinb$summary.fixed[4,5])
inla.cases.svc.exclude.ny.leroux.est.zinb.beta.popden.RR.upper <- exp(inla.cases.svc.exclude.ny.leroux.zinb$summary.fixed[5,5])
inla.cases.svc.exclude.ny.leroux.est.zinb.beta.medhouseval.RR.upper <- exp(inla.cases.svc.exclude.ny.leroux.zinb$summary.fixed[6,5])
inla.cases.svc.exclude.ny.leroux.est.zinb.beta.medhouseinc.RR.upper <- exp(inla.cases.svc.exclude.ny.leroux.zinb$summary.fixed[7,5])
inla.cases.svc.exclude.ny.leroux.est.zinb.beta.edu.RR.upper <- exp(inla.cases.svc.exclude.ny.leroux.zinb$summary.fixed[8,5])
inla.cases.svc.exclude.ny.leroux.est.zinb.beta.old.RR.upper <- exp(inla.cases.svc.exclude.ny.leroux.zinb$summary.fixed[9,5])

inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.black.RR.upper <- exp(inla.deaths.svc.exclude.ny.leroux.zinb$summary.fixed[2,5])
inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.hispanic.RR.upper <- exp(inla.deaths.svc.exclude.ny.leroux.zinb$summary.fixed[3,5])
inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.poverty.RR.upper <- exp(inla.deaths.svc.exclude.ny.leroux.zinb$summary.fixed[4,5])
inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.popden.RR.upper <- exp(inla.deaths.svc.exclude.ny.leroux.zinb$summary.fixed[5,5])
inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.medhouseval.RR.upper <- exp(inla.deaths.svc.exclude.ny.leroux.zinb$summary.fixed[6,5])
inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.medhouseinc.RR.upper <- exp(inla.deaths.svc.exclude.ny.leroux.zinb$summary.fixed[7,5])
inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.edu.RR.upper <- exp(inla.deaths.svc.exclude.ny.leroux.zinb$summary.fixed[8,5])
inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.old.RR.upper <- exp(inla.deaths.svc.exclude.ny.leroux.zinb$summary.fixed[9,5])


RRs.cases.us <- 
  c(inla.cases.svc.leroux.est.zinb.beta.black.RR,
    inla.cases.svc.leroux.est.zinb.beta.hispanic.RR,
    inla.cases.svc.leroux.est.zinb.beta.poverty.RR,
    inla.cases.svc.leroux.est.zinb.beta.popden.RR,
    inla.cases.svc.leroux.est.zinb.beta.medhouseval.RR,
    inla.cases.svc.leroux.est.zinb.beta.medhouseinc.RR,
    inla.cases.svc.leroux.est.zinb.beta.edu.RR,
    inla.cases.svc.leroux.est.zinb.beta.old.RR)

RRs.cases.ny <- 
  c(inla.cases.svc.ny.leroux.est.zinb.beta.black.RR,
    inla.cases.svc.ny.leroux.est.zinb.beta.hispanic.RR,
    inla.cases.svc.ny.leroux.est.zinb.beta.poverty.RR,
    inla.cases.svc.ny.leroux.est.zinb.beta.popden.RR,
    #inla.cases.svc.ny.leroux.est.zinb.beta.medhouseval.RR,
    inla.cases.svc.ny.leroux.est.zinb.beta.medhouseinc.RR,
    inla.cases.svc.ny.leroux.est.zinb.beta.edu.RR,
    inla.cases.svc.ny.leroux.est.zinb.beta.old.RR)

RRs.cases.exclude.ny <- 
  c(inla.cases.svc.exclude.ny.leroux.est.zinb.beta.black.RR,
    inla.cases.svc.exclude.ny.leroux.est.zinb.beta.hispanic.RR,
    inla.cases.svc.exclude.ny.leroux.est.zinb.beta.poverty.RR,
    inla.cases.svc.exclude.ny.leroux.est.zinb.beta.popden.RR,
    inla.cases.svc.exclude.ny.leroux.est.zinb.beta.medhouseval.RR,
    inla.cases.svc.exclude.ny.leroux.est.zinb.beta.medhouseinc.RR,
    inla.cases.svc.exclude.ny.leroux.est.zinb.beta.edu.RR,
    inla.cases.svc.exclude.ny.leroux.est.zinb.beta.old.RR)



low.RRs.cases.us <- 
  c(inla.cases.svc.leroux.est.zinb.beta.black.RR.low,
    inla.cases.svc.leroux.est.zinb.beta.hispanic.RR.low,
    inla.cases.svc.leroux.est.zinb.beta.poverty.RR.low,
    inla.cases.svc.leroux.est.zinb.beta.popden.RR.low,
    inla.cases.svc.leroux.est.zinb.beta.medhouseval.RR.low,
    inla.cases.svc.leroux.est.zinb.beta.medhouseinc.RR.low,
    inla.cases.svc.leroux.est.zinb.beta.edu.RR.low,
    inla.cases.svc.leroux.est.zinb.beta.old.RR.low)

low.RRs.cases.ny <- 
  c(inla.cases.svc.ny.leroux.est.zinb.beta.black.RR.low,
    inla.cases.svc.ny.leroux.est.zinb.beta.hispanic.RR.low,
    inla.cases.svc.ny.leroux.est.zinb.beta.poverty.RR.low,
    inla.cases.svc.ny.leroux.est.zinb.beta.popden.RR.low,
    #inla.cases.svc.ny.leroux.est.zinb.beta.medhouseval.RR.low,
    inla.cases.svc.ny.leroux.est.zinb.beta.medhouseinc.RR.low,
    inla.cases.svc.ny.leroux.est.zinb.beta.edu.RR.low,
    inla.cases.svc.ny.leroux.est.zinb.beta.old.RR.low)

low.RRs.cases.exclude.ny <- 
  c(inla.cases.svc.exclude.ny.leroux.est.zinb.beta.black.RR.low,
    inla.cases.svc.exclude.ny.leroux.est.zinb.beta.hispanic.RR.low,
    inla.cases.svc.exclude.ny.leroux.est.zinb.beta.poverty.RR.low,
    inla.cases.svc.exclude.ny.leroux.est.zinb.beta.popden.RR.low,
    inla.cases.svc.exclude.ny.leroux.est.zinb.beta.medhouseval.RR.low,
    inla.cases.svc.exclude.ny.leroux.est.zinb.beta.medhouseinc.RR.low,
    inla.cases.svc.exclude.ny.leroux.est.zinb.beta.edu.RR.low,
    inla.cases.svc.exclude.ny.leroux.est.zinb.beta.old.RR.low)


upper.RRs.cases.us <- 
  c(inla.cases.svc.leroux.est.zinb.beta.black.RR.upper,
    inla.cases.svc.leroux.est.zinb.beta.hispanic.RR.upper,
    inla.cases.svc.leroux.est.zinb.beta.poverty.RR.upper,
    inla.cases.svc.leroux.est.zinb.beta.popden.RR.upper,
    inla.cases.svc.leroux.est.zinb.beta.medhouseval.RR.upper,
    inla.cases.svc.leroux.est.zinb.beta.medhouseinc.RR.upper,
    inla.cases.svc.leroux.est.zinb.beta.edu.RR.upper,
    inla.cases.svc.leroux.est.zinb.beta.old.RR.upper)

upper.RRs.cases.ny <- 
  c(inla.cases.svc.ny.leroux.est.zinb.beta.black.RR.upper,
    inla.cases.svc.ny.leroux.est.zinb.beta.hispanic.RR.upper,
    inla.cases.svc.ny.leroux.est.zinb.beta.poverty.RR.upper,
    inla.cases.svc.ny.leroux.est.zinb.beta.popden.RR.upper,
    #inla.cases.svc.ny.leroux.est.zinb.beta.medhouseval.RR.upper,
    inla.cases.svc.ny.leroux.est.zinb.beta.medhouseinc.RR.upper,
    inla.cases.svc.ny.leroux.est.zinb.beta.edu.RR.upper,
    inla.cases.svc.ny.leroux.est.zinb.beta.old.RR.upper)

upper.RRs.cases.exclude.ny <- 
  c(inla.cases.svc.exclude.ny.leroux.est.zinb.beta.black.RR.upper,
    inla.cases.svc.exclude.ny.leroux.est.zinb.beta.hispanic.RR.upper,
    inla.cases.svc.exclude.ny.leroux.est.zinb.beta.poverty.RR.upper,
    inla.cases.svc.exclude.ny.leroux.est.zinb.beta.popden.RR.upper,
    inla.cases.svc.exclude.ny.leroux.est.zinb.beta.medhouseval.RR.upper,
    inla.cases.svc.exclude.ny.leroux.est.zinb.beta.medhouseinc.RR.upper,
    inla.cases.svc.exclude.ny.leroux.est.zinb.beta.edu.RR.upper,
    inla.cases.svc.exclude.ny.leroux.est.zinb.beta.old.RR.upper)



betas.low.upper.cases.us <- cbind(RRs.cases.us, low.RRs.cases.us, upper.RRs.cases.us)
betas.low.upper.cases.us

betas.low.upper.cases.ny <- cbind(RRs.cases.ny, low.RRs.cases.ny, upper.RRs.cases.ny)
betas.low.upper.cases.ny

betas.low.upper.cases.exclude.ny <- cbind(RRs.cases.exclude.ny, low.RRs.cases.exclude.ny, upper.RRs.cases.exclude.ny)
betas.low.upper.cases.exclude.ny


RRs.deaths.us <- 
  c(inla.deaths.svc.leroux.est.zinb.beta.black.RR,
    inla.deaths.svc.leroux.est.zinb.beta.hispanic.RR,
    inla.deaths.svc.leroux.est.zinb.beta.poverty.RR,
    inla.deaths.svc.leroux.est.zinb.beta.popden.RR,
    inla.deaths.svc.leroux.est.zinb.beta.medhouseval.RR,
    inla.deaths.svc.leroux.est.zinb.beta.medhouseinc.RR,
    inla.deaths.svc.leroux.est.zinb.beta.edu.RR,
    inla.deaths.svc.leroux.est.zinb.beta.old.RR)

RRs.deaths.ny <- 
  c(inla.deaths.svc.ny.leroux.est.zinb.beta.black.RR,
    inla.deaths.svc.ny.leroux.est.zinb.beta.hispanic.RR,
    inla.deaths.svc.ny.leroux.est.zinb.beta.poverty.RR,
    inla.deaths.svc.ny.leroux.est.zinb.beta.popden.RR,
    #inla.deaths.svc.ny.leroux.est.zinb.beta.medhouseval.RR,
    inla.deaths.svc.ny.leroux.est.zinb.beta.medhouseinc.RR,
    inla.deaths.svc.ny.leroux.est.zinb.beta.edu.RR,
    inla.deaths.svc.ny.leroux.est.zinb.beta.old.RR)

RRs.deaths.exclude.ny <- 
  c(inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.black.RR,
    inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.hispanic.RR,
    inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.poverty.RR,
    inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.popden.RR,
    inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.medhouseval.RR,
    inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.medhouseinc.RR,
    inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.edu.RR,
    inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.old.RR)



low.RRs.deaths.us <- 
  c(inla.deaths.svc.leroux.est.zinb.beta.black.RR.low,
    inla.deaths.svc.leroux.est.zinb.beta.hispanic.RR.low,
    inla.deaths.svc.leroux.est.zinb.beta.poverty.RR.low,
    inla.deaths.svc.leroux.est.zinb.beta.popden.RR.low,
    inla.deaths.svc.leroux.est.zinb.beta.medhouseval.RR.low,
    inla.deaths.svc.leroux.est.zinb.beta.medhouseinc.RR.low,
    inla.deaths.svc.leroux.est.zinb.beta.edu.RR.low,
    inla.deaths.svc.leroux.est.zinb.beta.old.RR.low)

low.RRs.deaths.ny <- 
  c(inla.deaths.svc.ny.leroux.est.zinb.beta.black.RR.low,
    inla.deaths.svc.ny.leroux.est.zinb.beta.hispanic.RR.low,
    inla.deaths.svc.ny.leroux.est.zinb.beta.poverty.RR.low,
    inla.deaths.svc.ny.leroux.est.zinb.beta.popden.RR.low,
    #inla.deaths.svc.ny.leroux.est.zinb.beta.medhouseval.RR.low,
    inla.deaths.svc.ny.leroux.est.zinb.beta.medhouseinc.RR.low,
    inla.deaths.svc.ny.leroux.est.zinb.beta.edu.RR.low,
    inla.deaths.svc.ny.leroux.est.zinb.beta.old.RR.low)

low.RRs.deaths.exclude.ny <- 
  c(inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.black.RR.low,
    inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.hispanic.RR.low,
    inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.poverty.RR.low,
    inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.popden.RR.low,
    inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.medhouseval.RR.low,
    inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.medhouseinc.RR.low,
    inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.edu.RR.low,
    inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.old.RR.low)


upper.RRs.deaths.us <- 
  c(inla.deaths.svc.leroux.est.zinb.beta.black.RR.upper,
    inla.deaths.svc.leroux.est.zinb.beta.hispanic.RR.upper,
    inla.deaths.svc.leroux.est.zinb.beta.poverty.RR.upper,
    inla.deaths.svc.leroux.est.zinb.beta.popden.RR.upper,
    inla.deaths.svc.leroux.est.zinb.beta.medhouseval.RR.upper,
    inla.deaths.svc.leroux.est.zinb.beta.medhouseinc.RR.upper,
    inla.deaths.svc.leroux.est.zinb.beta.edu.RR.upper,
    inla.deaths.svc.leroux.est.zinb.beta.old.RR.upper)

upper.RRs.deaths.ny <- 
  c(inla.deaths.svc.ny.leroux.est.zinb.beta.black.RR.upper,
    inla.deaths.svc.ny.leroux.est.zinb.beta.hispanic.RR.upper,
    inla.deaths.svc.ny.leroux.est.zinb.beta.poverty.RR.upper,
    inla.deaths.svc.ny.leroux.est.zinb.beta.popden.RR.upper,
    #inla.deaths.svc.ny.leroux.est.zinb.beta.medhouseval.RR.upper,
    inla.deaths.svc.ny.leroux.est.zinb.beta.medhouseinc.RR.upper,
    inla.deaths.svc.ny.leroux.est.zinb.beta.edu.RR.upper,
    inla.deaths.svc.ny.leroux.est.zinb.beta.old.RR.upper)

upper.RRs.deaths.exclude.ny <- 
  c(inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.black.RR.upper,
    inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.hispanic.RR.upper,
    inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.poverty.RR.upper,
    inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.popden.RR.upper,
    inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.medhouseval.RR.upper,
    inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.medhouseinc.RR.upper,
    inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.edu.RR.upper,
    inla.deaths.svc.exclude.ny.leroux.est.zinb.beta.old.RR.upper)



betas.low.upper.deaths.us <- cbind(RRs.deaths.us, low.RRs.deaths.us, upper.RRs.deaths.us)
betas.low.upper.deaths.us

betas.low.upper.deaths.ny <- cbind(RRs.deaths.ny, low.RRs.deaths.ny, upper.RRs.deaths.ny)
betas.low.upper.deaths.ny

betas.low.upper.deaths.exclude.ny <- cbind(RRs.deaths.exclude.ny, low.RRs.deaths.exclude.ny, upper.RRs.deaths.exclude.ny)
betas.low.upper.deaths.exclude.ny

##################
##################
# figure: stacked plots
##################
##################
library(ggplot2)

# entire US
# create labels
#boxLabels <- c("Cases (US)", "Cases (NY)","Cases (US - excluding NY)")
boxLabels <- c("Black", "Hispanic", "Poverty", "Population Density", "Median House Value", "Median House Income", "Education", "At Least 65 Years")

# Enter summary data. boxOdds are the odds ratios (calculated elsewhere), boxCILow is the lower bound of the CI, boxCIHigh is the upper bound.
df <- data.frame(
  yAxis = length(boxLabels):1,
  boxOdds = c(inla.cases.svc.leroux.est.zinb.beta.black.RR,
              inla.cases.svc.leroux.est.zinb.beta.hispanic.RR,
              inla.cases.svc.leroux.est.zinb.beta.poverty.RR,
              inla.cases.svc.leroux.est.zinb.beta.popden.RR,
              inla.cases.svc.leroux.est.zinb.beta.medhouseval.RR,
              inla.cases.svc.leroux.est.zinb.beta.medhouseinc.RR,
              inla.cases.svc.leroux.est.zinb.beta.edu.RR,
              inla.cases.svc.leroux.est.zinb.beta.old.RR),
  boxCILow = c(inla.cases.svc.leroux.est.zinb.beta.black.RR.low,
               inla.cases.svc.leroux.est.zinb.beta.hispanic.RR.low,
               inla.cases.svc.leroux.est.zinb.beta.poverty.RR.low,
               inla.cases.svc.leroux.est.zinb.beta.popden.RR.low,
               inla.cases.svc.leroux.est.zinb.beta.medhouseval.RR.low,
               inla.cases.svc.leroux.est.zinb.beta.medhouseinc.RR.low,
               inla.cases.svc.leroux.est.zinb.beta.edu.RR.low,
               inla.cases.svc.leroux.est.zinb.beta.old.RR.low),
  boxCIHigh = c(inla.cases.svc.leroux.est.zinb.beta.black.RR.upper,
                inla.cases.svc.leroux.est.zinb.beta.hispanic.RR.upper,
                inla.cases.svc.leroux.est.zinb.beta.poverty.RR.upper,
                inla.cases.svc.leroux.est.zinb.beta.popden.RR.upper,
                inla.cases.svc.leroux.est.zinb.beta.medhouseval.RR.upper,
                inla.cases.svc.leroux.est.zinb.beta.medhouseinc.RR.upper,
                inla.cases.svc.leroux.est.zinb.beta.edu.RR.upper,
                inla.cases.svc.leroux.est.zinb.beta.old.RR.upper)
)

# Plot
pdf("/Users/lpp22/Dropbox/Research/COVID/COVID-19/Figures/bar_plots_cases_us.pdf")
yAxis = length(boxLabels):1
p <- ggplot(df, aes(x = boxOdds, y = yAxis))
p + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "black") +
  geom_point(size = 2.5, color = "red") +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = yAxis, labels = boxLabels) +
  scale_x_continuous(breaks = seq(0,7,1) ) +
  coord_trans(x = "identity") +
  ylab("COVID-19 Cases: Entire US") +
  xlab("Estimated Relative Risks (95% Confience Intervals)") +
  annotate(geom = "text", y =1.1, x = 1.5, label ="", size = 3.5, hjust = 0) + ggtitle(" ")
dev.off()


# create labels
#boxLabels <- c("Cases (US)", "Cases (NY)","Cases (US - excluding NY)")
boxLabels <- c("Black", "Hispanic", "Poverty", "Population Density", "Median House Value", "Median House Income", "Education", "At Least 65 Years")

# Enter summary data. boxOdds are the odds ratios (calculated elsewhere), boxCILow is the lower bound of the CI, boxCIHigh is the upper bound.
df <- data.frame(
  yAxis = length(boxLabels):1,
  boxOdds = c(inla.deaths.svc.leroux.est.zinb.beta.black.RR,
              inla.deaths.svc.leroux.est.zinb.beta.hispanic.RR,
              inla.deaths.svc.leroux.est.zinb.beta.poverty.RR,
              inla.deaths.svc.leroux.est.zinb.beta.popden.RR,
              inla.deaths.svc.leroux.est.zinb.beta.medhouseval.RR,
              inla.deaths.svc.leroux.est.zinb.beta.medhouseinc.RR,
              inla.deaths.svc.leroux.est.zinb.beta.edu.RR,
              inla.deaths.svc.leroux.est.zinb.beta.old.RR),
  boxCILow = c(inla.deaths.svc.leroux.est.zinb.beta.black.RR.low,
               inla.deaths.svc.leroux.est.zinb.beta.hispanic.RR.low,
               inla.deaths.svc.leroux.est.zinb.beta.poverty.RR.low,
               inla.deaths.svc.leroux.est.zinb.beta.popden.RR.low,
               inla.deaths.svc.leroux.est.zinb.beta.medhouseval.RR.low,
               inla.deaths.svc.leroux.est.zinb.beta.medhouseinc.RR.low,
               inla.deaths.svc.leroux.est.zinb.beta.edu.RR.low,
               inla.deaths.svc.leroux.est.zinb.beta.old.RR.low),
  boxCIHigh = c(inla.deaths.svc.leroux.est.zinb.beta.black.RR.upper,
                inla.deaths.svc.leroux.est.zinb.beta.hispanic.RR.upper,
                inla.deaths.svc.leroux.est.zinb.beta.poverty.RR.upper,
                inla.deaths.svc.leroux.est.zinb.beta.popden.RR.upper,
                inla.deaths.svc.leroux.est.zinb.beta.medhouseval.RR.upper,
                inla.deaths.svc.leroux.est.zinb.beta.medhouseinc.RR.upper,
                inla.deaths.svc.leroux.est.zinb.beta.edu.RR.upper,
                inla.deaths.svc.leroux.est.zinb.beta.old.RR.upper)
)

# Plot
pdf("/Users/lpp22/Dropbox/Research/COVID/COVID-19/Figures/bar_plots_deaths_us.pdf")
yAxis = length(boxLabels):1
p <- ggplot(df, aes(x = boxOdds, y = yAxis))
p + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "black") +
  geom_point(size = 2.5, color = "red") +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = yAxis, labels = boxLabels) +
  scale_x_continuous(breaks = seq(0,7,1) ) +
  coord_trans(x = "identity") +
  ylab("COVID-19 Deaths: Entire US") +
  xlab("Estimated Relative Risks (95% Confience Intervals)") +
  annotate(geom = "text", y =1.1, x = 1.5, label ="", size = 3.5, hjust = 0) + ggtitle(" ")
dev.off()





# NY only
# create labels
#boxLabels <- c("Cases (US)", "Cases (NY)","Cases (US - excluding NY)")
boxLabels <- c("Black", "Hispanic", "Poverty", "Population Density", "Median House Income", "Education", "At Least 65 Years")

# Enter summary data. boxOdds are the odds ratios (calculated elsewhere), boxCILow is the lower bound of the CI, boxCIHigh is the upper bound.
df <- data.frame(
  yAxis = length(boxLabels):1,
  boxOdds = c(inla.cases.svc.ny.leroux.est.zinb.beta.black.RR,
              inla.cases.svc.ny.leroux.est.zinb.beta.hispanic.RR,
              inla.cases.svc.ny.leroux.est.zinb.beta.poverty.RR,
              inla.cases.svc.ny.leroux.est.zinb.beta.popden.RR,
              #inla.cases.svc.ny.leroux.est.zinb.beta.medhouseval.RR,
              inla.cases.svc.ny.leroux.est.zinb.beta.medhouseinc.RR,
              inla.cases.svc.ny.leroux.est.zinb.beta.edu.RR,
              inla.cases.svc.ny.leroux.est.zinb.beta.old.RR),
  boxCILow = c(inla.cases.svc.ny.leroux.est.zinb.beta.black.RR.low,
               inla.cases.svc.ny.leroux.est.zinb.beta.hispanic.RR.low,
               inla.cases.svc.ny.leroux.est.zinb.beta.poverty.RR.low,
               inla.cases.svc.ny.leroux.est.zinb.beta.popden.RR.low,
               #inla.cases.svc.ny.leroux.est.zinb.beta.medhouseval.RR.low,
               inla.cases.svc.ny.leroux.est.zinb.beta.medhouseinc.RR.low,
               inla.cases.svc.ny.leroux.est.zinb.beta.edu.RR.low,
               inla.cases.svc.ny.leroux.est.zinb.beta.old.RR.low),
  boxCIHigh = c(inla.cases.svc.ny.leroux.est.zinb.beta.black.RR.upper,
                inla.cases.svc.ny.leroux.est.zinb.beta.hispanic.RR.upper,
                inla.cases.svc.ny.leroux.est.zinb.beta.poverty.RR.upper,
                inla.cases.svc.ny.leroux.est.zinb.beta.popden.RR.upper,
                #inla.cases.svc.ny.leroux.est.zinb.beta.medhouseval.RR.upper,
                inla.cases.svc.ny.leroux.est.zinb.beta.medhouseinc.RR.upper,
                inla.cases.svc.ny.leroux.est.zinb.beta.edu.RR.upper,
                inla.cases.svc.ny.leroux.est.zinb.beta.old.RR.upper)
)

# Plot
pdf("/Users/lpp22/Dropbox/Research/COVID/COVID-19/Figures/bar_plots_cases_ny.pdf")
yAxis = length(boxLabels):1
p <- ggplot(df, aes(x = boxOdds, y = yAxis))
p + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "black") +
  geom_point(size = 2.5, color = "red") +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = yAxis, labels = boxLabels) +
  scale_x_continuous(breaks = seq(0,7,1) ) +
  coord_trans(x = "identity") +
  ylab("COVID-19 Cases: NY") +
  xlab("Estimated Relative Risks (95% Confience Intervals)") +
  annotate(geom = "text", y =1.1, x = 1.5, label ="", size = 3.5, hjust = 0) + ggtitle(" ")
dev.off()


# create labels
#boxLabels <- c("Cases (US)", "Cases (NY)","Cases (US - excluding NY)")
boxLabels <- c("Black", "Hispanic", "Poverty", "Population Density", "Median House Income", "Education", "At Least 65 Years")

# Enter summary data. boxOdds are the odds ratios (calculated elsewhere), boxCILow is the lower bound of the CI, boxCIHigh is the upper bound.
df <- data.frame(
  yAxis = length(boxLabels):1,
  boxOdds = c(inla.deaths.svc.ny.leroux.est.zinb.beta.black.RR,
              inla.deaths.svc.ny.leroux.est.zinb.beta.hispanic.RR,
              inla.deaths.svc.ny.leroux.est.zinb.beta.poverty.RR,
              inla.deaths.svc.ny.leroux.est.zinb.beta.popden.RR,
              #inla.deaths.svc.ny.leroux.est.zinb.beta.medhouseval.RR,
              inla.deaths.svc.ny.leroux.est.zinb.beta.medhouseinc.RR,
              inla.deaths.svc.ny.leroux.est.zinb.beta.edu.RR,
              inla.deaths.svc.ny.leroux.est.zinb.beta.old.RR),
  boxCILow = c(inla.deaths.svc.ny.leroux.est.zinb.beta.black.RR.low,
               inla.deaths.svc.ny.leroux.est.zinb.beta.hispanic.RR.low,
               inla.deaths.svc.ny.leroux.est.zinb.beta.poverty.RR.low,
               inla.deaths.svc.ny.leroux.est.zinb.beta.popden.RR.low,
               #inla.deaths.svc.ny.leroux.est.zinb.beta.medhouseval.RR.low,
               inla.deaths.svc.ny.leroux.est.zinb.beta.medhouseinc.RR.low,
               inla.deaths.svc.ny.leroux.est.zinb.beta.edu.RR.low,
               inla.deaths.svc.ny.leroux.est.zinb.beta.old.RR.low),
  boxCIHigh = c(inla.deaths.svc.ny.leroux.est.zinb.beta.black.RR.upper,
                inla.deaths.svc.ny.leroux.est.zinb.beta.hispanic.RR.upper,
                inla.deaths.svc.ny.leroux.est.zinb.beta.poverty.RR.upper,
                inla.deaths.svc.ny.leroux.est.zinb.beta.popden.RR.upper,
                #inla.deaths.svc.ny.leroux.est.zinb.beta.medhouseval.RR.upper,
                inla.deaths.svc.ny.leroux.est.zinb.beta.medhouseinc.RR.upper,
                inla.deaths.svc.ny.leroux.est.zinb.beta.edu.RR.upper,
                inla.deaths.svc.ny.leroux.est.zinb.beta.old.RR.upper)
)

# Plot
pdf("/Users/lpp22/Dropbox/Research/COVID/COVID-19/Figures/bar_plots_deaths_ny.pdf")
yAxis = length(boxLabels):1
p <- ggplot(df, aes(x = boxOdds, y = yAxis))
p + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "black") +
  geom_point(size = 2.5, color = "red") +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = yAxis, labels = boxLabels) +
  scale_x_continuous(breaks = seq(0,7,1) ) +
  coord_trans(x = "identity") +
  ylab("COVID-19 Deaths: NY") +
  xlab("Estimated Relative Risks (95% Confience Intervals)") +
  annotate(geom = "text", y =1.1, x = 1.5, label ="", size = 3.5, hjust = 0) + ggtitle(" ")
dev.off()



