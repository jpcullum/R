#Purpose:
#
#Visalize some comparisons of various data types
#to see if there may be correlations, and could we build
#a predictive model off of these variables

setwd("parent directory")

library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(stringr)
library(stringi)
library(leaflet)
library(rgdal)
library(shiny)
library(xts)
library(shinyWidgets)
library(mapview)
library(magick)
library(animation)
library(dataRetrieval)


#raw data from our database

OracleData <- read.csv("LakeAnalysis.csv")

siteNumber <- "06856600"

parameterCd <- "00060"

#Pull in discharge data from USGS

DischargeData <- readNWISdv(siteNumber, parameterCd)

WaterTempData <- read.csv("BLANKLake_WaterTempData.csv")

WeatherData <- read.csv("WeatherDataRaw.csv")


#Clean the data up a bit

OracleCleaned <- OracleData %>%
  mutate(COL_DATEX = as.Date(as.character(COL_DATEX), format = "%Y%m%d")) %>%
  mutate(LakeName = substr(SITE_NAME, 1,6)) %>%
  filter(LakeName == "LMA190" | LakeName == "LMB190" | LakeName == "LMC190" | LakeName == "LM0190") %>%
  select(LakeName, COL_DATEX, CCCYANO, MCELISA) %>%
  filter(CCCYANO > 1)

DischargeCleaned <- DischargeData %>%
  rename(Discharge = X_00060_00003) %>%
  select(Date, Discharge) %>%
  filter(Discharge > 0)

WeatherCleaned <- WeatherData %>%
  mutate(DATE = as.Date(as.character(DATE), format = "%Y%m%d")) %>%
  select(DATE, PRCP, SNOW, TMAX, TMIN) %>%
  filter(PRCP >= 0 & SNOW >= 0 & TMAX > -50 & TMIN > -50)

str(WaterTempData)

TempDataCleaned <- WaterTempData %>%
  rename(Temp = X238075_00010) %>%
  mutate(Date = substr(datetime, 1,10)) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  filter(Temp > -50)

TempDataCleaned <- TempDataCleaned %>%
  select(Date, Temp) %>%
  group_by(Date) %>%
  summarise_at(vars(Temp), list(mean = mean))%>%
  ungroup()

#set a date range

FilterDate <- "2015-01-01"
Finish <- "2015-12-30"

#Make graphs for date range of cleaned data for each type

Cells <- ggplot(filter(OracleCleaned, COL_DATEX > as.Date(FilterDate) & COL_DATEX < as.Date(Finish)), aes(COL_DATEX, CCCYANO)) +
  geom_point(color = "red", size = 3)+
  geom_line(color = "blue", size = 1) +
  xlim(as.Date(FilterDate), as.Date(Finish)) +
  ggtitle("Cell Count") +
  xlab("") +
  ylab("Cell Count (cells/mL)") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  theme(plot.background = element_rect(fill = "#92BDFD", colour = "grey50")) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA))

Discharge <- ggplot(filter(DischargeCleaned, Date > as.Date(FilterDate) & Date < as.Date(Finish)), aes(Date, Discharge)) +
  geom_point(color = "red", size = 3)+
  geom_line(color = "blue", size = 1) +
  xlim(as.Date(FilterDate), as.Date(Finish)) +
  ggtitle("Daily Discharge") +
  xlab("") +
  ylab("Discharge (cfs?)") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  theme(plot.background = element_rect(fill = "#92BDFD", colour = "grey50")) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA))

Precip <- ggplot(filter(WeatherCleaned, DATE > as.Date(FilterDate) & DATE < as.Date(Finish)), aes(DATE, PRCP)) +
  geom_point(color = "red", size = 3)+
  geom_line(color = "blue", size = 1) +
  xlim(as.Date(FilterDate), as.Date(Finish)) +
  ggtitle("Precipitation") +
  xlab("") +
  ylab("Precip (in)") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  theme(plot.background = element_rect(fill = "#92BDFD", colour = "grey50")) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA))

Temp <- ggplot(filter(WeatherCleaned, DATE > as.Date(FilterDate) & DATE < as.Date(Finish)), aes(DATE, TMAX)) +
  geom_point(color = "red", size = 3)+
  geom_line(color = "blue", size = 1) +
  xlim(as.Date(FilterDate), as.Date(Finish)) +
  ggtitle("Max Daily Temperature") +
  xlab("") +
  ylab("Temperature (F)") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  theme(plot.background = element_rect(fill = "#92BDFD", colour = "grey50")) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA))

WaterTemp <- ggplot(filter(TempDataCleaned, Date > as.Date(FilterDate) & Date < as.Date(Finish)), aes(Date, mean)) +
  geom_point(color = "red", size = 3)+
  geom_line(color = "blue", size = 1) +
  xlim(as.Date(FilterDate), as.Date(Finish)) +
  ggtitle("Mean Daily Water Temperature") +
  xlab("") +
  ylab("Temperature (C)") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  theme(plot.background = element_rect(fill = "#92BDFD", colour = "grey50")) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA))


#Make a stacked plot of all graphs to compare

GraphsCombined <- grid.arrange(Cells, Discharge, Precip, Temp, WaterTemp, nrow=5)

GraphsCombined


