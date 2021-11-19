#Purpose:
#
#Visualizing data, looking at Cell Counts of Toxins
#By lake, for specified date ranges

setwd("parent directory")

library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(stringr)
library(stringi)



rawdata <- read.csv("LakeAnalysis.csv")

head(rawdata)
str(rawdata)


#Just getting a list of all lakes, and their associated name

Names <- rawdata %>%
  mutate(rawdata, LakeName = substr(rawdata$SITE_NAME, 1,6)) %>%
  select(SITE_DESC,LakeName)

Names <- Names[!duplicated(Names[,c('LakeName')]),]

#Make lowercase names to make an easy search

Names$SITE_DESC <- tolower(Names$SITE_DESC)


#Here we can look up the code for a lake we don't know

Names%>%
  filter(str_detect(SITE_DESC, "scott"))


#Initial setup for the lake, by lake ID
#The working year start and end allows us to see a graph for a specified
#date range, whether this is one year, etc.

LakeID <- c("LM0450")
LakeName <- "Milford Lake Zone C"
WorkingYearStart <- "2021-04-01"
WorkingYearEnd <- "2021-10-31"

         
LakeData <- rawdata %>%
  filter(str_detect(SITE_NAME, LakeID))

#Switch to date format for analysis

LakeData$COL_DATEX <- as.Date(as.character(LakeData$COL_DATEX), format = "%Y%m%d")

str(LakeData)

LakeData$CCCYANO[LakeData$CCCYANO == 0] <- NA

LakeData$MCELISA[LakeData$MCELISA == 0] <- NA

#Nicely cleaned data for easy looking, but we really don't need this for the code or analysis
#This is purely useful for looking at the data ourselves to check for errors, with less columns

CleanedRaw <- rawdata %>%
  select(SITE_NAME, COL_DATEX,CCCYANO,MCELISA)

CleanedData <- LakeData %>%
  select(SITE_NAME, COL_DATEX, CCCYANO, MCELISA)

#End Basic Filter




#Find ideal Max Y for Cells

MaxCells_Yearly <- LakeData %>%
  filter(COL_DATEX >= as.Date(WorkingYearStart) & COL_DATEX <= as.Date(WorkingYearEnd)) %>%
  summarise(max = max(CCCYANO, na.rm=TRUE)) %>%
  max()

MaxYScale_YearlyCells <- MaxCells_Yearly*1.1

MaxCells_AllTime <- max(LakeData$CCCYANO, na.rm=TRUE)

MaxYScale_AllTimeCells <- MaxCells_AllTime*1.1


#Find ideal Max Y for Toxins

MaxTox_Yearly <- LakeData %>%
  filter(COL_DATEX >= as.Date(WorkingYearStart) & COL_DATEX <= as.Date(WorkingYearEnd)) %>%
  summarise(max = max(MCELISA, na.rm=TRUE)) %>%
  max()

MaxYScale_YearlyToxin <- MaxTox_Yearly*1.1

MaxTox_AllTime <- LakeData %>%
  summarise(max = max(MCELISA, na.rm=TRUE)) %>%
  max()

MaxYScale_AllTimeToxin <- MaxTox_AllTime*1.1


#Make some graphs of this data

#Cell counts by year by lake

YearlyGraph_Cells <- ggplot(LakeData, aes(COL_DATEX, CCCYANO)) + 
  geom_point(color = ifelse(LakeData$CCCYANO>250000, "red", ifelse(LakeData$CCCYANO>80000, "yellow","green")), size = 4) + 
  geom_point(shape=1,size=4,color="#696969") +
  xlim(as.Date(c(WorkingYearStart,WorkingYearEnd))) + 
  scale_y_log10(breaks = c(5000,80000,250000,1000000,MaxYScale_YearlyCells)) +
  expand_limits(y = 100) +
  ggtitle(paste("Cyanobacterial Cell Counts \n ", LakeName, ", KS - 2021 HAB Season", sep = "")) +
  xlab("") +
  ylab("Cell Count (cells/mL)") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  theme(plot.background = element_rect(fill = "#92BDFD", colour = "grey50")) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA))
YearlyGraph_Cells

#Toxin by year by lake

YearlyGraph_Toxin <- ggplot(LakeData, aes(COL_DATEX, MCELISA)) +
  geom_point(color = ifelse(LakeData$MCELISA >8, "red", ifelse(LakeData$MCELISA>4, "yellow", "green")), size = 4) +
  geom_point(shape=1,size=4,color="#696969") +
  xlim(as.Date(c(WorkingYearStart,WorkingYearEnd))) +
  scale_y_log10(breaks = c(0.15,0.5,1,10,MaxYScale_YearlyToxin)) +
  ggtitle(paste("Microcystin Counts \n ", LakeName, ", KS - 2021 HAB Season", sep = "")) +
  xlab("") +
  ylab("Microcystin (ug/L)") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  theme(plot.background = element_rect(fill = "#92BDFD", colour = "grey50")) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA))
YearlyGraph_Toxin

#combined Cell Count and Toxin graphs (stacked) for specified date range

YearlyGraph_Combined <- grid.arrange(YearlyGraph_Cells, YearlyGraph_Toxin, nrow=2)


#GGPlot All Time for a lake

AllTimeGraph_Cells <- ggplot(LakeData, aes(COL_DATEX, CCCYANO)) + 
  geom_point(color = ifelse(LakeData$CCCYANO>250000, "red", ifelse(LakeData$CCCYANO>80000, "yellow","green")), size = 3) +
  geom_point(shape=1,size=3,color="#696969") +
  scale_y_log10(breaks = c(5000,80000,250000,1000000,MaxYScale_AllTimeCells)) +
  expand_limits(y = 100) +
  ggtitle(paste("Cyanobacterial Cell Counts \n ", LakeName, ", KS - HAB Season Data", sep = "")) +
  xlab("") +
  ylab("Cell Count (cells/mL)") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  theme(plot.background = element_rect(fill = "#92BDFD", colour = "grey50")) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA))
AllTimeGraph_Cells



AllTimeGraph_Toxin <- ggplot(LakeData, aes(COL_DATEX, MCELISA)) +
  geom_point(color = ifelse(LakeData$MCELISA >8, "red", ifelse(LakeData$MCELISA>4, "yellow", "green")), size = 3) +
  geom_point(shape=1,size=3,color="#696969") +
  scale_y_log10(breaks = c(0.15, 0.5, 1,10,MaxYScale_AllTimeToxin)) +
  ggtitle(paste("Microcystin Counts \n ", LakeName, ", KS - HAB Season Data", sep = "")) +
  xlab("") +
  ylab("Microcystin (ug/L)") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  theme(plot.background = element_rect(fill = "#92BDFD", colour = "grey50")) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA))
AllTimeGraph_Toxin


AllTimeGraph_Combined <- grid.arrange(AllTimeGraph_Cells, AllTimeGraph_Toxin, nrow=2)


