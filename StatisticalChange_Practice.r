#Purpose:
#
#Some basic statistical change practice to see if we have enough data per lake to
#even consider looking at statistically significant change over time
#Turns out here, we do not at all



setwd("parent directory")

library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(stringr)
library(stringi)


#Bring in raw data

rawdata <- read.csv("LakeAnalysis.csv")


#Make sure to correct the data format, as it was in integer format

rawdata$COL_DATEX <- as.Date(as.character(rawdata$COL_DATEX), format = "%Y%m%d")

#Double check

str(rawdata)


#View a simple plot to see what we are dealing with, can manipulate this to zoom in

plot(rawdata$COL_DATEX, rawdata$CCCYANO, ylim = c(0,2000000))


#Note that data collection was not consistent until about 2011
#Break here chosen as 2016 because it is about half way


#Code to set up a list of lake names and their ID for reference

Names <- rawdata %>%
  mutate(rawdata, LakeName = substr(rawdata$SITE_NAME, 1,6)) %>%
  select(SITE_DESC,LakeName)

Names <- Names[!duplicated(Names[,c('LakeName')]),]

#Make lowercase names to make an easy search

Names$SITE_DESC <- tolower(Names$SITE_DESC)


#Select Break Year for the data

BreakYear <- "2016-01-01"

#Choose a cutoff year for how far back data goes, keeping in mind minimal reporting prior to 2011

CutOffYear <- "2010-01-01"


#Select a number of observations you would like as base N, at least 5

N_thresh <- 5


#Select the columns we want, as well as add a generic LakeName column 
#instead of the site name (which includes AA, etc.)

CleanedData <- rawdata %>%
  mutate(rawdata, LakeName = substr(rawdata$SITE_NAME, 1,6)) %>%
  select(SITE_DESC,LakeName, SITE_NAME, COL_DATEX,CCCYANO) %>%
  filter(COL_DATEX > as.Date(CutOffYear)) %>%
  mutate(ERA = ifelse(COL_DATEX >= as.Date(BreakYear), "POST", "PRE"))

CleanedData$CCCYANO[CleanedData$CCCYANO == 0] <- NA



#Make an empty vector

pval <- character()




#Loop to get p.values for each lake, comparing for the break year

for (i in Names$LakeName) {

CleanedData_temp <- filter(CleanedData, i == LakeName)

pval <- append(pval,ifelse(sum(with(CleanedData_temp, ERA == "PRE")) > N_thresh,
       ifelse(sum(with(CleanedData_temp, ERA == "POST")) > N_thresh, 
              t.test(CleanedData_temp$CCCYANO ~ CleanedData_temp$ERA, paired=FALSE, na.rm=TRUE )$p.value, "NA"),
       "NA"))

}


Names$pvalue <- pval

View(Names)






#This is for running on a single lake for easy stuff, to grab just the data

#Here is how to search the list for an individual code, based on the description (name)

Names%>%
  filter(str_detect(SITE_DESC, "tuttle"))

#Finally, we can look at an individual lake now that we know the code

LakeID <- c("LM0121")

SingleLake <- rawdata %>%
  mutate(rawdata, LakeName = substr(rawdata$SITE_NAME, 1,6)) %>%
  select(SITE_DESC,LakeName, SITE_NAME, COL_DATEX,CCCYANO, MCELISA) %>%
  filter(LakeName == LakeID)

SingleLake$CCCYANO[SingleLake$CCCYANO == 0] <- NA

plot(SingleLake$COL_DATEX, SingleLake$CCCYANO)






