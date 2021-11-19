#Purpose:
#
#To look at the cyanobacteria data for BLANK state, and analyze it to see
#If there are any patterns or coorelations we can see between the cell count
#measured in a lake and the toxin level measured within a few weeks
#This more easily allows us to back up our science that tells us
#if putting an advisory on that waterbody is justifiable.

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


#Pull in the data from a csv that holds the data from our Oracle Database

OracleData <- read.csv("LakeAnalysis.csv")

head(OracleData)
str(OracleData)

#Reset the date column to an actual date format for easier analysis

OracleData$COL_DATEX <- as.Date(as.character(OracleData$COL_DATEX), format = "%Y%m%d")

#Double check we correctly changed it

str(OracleData)


#We need to clean up the data, including splitting off Lake ID, the year, etc.

CleanedRaw <- OracleData %>%
  mutate(LakeName = substr(OracleData$SITE_NAME, 1,6)) %>%
  mutate(Year = format(COL_DATEX, "%Y")) %>%
  select(LakeName, SITE_DESC, COL_DATEX, CCCYANO, MCELISA, Year) %>%
  mutate(Type = substr(OracleData$SITE_NAME, 1,2)) %>%
  filter(Type == "LM") %>%
  filter(CCCYANO > 2) %>%
  filter(MCELISA >= 0.15) %>%
  filter(!Year == "2020")

#For log analyses, you need to use a +1 because some values are less than 1
#and a log function will error if a value is less than 1
#This is a Log+1 Transformation of data

CleanedRaw$Cells1 <- CleanedRaw$CCCYANO +1
CleanedRaw$Toxin1 <- CleanedRaw$MCELISA +1

CleanedRaw$CellsLog1 <- log(CleanedRaw$CCCYANO +1)
CleanedRaw$ToxinLog1 <- log(CleanedRaw$MCELISA +1)

#We can take a look at the individual lakes present in the dataset

NameList <- CleanedRaw$LakeName

NameList <- NameList[!duplicated(NameList)]

#Here we can grab all of the individual dates present to take a look at them

AllDates <- CleanedRaw$COL_DATEX
AllDates <- AllDates[!duplicated(AllDates)]





#Clean up the data to only provide the maximum values per waterbody per date, that avoids all of the 0 values
#it also helps clean it up to provide a better relationship
#because, in the end, we want to know if there are high cells somewhere on the lake
#is there a relationship in that we can confidently say
#there is a high probability that high toxins will show up at some point soon on that waterbody
#if there is little to no relationship, we have to weigh the risks and benefits
#for example, maybe we only put Watches out for cell counts, and maybe even warnings
#but do no beach closures
#we may also want to do an analysis of all cell counts above a certain value
#for example, maybe we cannot presume high toxins based on certain cell counts, but at a certain level
#the relationship becomes very clear
#this will help identify that


#make our blank dataframe to fill


CleanedRaw_Final <- data.frame(LakeName=character(),
                               CCCYANO=numeric(),
                               MCELISA=numeric(),
                               COL_DATEX=as.Date(character()),
                               Year=character())



#Here we order and grab the maximum value for each lake, at each date (there are often multiple points sampled)

for (i in 1:length(AllDates)) {

CleanedRaw_1 <- CleanedRaw %>%
  filter(COL_DATEX == AllDates[i])


CleanedRawToxin <- select(CleanedRaw_1, LakeName, MCELISA, Year)

CleanedRawToxin <- CleanedRawToxin[order(CleanedRawToxin$LakeName, -abs(CleanedRawToxin$MCELISA)),]

CleanedRawToxin <- CleanedRawToxin[!duplicated(CleanedRawToxin$LakeName),]


CleanedRawCells <- select(CleanedRaw_1, LakeName, CCCYANO)

CleanedRawCells <- CleanedRawCells[order(CleanedRawCells$LakeName, -abs(CleanedRawCells$CCCYANO)),]

CleanedRawCells <- CleanedRawCells[!duplicated(CleanedRawCells$LakeName),]


CleanedRaw_2 <- merge(CleanedRawCells, CleanedRawToxin, by.x="LakeName", by.y="LakeName")

CleanedRaw_2$COL_DATEX <- AllDates[i]

CleanedRaw_Final <- rbind(CleanedRaw_Final, CleanedRaw_2)

}







#Again, make a blank dataframe here


FinalData <- data.frame(LakeName=character(),
                          CCCYANO=numeric(),
                          Maxof4=numeric())


#stacked loops in order to do a full analysis
#initially, we are looking at each name in the name list, first
#then, we are ordering the list by date
#then, we select only the columns we want, and renaming the rows so that order properly from here on out
#then we grab the list of years present in the data
#within this loop, we have another loop that looks at this data for a specific lake
#but looks year-wise, so as not to mix years
#we then look another loop within that loop, which allows us to grab
#the highest value within a specified range
#for example, we look at the value for a specific date, and we look to see
#what is the maximum value this lake reached within x sampling dates?
#if the lake was at 10,000 cells, within the next three sampling dates, what was the maximum it reached?
#this allows us, with the data we have, to get a rough probability
#of what is the likelyhood that it reached a certain toxin level?



for (b in 1:length(NameList)) {


CellstoToxins <- CleanedRaw_Final %>%
  filter(LakeName == NameList[b])

attach(CellstoToxins)

CellstoToxins <- CellstoToxins[order(COL_DATEX),]

detach(CellstoToxins)



CellstoToxins <- CellstoToxins %>%
  select(LakeName, COL_DATEX, CCCYANO, MCELISA, Year)

rownames(CellstoToxins) <- NULL


YearSet <- CellstoToxins$Year

YearSet <- YearSet[!duplicated(YearSet)]







for (a in 1:length(YearSet)) {

YearCleared <- CellstoToxins %>%
  filter(Year == YearSet[a])


for (i in 1:nrow(YearCleared)) {

Start <- i
End <- Start + 2

YearCleared$"Maxof4"[i] <- max(YearCleared$MCELISA[Start:End], na.rm = TRUE)

YearlyData <- YearCleared %>%
  select(LakeName, CCCYANO, Maxof4)

}

FinalData <- rbind(FinalData,YearlyData)

}

}












#Now that we have data, we can filter as needed and make graphs

#if you need to filter it
#FinalData <- FinalData %>%
#  filter(CCCYANO < 3000000) %>%
#  filter(Maxof4 < 3000)

#Again, n+1 if we need to use a log scale


FinalData$Cells1 <- FinalData$CCCYANO+1
FinalData$Toxin1 <- FinalData$Maxof4+1

FinalData$CellsLog1 <- log(FinalData$CCCYANO+1)
FinalData$ToxinLog1 <- log(FinalData$Maxof4+1)


  
ggplot(FinalData, aes(CCCYANO, Maxof4)) +  
  geom_point(color = "blue", size = 3) +
  scale_y_log10() +
  scale_x_log10()

ggplot(FinalData, aes(CCCYANO, Maxof4)) +  
  geom_point(color = "blue", size = 3)



Cleaned <- ggplot(FinalData, aes(Cells1, Toxin1)) +  
  geom_point(color = "blue", size = 3) +
  scale_y_log10() +
  scale_x_log10()


Raw <- ggplot(CleanedRaw, aes(Cells1, Toxin1)) +
  geom_point(color = "blue", size = 3) +
  scale_y_log10() +
  scale_x_log10()


Compared <- grid.arrange(Cleaned, Raw, ncol=2)



#Filter, if you want it, to clean outliers

RemoveOutliers <- CleanedRaw %>%
  filter(CCCYANO < 3000000) %>%
  filter(MCELISA < 3000)

ggplot(RemoveOutliers, aes(CCCYANO, MCELISA)) +
  geom_point(color = "blue", size = 3)





#Working with probabilities, here is where it gets fun!

#we can filter the data, and see the cell counts below a level, etc.

Watch <- FinalData %>%
  filter(CCCYANO < 250000)


#check out code here
(nrow(filter(Watch, Maxof4 > 8))/nrow(Watch))*100

Warning <- FinalData %>%
  filter(CCCYANO > 250000)


(nrow(filter(Warning, Maxof4 > 8))/nrow(Warning))*100


Hazard <- FinalData %>%
  filter(CCCYANO > 10000000)

(nrow(filter(Hazard, Maxof4 > 1000))/nrow(Hazard))*100


#instead of setting thresholds and getting graphs, we can look at probability at a threshold by
#creating a sequence between numbers, and getting a value for that threshold

Thresholds <- c(seq(from = 1, to = 100000000, by = 10000))

Percents <- c()

PercentsWat <- c()

#here we can see the percent probability the toxins will exceed a specified threshold (here it is 8)
#with a certain cell count i, within the sampling range we selected above

for (i in Thresholds) {

WarThresh <- i

ExpWarning <- FinalData %>%
  filter(CCCYANO > WarThresh)

Percents <- append(Percents, ((nrow(filter(ExpWarning, Maxof4 > 8))/nrow(ExpWarning))*100))

}

for (i in Thresholds) {
  
  WatThresh <- i
  
  ExpWarning <- FinalData %>%
    filter(CCCYANO > WatThresh)
  
  PercentsWat <- append(PercentsWat, ((nrow(filter(ExpWarning, Maxof4 > 4))/nrow(ExpWarning))*100))
  
}



Experimental <- data.frame(Thresholds, Percents, PercentsWat)

#clean this up, as we can see that above 80% there really isn't enough data to draw any conclusions

Experimental_filtered <- filter(Experimental, Percents < 80)


plot(Experimental_filtered$Thresholds, Experimental_filtered$Percents)

plot(Experimental_filtered$Thresholds, Experimental_filtered$PercentsWat)


#and here we put it together! this will allow us to see what the probability
#is that a lake will, within a specified range, reach a toxin threshold
#based on cell count, comparing two experimental thresholds (for Watch and Warning)


graph <- ggplot(Experimental_filtered, aes(CellCount, AdvisoryChance)) +
  geom_point(aes(x = Thresholds, y = Percents, col = "Warning")) +
  geom_point(aes(x = Thresholds, y = PercentsWat, col = "Watch")) 

graph




#Run for a nice, organized (by cells) output file, or just run the last time for organized by lake

attach(FinalData)

FinalData <- FinalData[order(CCCYANO),]

detach(FinalData)


rownames(FinalData) <- NULL


write.csv(FinalData, "AdjustedCellsVsToxin.csv")



