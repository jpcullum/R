#Purpose:
#
#Toxin Levels vs Cell Count probability analysis
#However, this is a bit different as we are looking 
#Only at the Microcystis genera for cell count
#As this is the most likely to produce Microcystin toxin in the lakes
#We may see better coorelation for our lakes and get a better handle
#on when we should put out advisories

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
library(scales)


#bring in our data

OracleData <- read.csv("LakeAnalysis.csv")

head(OracleData)
str(OracleData)

#Date to date format

OracleData$COL_DATEX <- as.Date(as.character(OracleData$COL_DATEX), format = "%Y%m%d")

str(OracleData)

#Clean up our data, removing 2020 due to the lake of data (COVID, no fieldwork allowed)

CleanedRaw <- OracleData %>%
  mutate(LakeName = substr(OracleData$SITE_NAME, 1,6)) %>%
  mutate(Year = format(COL_DATEX, "%Y")) %>%
  select(LakeName, SITE_DESC, COL_DATEX, CCMICROCY, MCELISA, Year) %>%
  mutate(Type = substr(OracleData$SITE_NAME, 1,2)) %>%
  filter(Type == "LM") %>%
  filter(CCMICROCY > 2) %>%
  filter(MCELISA >= 0.15) %>%
  filter(!Year == "2020")

#for log analysis if needed

CleanedRaw$Cells1 <- CleanedRaw$CCMICROCY +1
CleanedRaw$Toxin1 <- CleanedRaw$MCELISA +1

CleanedRaw$CellsLog1 <- log(CleanedRaw$CCMICROCY +1)
CleanedRaw$ToxinLog1 <- log(CleanedRaw$MCELISA +1)

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
                               CCMICROCY=numeric(),
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
  
  
  CleanedRawCells <- select(CleanedRaw_1, LakeName, CCMICROCY)
  
  CleanedRawCells <- CleanedRawCells[order(CleanedRawCells$LakeName, -abs(CleanedRawCells$CCMICROCY)),]
  
  CleanedRawCells <- CleanedRawCells[!duplicated(CleanedRawCells$LakeName),]
  
  
  CleanedRaw_2 <- merge(CleanedRawCells, CleanedRawToxin, by.x="LakeName", by.y="LakeName")
  
  CleanedRaw_2$COL_DATEX <- AllDates[i]
  
  CleanedRaw_Final <- rbind(CleanedRaw_Final, CleanedRaw_2)
  
}







#Again, make a blank dataframe here


FinalData <- data.frame(LakeName=character(),
                        CCMICROCY=numeric(),
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
#this analysis is a bit cleaner and better than the generic code I have posted
#as I worked more fully on this code
#As this looks at a day number in the future, instead of sampling dates
#this negates those lakes where we may have had three sampling dates
#over five months, whereas other lakes may have 15 sampling dates
#over the same time period
#and this skews the data, or at least is inconsistent
#thus, this analysis is better as space between
#sampling dates is inconsistent


for (b in 1:length(NameList)) {
  
  
  CellstoToxins <- CleanedRaw_Final %>%
    filter(LakeName == NameList[b])
  
  attach(CellstoToxins)
  
  CellstoToxins <- CellstoToxins[order(COL_DATEX),]
  
  detach(CellstoToxins)
  
  
  
  CellstoToxins <- CellstoToxins %>%
    select(LakeName, COL_DATEX, CCMICROCY, MCELISA, Year)
  
  rownames(CellstoToxins) <- NULL
  
  
  YearSet <- CellstoToxins$Year
  
  YearSet <- YearSet[!duplicated(YearSet)]
  
  
  
  
  
  
  
  for (a in 1:length(YearSet)) {
    
    YearCleared <- CellstoToxins %>%
      filter(Year == YearSet[a])
    
    
    for (i in 1:nrow(YearCleared)) {
      
      Start <- i
      End <- max(which(YearCleared$COL_DATEX <= (YearCleared$COL_DATEX[i] + days(27))))
      
      YearCleared$"Maxof4"[i] <- max(YearCleared$MCELISA[Start:End], na.rm = TRUE)
      
      YearlyData <- YearCleared %>%
        select(LakeName, CCMICROCY, Maxof4, COL_DATEX)
      
    }
    
    FinalData <- rbind(FinalData,YearlyData)
    
  }
  
}







#Now that we have data, we can filter as needed and make graphs



#if you need to filter it
#FinalData <- FinalData %>%
#  filter(CCMICROCY < 3000000) %>%
#  filter(Maxof4 < 3000)


FinalData$Cells1 <- FinalData$CCMICROCY+1
FinalData$Toxin1 <- FinalData$Maxof4+1

FinalData$CellsLog1 <- log(FinalData$CCMICROCY+1)
FinalData$ToxinLog1 <- log(FinalData$Maxof4+1)



ggplot(FinalData, aes(CCMICROCY, Maxof4)) +  
  geom_point(color = "blue", size = 3) +
  scale_y_log10() +
  scale_x_log10()

ggplot(FinalData, aes(CCMICROCY, Maxof4)) +  
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
  filter(CCMICROCY < 3000000) %>%
  filter(MCELISA < 3000)

ggplot(RemoveOutliers, aes(CCMICROCY, MCELISA)) +
  geom_point(color = "blue", size = 3)





#Working with probabilities

Watch <- FinalData %>%
  filter(CCMICROCY < 250000)


(nrow(filter(Watch, Maxof4 > 4))/nrow(Watch))*100

Warning <- FinalData %>%
  filter(CCMICROCY > 250000)


(nrow(filter(Warning, Maxof4 > 8))/nrow(Warning))*100


Hazard <- FinalData %>%
  filter(CCMICROCY > 10000000)

(nrow(filter(Hazard, Maxof4 > 1000))/nrow(Hazard))*100


#instead of setting thresholds and getting graphs, we can look at probability at a threshold by
#creating a sequence between numbers, and getting a value for that threshold


Thresholds <- c(seq(from = 1, to = 100000000, by = 10000))

Percents <- c()

PercentsWat <- c()

for (i in Thresholds) {
  
  WarThresh <- i
  
  ExpWarning <- FinalData %>%
    filter(CCMICROCY > WarThresh)
  
  Percents <- append(Percents, ((nrow(filter(ExpWarning, Maxof4 > 8))/nrow(ExpWarning))*100))
  
}

for (i in Thresholds) {
  
  WatThresh <- i
  
  ExpWarning <- FinalData %>%
    filter(CCMICROCY > WatThresh)
  
  PercentsWat <- append(PercentsWat, ((nrow(filter(ExpWarning, Maxof4 > 4))/nrow(ExpWarning))*100))
  
}



Experimental <- data.frame(Thresholds, Percents, PercentsWat)

#clean this up, as we can see that above 80% there really isn't enough data to draw any conclusions

Experimental_filtered_percent <- filter(Experimental, Percents < 80)

Experimental_filtered_cells <- filter(Experimental, Thresholds < 2500000)


plot(Experimental_filtered$Thresholds, Experimental_filtered$Percents)

plot(Experimental_filtered$Thresholds, Experimental_filtered$PercentsWat)


graphcolors <- c("Warning" = "red", "Watch" = "yellow")

graph <- ggplot(Experimental_filtered_cells, aes(CellCount, AdvisoryChance)) +
  geom_point(aes(x = Thresholds, y = Percents, col = "Warning"), size = 2) +
  geom_point(aes(x = Thresholds, y = PercentsWat, col = "Watch"), size = 2) +
  xlab("Microcystis Cell Count (cells/mL)") +
  ylab("Chance of Toxins Reaching Advisory Level") +
  scale_y_continuous(breaks = seq(0, 100, 10), limits =c(0, 100)) +
  scale_x_continuous(breaks = c(80000, 250000, 500000, 1000000, 2000000, 2500000), labels = comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(colour = "Advisory Type") +
  scale_color_manual(values = graphcolors) +
  ggtitle("Probability Microcystis Cell Count \n Leads to Advisory Level Toxins") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  theme(plot.background = element_rect(fill = "#92BDFD", colour = "grey50")) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA))

graph


smallgraph <- ggplot(Experimental_filtered_cells, aes(CellCount, AdvisoryChance)) +
  geom_point(aes(x = Thresholds, y = Percents, col = "Warning"), size = 2) +
  geom_point(aes(x = Thresholds, y = PercentsWat, col = "Watch"), size = 2) +
  xlab("Microcystis Cell Count (cells/mL)") +
  ylab("Chance of Toxins Reaching Advisory Level") +
  ylim(25,90) +
  scale_x_continuous(breaks = c(80000, 250000, 500000), labels = comma, limits = c(0,500000)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(colour = "Advisory Type") +
  scale_color_manual(values = graphcolors) +
  ggtitle("Probability Microcystis Cell Count \n Leads to Advisory Level Toxins") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  theme(plot.background = element_rect(fill = "#92BDFD", colour = "grey50")) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA))


smallgraph







#Run for a nice, organized (by cells) output file, or just run the last time for organized by lake

attach(FinalData)

FinalData <- FinalData[order(CCMICROCY),]

detach(FinalData)


rownames(FinalData) <- NULL


write.csv(FinalData, "AdjustedCellsVsToxin.csv")



