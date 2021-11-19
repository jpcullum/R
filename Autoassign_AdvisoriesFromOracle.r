#Purpose:
#
#This code grabs sampling data from an csv, organizes it, and recommends
#specific advisories for each waterbody we sampled this week
#It also gives the raw data from each waterbody, in appropriate format, for review


setwd("parent directory")

library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(stringr)
library(stringi)

#pull in data

OracleData <- read.csv("LakeAnalysis.csv")

head(OracleData)
str(OracleData)

#format to date

OracleData$COL_DATEX <- as.Date(as.character(OracleData$COL_DATEX), format = "%Y%m%d")

#choose the sampling date
#choose previous date (column from excel file)
#choose current week date (column in excel file)

SampleDate <- "2021-10-25"

Previous <- "X10.18"
This <- "X10.25"


#next week's sampling date

Next <- "2021-11-01"

#basic filter and clean

CleanedRaw <- OracleData %>%
  mutate(LakeName = substr(OracleData$SITE_NAME, 1,6)) %>%
  filter(COL_DATEX >= as.Date(SampleDate)) %>%
  select(LakeName, CCCYANO, MCELISA) %>%
  rename(Cells = CCCYANO) %>%
  rename(Microcystin= MCELISA)

#looking purely at toxin levels

CleanedRawToxin <- select(CleanedRaw, LakeName, Microcystin)

#order to look for max

CleanedRawToxin <- CleanedRawToxin[order(CleanedRawToxin$LakeName, -abs(CleanedRawToxin$Microcystin)),]

#delete all records except for max

CleanedRawToxin <- CleanedRawToxin[!duplicated(CleanedRawToxin$LakeName),]

#Tells us what the advisory level will be based on toxin level

CleanedRawToxin <- mutate(CleanedRawToxin, ToxinAd = ifelse(CleanedRawToxin$Microcystin >= 8, "Warning", ifelse(CleanedRawToxin$Microcystin >= 4, "Watch", "None")))

#looking at cell counts

CleanedRawCells <- select(CleanedRaw, LakeName, Cells)

#looks for max value of cell count by lake

CleanedRawCells <- CleanedRawCells[order(CleanedRawCells$LakeName, -abs(CleanedRawCells$Cells)),]

CleanedRawCells <- CleanedRawCells[!duplicated(CleanedRawCells$LakeName),]

#tells advisory level based on cell count

CleanedRawCells <- mutate(CleanedRawCells, CellsAd = ifelse(CleanedRawCells$Cells >= 250001, "Warning", ifelse(CleanedRawCells$Cells >= 80001, "Watch", "None")))


#pulls in our weekly tracker file of lakes to format data correctly

Tracker <- read.csv("WeeklyStats_2.csv")

#cleans the tracker file

Tracker <- Tracker %>%
  rename(PreviousWeek = all_of(Previous)) %>%
  rename(ThisWeek = all_of(This)) %>%
  rename(Cells = This.week.cell.count..cells.mL.) %>%
  rename(Microcystin = This.week.microcystin..ug.L.) %>%
  rename(NextSampling = Projected.next.sample.date.) %>%
  mutate(NextSampling = as.Date(NextSampling, format = "%m/%d")) %>%
  select(-Cells, -Microcystin)

Tracker <- filter(Tracker, !Waterbody == "")
Tracker <- select(Tracker, -X)

#now we can look at the oracle data and feed it into our tracker file

Merged <- merge(Tracker, CleanedRawCells, by.x="ID", by.y="LakeName", all=TRUE)

Merged <- merge(Merged, CleanedRawToxin, by.x="ID", by.y="LakeName", all=TRUE)

Merged$ThisWeek <- ""


#This ifelse statement tells us what exactly the advisory will be
#and what it is for; for example, it may be a Watch level for cells, but not toxins
#or it may be a Watch for toxins, but Warning for Cells
#this allows for easy analysis afterwards according to pre-set procedures

Merged$ThisWeek <- ifelse(is.na(Merged$Cells), 
                          ifelse(substr(Merged$PreviousWeek, 1,3) == "War", "War-o", ifelse(Merged$PreviousWeek == "", "VISUAL", "Wat-o")), ifelse(Merged$PreviousWeek == "", "NEW", ifelse(Merged$PreviousWeek == "L", "NEW", ifelse(Merged$CellsAd == "Warning" & Merged$ToxinAd == "Warning", "War-cm",
                                                                                                                           ifelse(Merged$CellsAd == "Warning" & Merged$ToxinAd == "Watch", "War-c",
                                                                                                                                  ifelse(Merged$CellsAd == "Warning" & Merged$ToxinAd == "None", "War-c",
                                                                                                                                         ifelse(Merged$CellsAd == "Watch" & Merged$ToxinAd == "Warning", "War-m",
                                                                                                                                                ifelse(Merged$CellsAd == "None" & Merged$ToxinAd == "Warning", "War-m",
                                                                                                                                                       ifelse(Merged$CellsAd == "Watch" & Merged$ToxinAd == "Watch", "Wat-cm",
                                                                                                                                                              ifelse(Merged$CellsAd == "Watch" & Merged$ToxinAd == "None", "Wat-c",
                                                                                                                                                                     ifelse(Merged$CellsAd == "None" & Merged$ToxinAd == "Watch", "Wat-m", "L")))))))))))



#final document

Final <- select(Merged, ID, Waterbody, County, PreviousWeek, ThisWeek, Cells, Microcystin, NextSampling)

Final$Cells <- round(as.numeric(Final$Cells), digits = -2)

Final[is.na(Final)] <- ""

attach(Final)

Final <- Final[order(County, Waterbody),]

detach(Final)

write.csv(Final, "NextProposedAdvisories.csv")



#What to put into Apex, and what to expect, the raw data for us to double check over

Apex <- OracleData %>%
  select(SITE_NAME, SITE_DESC, COL_DATEX, COL_TIME, CCCYANO, MCELISA) %>%
  filter(COL_DATEX >= as.Date(SampleDate))

attach(Apex)

Apex <- Apex[order(SITE_NAME),]

detach(Apex)

write.csv(Apex, "ApexData.csv")
