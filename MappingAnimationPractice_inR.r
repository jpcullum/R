#Purpose:
#
#Making animated maps in R from spacial data

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

#bring in raw data

OracleData <- read.csv("LakeAnalysis.csv")

head(OracleData)
str(OracleData)

#format to date

OracleData$COL_DATEX <- as.Date(as.character(OracleData$COL_DATEX), format = "%Y%m%d")

#clean up our data to only look at lakes

CleanedRaw <- OracleData %>%
  mutate(LakeName = substr(OracleData$SITE_NAME, 1,6)) %>%
  select(LakeName, SITE_NAME, SITE_DESC, COL_DATEX, CCCYANO, MCELISA) %>%
  rename(Cells = CCCYANO) %>%
  rename(Microcystin= MCELISA) %>%
  mutate(Type = substr(OracleData$SITE_NAME, 1,2)) %>%
  filter(Type == "LM")

#bring in coordinates for lakes

LakeNames <- read.csv("LakeData.csv")

LakeNames <- LakeNames %>%
  rename(ID = PROGKEY) %>%
  rename(County = FEATURE_TYPE) %>%
  rename(Name = FEATURE_NAME) %>%
  select(ID, Name, LONGITUDE, LATITUDE)

#attach coordinates to cell counts and toxin levels 

Merged <- merge(CleanedRaw, LakeNames, by.x = "LakeName", by.y = "ID")

Merged <- Merged %>%
  select(LakeName, Name, COL_DATEX, Cells, LONGITUDE, LATITUDE)



#Here we can use leaflet to make maps

world_spdf <- readOGR( 
  dsn= "parent directory/world_shape_file" , 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)


#looking at what dates we are working with

Dates <- Merged$COL_DATEX

Dates <- Dates[!duplicated(Dates)]


#make a basic map, with markers based on cell count

map <- leaflet(world_spdf) %>% 
  setView(lng = -98.32, lat = 38.49, zoom = 7) %>% 
  addTiles() %>%
  
  addCircleMarkers(lng = ~Merged$LONGITUDE,
                   lat = ~Merged$LATITUDE,
                   radius = (Merged$Cells^0.48)/100,
                   stroke=FALSE,
                   fillOpacity = 0.3)

map

#all-time map of BLANK state

mapshot(map, file = "Map2.png")

mapimage <- image_read("Map2.png")

mapimage <- image_annotate(mapimage, "All Time", size= 45, gravity =  "northwest", color = "black")

image_write(mapimage, "Map2.png")





#Here we can run a looped session to get maps for each time point
#this is the base of the animation
#we then export each image separately
#we print the date of each map in order to be able to watch progress of the session

PickedYears <- c(2011:2021)

Start <- as.Date(sapply(PickedYears, paste, "-01-01", sep=""))
End <- as.Date(sapply(PickedYears, paste, "-12-31", sep=""))

YearSelection <- data.frame(Start,End)

Dates <- Dates[order(Dates)]



for (a in 1:nrow(YearSelection)) {

PostDate <- min(which(Dates > as.Date(YearSelection$Start[a])))
PreDate <- max(which(Dates < as.Date(YearSelection$End[a])))

CleanedDates <- Dates[PostDate:PreDate]

Min <- min(CleanedDates)
Max <- max(CleanedDates)

WeeklyAdvisories <- Merged %>%
  filter(COL_DATEX == CleanedDates[1]) %>%
  select(-COL_DATEX)

CurrentAdvisories <- WeeklyAdvisories



for (i in CleanedDates) {

WeeklyAdvisories2 <- Merged %>%
  filter(COL_DATEX == as.Date(i)) %>%
  select(-COL_DATEX)


CurrentAdvisories <- anti_join(CurrentAdvisories, WeeklyAdvisories2, by="LakeName") %>% bind_rows(WeeklyAdvisories2) %>% filter(Cells > 80000)

m <- leaflet(world_spdf) %>% 
  setView(lng = -98.32, lat = 38.49, zoom = 7) %>% 
  addTiles() %>%
  
  addCircleMarkers(lng = ~CurrentAdvisories$LONGITUDE,
                   lat = ~CurrentAdvisories$LATITUDE,
                   radius = (CurrentAdvisories$Cells^0.48)/100,
                   stroke = FALSE,
                   fillOpacity = 0.3) %>%
  
  addCircleMarkers(lng = ~CurrentAdvisories$LONGITUDE,
                   lat = ~CurrentAdvisories$LATITUDE,
                   radius = 0.05,
                   stroke=FALSE,
                   fillOpacity = 1)

mapshot(m, file = paste0("parent directory/MapsGif/map_", as.Date(i), ".png"))


mapimage <- image_read(paste0("parent directory/MapsGif/map_", as.Date(i), ".png"))

mapimage <- image_annotate(mapimage, as.Date(i), size= 45, gravity =  "northwest", color = "black")

image_write(mapimage, paste0("parent directory/MapsGif/map_", as.Date(i), ".png"))



print(as.Date(i))

}
}

#Make sure to run from the beginning


#make them into a gif, using Magick package

#install.packages("magick")

imgs <- list.files("parent directory/MapsGif", full.names = TRUE)

img_list <- lapply(imgs, image_read)

img_joined <- image_join(img_list)

img_animated <- image_animate(img_joined, fps = 10)

image_write(image = img_animated,
            path= "AnimatedMap.gif")







#Looking at BLANK Lake specifically


#BLANK Lake by point

BLANKPoints <- c("LMC190AD", "LMC190AC", "LMC190AF", "LMB190AI", "LMB190AB", "LMA190AE", "LMA190AH", "LMA190AG", "LMA190AA")
BLANKLat <- c(39.21155, 39.21067, 39.20984, 39.150576, 39.16522, 39.08555, 39.07775684, 39.115556, 39.09447)
BLANKLong <- c(-97.00559,-96.97298, -96.96101, -96.930349, -96.91105, -96.94028, -96.90590805, -96.898889, -96.90462)


BLANK <- data.frame(BLANKPoints, BLANKLat, BLANKLong)

BLANK <- BLANK %>%
  rename(lat = BLANKLat) %>%
  rename(long = BLANKLong) %>%
  rename(PointName = BLANKPoints)

BLANKMerged <- merge(CleanedRaw, BLANK, by.x = "SITE_NAME", by.y = "PointName")

BLANKMerged <- BLANKMerged %>%
  select(SITE_NAME, COL_DATEX, Cells, Microcystin, lat, long)



#make a map of the lake

BLANK <- leaflet(world_spdf) %>% 
  setView(lng = -96.945343, lat = 39.149640, zoom = 11.5) %>% 
  addTiles() %>%
  
  addCircleMarkers(lng = ~BLANKMerged$long,
                   lat = ~BLANKMerged$lat,
                   radius = (BLANKMerged$Cells^0.48)/100,
                   stroke=FALSE,
                   fillOpacity = 0.3)

BLANK



#Full animation of BLANK

DatesBLANK <- BLANKMerged$COL_DATEX



DatesBLANK <- DatesBLANK[!duplicated(DatesBLANK)]

PickedYears <- c(2011:2021)

Start <- as.Date(sapply(PickedYears, paste, "-01-01", sep=""))
End <- as.Date(sapply(PickedYears, paste, "-12-31", sep=""))

YearSelection <- data.frame(Start,End)

DatesBLANK <- DatesBLANK[order(DatesBLANK)]



for (a in 1:nrow(YearSelection)) {
  
  PostDate <- min(which(Dates > as.Date(YearSelection$Start[a])))
  PreDate <- max(which(Dates < as.Date(YearSelection$End[a])))
  
  CleanedDates <- DatesBLANK[PostDate:PreDate]
  
  Min <- min(CleanedDates)
  Max <- max(CleanedDates)
  
  WeeklyAdvisories <- BLANKMerged %>%
    filter(COL_DATEX == CleanedDates[1]) %>%
    select(-COL_DATEX)
  
  CurrentAdvisories <- WeeklyAdvisories
  
  
  
  for (i in CleanedDates) {
    
    WeeklyAdvisories2 <- BLANKMerged %>%
      filter(COL_DATEX == as.Date(i)) %>%
      select(-COL_DATEX)
    
    
    CurrentAdvisories <- anti_join(CurrentAdvisories, WeeklyAdvisories2, by="SITE_NAME") %>% bind_rows(WeeklyAdvisories2) %>% filter(Cells > 80000)
    
    m <- leaflet(world_spdf) %>% 
      setView(lng = -96.945343, lat = 39.149640, zoom = 11.5) %>% 
      addTiles() %>%
      
      addCircleMarkers(lng = ~CurrentAdvisories$long,
                       lat = ~CurrentAdvisories$lat,
                       radius = (CurrentAdvisories$Cells^0.48)/100,
                       stroke = FALSE,
                       fillOpacity = 0.3) %>%
      
      addCircleMarkers(lng = ~CurrentAdvisories$long,
                       lat = ~CurrentAdvisories$lat,
                       radius = 0.05,
                       stroke=FALSE,
                       fillOpacity = 1)
    
    mapshot(m, file = paste0("parent directory/MapsGif/BLANK/map_", as.Date(i), ".png"))
    
    
    mapimage <- image_read(paste0("parent directory/MapsGif/BLANK/map_", as.Date(i), ".png"))
    
    mapimage <- image_annotate(mapimage, as.Date(i), size= 45, gravity =  "northwest", color = "black")
    
    image_write(mapimage, paste0("parent directory/MapsGif/BLANK/map_", as.Date(i), ".png"))
    
    
    
    print(as.Date(i))
    
  }
}


imgs <- list.files("parent directory/MapsGif/BLANK", full.names = TRUE)

img_list <- lapply(imgs, image_read)

img_joined <- image_join(img_list)

img_animated <- image_animate(img_joined, fps = 10)

image_write(image = img_animated,
            path= "AnimatedMap_BLANK.gif")