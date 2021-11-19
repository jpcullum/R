#Purpose:
#
#End of the week automation I am working on
#This simply makes a file structure and an end of the week email
#I will be integrating this into my main meeting automation to make things easy


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
library(rio)
library(expss)
library(openxlsx)
library(rtf)
library(tibble)




#Really want an end of the week script, that will create the necessary folders for next week, as well as make me
#the perfect email to send for "Next Expected Samples"
#This will make it very easy for me, and not a chore


#Make the folders first
dir.create(paste(c("_SitePhotos_",as.character(today(),format="%Y")),collapse=""))
dir.create(paste(c("_CubiPhotos_",as.character(today(),format="%Y")),collapse=""))
dir.create(paste(c("_ChainOfCustody_",as.character(today(),format="%Y")),collapse=""))

#if run on Fridays, this will do Monday

CoC <- paste(c("_ChainOfCustody_", as.character(today(), format = "%Y"), "/", as.character(today()+3, format = "%Y%m%d")), collapse ="")

dir.create(CoC)

CubiPhotos <- paste(c("_CubiPhotos_", as.character(today(), format = "%Y"), "/", as.character(today()+3, format = "%Y%m%d")), collapse ="")

dir.create(CubiPhotos)

SitePhotos <- paste(c("_SitePhotos_", as.character(today(), format = "%Y"), "/", as.character(today()+3, format = "%Y%m%d")), collapse ="")

dir.create(SitePhotos)



NextSamples <- read.csv(paste(c("X:/_WeeklyStatus_",as.character(today(),format="%Y"),"/WeeklyScript/NextExpectedSamples.csv"),collapse=""))

NumberSamples <- 1:nrow(NextSamples)

for (i in NumberSamples) {
  dir.create(paste(c(CoC,"/",NextSamples$Sampler[i]),collapse=""))
  dir.create(paste(c(CubiPhotos,"/",NextSamples$Waterbody[i]),collapse=""))
  dir.create(paste(c(SitePhotos,"/",NextSamples$Waterbody[i]),collapse=""))
}



Expected <- NextSamples %>%
  select(-X) %>%
  mutate("Cell Count (greenest)"= 1) %>%
  mutate(Toxin = "") 


Expected <- Expected %>%
  add_row(tibble_row(ID = "TOTALS", Waterbody = "", Sampler = "", "Cell Count (greenest)" = sum(Expected$Cell), Toxin = ""))


    
SamplesExpected <- RTF("Next_Expected_Samples.doc")

addTable(SamplesExpected, Expected)

addParagraph(SamplesExpected, "\nLet me know if you see anything amiss. 
 
Have a great weekend all!
\n")

done(SamplesExpected)



