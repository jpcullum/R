#Purpose:
#
#Automate Thursday morning meeting prep
#This process was previously done manually, but it is entirely the same every week
#As such, I wanted to automate it to free up my time
#Here we grab the data from a tracker spreadsheet (working on automatically filling that out)
#This grabs the data, makes several files that used to be made by hand, writes a few emails
#and tells us what we need to do next





#Good morning, and welcome to the script that makes your Thursdays easy!
#Follow the instructions here, and all will go smoothly
#1) Make sure to fill in the tracker with all data and advisories, as you normlly would
#This step takes a bit of a human mind, so it is not automated currently, although we may in the future
#2) Copy the tracker (the currently active lakes) over to the WeeklyTrackerFilled file
#3) Save the file and you're good to go!

#4) Look at the script below and change the following:
#       Change the "Previous" and "This" dates to the dates for the previous week and this week, using the same format
#       Do the same for "Next" and use next week's sampling date (usually a Monday)

#5) Run the script!

#NOTE: you may need to install these packages if you have never used them on your computer before
#Simple delete the # in from of the "install.packages()" command, highlight it
#and hit "Run" in the top right corner of this window

#install.packages(dplyer)
#install.packages(dplyr)
#install.packages(lubridate)
#install.packages(ggplot2)
#install.packages(gridExtra)
#install.packages(stringr)
#install.packages(stringi)
#install.packages(expss)
#install.packages(openxlsx)
#install.packages(rtf)



#This sets the Working Directory for the file
#It tells R where to look for the saved files

setwd("parent directory")

#This "calls forward" the various packages you have installed, see above if you need to install them

library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(stringr)
library(stringi)
library(expss)
library(openxlsx)
library(rtf)


#Specify the dates for this week and last week, using the format below (just change the numbers!)
#Also setting the date for next week's sampling, and the initial beginning of this year's sampling

Previous <- "X18.Oct"
This <- "X25.Oct"

Next <- "2021-10-25"

SeasonStart <- "2021-04-01"


#Bring in data and set up

raw <- read.csv("WeeklyTrackerFilled.csv")

raw <- raw %>%
  rename(PreviousWeek = all_of(Previous)) %>%
  rename(ThisWeek = all_of(This)) %>%
  rename(Cells = This.week.cell.count..cells.mL.) %>%
  rename(Microcystin = This.week.microcystin..ug.L.) %>%
  rename(NextSampling = Projected.next.sample.date.) %>%
  mutate(NextSampling = as.Date(NextSampling, format = "%m/%d"))

raw <- filter(raw, !Waterbody == "")
raw <- select(raw, -X)

str(raw)

#Anything lifted

Lifted_df <- raw %>%
  select(Waterbody, County, PreviousWeek, ThisWeek, Cells, Microcystin) %>%
  filter(ThisWeek == "L") %>%
  mutate(Type = "LIFTED") %>%
  mutate(PreviousWeek = substr(PreviousWeek, 1,3)) %>%
  mutate(ThisWeek = substr(ThisWeek, 1,3)) %>%
  mutate(ThisWeek = ifelse(ThisWeek == "L", "Lifted", 
                           ifelse(ThisWeek == "Wat", "Watch", "Warning"))) %>%
  mutate(PreviousWeek = ifelse(PreviousWeek == "L", "Lifted", 
                               ifelse(PreviousWeek == "Wat", "Watch", "Warning")))


#Anything new

New_df <- raw %>%
  select(Waterbody, County, PreviousWeek, ThisWeek, Cells, Microcystin) %>%
  filter(PreviousWeek == "" | PreviousWeek == "L") %>%
  mutate(Type = "NEW") %>%
  mutate(ThisWeek = substr(ThisWeek, 1,3)) %>%
  mutate(ThisWeek = ifelse(ThisWeek == "L", "Lifted", 
                           ifelse(ThisWeek == "Wat", "Watch", "Warning")))



#Sampled waterbodies

Sampled_df <- raw %>%
  select(Waterbody, County, PreviousWeek, ThisWeek, Cells, Microcystin) %>%
  filter(!Cells == "") %>%
  mutate(PreviousWeek = substr(PreviousWeek, 1,3)) %>%
  mutate(ThisWeek = substr(ThisWeek, 1,3)) %>%
  mutate(ThisWeek = ifelse(ThisWeek == "L", "Lifted",ifelse(ThisWeek == "Wat", "Watch", "Warning"))) %>%
  mutate(PreviousWeek = ifelse(PreviousWeek == "L", "Lifted",ifelse(PreviousWeek == "Wat", "Watch", "Warning")))


#Supported by new data, but no change

Supported_df <- Sampled_df %>%
  filter(PreviousWeek == ThisWeek) %>%
  mutate(Type = "SUPPORTED")



#Changed, but not lifted

Changed_df <- Sampled_df %>%
  filter(!PreviousWeek == ThisWeek & !ThisWeek == "Lifted") %>%
  mutate(Type = "CHANGED")


#Carryover

Carryover_df <- raw %>%
  filter(Cells == "" & !PreviousWeek == "") %>%
  select(Waterbody, County, PreviousWeek, ThisWeek, Cells, Microcystin) %>%
  mutate(Type = "CARRYOVER") %>%
  mutate(PreviousWeek = substr(PreviousWeek, 1,3)) %>%
  mutate(ThisWeek = substr(ThisWeek, 1,3)) %>%
  mutate(ThisWeek = ifelse(ThisWeek == "L", "Lifted", 
                           ifelse(ThisWeek == "Wat", "Watch", "Warning"))) %>%
  mutate(PreviousWeek = ifelse(PreviousWeek == "L", "Lifted", 
                               ifelse(PreviousWeek == "Wat", "Watch", "Warning")))



Master_df <- rbind(Lifted_df, Supported_df, Changed_df, New_df, Carryover_df)


#This will create a master file if you need the raw CSV of the waterbodies, organized

#if(nrow(Master_df) >= 1) {write.csv(Master_df, 'CallNotesWaterbodies.csv')
#} else{
#  print(NA)}




#HAB for Hotline script

Hotline_df <- raw %>%
  mutate(Name = paste(Waterbody, ", ", County)) %>%
  mutate(Name = paste(Name, "County")) %>%
  select(Name, ThisWeek) %>%
  mutate(ThisWeek = substr(ThisWeek, 1,3))

attach(Hotline_df)

Hotline_df <- Hotline_df[order(ThisWeek, Name),]

detach(Hotline_df)

#if you need to create an individual file here, run this:

#if(nrow(Hotline_df) >= 1) {write.csv(Hotline_df, 'Hotlinescript.csv')
#} else{
#  print(NA)}


today <- Sys.Date()

Advisories_df <- Hotline_df %>%
  filter(!ThisWeek == "L")

Advisories_Lifted_df <- Hotline_df %>%
  filter(ThisWeek == "L")

Advisories_Watch_df <- Hotline_df %>%
  filter(ThisWeek == "Wat")

Advisories_Warning_df <- Hotline_df %>%
  filter(ThisWeek == "War")


HotlineScript <- RTF("HotlineScript.doc")
addParagraph(HotlineScript, paste(c("
BLANK

This is the BLANK Harmful Algal Blooms hotline.  The active HAB response program for recreational water bodies runs April 1-October 31.

In BLANK due to HABs are determined by the test results for harmful toxins or cyanobacterial cell counts above specific threshold. Advisories are typically issued on Thursdays.

As of ",format(today, "%a"), " ", format(today, "%b %d %Y"), " there are ", nrow(Advisories_df), " water bodies on advisory\n
",nrow(Advisories_Warning_df)," lakes are on WARNING status:\n"), collapse = "", sep = "\n"))

addTable(HotlineScript, select(Advisories_Warning_df, Name))

addParagraph(HotlineScript, paste(c("\nA Warning status indicates that conditions are unsafe for human and pet exposure.  Contact with the waterbody should be avoided.

", nrow(Advisories_Watch_df), " lakes are on WATCH status:\n"), collapse = "", sep ="\n"))

addTable(HotlineScript, select(Advisories_Watch_df, Name))

addParagraph(HotlineScript, paste(c("\nA Watch Status indicates that hazardous conditions may exist.  Water activities such as boating and fishing are safe; however, use caution when in contact with lake water and wash with clean water afterwards.

Also, since last week, ", nrow(Advisories_Lifted_df),  " advisory/ies have been LIFTED - so the following lakes are now on an all clear status:\n"), collapse = "", sep = "\n"))

addTable(HotlineScript, select(Advisories_Lifted_df, Name))

addParagraph(HotlineScript, "\n\nIf you wish to report a suspected bloom, please visit the BLANK.  If you do not have internet access but wish to report a bloom, call BLANK to be connected to an BLANK. That is BLANK; if you are calling after hours, leave a message with your name and number for callback.

At any time of year, if you are concerned that you have been exposed to harmful algae, contact your health care provider immediately. If you are concerned that your pets or livestock have been exposed, contact your veterinarian immediately.  Then, report any suspected HAB-related human or animal health incidents by calling BLANK, or visit the BLANK website and follow links to BLANK. 

Exposure to Harmful Algal Blooms, whether through contact or ingestion, can cause illness and even death in animals and humans. If you encounter a HAB, do not allow livestock, pets, or working animals such as hunting dogs to drink from affected waters, eat dried scum on shorelines, or lick their fur after exposure.  
HABs may take on a variety of appearances, such as a green or blue-green scum, floating clumps or strands, or water that is colored dark green.  Blooms are unpredictable and can emerge very quickly under certain conditions, so educate yourself, and remain vigilant. 
So remember, when in doubt, stay out!
")

done(HotlineScript)




#What is sampled next, and by whom

NextWeek_df <- raw %>%
  filter(NextSampling == as.Date(Next)) %>%
  select(Waterbody, County, Sampler) %>%
  mutate(Name = paste(Waterbody, ", ", County)) %>%
  select(Name, Sampler)


attach(NextWeek_df)

NextWeek_df <- NextWeek_df[order(Sampler, Name),]

detach(NextWeek_df)

#Samples we can expect


ExpectedSamples <- raw %>%
  filter(NextSampling == as.Date(Next)) %>%
  select(ID, Waterbody, Sampler)

attach(ExpectedSamples)

ExpectedSamples <- ExpectedSamples[order(Sampler, Waterbody),]

detach(ExpectedSamples)

write.csv(ExpectedSamples, "NextExpectedSamples.csv")

#if you need to create an individual file here, run this:

#if(nrow(NextWeek_df) >= 1) {write.csv(NextWeek_df, 'NextSampling.csv')
#} else{
#  print(NA)}


#HAB Call File

RollCall_df <- data.frame(BLANK_1 = c(""),
                          BLANK_2 = c("NE","NC","NW","SW","SC","SE"),
                          BLANK_3 = c(""),
                          BLANK_4 = c(""),
                          BLANK_5 = c(""))

RollCall_df <- rename(RollCall_df, "BLANK 1" = BLANK_1)
RollCall_df <- rename(RollCall_df, "BLANK 2" = BLANK_2)
RollCall_df <- rename(RollCall_df, "BLANK 3" = BLANK_3)
RollCall_df <- rename(RollCall_df, "BLANK 4" = BLANK_4)
RollCall_df <- rename(RollCall_df, "BLANK 5" = BLANK_5)



CallScript <- RTF("CallScript.doc")
addParagraph(CallScript,paste(c("HAB call on ", format(today, "%a %b %d %Y"),
                              "\n\nBLANK
BLANK
BLANK

Roll call - \n
"), collapse = "", sep = "\n"))

addTable(CallScript, RollCall_df)

addParagraph(CallScript, paste(c("\n\nThanks to the lake managers, sampling crews, and analytical staff who make the program work.

GENERAL REMINDERS/ANNOUNCEMENTS:  List here
                               

LAKE STATUS REPORTS

Last week had [[ENTER NUMBER HERE]] advisories. We are lifting ", nrow(Advisories_Lifted_df), " and adding ", nrow(New_df), " so this week we have ", nrow(Advisories_df),
                               " I will only mention resampling plans if they are for next week.

ADVISORIES LIFTED:  
\n\n"), collapse = "", sep = "\n"))

addTable(CallScript, select(Lifted_df, -Type))

addParagraph(CallScript, "\n\nEXISTING ADVISORIES SUPPORTED BY NEW DATA:\n\n ")

addTable(CallScript, select(Supported_df, -PreviousWeek, -Type))

addParagraph(CallScript, "\n\nADVISORIES CHANGED:\n\n")

addTable(CallScript, select(Changed_df, -Type))

addParagraph(CallScript, "\n\nNEW ADVISORIES\n\n")

addTable(CallScript, select(New_df, -PreviousWeek, -Type))

addParagraph(CallScript, "\n\nCARRYOVER ADVISORIES:  no new data\n\n")

addTable(CallScript, select(Carryover_df, Waterbody, County, ThisWeek))

addParagraph(CallScript, "\n\nOTHER SAMPLE RESULTS (for example, complaints investigated but not resulting in advisory)\n\n\n\n
Summary of sampling next week:  

We have projected resample dates on the attachment sent with email, so Ill only mention if they are scheduled for sampling next week.

NOTE TO SAMPLERS: we may not get formal sampling requests issued from the system, but we will make sure you get info you need.
\n\n")

addTable(CallScript, NextWeek_df)

addParagraph(CallScript,"\n\nNew Animal/Human Health Complaints under investigation:\n\n")

done(CallScript)


#Making them emails

Emails_df <- rbind(Lifted_df, Supported_df, Changed_df, New_df)

#if we need to add back in stuff for what happened to it
#  mutate(Advisory = ifelse(Type == "CHANGED", ifelse(PreviousWeek == "Watch", "elevated", "lowered"), 
#ifelse(Type == "LIFTED", "lifted", ifelse(Type == "NEW", "new", "supported"))))

#Now that we have a list, we can create an ifelse set to print an email


nrow(Emails_df)


stuff <- c(1:nrow(Emails_df))

#Runs before to clear out previous emails there

write("", "Emails.txt")



for (i in stuff) {
  
  Email <- paste(c("\nSTART EMAIL\n\n\nGood morning,\n\n",
                   if (Emails_df$Type[i] == "SUPPORTED") {paste(c("I am writing to let you know that we recieved new HAB data from sampling this week for ",Emails_df$Waterbody[i]))},
                   if (!Emails_df$Type[i] == "SUPPORTED") {paste(c("I am writing to let you know that we have had a tentative advisory change for ",Emails_df$Waterbody[i]
                   ))},
                   ". The data indicates that the waterbody is tentatively currently at a ",
                   Emails_df$ThisWeek[i], " status, based on a cell count of ", Emails_df$Cells[i], " cells/mL and a toxin count of ",
                   Emails_df$Microcystin[i], " ug/L.\nAs long as there are no last minute changes during our meeting today, the advisory will go into effect immediately after. Please keep an eye out for the news release for confirmation. If there are any changes we will reach out as soon as possible.",
                   if (Emails_df$Type[i] == "NEW") {paste(c("\n\nOnce the meeting today please put ", Emails_df$ThisWeek[i], " signage in place.\n"))},
                   if (Emails_df$Type[i] == "CHANGED") {paste(c("\n\nOnce the meeting today is concluded please remove any current ", Emails_df$PreviousWeek[i], " signage and replace it with ", Emails_df$ThisWeek[i], " signage."))},
                   if (Emails_df$Type[i] == "LIFTED") {paste(c("\n\nOnce the meeting today is concluded please remove any current ", Emails_df$PreviousWeek[i], " signage, but keep an eye out for future blooms."))},
                   "\n\nOur current Advisory thresholds are as follows:\n\nWatch: Greater than 80,000 cells/mL or 4.0 ug/L of toxins
        \nWarning: Greater than 250,000 cells/mL or 8.0 ug/L of toxins
        \nHazard: Greater than 10,000,000 cells/mL or 2,000 ug/L of toxins
        \n\nPlease let us know if you have any questions! Feel free to email or call if you see anything that does not seem quite right.
        \nIf you are not the correct contact for this waterbody, please feel free to forward this email on.
\nBest regards,
        
BLANK

        \n\n\nEND EMAIL"
  ), collapse = "", sep = "\n")
  
  
  write(Email, "Emails.txt", append =TRUE)
  
}


#This will help us plan for next sampling dates

Difference <- as.numeric(today() - as.Date(SeasonStart))/7

rawschedule <- read.csv("WeeklyTrackerFilled.csv", na.strings = c("", "NA"))

rawschedule <- rawschedule %>%
  rename(PreviousWeek = all_of(Previous)) %>%
  rename(ThisWeek = all_of(This)) %>%
  rename(Cells = This.week.cell.count..cells.mL.) %>%
  rename(Microcystin = This.week.microcystin..ug.L.) %>%
  rename(NextSampling = Projected.next.sample.date.) %>%
  mutate(NextSampling = as.Date(NextSampling, format = "%m/%d/%Y"))

End <- ncol(raw) - 3

position <- c(8:End)

Working <- raw %>%
  select(position)

Working[Working=="L"] <- 0
Working[!Working=="" & !Working=="0"] <- 1


Working <- as.data.frame(lapply(Working,as.numeric))

str(Working)


Working$Number <- rowSums(Working,na.rm=TRUE)


Working$Waterbody <- raw$Waterbody

Working$VISPercent <- raw$VIS

Working$CurrentAdvisory <- raw$ThisWeek

Working$PWS <- raw$PWS

Working$Cells <- raw$Cells

Scheduling <- Working %>%
  mutate(CurrentAdvisory = substr(CurrentAdvisory, 1,3)) %>%
  mutate(CurrentAdvisory = ifelse(CurrentAdvisory == "L", "Lifted", 
                                  ifelse(CurrentAdvisory == "Wat", "Watch", "Warning"))) %>%
  mutate(PWS = ifelse(PWS == "Y", -1, 0)) %>%
  filter(!Cells == "" & !CurrentAdvisory == "Lifted") %>%
  select(Waterbody, PWS, VISPercent, Number, CurrentAdvisory)

Scheduling[is.na(Scheduling)] <- 0

Scheduling$VISPercent <- ifelse(Scheduling$VISPercent == "nr", 80, ifelse(Scheduling$VISPercent == "25%", 25, 75))

str(Scheduling)

Scheduling$HABTimePercent <- ceiling((Scheduling$Number/Difference)*100)

Scheduling$Time <- ifelse(Scheduling$HABTimePercent >= 50, 2, 0)

Scheduling$Type <- ifelse(Scheduling$CurrentAdvisory == "Warning", 1, 2)

Scheduling$VIS <- ifelse(Scheduling$VISPercent <= 25, 0, ifelse(Scheduling$VISPercent == 75, 2, 3))

Suggestions <- Scheduling %>%
  select(PWS,Time,Type,VIS)

Suggestions$Score <- rowSums(Suggestions)

Suggestions$Waterbody <- Scheduling$Waterbody

Suggestions$Frequency <- ifelse(Suggestions$Score <= 1, "Weekly", ifelse(Suggestions$Score == 2, "Biweekly", ifelse(Suggestions$Score == 3, "Biweekly", ifelse(Suggestions$Score == 4, "Monthly", "Close of Season"))))

Suggestions <-  Suggestions %>%
  select(Waterbody, Frequency)

View(Suggestions)



#HAB for Communications


Communications_df <- raw %>%
  select(Waterbody, County, PreviousWeek, ThisWeek) %>%
  mutate(ThisWeek_s = substr(ThisWeek, 1,3)) %>%
  mutate(PreviousWeek_s = substr(PreviousWeek, 1,3)) %>%
  mutate("CURRENT ADVISORY STATUS" = ifelse(ThisWeek_s == "L", "Lifted", 
                                            ifelse(ThisWeek_s == "Wat", "Watch", "Warning"))) %>%
  mutate(Changed = ifelse(PreviousWeek_s == "" | PreviousWeek_s == "L", "Yes - NEW",
                          ifelse(PreviousWeek_s == ThisWeek_s, "No", 
                                 ifelse(ThisWeek_s == "L", "Yes - Lifted",
                                        ifelse(ThisWeek_s == "Wat", "Yes - lowered", "Yes - elevated"))))) %>%
  select(Waterbody, County, PreviousWeek, ThisWeek, "CURRENT ADVISORY STATUS", Changed)


if(nrow(Communications_df) >= 1) {write.xlsx(Communications_df, 'HABCommunications.xlsx', overwrite = TRUE)
} else{
  print(NA)}

