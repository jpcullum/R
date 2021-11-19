#Purpose:
#
#Here we are grabbing raw lab benchsheet files, organizing them
#pulling the exact data we need, and then analyzing that data
#This focuses on cell counts of various genera of cyanobacteria sampled in BLANK
#state's waterbodies


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


#convert all files from xlsx to csv, and delete the extra xlsx files

xls <- dir(pattern = "xlsx")
created <- mapply(convert, xls, gsub("xlsx", "csv", xls))
unlink(xls)



#grab all of the file names in the folder

myfiles <- list.files(pattern="*.csv")


#prep the data frame

Final <- data.frame(Genera = as.character())



#now lets loop this in order to organize all files properly
#this looks at the structure of the file (which are all nearly identical)
#cuts off unnecessary rows, columns, etc.
#and it does this by looking for key phrases
#then, it organizes this data into one dataframe for easy analysis


for (i in myfiles) {

filename <- i

raw <- read.csv(filename)

Cleaned = raw[c(1:50), c(1,3,5,7,8, 11)]

colnames(Cleaned) <- "Class"

Starting <- which(Cleaned$Class == "Cyanophytes") +1

Stop <- which(Cleaned$Class == "Diatoms/Chrysophytes") -1

Cleaned = Cleaned[c(Starting:Stop),c(2:6)]

colnames(Cleaned) <- c("Genera", "RawCount", "SizingFactor", "CellCount", "mm3mL")

Cleaned <- filter(Cleaned, !Genera == "")

Cleaned$Date <- as.Date(substr(filename, 1,8), format = "%Y%m%d")

Cleaned$LakeID <- substr(filename, 10,15)

Cleaned$SiteID <- substr(filename, 10,17)

Cleaned$CellCount <- round(as.numeric(Cleaned$CellCount),0)

Cleaned$Percent <- (Cleaned$CellCount/sum(Cleaned$CellCount))*100

Cleaned$Percent <- round(as.numeric(Cleaned$Percent),2)

Final <- rbind(Final,Cleaned)

}


#We have to clean this up as there are a couple data rows that are useless for this analysis


#Final Clean

Final <- filter(Final, !Genera == "no cyanobacteria")

Final <- filter(Final, !Genera == "no cyanos counted")

Final <- filter(Final, !Genera == "<5,000")

Final$Genera <- str_replace_all(Final$Genera, "\\?","")


#Now we need to look at and analyze the data, first looking to clean up
#any misspellings on the genera categories

Types <- Final$Genera[!duplicated(Final$Genera)]

Types <- Types[order(Types)]


#Looking at the current data we had, we need to make the following changes
#This code renames misspellings to the proper names so we can categorize and graph properly


Final$Genera <- str_replace_all(Final$Genera, c("Syneccocus" = "Synechococcus", "Synechoccus" = "Synechococcus", "Synecoccus" = "Synechococcus"))

Final$Genera <- str_replace_all(Final$Genera, c("Aphanacapsa" = "Aphanocapsa", "Aphanoapsa" = "Aphanocapsa"))

Final$Genera <- str_replace_all(Final$Genera, c("Raphidiopsis" = "Raphidiopsis", "Cylindrospermopsis" = "Raphidiopsis", "Rahidiopsis" = "Raphidiopsis"))

Final$Genera <- str_replace_all(Final$Genera, c("Dolichspermum" = "Dolichospermum", "Dolichospermum \\(Anabaena\\)" = "Dolichospermum", "Dolichospermum" = "Dolichospermum"))

Final$Genera <- str_replace_all(Final$Genera,"Coleastrum", "Coelastrum")

Final$Genera <- str_replace_all(Final$Genera,"Cudpodothrix", "Cuspodothrix")

Final$Genera <- str_replace_all(Final$Genera,c("Planktothrix sp." = "Planktothrix", "Planktothrix isothrix" = "Planktothrix"))

Final$Genera <- str_replace_all(Final$Genera,"Microcystis wesenbergii", "Microcystis")


#Now, we can run this again and make sure we caught all of the problem entries

Types <- Final$Genera[!duplicated(Final$Genera)]

Types <- Types[order(Types)]

Types


#With all the data in a good spot, we can export to a final document for
#sending off to anyone, storage, and further analysis

write.csv(Final, "parent directory/TaxaFiles/TaxaAnalysis.csv")





#To group by week, lets first find all of the Mondays of this year

Daynumber <- today()-as.Date("2021-01-01")


getAllMondays <- function(year) {
  days <- as.POSIXlt(paste(year, 1:Daynumber, sep="-"), format="%Y-%j")
  Ms <- days[days$wday==1]
  Ms[!is.na(Ms)]  # Needed to remove NA from day 366 in non-leap years
}

Mondays <- as.Date(getAllMondays(2021))

Firstday <- min(Final$Date)
Lastday <- max(Final$Date)

Mondays_clean <- Mondays[Mondays >= Firstday]

Mondays_clean <- append(Mondays_clean, today())



#Here we can look start cleaning this up in various ways in order to play with visualizations
#initially, we are cleaning it up so that anything from a week (Starting Monday) will fall onto that week\
#this allows us to basically create bins to be used for barplots
#and still order by date

Barplot_clean_dump <- data.frame(Genera = as.character())

End <- length(Mondays_clean)

Dates <- 1:End


for (i in Dates) {
  
  Barplot_clean_filtered <- filter(Final, Date >= as.Date(Mondays_clean[i]) & Date < as.Date(Mondays_clean[i+1]))
  
  if (nrow(Barplot_clean_filtered > 0)) {Barplot_clean_filtered$Date = as.Date(Mondays_clean[i])}
  
  Barplot_clean_dump <- rbind(Barplot_clean_dump, Barplot_clean_filtered)
  
}


Barplot_clean_dump_2 <- Barplot_clean_dump


Barplot_clean_dump_2$Date <- as.character(Barplot_clean_dump$Date)



#Shows total occurrence

TaxaOccurancy <- Barplot_clean_dump_2 %>%
  group_by(Genera) %>%
  summarise(YearlyOccurence = round((sum(CellCount)/(sum(Final$CellCount)))*100,4))


#By genera by week, occurrence

Lake_Week <- Barplot_clean_dump_2 %>%
  group_by(Date, Genera) %>%
  summarise(Genera_CellCount = sum(CellCount))

WeekCount <- group_by(Barplot_clean_dump_2, Date) %>% summarise(Total_CellCount = sum(CellCount))

Lake_Week <- merge(Lake_Week, WeekCount, by = "Date")

Lake_Week <- Lake_Week %>%
  mutate(WeeklyOccurence = round((Genera_CellCount/Total_CellCount) *100,2)) %>%
  select(Genera, Date, WeeklyOccurence, Genera_CellCount)


Lake_Week <- merge(Lake_Week, TaxaOccurancy, by = "Genera")


b <- 1:nrow(Lake_Week)

for (i in b) {

if (Lake_Week$WeeklyOccurence[i] < 10 & Lake_Week$YearlyOccurence[i] < 10) {Lake_Week$Genera[i] = "*Other"}

}




#Number of lakes per week

Lakes_per_week <- Barplot_clean_dump_2 %>%
  group_by(Date, LakeID) %>%
  count()

Lakes_per_week_2 <- Lakes_per_week %>%
  group_by(Date) %>%
  count()

Lakes_per_week_2$pos <- (group_by(Barplot_clean_dump_2, Date) %>% summarise(Sum = sum(CellCount)))$Sum



#Plot it all, the big, final plot
#I'm not great at color choice, so I need something more aesthetic for colors
#but this works as is, and you can tell genera apart on the graph
#there are too many genera to use the generic colors

ggplot(Lake_Week, aes(x = Date, y = Genera_CellCount, fill = Genera)) + 
  geom_bar(position = "stack", stat="identity") +
  scale_fill_manual(values = c("#b5b5b5", "#ebdb78", "#27f527", "#1c801c", "#05a8a0", "#3da8f5", "#234df7", "#a078ff", "#6728fa", "#bb28fa", "#c219a6", "#a10d25")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # scale_y_log10()+
  xlab("") +
  ylab("Cell Count (cells/mL)") +
  ggtitle("Cell Counts by Genera") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  theme(plot.background = element_rect(fill = "#92BDFD", colour = "grey50")) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  labs(caption = "n = Lake Number\n*Mix of uncommon Cyanobacteria species\n**Dolichospermum formerly Anabaena\n ***Raphidiopsis formerly Cylindrospermopsis")+  
  geom_text(aes(Date, pos, label = paste("n =", n), fill = NULL), data = Lakes_per_week_2, vjust = -.5)













#Other analysis experimentation, works in progress


#Clean it more, so that we only get genera with three or more data points

Cleaned_final <- Final

CleanedList <- as.character()

for (i in Types) {
  if(nrow(filter(Cleaned_final, Genera == i)) >= 10) {CleanedList = append(CleanedList, i)}
  
}

Cleaned_final <- subset(Cleaned_final, Genera %in% CleanedList)


#Group them by averages to make better graphs


TaxaAvg <- Cleaned_final %>%
  group_by(Genera) %>%
  summarise(avg = round(mean(CellCount),0))



Low_tax <- filter(TaxaAvg, avg < 10000)$Genera
Mid_tax <- filter(TaxaAvg, avg >= 10000 & avg < 100000)$Genera
High_tax <- filter(TaxaAvg, avg > 100000)$Genera


Low_df <- subset(Cleaned_final, Genera %in% Low_tax)

Mid_df <- subset(Cleaned_final, Genera %in% Mid_tax)

High_df <- subset(Cleaned_final, Genera %in% High_tax)



#Full graph, not cleaned

ggplot(Final, aes(Date, CellCount)) +
  geom_point(aes(color = Genera)) +
  scale_y_log10()



Low <- ggplot(Low_df, aes(Date, CellCount, color = Genera)) +
  geom_smooth() +
  #geom_point(aes(color = Genera)) +
  scale_y_log10()



Mid <- ggplot(Mid_df, aes(Date, CellCount, color = Genera)) +
  geom_smooth() +
  #geom_point(aes(color = Genera)) +
  scale_y_log10()



High <- ggplot(High_df, aes(Date, CellCount, color = Genera)) +
  geom_smooth() +
  #geom_point(aes(color = Genera)) +
  scale_y_log10()


Compared <- grid.arrange(Low, Mid, High, nrow=3)



CellCounts <- ggplot(Cleaned_final, aes(Date, CellCount, color = Genera)) +
  geom_smooth(se=FALSE, size = 2) +
  scale_y_log10()


#Goal: look at the cyanobacteria as percents of the population
#How? Look at a week of data, count up all bacteria, then count up totals of each group, and get a percent
#What will this accomplish: this will show percent of the population that is occupied by species
#We will then see if there are any patterns across the summer! Raw numbers are cool, but weighted stuff is good too

#Another, similar way is to take each individual record and look at percent at each location, that may be better
#This will give more preceise data points and we can average, and it will be easier to tell
#if there are odd outliers

Percents <- ggplot(Cleaned_final, aes(Date, Percent, color = Genera)) +
  geom_smooth(se=FALSE, size= 2.5) +
  scale_y_log10()


Compared_Percents <- grid.arrange(CellCounts, Percents, nrow=2)


Barplot_clean <- Cleaned_final

Barplot_clean$Date <- as.character(Barplot_clean$Date)

ggplot(Barplot_clean, aes(x = Date, y = CellCount, fill = Genera)) + 
  geom_bar(position = "stack", stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("") +
  ylab("Cell Count (cells/mL)") +
  ggtitle("Cell Counts by Genera") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  theme(plot.background = element_rect(fill = "#92BDFD", colour = "grey50")) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA))







