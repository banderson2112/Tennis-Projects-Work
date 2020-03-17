#Set wd
setwd("~/Desktop/SASUniversityEdition/mysasfolders")

#Read in data from url using RCurl
library(RCurl)
matchChartingProject <- read.csv(text=getURL("https://raw.githubusercontent.com/JeffSackmann/tennis_MatchChartingProject/master/charting-m-points.csv"), header = TRUE)

#Export this to use for SAS as well
write.csv(matchChartingProject,"matchChartingProject.csv")

#Create new column of player name-using dpylr and pipes and stringr package
library(stringr)
library(dplyr)

#Just fed
fed_data<-matchChartingProject %>%
  filter(str_detect(match_id, "Federer"))

#Using sqldf and SQL commands, grab GOATS
library(sqldf)
fed_djoker_rafa <- sqldf("SELECT * FROM matchChartingProject WHERE match_id LIKE '%Federer%' OR match_id LIKE '%Djokovic%' OR match_id LIKE '%Nadal%'")

#Get year column
fed_djoker_rafa$year <- with(fed_djoker_rafa, substr(match_id,1,4))

#Make facet plots with ggplot2
library(ggplot2)
library(plotly)

#Lets focus on 2018
goats_serve<-c("RN","RF","ND")
goats_2018 <- subset(fed_djoker_rafa,year=="2018" & Serving %in% goats_serve)
#Create labels as factor of serving variable
goats_2018$Serving <- factor(goats_2018$Serving, labels = c("Djokovic", "Federer", "Nadal"))

######## Gorgeous 2018 Serve Rally Length Plot of GOATS #########
library(scales) #to get to PERCENT!
rally<-ggplot(goats_2018,aes(x=rallyLen))+
  geom_histogram(binwidth=1,aes(y =..density..,fill=Serving))+
  scale_y_continuous(labels=percent, name="Percent")+
  labs(title="Rally Length on Serve 2018")+xlab("Rally Length")
rally+facet_grid(.~Serving)

#Djokovic and Nadal slighlty longer rallies on serve in 2018-no surprise

####### Next Awesome Graphic #######

#Serve plus one strategy
attach(goats_2018)

#Plusoneshot
goats_2018$plusone <- substr(rallyNoDirection,2,2)

goats_2018$plusone_shot <- ifelse(grepl("f|r|u|l|h|j|v|o",plusone),"Forehand",
                      ifelse(grepl("b|s|y|m|i|k|z|p",plusone),"Backhand",NA))

attach(goats_2018)

#See unique values and replace
unique(plusone)
unique(plusone_shot)

#Using dplyr and group_by plus pipes %>%
goats_2018 %>%
  group_by(Serving) %>%
  summarize(n())
#Feds got twice the data as djoker and nadal on serving points 2018--allez

#Now group by and then group by again to check differences in plus one shot
data_plusone<-goats_2018 %>%
  group_by(Serving,plusone_shot) %>%
  summarize(n())

#MMMMMMMM
attach(data_plusone)
data_plusone[2,4]
pct_plusone <- function(data) {
  for (i in 1:3) {
    data$pct_b <- data[i,i+2] / (sum(data[i,i+2],data[i+1,i+2],data[i+2,i+2]))
  }
}

#Lmao one time for loop
data_plusone$pct <- NA
for (i in seq(1,9,2) ) {
data_plusone$pct[1] <- data_plusone[i,i+2] / (sum(data_plusone[i,i+2],data_plusone[i+1,i+2],data_plusone[i+2,i+2]))
}
length(data_plusone$pct)

#Try making a function--didnt work-but DID LEARN how to do various manipulations on for loop and its syntax
rowstart <- c(1,4,7)

pct_group <- function(data,rowstart) {
  for (i in length(data$pct) )
       {
    for (j in rowstart) 
      {
  data$pct[i] <- data[j,3] / (sum(data[j,3],data[j+1,3],data[j+2,3]))
  
    }
  }
}
data_plusone$pct[] <- data_plusone[rowstart,3] / (sum(data_plusone[rowstart,3],data_plusone[rowstart+1,3],data_plusone[rowstart+2,3]))
#ABOVE NO GOOD

#Apply function
rowstart <- c(1,4,7)
data_test <- pct_group(data_plusone, 1)


#Figure out way to subset each type (forehand or backhand) by player and get win
#%s for each
library(sqldf)
sqldf("select * from goats_2018 where plusone_shot like 'Forehand'")

#Filter out unreturned-dplyr
no_unret <- data_plusone %>% 
  group_by(Serving) %>%
  filter(!is.na(plusone_shot)) 

#Rearrange and name
no_unret$count <- no_unret$`n()`
no_unret<-no_unret[-3]
no_unret

#Get pct for each subgroup for each player
attach(no_unret)
no_unret_final <- 
  group_by(no_unret, Serving) %>% mutate(pct = count/sum(count))

#So we see that Nadal and Federer really get that plus one on the forehand wing!!!

#Lets make a nice plot of this
library(scales)
library(ggplot2)
#FIREEE
plusone_plot <-ggplot(no_unret_final,aes(x=Serving,y=pct,fill=plusone_shot)) +
  geom_col() +
  scale_y_continuous(labels=percent, name="Percent")+
  scale_fill_manual(name="Shot",values = c("red", "darkgreen")) +
  labs(title="Serve 'Plus One'",x="Player") 
plusone_plot

#Center plot ggplot 
theme_update(plot.title = element_text(hjust = 0.5))

#Finally, get relative win % in serve plus one tactic for each player

#OLD STUFF FOR REFERENCE
goats_serve<-c("RN","RF","ND")
goats_2018 <- subset(fed_djoker_rafa,year=="2018" & Serving %in% goats_serve)
#Create labels as factor of serving variable
goats_2018$Serving <- factor(goats_2018$Serving, labels = c("Djokovic", "Federer", "Nadal"))

fed_2018 <- subset(goats_2018, Serving == "Federer" )
fed_2018_forehand <- subset(fed_2018, plusone_shot == "Forehand")
with(fed_2018_forehand, sum(isSvrWinner))/nrow(fed_2018_forehand) #0.635

rafa_2018 <- subset(goats_2018, Serving == "Nadal" )
rafa_2018_forehand <- subset(rafa_2018, plusone_shot == "Forehand")
with(rafa_2018_forehand, sum(isSvrWinner))/nrow(rafa_2018_forehand) #0.592

djoker_2018 <- subset(goats_2018, Serving == "Djokovic" )
djoker_2018_forehand <- subset(djoker_2018, plusone_shot == "Forehand")
with(djoker_2018_forehand, sum(isSvrWinner))/nrow(djoker_2018_forehand) #0.617

#FED KING

