# Get to proper wd
setwd("~/GoogleDrive/GradSchool/Creative Component")

##### Bring in Points Data & Clean ######

library(RCurl)
mcp_data <- getURL("https://raw.githubusercontent.com/JeffSackmann/tennis_MatchChartingProject/master/charting-m-points.csv")
mcp_points <- read.csv(text = mcp_data)

# Store in case shit goes awry
write.csv(mcp_points, "mcp_points.csv")

# Create columns of player names using 'separate' into first and last names using regular expressions
  # Lookahead, modifiers, patterns, character splits, etc.
library(tidyverse)
mcp_points <- separate(mcp_points,"match_id",c("id","Sex","Tournament","Round","Player1","Player2"), sep = "-")

# Fix names ending in '_' character
mcp_points$Player1 <- sub("_$", "",mcp_points$Player1)
mcp_points$Player2 <- sub("_$", "",mcp_points$Player2)

# Split name into first and last (last piece of name to match with tennis.co.uk)
mcp_points <- separate(mcp_points,"Player1", c("First1","Last1"), sep = "_+(?!.+_.+$)")
mcp_points <- separate(mcp_points,"Player2", c("First2","Last2"), sep = "_+(?!.+_.+$)")


# Keep only first character of first name (to match with tennis.co.uk) and add decimal
mcp_points$First1 <- substr(mcp_points$First1,1,1)
mcp_points$First1 <- paste0(mcp_points$First1, ".")
mcp_points$First2 <- substr(mcp_points$First2,1,1)
mcp_points$First2 <- paste0(mcp_points$First2, ".")

# Get 'Date' Variable and change its name
mcp_points$id <- as.Date(mcp_points$id, format = "%Y%m%d")
mcp_points$id <- format(mcp_points$id,format = "%m/%d/%y")
colnames(mcp_points)[1] <- "Date"
mcp_points$Date <- as.Date(mcp_points$Date, format = "%m/%d/%y")

##########################################
######## Adding New Variables ############
##########################################

###### NOTE: All variables will be measured as DIFFERENCES between players ######

##### Multiple Datasets with different Statistics #####

# Get overview data from matches 
mcp_overview <- getURL("https://raw.githubusercontent.com/JeffSackmann/tennis_MatchChartingProject/master/charting-m-stats-Overview.csv")
mcp_overview <- read.csv(text = mcp_overview)

# Get net points data
mcp_net <- getURL("https://raw.githubusercontent.com/JeffSackmann/tennis_MatchChartingProject/master/charting-m-stats-NetPoints.csv")
# Need to remove weird trailing 3+ commas before can read it in as csv
mcp_net <- gsub(",{3,12}","", mcp_net)
# Now read in as a csv file
mcp_net <- read.csv(text = mcp_net)

# Get 0-3 rally points data
mcp_0t3 <- getURL("https://raw.githubusercontent.com/JeffSackmann/tennis_MatchChartingProject/master/charting-m-stats-SvBreakTotal.csv")
# Need to remove weird trailing 3+ commas before can read it in as csv
mcp_0t3 <- gsub(",{3,12}","", mcp_0t3)
# Now read in as a csv file
mcp_0t3 <- read.csv(text = mcp_0t3)

###### Merge all Statistic Datasets together #####
mcp_stats <- inner_join(mcp_overview, mcp_net, by = "match_id")
mcp_stats <- inner_join(mcp_stats, mcp_0t3, by = "match_id")

# Clean it up - get same matches as in points_lines_final data and remove duplicate columns - CURRENTLY HERE

mcp_stats <- separate(mcp_stats,"match_id",c("id","Sex","Tournament","Round","Player1","Player2"), sep = "-")

# Fix names ending in '_' character
mcp_stats$Player1 <- sub("_$", "",mcp_stats$Player1)
mcp_stats$Player2 <- sub("_$", "",mcp_stats$Player2)

# Split name into first and last (last piece of name to match with tennis.co.uk)
mcp_stats <- separate(mcp_stats,"Player1", c("First1","Last1"), sep = "_+(?!.+_.+$)")
mcp_stats <- separate(mcp_stats,"Player2", c("First2","Last2"), sep = "_+(?!.+_.+$)")


# Keep only first character of first name (to match with tennis.co.uk) and add decimal
mcp_stats$First1 <- substr(mcp_stats$First1,1,1)
mcp_stats$First1 <- paste0(mcp_stats$First1, ".")
mcp_stats$First2 <- substr(mcp_stats$First2,1,1)
mcp_stats$First2 <- paste0(mcp_stats$First2, ".")

# Get 'Date' Variable and change its name
mcp_stats$id <- as.Date(mcp_stats$id, format = "%Y%m%d")
mcp_stats$id <- format(mcp_stats$id,format = "%m/%d/%y")
colnames(mcp_stats)[1] <- "Date"
mcp_stats$Date <- as.Date(mcp_stats$Date, format = "%m/%d/%y")

# Filter to narrow dataset down
library(lubridate)
mcp_stats$Year <- year(mcp_stats$Date)
mcp_stats <- mcp_stats[mcp_stats$Year > 2008,]
# Just 'Total' stats from each match - not by set
mcp_stats <- mcp_stats[mcp_stats$set == "Total", ]
# Remove na's
mcp_stats <- mcp_stats[complete.cases(mcp_stats), ]

##### Aggregate metrics first - first by match ######

# Serving
# 1st Serve %, 1st W%, 2nd W%, Aces/DF Ratio
mcp_stats <- mutate(mcp_stats, Serve1.pct = first_in.x / serve_pts ,
                    Serve1.Wpct = first_won / first_in.x ,
                    Serve2.Wpct = second_won / second_in,
                    Ace.DF.ratio = aces.x / dfs.x )

# Winner/Error and BP Save Rate (if 0 BP allowed - put 100 %)
mcp_stats <- mutate(mcp_stats, Win.Err.Ratio = winners / unforced ,
                    BP.Save.Pct = bp_saved / bk_pts 
)

mcp_stats$BP.Save.Pct <-  ifelse(mcp_stats$BP.Save.Pct == "NaN",1,mcp_stats$BP.Save.Pct)

# Net points
mcp_stats <- mcp_stats[,-c(38:47)]
mcp_stats <- mcp_stats[mcp_stats$row.x == "NetPoints",]
mcp_stats <- mcp_stats[!duplicated(mcp_stats),]

mcp_stats <- mutate(mcp_stats, Net.Wpct = pts_won.x / net_pts)

# Remove duplicated rows 
mcp_stats <- mcp_stats[-seq(2,nrow(mcp_stats),4), ]
mcp_stats <- mcp_stats[-seq(2,nrow(mcp_stats),3), ]

# Create Overall Points won % Stat
mcp_stats <- mutate(mcp_stats, Overall.Wpct = (first_won + second_won + return_pts_won)/(serve_pts + return_pts))
  
# Remove unecessary columns 
mcp_stats <- mcp_stats[ , -c(10:37)]

# Fix INF in Ace.DF.Ratio and Win.Err.Ratio - Give them max (of finite) values
max_Win.Err.Ratio <- max(mcp_stats$Win.Err.Ratio[is.finite(mcp_stats$Win.Err.Ratio)])
max_Ace.Df.Ratio <- max(mcp_stats$Ace.DF.ratio[is.finite(mcp_stats$Ace.DF.ratio)])

mcp_stats$Win.Err.Ratio <-  ifelse(is.infinite(mcp_stats$Win.Err.Ratio),max_Win.Err.Ratio,mcp_stats$Win.Err.Ratio)
mcp_stats$Ace.DF.ratio <-  ifelse(is.infinite(mcp_stats$Ace.DF.ratio),max_Ace.Df.Ratio,mcp_stats$Ace.DF.ratio)

#### Feature Description #####

# 12 'Features' to Start- Need to be aggregated/averaged prior to time of each match - try different ones out
  ##### For Training - do NOT do this #####

#### Overall Points won % - Create from Overview Data ####

#### 1st Serve % - Create from Overview Data ####

#### 1st Serve Win % - Create from Overview Data ####

#### 2nd Serve Win % - Create from Overview Data ####

#### Winner/Error Ratio - Create from Overview Data ####

#### Aces/Double Fault Ratio - Create from Overview Data ####

#### Break Point Save % - Create from Overview Data ####

#### Net Points Win % - Create from Net Data ####

#### ATP Ranking Points - HAVE (betting lines data) ####

#### Surface (Dummy - H/C/G) - HAVE (need to make dummy from betting lines data) ####


#######################################
######### Raw Data Stats ##############  
#######################################  

#### Serve +1 F %  ####

# Create column for 2nd shot of rally (if there is one - to track how many "+1 F"'s)
mcp_points$Server.Rally1st <- substr(mcp_points$rallyNoDirection, start = 2, stop = 2)

# Now make +1 F logical 1/0 count - forehands are topspin, swing volley, and drop shots (on F side)
mcp_points$Serve.Plus1 <- ifelse(grepl("f|j|u",mcp_points$Server.Rally1st),1,0)

#### 0-4 Win %  ####

# Dummy first for 0-4 length rallies - do for each player if they (number listed) WIN the point
mcp_points$Zero.to.Four1 <- ifelse(mcp_points$rallyLen < 5 & mcp_points$PtWinner == 1,1,0)
mcp_points$Zero.to.Four2 <- ifelse(mcp_points$rallyLen < 5 & mcp_points$PtWinner == 2,1,0)

#### Now for each - roll up at player/match level - aggregate and create % of each ####
library(dplyr)
mcp_splus1_wpct <- mcp_points %>%
                      group_by(Date,First1,Last1,First2,Last2, Svr) %>%
                        summarize(Serve.Plus1.Pct = mean(Serve.Plus1))

# 0-4 -> IM HERE
mcp_zt4_wpct <- mcp_points %>%
                  group_by(Date,First1,Last1,First2,Last2) %>%
                    summarize(Zero.to.Four.Wpct1 = mean(Zero.to.Four1),
                              Zero.to.Four.Wpct2 = mean(Zero.to.Four2))

##### Get mcp_stats and mcp_splus1_wpct datasets into one row for each match with both players stats #####

# mcp_stats - gather by all variables of interest (all but 1:9 duplicated), unite by player, then spread by values for columns that were duplicated
 # UNDERSTAND FUNCTION BETTER - how it is working
mcp_stats <- mcp_stats %>% 
  gather(variable, value, -(1:9)) %>%
  unite(temp, player.x, variable) %>%
  spread(temp, value)

# Remove duplicate column and rename year
mcp_stats <- mcp_stats[,-26]
colnames(mcp_stats)[17] <- "Year"

# mcp_splus1_wpct

mcp_splus1_wpct <- mcp_splus1_wpct %>% 
  gather(variable, value, -(1:6)) %>%
  unite(temp, Svr, variable) %>%
  spread(temp, value)

#### Now aggregate all 3 and take differences of player values for variables
  #THEN MERGE WITH LINES --- IM HERE :)

# Filter to narrow dataset like in mcp_stats
mcp_splus1_wpct$Year <- year(mcp_splus1_wpct$Date)
mcp_splus1_wpct <- mcp_splus1_wpct[mcp_splus1_wpct$Year > 2008,]

mcp_zt4_wpct$Year <- year(mcp_zt4_wpct$Date)
mcp_zt4_wpct <- mcp_zt4_wpct[mcp_zt4_wpct$Year > 2008,]

# Combine them all
mcp_stats_all <- left_join(mcp_zt4_wpct,mcp_stats, by = c("Date","First1","Last1","First2","Last2"))
mcp_stats_all <- left_join(mcp_stats_all, mcp_splus1_wpct, by = c("Date","First1","Last1","First2","Last2"))
mcp_stats_all <- mcp_stats_all[complete.cases(mcp_stats_all),] # 1,548 matches!!!

###### Difference Variable for each Stat: Player 1 - Player 2 ######
mcp_stats_final <- transmute(mcp_stats_all, 
                          Last2 = Last2,
                          Year = Year,
                          Ace.DF.ratio = `1_Ace.DF.ratio` - `2_Ace.DF.ratio`,
                          BP.Save.Pct = `1_BP.Save.Pct` - `2_BP.Save.Pct`,
                          Net.Wpct = `1_Net.Wpct` - `2_Net.Wpct`,
                          Overall.Wpct = `1_Overall.Wpct` - `2_Overall.Wpct`,
                          Serve1.pct = `1_Serve1.pct` - `2_Serve1.pct`,
                          Serve1.Wpct = `1_Serve1.Wpct` - `2_Serve1.Wpct`,
                          Serve2.Wpct = `1_Serve2.Wpct` - `2_Serve2.Wpct`,
                          Win.Err.Ratio = `1_Win.Err.Ratio` - `2_Win.Err.Ratio`,
                          Serve.Plus1.Pct = `1_Serve.Plus1.Pct` - `2_Serve.Plus1.Pct`,
                          Zero.to.Four.Wpct = Zero.to.Four.Wpct1 - Zero.to.Four.Wpct2)
                          



#######################################
######## Betting Lines Data ###########
#######################################

#### Load tennis.co.uk Data ####
lines_tennis <- read.csv("tennis_results_lines_09_19.csv")

# Split name into first and last
lines_tennis <- separate(lines_tennis,"Winner", c("Last1","First1"), sep = "\\s{1,2}(?=(\\S{1}[.])+$)")
lines_tennis <- separate(lines_tennis,"Loser", c("Last2","First2"), sep = "\\s{1,2}(?=(\\S{1}[.])+$)")

# Weird cases that arent working
first1_weird <- lines_tennis[is.na(lines_tennis$First1),]
first2_weird <- lines_tennis[is.na(lines_tennis$First2),]
weird_rows <- rbind.data.frame(first1_weird,first2_weird)
weird_rows <- weird_rows[!duplicated(weird_rows),]

# Now get these to have last names correct 
weird_rows <- separate(weird_rows,"Last1", c("Last.1","First.1"), sep = "\\s{1}")
weird_rows <- separate(weird_rows,"Last2", c("Last.2","First.2"), sep = "\\s{1}")

weird_rows$First1 <- ifelse(!is.na(weird_rows$First1), weird_rows$First1, weird_rows$First.1)
weird_rows$First2 <- ifelse(!is.na(weird_rows$First2), weird_rows$First2, weird_rows$First.2)
colnames(weird_rows)[c(10,13)] <- c("Last1", "Last2")
weird_rows <- weird_rows[,-c(11,14)]

# Now remove rows in lines_tennis and replace them with 'weird_rows'
lines_tennis <- lines_tennis[!is.na(lines_tennis$First1),]
lines_tennis <- rbind.data.frame(lines_tennis,weird_rows)

# Fix hyphenated names - split these to get matching last names with mcp_points data
lines_tennis <- separate(lines_tennis,"Last1", c("Last_1","Last_2"),sep = "-+(?!.*-.*$)")
lines_tennis$Last_2 <- ifelse(!is.na(lines_tennis$Last_2), lines_tennis$Last_2, lines_tennis$Last_1)
lines_tennis <- separate(lines_tennis,"Last2", c("Last.1","Last.2"),sep = "-+(?!.*-.*$)")
lines_tennis$Last.2 <- ifelse(!is.na(lines_tennis$Last.2), lines_tennis$Last.2, lines_tennis$Last.1)

# Remove and rename
lines_tennis <- lines_tennis[,-c(10,13)]
colnames(lines_tennis)[10] <- "Last1"
colnames(lines_tennis)[12] <- "Last2"

# For 3 namers, remove first part of last name to match it with mcp_points
lines_tennis <- separate(lines_tennis,"Last1", c("Last_1","Last_2"),sep = "\\s+(?=\\S+.*$)")
lines_tennis$Last_2 <- ifelse(!is.na(lines_tennis$Last_2), lines_tennis$Last_2, lines_tennis$Last_1)
lines_tennis <- separate(lines_tennis,"Last2", c("Last.1","Last.2"),sep = "\\s+(?=\\S+.*$)")
lines_tennis$Last.2 <- ifelse(!is.na(lines_tennis$Last.2), lines_tennis$Last.2, lines_tennis$Last.1)

# Remove and rename
lines_tennis <- lines_tennis[,-c(10,13)]
colnames(lines_tennis)[10] <- "Last1"
colnames(lines_tennis)[12] <- "Last2"

# In first name just keep first letter and '.'
lines_tennis$First1 <- substr(lines_tennis$First1,1,1)
lines_tennis$First1 <- paste0(lines_tennis$First1, ".")
lines_tennis$First2 <- substr(lines_tennis$First2,1,1)
lines_tennis$First2 <- paste0(lines_tennis$First2, ".")

# Make date variable same format as in mcp_points dataset
  # Month
first_9 <- c("1/","2/","3/","4/","5/","6/","7/","8/","9/")
lines_tennis$Date <- as.character(lines_tennis$Date)
lines_tennis$Date <- ifelse(substr(lines_tennis$Date,1,2) %in% first_9,
                          paste0("0", lines_tennis$Date),as.character(lines_tennis$Date))
  # Day
lines_tennis$Date <- ifelse(substr(lines_tennis$Date,4,5) %in% first_9,
                          paste0(substr(lines_tennis$Date,1,3),"0", substring(lines_tennis$Date,4)),
                          as.character(lines_tennis$Date)) 

# Get as a Date object like mcp_points
lines_tennis$Date <- as.Date(lines_tennis$Date, format = "%m/%d/%y")


#### Merge two datasets ####
  # By: Player First Name, Player Last Name and Date (3 values create 'unique' -> primary key) 

# Merge by Date AND (Last1 = Last1 OR Last1 = Last2) AND (First1 = First1 OR First1 = First2) 
# for each 1 and 2 in mcp_stats_final

library(sqldf)
test <- sqldf("SELECT l.*, r.*
             FROM mcp_stats_final AS l
             LEFT JOIN lines_tennis AS r
             ON  l.Date = r.Date AND (l.Last1 = r.Last1 OR l.Last1 = r.Last2) AND 
             (l.First1 = r.First1 OR l.First1 = r.First2) AND (l.Last2 = r.Last1 OR l.Last2 = R.Last2) AND
             (l.First2 = r.First1 OR l.First2 = R.First2)")

# Have some issues: Fix Dates that don't match but are the from the same tennis match
  # Get unique first1, last1, first2, last2 and date combos from test and mcp_points
test_unique <- test[,c(1,5:8)]
test_unique <- distinct(test_unique) #### Number of unique matches n = 1548 ####

# Cases which are not "complete" - dates are not matching?? 
  # Appears many of the dates in mcp_stats_final are 1 day ahead of those lines_tennis

###### Find ATP Matches with Lines that Match ######
test4 <- test[complete.cases(test$B365W),] # 1024 so 524 are off in date
test3 <- test[is.na(test$B365W),]
test4 <- test4[,c(1:16)]
test3 <- test3[,c(1:16)]

# Add one day to these matches in the mcp_stats_final dataset 

# Get duplicated values from both data frames
mcp_stats_final <- rbind.data.frame(mcp_stats_final,test3)
mcp_stats_final_change_date <- mcp_stats_final[duplicated(mcp_stats_final),]
mcp_stats_final_no_change <- distinct(mcp_stats_final)

# Get same date format
mcp_stats_final_no_change$Date <- as.Date(mcp_stats_final_no_change$Date,format = "%m/%d/%y")

mcp_stats_final_change_date$Date <- as.Date(mcp_stats_final_change_date$Date,format = "%m/%d/%y")

# Get no change as its own piece distinct from the ones I need to change
mcp_stats_final_no_change <- test4

# Change date by -1 in one needed
mcp_stats_final_change_date$Date <- mcp_stats_final_change_date$Date - 1

# Re-merge them back together (long and probs not best process - oh well!)
mcp_stats_final <- rbind.data.frame(mcp_stats_final_change_date,mcp_stats_final_no_change)

##### Get lines_tennis dataset as class 'Date' like with mcp_points #####
lines_tennis$Date <- as.Date(lines_tennis$Date, format = "%m/%d/%y")

# Now re-run all process from above and do complete cases to just get ATP matches with odds!
# Merge by Date AND (Last1 = Last1 OR Last1 = Last2) AND (First1 = First1 OR First1 = First2) 
# for each 1 and 2 in mcp_points

library(sqldf)
mcp_stats_lines <- sqldf("SELECT l.*, r.*
             FROM mcp_stats_final AS l
             LEFT JOIN lines_tennis AS r
             ON  l.Date = r.Date AND (l.Last1 = r.Last1 OR l.Last1 = r.Last2) AND 
             (l.First1 = r.First1 OR l.First1 = r.First2) AND (l.Last2 = r.Last1 OR l.Last2 = R.Last2) AND
             (l.First2 = r.First1 OR l.First2 = R.First2)")


###### Get ATP matches only (with betting lines) ######

mcp_stats_lines4 <- mcp_stats_lines[complete.cases(mcp_stats_lines$B365W),] # 1054 so 494 off 
mcp_stats_lines3 <- mcp_stats_lines[is.na(mcp_stats_lines$B365W),]
mcp_stats_lines4 <- mcp_stats_lines4[,c(1:16)]
mcp_stats_lines3 <- mcp_stats_lines3[,c(1:16)]

#### TO FIX - CREATE DATE RANGE OF 10 days around matches #####

# +- 10 from mcp_stats_final date
mcp_stats_final$Date_H <- mcp_stats_final$Date + 10
mcp_stats_final$Date_L <- mcp_stats_final$Date - 10

# Try it again!
library(sqldf)
mcp_stats_lines <- sqldf("SELECT l.*, r.*
             FROM mcp_stats_final AS l
             LEFT JOIN lines_tennis AS r
             ON l.Date >= r.Date_L AND l.Date <= r.Date_H AND
              (l.Last1 = r.Last1 OR l.Last1 = r.Last2) AND 
              (l.First1 = r.First1 OR l.First1 = r.First2) AND 
              (l.Last2 = r.Last1 OR l.Last2 = r.Last2) AND
              (l.First2 = r.First1 OR l.First2 = r.First2)")



mcp_stats_lines5 <- mcp_stats_lines[complete.cases(mcp_stats_lines$B365W),] # Total 1293!! Better- NEARLY ALL THE REST (300) are non-ATP matches :)
mcp_stats_lines6 <- mcp_stats_lines[is.na(mcp_stats_lines$B365W),]
mcp_stats_lines5 <- mcp_stats_lines4[,c(1:16)]
mcp_stats_lines6 <- mcp_stats_lines3[,c(1:16)]

# Remove columns you know you don't need 
mcp_stats_lines_final <- mcp_stats_lines5
mcp_stats_lines_final <- mcp_stats_lines_final[ , -c(19,22,32,33,51:64)]
mcp_stats_lines_final <- mcp_stats_lines_final[ , -c(17,18)]

#### Last two Variables ####

# Create dummy for surface

# install.packages("fastDummies", lib="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library(fastDummies)
# Hard is one I removed for MC avoidance -- when both clay and grass are 0 the "default"
mcp_stats_lines_final <- dummy_cols(mcp_stats_lines_final, select_columns = "Surface", remove_first_dummy = TRUE)

# Difference of Points Variable: Winner - Loser ATP points
mcp_stats_lines_final$WPts <- as.numeric(mcp_stats_lines_final$WPts)
mcp_stats_lines_final$LPts <- as.numeric(mcp_stats_lines_final$LPts)

mcp_stats_lines_final$Rank.Pts.Diff <- mcp_stats_lines_final$WPts - mcp_stats_lines_final$LPts

#### Finally - Create Response Vector Win/Lose (y 1/0) ####
mcp_stats_lines_final$y <- ifelse(mcp_stats_lines_final$Last1 == mcp_stats_lines_final$Last1..28, 1, 0)

# Adjust Rank.Pts.Diff based on who won from below (flip sign if Last1 != Last1..28)
mcp_stats_lines_final$Rank.Pts.Diff <- ifelse(mcp_stats_lines_final$y == 1, mcp_stats_lines_final$Rank.Pts.Diff,
                                             -mcp_stats_lines_final$Rank.Pts.Diff)

######## Grand Hurrah -- Remove unnecessary columns :) #########
mcp_stats_lines_final <- mcp_stats_lines_final[ , -c(6,24:42)]

####### Store updated data  - Lines, Raw Points & Match Stats ########
write.csv(mcp_stats_lines_final, "mcp_stats_lines.csv")

######## DUE TO LIMITED DATA - > Train 7 years, Validate 1, Test on Final '2' ########

