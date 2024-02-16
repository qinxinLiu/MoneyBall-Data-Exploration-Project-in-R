library(data.table)
library(tidyr)
library(dplyr)
library(readr)

batting <- read_csv('/Users/mac/Desktop/Capstone Project/Batting.csv')

# Introduction of Features:
# "G"        
# "G_batting" 
# "AB" = At bat        
# "R" = Runs         
# "H" = Hits        
# "2B" = Doubles      
# "3B" = Triples       
# "HR" = Home Runs       
# "RBI" = Runs Batted In      
# "SB" = Stolen Bases      
# "CS" = Caught Stealing       
# "BB" = Bases on Balls (Walks)        
# "SO" = Strikeouts      
# "IBB" = Intentional Baseson Balls(Walks)      
# "HBP" = Hit By Pitch       
# "SH" = Sacrifice Hits (Bunts)     
# "SF" = Sacrifice fly        
# "GIDP" = Ground into Double Plays     
# "G_old" = Metals


# 1. Take a glance to the dataset
head(batting)

str(batting)

head(batting$AB,5)
head(batting[,'2B'])

#2. Feature Engineering

#Firstly, we need to calculate 3 more statistics : 
# (a) Batting Average : The measure of the performance of batter 
# - AVG = the number of hits divided by at bats
# (b) On Base Percentage: The measure of how frequently a batter reach a base 
# - OBP = (H+BB+HBP)/(AB+BB+HBP+SF)
# (c) Slugging Percentage: The measure of batting productivity of a hitter.
# -SLG = (1B + 2*2B + 3*3B + 4*HR)/AB

# (a)
batting$BA <- batting$H / batting$AB
# Alternative Way
mutate(batting, BA = H/AB)

tail(batting$BA, 5)

# (b)
batting$OBP <- (batting$H + batting$BB + batting$HBP) / (batting$AB + batting$BB + batting$HBP + batting$SF)

#(c)
#1B = H-2B-3B-HR
batting$'1B' <- batting$H - batting$'2B' - batting$'3B' - batting$HR
batting$SLG <- (batting$'1B' + 2*batting$'2B' + 3*batting$'3B' + 4*batting$HR)/batting$AB

str(batting)



#3. Merger batting dataframe with salary.csv

#We want to find the most undervalue player, thus, it is worth to look into salary dataset

salary <- read_csv('/Users/mac/Desktop/Capstone Project/Salaries.csv')

head(salary)
arrange(salary, yearID)
head(batting)
arrange(batting, yearID)
# NOTICE: Salaries dataset start at 1985, but batting dataset goes back to 1871
# We need to remove the rows with yearID prior to 1985 from batting

#Method 1
batting <- subset(batting, yearID >= 1985)

#Method 2 :batting <- filter(batting, yearID >= 1985)
arrange(batting, yearID)

#Merge
combo <- merge(batting, salary, by = c('playerID','yearID'))
head(combo)
summary(combo)


#4.As previously mentioned, the Oakland A's lost 3 key players 
# during the off-season. We'll want to get their stats to see what we have to replace
#The players lost were: first baseman 2000 AL MVP Jason Giambi (giambja01) to the
# New York Yankees, outfielder Johnny Damon (damonjo01) to the Boston Red 
#Sox and infielder Rainer Gustavo "Ray" Olmedo ('saenzol01').

lost_players <- subset(combo, playerID %in% c('giambja01','damonjo01', 'saenzol01'))
head(lost_players)

#Since all these players were lost in after 2001 in the offseason,

lost_players <- subset(lost_players,yearID == 2001)
lost_players <- lost_players[,c('playerID','H','2B','3B','HR','OBP','SLG','BA','AB')]
head(lost_players)
summary(lost_players)

#5. Find Replacement Players for the key three players we lost.

#constraints

#(1). The total combined salary of the three players can not exceed 15 million dollars.
#(2). Their combined number of At Bats (AB) needs to be equal to or greater than the lost players.
#(3). Their mean OBP had to equal to or greater than the mean OBP of the lost players

#grab available players after 2001
avail.players <- filter(combo,yearID==2001)

#The mean OBP of lost player is 0.3639
summary(lost_players$OBP)

library(ggplot2)
ggplot(avail.players,aes(x=OBP,y=salary)) + geom_point()
# No one has salary 3 million, thus we could pick any of 3 players

avail.players <- filter(avail.players,salary<8000000,OBP>0.3639)

# The sum of AB of 3 lost players is 644+520+305 = 1469, thus, each of
#the AB of replace player should not less than 500

avail.players <- filter(avail.players,AB >= 500)

possible <- head(arrange(avail.players,desc(OBP)),10)
possible <- possible[,c('playerID','OBP','AB','salary')]
possible


