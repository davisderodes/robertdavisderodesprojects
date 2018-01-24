#These lines are importing all of the Player Basketball Statistics 
#PlayerStats 2005 for example is the performance (ie: Age, Minutes Played, All Star, Etc.) for the player 
#during the 2004-2005 Season

playerstats2005 <- read.csv("Advanced_Stats_2005season_cleaned.csv")
playerstats2006 <- read.csv("Advanced_Stats_2006season_cleaned.csv")
playerstats2007 <- read.csv("Advanced_Stats_2007season_cleaned.csv")
playerstats2008 <- read.csv("Advanced_Stats_2008season_cleaned.csv")
playerstats2009 <- read.csv("Advanced_Stats_2009season_cleaned.csv")
playerstats2010 <- read.csv("Advanced_Stats_2010season_cleaned.csv")
playerstats2011 <- read.csv("Advanced_Stats_2011season_cleaned.csv")
playerstats2012 <- read.csv("Advanced_Stats_2012season_cleaned.csv")
playerstats2013 <- read.csv("Advanced_Stats_2013season_cleaned.csv")
playerstats2014 <- read.csv("Advanced_Stats_2014season_cleaned.csv")
playerstats2015 <- read.csv("Advanced_Stats_2015season_cleaned.csv")
playerstats2016 <- read.csv("Advanced_Stats_2016season_cleaned.csv")

#These lines are importing all of the Player Contract Data
#contracts2007 for example is the contract infromation (ie: Player, Amount, Duration, Etc.) for each contract
#during the summer of 2007 prior to the 2007-2008 season. 

#The data for player performance during contract year for contracts2007 would therefore be playerstats2007

contracts2007 <- read.csv("Free Agent Contract Data 2007.csv")
contracts2008 <- read.csv("Free Agent Contract Data 2008.csv")
#For some reason our data adds many blank observations for some of our csv's, so we remove them from the dataset
contracts2008 <- contracts2008[1:41,] 
contracts2009 <- read.csv("Free Agent Contract Data 2009.csv")
contracts2010 <- read.csv("Free Agent Contract Data 2010.csv")
contracts2011 <- read.csv("Free Agent Contract Data 2011.csv")
contracts2012 <- read.csv("Free Agent Contract Data 2012.csv")
#For some reason our data adds many blank observations for some of our csv's, so we remove them from the dataset
contracts2012 <- contracts2012[1:266,]
contracts2013 <- read.csv("Free Agent Contract Data 2013.csv")
contracts2014 <- read.csv("Free Agent Contract Data 2014.csv")
#For some reason our data adds many blank observations for some of our csv's, so we remove them from the dataset
contracts2014 <- contracts2014[1:136,]

#These are all of the Player Statistics Data Sets and Player Contract Data Sets needed to conduct the replication
#The Replication looks at contracts between the Summer of 2007 to the Summer of 2011
replicate_analysis_pstats <- list(playerstats2005, playerstats2006, playerstats2007, playerstats2008, playerstats2009, playerstats2010, playerstats2011, playerstats2012, playerstats2013)
replicate_analysis_contracts <- list(contracts2007, contracts2008, contracts2009, contracts2010, contracts2011)

#We will be creating a large data frame merging contract data with a subset of the player data from
#2 years prior to the contract year, 1 year prior the contract year, the contract year, 1 year after the contract year
#and 2 years after the contract year

CBA2005 <- data.frame()

for (i in 3:7){
  
  #Current Contract is one year's worth of contracts (ie: contracts 2007)
  current_contract <- data.frame(replicate_analysis_contracts[i-2])
  
  #CY_B2 represents the subsetted player data from 2 years back or prior to the Contract Year
  CY_B2 <- data.frame(replicate_analysis_pstats[i-2])
  CY_B2 <- subset(CY_B2, select = c(Player, MP, WS.48, AllStar, AllNBA, AllDef, Center))
  colnames(CY_B2) <- paste("B2", colnames(CY_B2), sep = "_")
  CY_B2$Player <- CY_B2$B2_Player
  CY_B2 <- subset(CY_B2, select = c(Player, B2_MP, B2_WS.48, B2_AllStar, B2_AllNBA, B2_AllDef, B2_Center))
  
  #CY_B1 represents the subsetted player data from 1 year back or prior to the Contract Year
  CY_B1 <- data.frame(replicate_analysis_pstats[i-1])
  CY_B1 <- subset(CY_B1, select = c(Player, MP, WS.48, AllStar, AllNBA, AllDef, Center))
  colnames(CY_B1) <- paste("B1", colnames(CY_B1), sep = "_")
  CY_B1$Player <- CY_B1$B1_Player
  CY_B1 <- subset(CY_B1, select = c(Player, B1_MP, B1_WS.48, B1_AllStar, B1_AllNBA, B1_AllDef, B1_Center))
  
  #CY represents the subsetted player data for the Contract Year
  CY <- data.frame(replicate_analysis_pstats[i])
  CY <- subset(CY, select = c(Player, MP, Age, WS.48, AllStar, AllNBA, AllDef, Center))
  colnames(CY) <- paste("CY0", colnames(CY), sep = "_")
  CY$Player <- CY$CY0_Player
  CY <- subset(CY, select = c(Player, CY0_MP, CY0_Age, CY0_WS.48, CY0_AllStar, CY0_AllNBA, CY0_AllDef, CY0_Center))
  
  #CY_F1 represents the subsetted player data from 1 year forward or after the Contract Year
  CY_F1 <- data.frame(replicate_analysis_pstats[i+1])
  CY_F1 <- subset(CY_F1, select = c(Player, MP, WS.48, AllStar, AllNBA, AllDef, Center))
  colnames(CY_F1) <- paste("F1", colnames(CY_F1), sep = "_")
  CY_F1$Player <- CY_F1$F1_Player
  CY_F1 <- subset(CY_F1, select = c(Player, F1_MP, F1_WS.48, F1_AllStar, F1_AllNBA, F1_AllDef, F1_Center))
  
  #CY_F2 represents the subsetted player data from 2 years forward or after the Contract Year
  CY_F2 <- data.frame(replicate_analysis_pstats[i+2])
  CY_F2 <- subset(CY_F2, select = c(Player, MP, WS.48, AllStar, AllNBA, AllDef, Center))
  colnames(CY_F2) <- paste("F2", colnames(CY_F2), sep = "_")
  CY_F2$Player <- CY_F2$F2_Player
  CY_F2 <- subset(CY_F2, select = c(Player, F2_MP, F2_WS.48, F2_AllStar, F2_AllNBA, F2_AllDef, F2_Center))
  
  #We merge all of the contract data with all of the player statistics 
  updated_current_contract <- merge(current_contract, CY_B2, by = "Player", all.x = TRUE)
  updated_current_contract <- merge(updated_current_contract, CY_B1, by = "Player", all.x = TRUE)
  updated_current_contract <- merge(updated_current_contract, CY, by = "Player", all.x = TRUE)
  updated_current_contract <- merge(updated_current_contract, CY_F1, by = "Player", all.x = TRUE)
  updated_current_contract <- merge(updated_current_contract, CY_F2, by = "Player", all.x = TRUE)
  
  #A Master Data Frame with all of the contracts in the replication 
  CBA2005 <- rbind(CBA2005, updated_current_contract)
}

#Future Win Share is an addition of the win share of two years after the contract year
#It is an indicator of future performance 
CBA2005$FutureWS <- CBA2005$F2_WS.48 + CBA2005$F1_WS.48

#True Center is an indicator if a player was listed as a Center in his contract year or 2 years before
CBA2005$TrueCenter <- ifelse(CBA2005$B2_Center == 1, 1, 0)
CBA2005$TrueCenter <- ifelse(CBA2005$B1_Center == 1, 1, CBA2005$TrueCenter)
CBA2005$TrueCenter <- ifelse(CBA2005$CY0_Center == 1, 1, CBA2005$TrueCenter)

#Age Squared is intuitively Age*Age
CBA2005$AgeSquared <- CBA2005$CY0_Age^2

#Replication Regression for Future Win Share
Replication_FutureWS_Regression <- lm(CBA2005$FutureWS ~ CBA2005$B2_WS.48 + CBA2005$B1_WS.48 + CBA2005$CY0_WS.48 + CBA2005$CY0_Age + CBA2005$AgeSquared + CBA2005$TrueCenter + CBA2005$B2_MP + CBA2005$B1_MP + CBA2005$CY0_MP)
summary(Replication_FutureWS_Regression)

#Replication Regression for Natural Log of Total Contract Value
CBA2005$lnDollars <- log(CBA2005$DOLLARS)
Replication_Dollars_Regression <- lm(CBA2005$lnDollars ~ CBA2005$B2_WS.48 + CBA2005$B1_WS.48 + CBA2005$CY0_WS.48 + CBA2005$CY0_Age + CBA2005$AgeSquared + CBA2005$TrueCenter + CBA2005$B2_MP + CBA2005$B1_MP + CBA2005$CY0_MP)
summary(Replication_Dollars_Regression)

#Replication Regression for Natural Log of Average Salary 
CBA2005$lnAvg <- log(CBA2005$AVG..SALARY)
Replication_Average_Contract_Regression <- lm(CBA2005$lnAvg ~ CBA2005$B2_WS.48 + CBA2005$B1_WS.48 + CBA2005$CY0_WS.48 + CBA2005$CY0_Age + CBA2005$AgeSquared + CBA2005$TrueCenter + CBA2005$B2_MP + CBA2005$B1_MP + CBA2005$CY0_MP)
summary(Replication_Average_Contract_Regression)

#Replication Regression for Natural Log of Contract Duration
CBA2005$lnDuration <- log(CBA2005$YRS)
Replication_Duration_Regression <- lm(CBA2005$YRS ~ CBA2005$B2_WS.48 + CBA2005$B1_WS.48 + CBA2005$CY0_WS.48 + CBA2005$CY0_Age + CBA2005$AgeSquared + CBA2005$TrueCenter + CBA2005$B2_MP + CBA2005$B1_MP + CBA2005$CY0_MP)
summary(Replication_Duration_Regression)

#___________________________________________

#UPDATED ANALYSIS 

#These are all of the Player Statistics Data Sets and Player Contract Data Sets needed to conduct the update
#Update looks at Contracts from the Summer of 2012 to the Summer of 2014
update_analysis_pstats <- list(playerstats2010, playerstats2011, playerstats2012, playerstats2013, playerstats2014, playerstats2015, playerstats2016)
update_analysis_contracts <- list(contracts2012, contracts2013, contracts2014)

#We will be creating a large data frame merging contract data with a subset of the player data from
#2 years prior to the contract year, 1 year prior the contract year, the contract year, 1 year after the contract year
#and 2 years after the contract year

CBA2012 <- data.frame()

for (i in 3:5){
  
  #Current Contract is one year's worth of contracts (ie: contracts 2012)
  current_contract <- data.frame(update_analysis_contracts[i-2])
  
  #CY_B2 represents the subsetted player data from 2 years back or prior to the Contract Year
  CY_B2 <- data.frame(update_analysis_pstats[i-2])
  CY_B2 <- subset(CY_B2, select = c(Player, MP, WS.48, AllStar, AllNBA, AllDef, Center))
  colnames(CY_B2) <- paste("B2", colnames(CY_B2), sep = "_")
  CY_B2$Player <- CY_B2$B2_Player
  CY_B2 <- subset(CY_B2, select = c(Player, B2_MP, B2_WS.48, B2_AllStar, B2_AllNBA, B2_AllDef, B2_Center))
  
  #CY_B1 represents the subsetted player data from 1 year back or prior to the Contract Year
  CY_B1 <- data.frame(update_analysis_pstats[i-1])
  CY_B1 <- subset(CY_B1, select = c(Player, MP, WS.48, AllStar, AllNBA, AllDef, Center))
  colnames(CY_B1) <- paste("B1", colnames(CY_B1), sep = "_")
  CY_B1$Player <- CY_B1$B1_Player
  CY_B1 <- subset(CY_B1, select = c(Player, B1_MP, B1_WS.48, B1_AllStar, B1_AllNBA, B1_AllDef, B1_Center))
  
  #CY represents the subsetted player data for the Contract Year
  CY <- data.frame(update_analysis_pstats[i])
  CY <- subset(CY, select = c(Player, MP, Age, WS.48, AllStar, AllNBA, AllDef, Center))
  colnames(CY) <- paste("CY0", colnames(CY), sep = "_")
  CY$Player <- CY$CY0_Player
  CY <- subset(CY, select = c(Player, CY0_MP, CY0_Age, CY0_WS.48, CY0_AllStar, CY0_AllNBA, CY0_AllDef, CY0_Center))
  
  #CY_F1 represents the subsetted player data from 1 year forward or after the Contract Year
  CY_F1 <- data.frame(update_analysis_pstats[i+1])
  CY_F1 <- subset(CY_F1, select = c(Player, MP, WS.48, AllStar, AllNBA, AllDef, Center))
  colnames(CY_F1) <- paste("F1", colnames(CY_F1), sep = "_")
  CY_F1$Player <- CY_F1$F1_Player
  CY_F1 <- subset(CY_F1, select = c(Player, F1_MP, F1_WS.48, F1_AllStar, F1_AllNBA, F1_AllDef, F1_Center))
  
  #CY_F2 represents the subsetted player data from 2 years forward or after the Contract Year
  CY_F2 <- data.frame(update_analysis_pstats[i+2])
  CY_F2 <- subset(CY_F2, select = c(Player, MP, WS.48, AllStar, AllNBA, AllDef, Center))
  colnames(CY_F2) <- paste("F2", colnames(CY_F2), sep = "_")
  CY_F2$Player <- CY_F2$F2_Player
  CY_F2 <- subset(CY_F2, select = c(Player, F2_MP, F2_WS.48, F2_AllStar, F2_AllNBA, F2_AllDef, F2_Center))
  
  #We merge all of the contract data with all of the player statistics
  updated_current_contract <- merge(current_contract, CY_B2, by = "Player", all.x = TRUE)
  updated_current_contract <- merge(updated_current_contract, CY_B1, by = "Player", all.x = TRUE)
  updated_current_contract <- merge(updated_current_contract, CY, by = "Player", all.x = TRUE)
  updated_current_contract <- merge(updated_current_contract, CY_F1, by = "Player", all.x = TRUE)
  updated_current_contract <- merge(updated_current_contract, CY_F2, by = "Player", all.x = TRUE)
  
  #A Master Data Frame with all of the contracts in the replication 
  CBA2012 <- rbind(CBA2012, updated_current_contract)
}

#Future Win Share is an addition of the win share of two years after the contract year
#It is an indicator of future performance 
CBA2012$FutureWS <- CBA2012$F2_WS.48 + CBA2012$F1_WS.48

#True Center is an indicator if a player was listed as a Center in his contract year or 2 years before
CBA2012$TrueCenter <- ifelse(CBA2012$B2_Center == 1, 1, 0)
CBA2012$TrueCenter <- ifelse(CBA2012$B1_Center == 1, 1, CBA2012$TrueCenter)
CBA2012$TrueCenter <- ifelse(CBA2012$CY0_Center == 1, 1, CBA2012$TrueCenter)

#Age Squared is intuitively Age*Age
CBA2012$AgeSquared <- CBA2012$CY0_Age^2

#Updated Regression for Future Win Share
Update_FutureWS_Regression <- lm(CBA2012$FutureWS ~ CBA2012$B2_WS.48 + CBA2012$B1_WS.48 + CBA2012$CY0_WS.48 + CBA2012$CY0_Age + CBA2012$AgeSquared + CBA2012$TrueCenter + CBA2012$B2_MP + CBA2012$B1_MP + CBA2012$CY0_MP)
summary(Update_FutureWS_Regression)

#Updated Regression for Natural Log of total contract value
CBA2012$lnDollars <- log(CBA2012$DOLLARS)
Update_Dollars_Regression <- lm(CBA2012$lnDollars ~ CBA2012$B2_WS.48 + CBA2012$B1_WS.48 + CBA2012$CY0_WS.48 + CBA2012$CY0_Age + CBA2012$AgeSquared + CBA2012$TrueCenter + CBA2012$B2_MP + CBA2012$B1_MP + CBA2012$CY0_MP)
summary(Update_Dollars_Regression)

#Updated Regression for Natural Log of annual contract value
CBA2012$lnAvg<- log(CBA2012$AVG..SALARY)
Update_Average_Contract_Regression <- lm(CBA2012$lnAvg ~ CBA2012$B2_WS.48 + CBA2012$B1_WS.48 + CBA2012$CY0_WS.48 + CBA2012$CY0_Age + CBA2012$AgeSquared + CBA2012$TrueCenter + CBA2012$B2_MP + CBA2012$B1_MP + CBA2012$CY0_MP)
summary(Update_Average_Contract_Regression)

#Updated Regression for Natural Log of contract duration
CBA2012$lnDuration <- log(CBA2012$YRS)
Update_Duration_Regression <- lm(CBA2012$lnDuration ~ CBA2012$B2_WS.48 + CBA2012$B1_WS.48 + CBA2012$CY0_WS.48 + CBA2012$CY0_Age + CBA2012$AgeSquared + CBA2012$TrueCenter + CBA2012$B2_MP + CBA2012$B1_MP + CBA2012$CY0_MP)
summary(Update_Duration_Regression)