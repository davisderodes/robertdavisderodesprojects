library(tidyverse)
library(plm)
library(TSA)
library(strucchange)
library(reshape2)
library(vars)

data("Canada")

Team <- read.csv("Teams.csv")
NBA <- read.csv("TrueLeague.csv")

Teams <- select(Team, c("Season", "Tm", "W", "Age", "FGA", "X3P", "X3PA", "X3P.","TRB", "TOV", "Pace", "DRtg","ORB", "Wlag", "STL","BLK", "DRB", "PF", "X2P."))
Teams$"X3PAR" = Teams$X3PA/Teams$FGA


Houston <- filter(Teams, Tm == "HOU")
H_Model = X3PAR ~ Season

quick = lm(Pace ~Age + TRB + STL + BLK + TOV + PF + X2P. + DRtg, data = Teams)
summary(quick)

?plm
model = plm(formula = W ~ Age + X3P. + Pace + X3PAR + TRB + STL + BLK + TOV + PF + X2P., data = Teams, effect = "twoways", model = "within", index = c("Tm", "Season"))


summary(model)


Teams_Var <- select(Teams,  c("Tm", "Season", "X3PAR"))
final <- dcast(Teams_Var, Season ~ Tm, value.var = "X3PAR")
Timmy <- dcast(Teams_Var, Tm~Season, value.var = "X3PAR")
final <- final[8:13,]

for(j in 2:31){
  vec <- c()
  total = 0
  for (i in 2:31){
    
    if(i != j){
      a <- grangertest(final[,j], final[,i], order = 1)
      if(a$`Pr(>F)`[2] <= .05){
        total = total + 1
        if(j == 26){print(Timmy[i-1,][1])}
      }
      
    }
  }
  print(Timmy[j-1,][1])
  print(total)
} 

  


