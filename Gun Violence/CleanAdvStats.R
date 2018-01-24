library(plm)
library(xtable)
library(car)
library(MASS)

mydata <- read.csv("ADVStatPaneledw2015Party.csv")

mydata$TotalPro <- mydata$PGP2013 + mydata$AAR2013 + mydata$PPC2013 + mydata$PS2013 + mydata$AM2013 + mydata$ABC2013 + mydata$NFL2013 + mydata$LGA2013 
mydata$TotalAnti <- mydata$AGP2013 + mydata$PAR2013 + mydata$PM2013 + mydata$PBC2013 + mydata$TGA2013 + mydata$LSG2013
mydata$TotalLaws <- mydata$TotalPro + mydata$TotalAnti

mydata$Pro <- ifelse(mydata$TotalPro == 0, 0, 1)
mydata$Anti <- ifelse(mydata$TotalAnti == 0, 0, 1)
mydata$Law <- ifelse(mydata$TotalLaws == 0, 0, 1)

panel <- mydata

panel$LegD <- ifelse(panel$LegR == -1, 1, 0)
panel$GovD <- ifelse(panel$GovR == -1, 1, 0)
panel$StateD <- ifelse(panel$StateR == -1, 1, 0)
panel$LegR <- ifelse(panel$LegR == -1, 0, panel$LegR)
panel$GovR <- ifelse(panel$GovR == -1, 0, panel$GovR)
panel$StateR <- ifelse(panel$StateR == -1, 0, panel$StateR)
panel$yd <- ifelse(panel$d2012 == 1, 0, 1)
panel$yd <- ifelse(panel$d2014 == 1, 2, panel$yd)
panel$yd <- ifelse(panel$d2015 == 1, 3, panel$yd)

panel<- pdata.frame(panel, index = c("State", "Year"))

dr_delta <- c()
unem_delta <- c()
for(i in 0:49){
  state_i <- i*4+1
  delta1213 <- panel[state_i + 1,]$DR - panel[state_i, ]$DR
  delta1314 <- panel[state_i + 2,]$DR - panel[state_i + 1, ]$DR
  delta1415 <- panel[state_i + 3,]$DR - panel[state_i + 2, ]$DR
  
  delta1213U <- panel[state_i + 1,]$Unemployment - panel[state_i, ]$Unemployment
  delta1314U <- panel[state_i + 2,]$Unemployment - panel[state_i + 1, ]$Unemployment
  delta1415U <- panel[state_i + 3,]$Unemployment - panel[state_i + 2, ]$Unemployment
  
  dr_delta <- c(dr_delta, 0, delta1213, delta1314, delta1415)
  unem_delta <- c(unem_delta, 0, delta1213U, delta1314U, delta1415U)
  
}
print(dr_delta)
ponel <- panel
ponel$dr_delta <- dr_delta
ponel$unem_delta <- unem_delta

ponel$PGP2013 <- ifelse(ponel$d2015 == 1, 0, ponel$PGP2013)
ponel$PPC2013 <- ifelse(ponel$d2015 == 1, 0, ponel$PPC2013)
ponel$AGP2013 <- ifelse(ponel$d2015 == 1, 0, ponel$AGP2013)
ponel$PS2013 <- ifelse(ponel$d2015 == 1, 0, ponel$PS2013)
ponel$PM2013 <- ifelse(ponel$d2015 == 1, 0, ponel$PM2013)
ponel$AM2013 <- ifelse(ponel$d2015 == 1, 0, ponel$AM2013)
ponel$PBC2013 <- ifelse(ponel$d2015 == 1, 0, ponel$PBC2013)
ponel$ABC2013 <- ifelse(ponel$d2015 == 1, 0, ponel$ABC2013)
ponel$PAR2013 <- ifelse(ponel$d2015 == 1, 0, ponel$PAR2013)
ponel$AAR2013 <- ifelse(ponel$d2015 == 1, 0, ponel$AAR2013)
ponel$NFL2013 <- ifelse(ponel$d2015 == 1, 0, ponel$NFL2013)
ponel$TGA2013 <- ifelse(ponel$d2015 == 1, 0, ponel$TGA2013)
ponel$LGA2013 <- ifelse(ponel$d2015 == 1, 0, ponel$LGA2013)
ponel$LSG2013 <- ifelse(ponel$d2015 == 1, 0, ponel$LSG2013)

#Full Models

text <- DR ~ PGP2013 + AGP2013 + PPC2013 + PS2013 + PM2013 + AM2013 + PBC2013 + 
  ABC2013 + PAR2013 + AAR2013 + NFL2013 + TGA2013 + LGA2013 + LSG2013 + LegR + LegD + GovR + StateR + StateD + Year
#Full Kitchen Sink
full_ks <- plm(formula = DR ~ PGP2013 + AGP2013 + PPC2013 + PS2013 + PM2013 + AM2013 + PBC2013 + 
            ABC2013 + PAR2013 + AAR2013 + NFL2013 + TGA2013 + LGA2013 + LSG2013 + LegD +  Year, index = c('State', 'Year'),
          data = panel, model = 'within')
summary(full_ks)

quick <- plm(formula = DR ~ LegD + LegR + GovR + StateR + StateD, index = c('State', 'Year'), data = panel, model = 'within')
summary(quick)

random1 <- plm(formula = DR ~ PGP2013 + AGP2013 + PPC2013 + PS2013 + PM2013 + AM2013 + PBC2013 + 
                 ABC2013 + PAR2013 + AAR2013 + NFL2013 + TGA2013 + LGA2013 + Year,
               data = panel, model = 'random')
summary(random1)

full_ks_party <- plm(formula = DR ~ GovR + LegR + LegD + StateD + StateR+ d2013 + d2014 + d2015, data = panel, effects = 'twoways', model = 'within')
summary(full_ks_party)

#First Differences

fd <- lm(dr_delta ~ PGP2013 + AGP2013 + PPC2013 + PS2013 + PM2013 + AM2013 + PBC2013 + 
           ABC2013 + PAR2013 + AAR2013 + NFL2013 + TGA2013 + LGA2013 + LSG2013 + d2014 + d2015 + unem_delta, data = ponel)

summary(fd)

fd2 <- lm(dr_delta ~ PPC2013 +  PM2013 + d2014 + d2015, data = ponel)
summary(fd2)



#Full First
full_1 <- plm(formula = DR ~   PPC2013 + PM2013 
             + LegR + LegD + GovR +StateR + StateD + Year,
             data = panel,  model = 'within')

summary(full_1, robust = TRUE)

#Full Problem 
full_3 <- plm(formula = DR ~  PM2013 
              + LegR + LegD + GovR + StateR + StateD + Year,
              data = panel,  model = 'within')

summary(full_3, robust = TRUE)

#Full Paired Down
full_2 <- plm(formula = DR ~   PPC2013 + PM2013 + PS2013 
              + LegR + LegD + GovR + StateR + StateD + Year,
              data = panel, model = 'within')

summary(full_2, robust = TRUE)

#Only Anti
full_anti <- plm(formula = DR ~ PM2013 + PBC2013 + 
               LegR + LegD + GovR + StateR + StateD,
               data = panel, effect = 'twoways', model = 'within')

summary(full_anti)

#Binary Kitchen Sink
b_ks <- plm(formula = DR ~  Pro + Anti + LegR + LegD + GovR + StateR + StateD + Year,
            data = panel, model = 'within')
summary(b_ks)

#Cummulative Kitchen Sink
c_ks <- plm(formula = DR ~  TotalPro + TotalAnti + LegR + LegD + GovR + StateR + StateD + Year,
            data = panel, model = 'within')
summary(c_ks)

panel$State[which(panel$Pro ==1 & panel$Anti == 1)]

phtest(text, data = panel)

#Brady Fixed Effects
Brady_ks <- plm(formula = DR ~  Brady + LegR + LegD + GovR + StateD + StateR + Year,
            data = panel, index = c('State', 'Year'))
summary(Brady_ks)

nrow(panel[panel$LegR == 1 & panel$GovR == 1,])
