library(TSA)
library(reshape2)
library(plyr)
library(strucchange)
library(vars)
library(lmtest)
library(tseries)
library(tsDyn)

CPI <- read.csv("cpi.csv")
CPI_melt <- melt(CPI, id = c("Year"))
CPI_melt <- arrange(CPI_melt, Year)
CPI_melt <- CPI_melt[499:1249, ]
Federal_Funds_rate <- read.csv("FederalFundsRate.csv")
Unemployment <- read.csv("Unemployment.csv")
Unemployment <- Unemployment[79:829,]

data <- CPI_melt
data <- subset(CPI_melt, select = c(value))
data <- rename(data, c("value" = "CPI"))
data$FFR <- Federal_Funds_rate$FEDFUNDS
data$UNEM <- Unemployment$UNRATE
data$Date <- Unemployment$observation_date
rownames(data) <- 1:nrow(data)
data <- data[1:738,]


#Here's the beginning of the code to get delta cpi
deltacpi <- c(0)

for(i in 2:738){
  
  deltacpi <- c(deltacpi, 1200*log(data$CPI[i]/data$CPI[i-1]))
  
}
data$deltacpi <- deltacpi

plot(data$deltacpi, type = 'l')

#Here's the smoothing code
deltacpiaveragefive <- c()
for (i in 3:736) {
  deltacpiaveragefive <- c(deltacpiaveragefive, mean(data$deltacpi[(i-2):(i+2)]))
  
}

#Essentially set the first two observations and the last two observations to NA because they don't have two observations
#before or after
deltacpiaveragefive <- c(NA,NA,deltacpiaveragefive,NA,NA)
data$deltacpiaveragefive <- deltacpiaveragefive

#Remove the observations with the NA's
data <- data[3:(nrow(data)-2),]
rownames(data) <- 1:nrow(data)

#This generates the models for the breakpoints
data$FFR_lagged <- c(NA, data$FFR[-nrow(data)])
data$UNEM_lagged <- c(NA, data$UNEM[-nrow(data)])
data$deltacpiaveragefive_lagged <- c(NA, data$deltacpiaveragefive[-nrow(data)])
data$deltacpi_lagged <- c(NA, data$deltacpi[-nrow(data)])
data$FFR_lagged2 <- c(NA, data$FFR_lagged[-nrow(data)])
data$UNEM_lagged2 <- c(NA, data$UNEM_lagged[-nrow(data)])
data$deltacpiaveragefive_lagged2 <- c(NA, data$deltacpiaveragefive_lagged[-nrow(data)])
data$deltacpi_lagged2 <- c(NA, data$deltacpi_lagged[-nrow(data)])

#Here's the VARModel Stuff
mydata <- data[353:nrow(data),]
mydata <- subset(mydata, select = c(FFR, UNEM, deltacpiaveragefive, Date))
rownames(mydata) <- 1:nrow(mydata)

?ar
model <- ar(mydata[,1:3])
model
model$aic

VAR(mydata[,1:3], p = 2)

?grangertest

grangertest(mydata[,1], mydata[,2], order = 2) #FFR -> IR
grangertest(mydata[,1],mydata[,3], order = 2) #FFR -> UNEM
grangertest(mydata[,2], mydata[,1], order = 2) #IR -> FFR
grangertest(mydata[,2], mydata[,3], order = 2) #IR -> UNEM
grangertest(mydata[,3], mydata[,1], order = 2) #UNEM -> FFR
grangertest(mydata[,3], mydata[,2], order = 2) #UNEM -> IR

var1 <- VAR(mydata, p=6)
BIC(var1)

#Breakpoints stuff
lagFFR_model <- FFR ~ UNEM_lagged + deltacpiaveragefive_lagged + FFR_lagged
lagdeltaCPI_model <- deltacpiaveragefive ~ UNEM_lagged + deltacpiaveragefive_lagged + FFR_lagged
lagUNEM_model <- UNEM ~ UNEM_lagged + deltacpiaveragefive_lagged + FFR_lagged

lagFFR_model2 <- FFR ~ UNEM_lagged + deltacpiaveragefive_lagged + FFR_lagged + UNEM_lagged2 + deltacpiaveragefive_lagged2 + FFR_lagged2
lagdeltaCPI_model2 <- deltacpiaveragefive ~ UNEM_lagged + deltacpiaveragefive_lagged + FFR_lagged + UNEM_lagged2 + deltacpiaveragefive_lagged2 + FFR_lagged2
lagUNEM_model2 <- UNEM ~ UNEM_lagged + deltacpiaveragefive_lagged + FFR_lagged+ UNEM_lagged2 + deltacpiaveragefive_lagged2 + FFR_lagged2

?breakpoints
bp1 <- breakpoints(lagFFR_model2, data = data)
?breakpoints
bp1
summary(bp1)
plot(bp1, main = "Breakpoints for FFR")

bp2 <- breakpoints(lagdeltaCPI_model2, data = data)
bp2
summary(bp2)
plot(bp2, main = "Breakpoints for CPI")

bp3 <- breakpoints(lagUNEM_model2, data = data)
bp3
plot(bp3, main = "Breakpoints for UNEM")
summary(bp3)



z <- nrow(data)
p_ffr <- c()
p_cpi <- c()
p_unem <- c()
p_ffr2 <- c()
p_cpi2 <- c()
p_unem2 <- c()
p_rc_ffr <- c()
p_rc_cpi <- c()
p_rc_unem <- c()
p_rc_ffr2 <- c()
p_rc_cpi2 <- c()
p_rc_unem2 <- c()

for(i in 8:724){
  print(i)
  x <- sctest(lagFFR_model, data = data, type = "Chow", point = i)
  y <- sctest(lagdeltaCPI_model, data = data, type = "Chow", point = i)
  q <- sctest(lagUNEM_model, data = data, type = "Chow", point = i)
  
  p_ffr <- c(p_ffr, x$p.value)
  p_cpi <- c(p_cpi, y$p.value)
  p_unem <- c(p_unem, q$p.value)
  
  a <- sctest(lagFFR_model2, data = data, type = "Chow", point = i)
  b <- sctest(lagdeltaCPI_model2, data = data, type = "Chow", point = i)
  c <- sctest(lagUNEM_model2, data = data, type = "Chow", point = i)
  
  p_ffr2 <- c(p_ffr2, a$p.value)
  p_cpi2 <- c(p_cpi2, b$p.value)
  p_unem2 <- c(p_unem2, c$p.value)
  
  e <- sctest(lagFFR_model, data = data[i:z,], type = "Rec-CUSUM")
  f <- sctest(lagdeltaCPI_model, data = data[i:z,], type = "Rec-CUSUM")
  g <- sctest(lagUNEM_model, data = data[i:z,], type = "Rec-CUSUM")
  
  h <- sctest(lagFFR_model2, data = data[i:z,], type = "Rec-CUSUM")
  j <- sctest(lagdeltaCPI_model2, data = data[i:z,], type = "Rec-CUSUM")
  k <- sctest(lagUNEM_model2, data = data[i:z,], type = "Rec-CUSUM")

  p_rc_ffr <- c(p_rc_ffr, e$p.value)
  p_rc_cpi <- c(p_rc_cpi, f$p.value)
  p_rc_unem <- c(p_rc_unem, g$p.value)
  p_rc_ffr2 <- c(p_rc_ffr2, h$p.value)
  p_rc_cpi2 <- c(p_rc_cpi2, j$p.value)
  p_rc_unem2 <- c(p_rc_unem2, k$p.value)
  
}


ffr_ts1 <- ts(p_ffr, start=c(1955, 4), end=c(2014, 12), frequency=12)
cpi_ts1 <- ts(p_cpi, start=c(1955, 4), end=c(2014, 12), frequency=12)
unem_ts1 <- ts(p_unem, start=c(1955, 4), end=c(2014, 12), frequency=12)

ffr_ts2 <- ts(p_ffr2, start=c(1955, 4), end=c(2014, 12), frequency=12)
cpi_ts2 <- ts(p_cpi2, start=c(1955, 4), end=c(2014, 12), frequency=12)
unem_ts2 <- ts(p_unem2, start=c(1955, 4), end=c(2014, 12), frequency=12)

ffr_ts3 <- ts(p_rc_ffr, start=c(1955, 4), end=c(2014, 12), frequency=12)
cpi_ts3 <- ts(p_rc_cpi, start=c(1955, 4), end=c(2014, 12), frequency=12)
unem_ts3 <- ts(p_rc_unem, start=c(1955, 4), end=c(2014, 12), frequency=12)

ffr_ts4 <- ts(p_rc_ffr2, start=c(1955, 4), end=c(2014, 12), frequency=12)
cpi_ts4 <- ts(p_rc_cpi2, start=c(1955, 4), end=c(2014, 12), frequency=12)
unem_ts4 <- ts(p_rc_unem2, start=c(1955, 4), end=c(2014, 12), frequency=12)


plot(ffr_ts1, type = 'l', ylab = "p-value", main = "P-Values of Chow-Test, FFR VAR(1)")
abline(a = .05, b = 0, col = 'red')
plot(cpi_ts1, type = 'l', ylab = "p-value", main = "P-Values of Chow-Test, CPI VAR(1)")
abline(a = .05, b = 0, col = 'red')
plot(unem_ts1, type = 'l', ylab = "p-value", main = "P-Values of Chow-Test, UNEM VAR(1)")
abline(a = .05, b = 0, col = 'red')

plot(ffr_ts2, type = 'l', ylab = "p-value", main = "P-Values of Chow-Test, FFR VAR(2)")
abline(a = .05, b = 0, col = 'red')
plot(cpi_ts2, type = 'l', ylab = "p-value", main = "P-Values of Chow-Test, CPI VAR(2)")
abline(a = .05, b = 0, col = 'red')
plot(unem_ts2, type = 'l', ylab = "p-value", main = "P-Values of Chow-Test, UNEM VAR(2)")
abline(a = .05, b = 0, col = 'red')

scus_ffr <- efp(lagFFR_model2, type = "Rec-CUSUM", data = data)
plot(scus_ffr)
scus_ffr2 <- efp(lagFFR_model2, type = "Rec-CUSUM", data = data[353:nrow(data),])
plot(scus_ffr2, main = "Recursive CUSUM Test for FFR")

scus_cpi <- efp(lagdeltaCPI_model2, type = "Rec-CUSUM", data = data)
plot(scus_cpi, main = "Recursive CUSUM Test for CPI")
scus_cpi2 <- efp(lagdeltaCPI_model2, type = "Rec-CUSUM", data = data[353:nrow(data),])
plot(scus_cpi2, main = "Recursive CUSUM Test for IR")

scus_unem <- efp(lagUNEM_model2, type = "Rec-CUSUM", data = data)
plot(scus_unem)
scus_unem2 <- efp(lagUNEM_model2, type = "Rec-CUSUM", data = data[353:nrow(data),])
plot(scus_unem2, main = "Recursive CUSUM Test for UNEM")


