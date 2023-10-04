library(readr)
library(lubridate)

HD <- read_csv("https://raw.githubusercontent.com/Lairere/simon/main/HD.csv")
AAPL <- read_csv("https://raw.githubusercontent.com/Lairere/simon/main/AAPL.csv")
CSCO <- read_csv("https://raw.githubusercontent.com/Lairere/simon/main/CSCO.csv")
VZ <- read_csv("https://raw.githubusercontent.com/Lairere/simon/main/VZ.csv")
MKT_RF <- read_csv("https://raw.githubusercontent.com/Lairere/simon/main/Factors.CSV")

#########Cleaning&Merge###########
#########HD#########
HD$Date <- as.Date(HD$Date)
HD$Date <- format(HD$Date, '%Y%m')
HD$R_HD <- 0
for (i in 2:121){
    HD$R_HD[i] <- (HD$Close[i]-HD$Close[i-1])/HD$Close[i-1]
}
HD$R_HD[1] <- NA 

#########VZ#########
VZ$Date <- as.Date(VZ$Date)
VZ$Date <- format(VZ$Date, '%Y%m')
VZ$R_VZ <- 0
for (i in 2:121){
    VZ$R_VZ[i] <- (VZ$Close[i]-VZ$Close[i-1])/VZ$Close[i-1]
}
VZ$R_VZ[1] <- NA 

########AAPL########
AAPL$Date <- as.Date(AAPL$Date)
AAPL$Date <- format(AAPL$Date, '%Y%m')
AAPL$R_AAPL <- 0
for (i in 2:121){
    AAPL$R_AAPL[i] <- (AAPL$Close[i]-AAPL$Close[i-1])/AAPL$Close[i-1]
}
AAPL$R_AAPL[1] <- NA 

########CSCO########
CSCO$Date <- as.Date(CSCO$Date)
CSCO$Date <- format(CSCO$Date, '%Y%m')
CSCO$R_CSCO <- 0
for (i in 2:118){
    CSCO$R_CSCO[i] <- (CSCO$Close[i]-CSCO$Close[i-1])/CSCO$Close[i-1]
}
CSCO$R_CSCO[1] <- NA 

########Mkt_Rf########
colnames(MKT_RF) <- c("Date","Rm-Rf","SMB","HML","Rf")
MKT_RF$year <- floor(MKT_RF$Date/100)
MKT_RF_90 <- MKT_RF[MKT_RF$year >= 1990 & MKT_RF$year <= 1999, ]

########Merge########
CAPM <- data.frame(HD$Date,HD$R_HD,AAPL$R_AAPL,VZ$R_VZ)
CAPM <- merge(CAPM, CSCO[,c('Date','R_CSCO')], by.x='HD.Date', by.y = 'Date', all.x=TRUE)
CAPM <- merge(CAPM, MKT_RF_90[,c('Date','Rm-Rf','Rf')], by.x='HD.Date', by.y = 'Date', all.x = TRUE)
colnames(CAPM) <- c("Date","HD","AAPL","VZ","CSCO","Rm-Rf","Rf")
CAPM$`Rm-Rf` <- CAPM$`Rm-Rf`/100
CAPM$`Rf` <- CAPM$`Rf`/100
CAPM <- CAPM[-1,]
for (i in 2:5){
    CAPM[,i] <- CAPM[,i]-CAPM$Rf
}

#########Linear Regression###########
HD_model <- lm(HD ~ `Rm-Rf`+0, data = CAPM)
AAPL_model <- lm(AAPL ~ `Rm-Rf`+0, data = CAPM)
VZ_model <- lm(VZ ~ `Rm-Rf`+0, data = CAPM)
CSCO_model <- lm(CSCO ~ `Rm-Rf`+0, data = CAPM)

summary(HD_model)
summary(AAPL_model)
summary(VZ_model)
summary(CSCO_model)

plot(CAPM$`Rm-Rf`,CAPM$HD)
plot(CAPM$`Rm-Rf`,CAPM$AAPL)
plot(CAPM$`Rm-Rf`,CAPM$VZ)
plot(CAPM$`Rm-Rf`,CAPM$CSCO)
