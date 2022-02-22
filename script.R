library(car)
library(describedata)
library(skedastic)
library(psych)
library(olsrr)
library(klaR)
library(sandwich)
library(lmtest)
library(systemfit)
library(DataCombine)
library(forecast)
library(tseries)
library(foreign)
library(readxl)
library(readxlsb)
library(xlsx)
library(readxl)
library(MLmetrics)
library(astsa)
library(readr)
library(xts)
library(reshape2)
library(ggfortify)
library(ggplot2)
library(sjPlot)



data <- read_csv("C:/Users/anton/Downloads/con_data.csv", 
                     col_types = cols(VENDITE_3 = col_double(), 
                                      SCONTRINI_3 = col_double(), DATA_4 = col_datetime(format = "%Y-%m-%d")))

data$...1 <- NULL
data$`Unnamed: 0` <- NULL
data$`PRESSIONEMEDIA mb` <- NULL
data$DATA_3 <- NULL


#______________________________AGGIUNGO COLONNA COLORE DELL'EMILIA ROMAGNA___________________
data$COLORE<-c("arancione")

for (i in 1:1405){
  data[i,31]<-NA
}

#dal 6 novembre al 14 novembre zona gialla
for (i in 1406:1414){
  data[i,31]<-c("gialla")
}

#dal 15 novembre al 5 dicembre zona aracione
for (i in 1415:1435){
  data[i,31]<-c("arancione")
}

#dal 6 dicembre al 23 dicembre zona gialla
for (i in 1436:1453){
  data[i,31]<-c("gialla")
}

#dal 24 al 27 dicembre rossa 
for (i in 1454:1457){
  data[i,31]<-c("rossa")
}

#dal 28 al 30 dicembre arancione 
for (i in 1458:1460){
  data[i,31]<-c("arancione")
}

#dal 31 dicembre al 3 gennaio rossa
for (i in 1461:1464){
  data[i,31]<-c("rossa")
}

data[1465,31]<-c("aracione")
data[1466,31]<-c("rossa")
data[1467,31]<-c("rossa")
data[1468,31]<-c("gialla")
data[1469,31]<-c("gialla")

#dal 9 al 31 gennaio arancione 
for (i in 1470:1492){
  data[i,31]<-c("arancione")
}

#dall'1 febbraio al 20 gialla 
for (i in 1471:1512){
  data[i,31]<-c("gialla")
}

#dal 21 febbraio al 14 marzo arancione 
for (i in 1513:1534){
  data[i,31]<-c("arancione")
}

#dal 15 marzo al 11 aprile rossa
for (i in 1535:1562){
  data[i,31]<-c("rossa")
}

data[1563,31]<-c("aracione")


#dal 9 marzo 2020 al 18 maggio 2020 lockdown
for (i in 1164:1234){
  data[i,31]<-c("lockdown")
}

##################################################################

#______________________________AGGIUNGO COLONNA COVID (1/0)___________________
data$COVID<-0

for (i in 1:1405){
  data[i,32]<-0
}

#dal 6 novembre al 14 novembre zona gialla
for (i in 1406:1414){
  data[i,32]<-1
}

#dal 15 novembre al 5 dicembre zona aracione
for (i in 1415:1435){
  data[i,32]<-1
}

#dal 6 dicembre al 23 dicembre zona gialla
for (i in 1436:1453){
  data[i,32]<-1
}

#dal 24 al 27 dicembre rossa 
for (i in 1454:1457){
  data[i,32]<-1
}

#dal 28 al 30 dicembre arancione 
for (i in 1458:1460){
  data[i,32]<-1
}

#dal 31 dicembre al 3 gennaio rossa
for (i in 1461:1464){
  data[i,32]<-1
}

data[1465,32]<-1
data[1466,32]<-1
data[1467,32]<-1
data[1468,32]<-1
data[1469,32]<-1

#dal 9 al 31 gennaio arancione 
for (i in 1470:1492){
  data[i,32]<-1
}

#dall'1 febbraio al 20 gialla 
for (i in 1471:1512){
  data[i,32]<-1
}

#dal 21 febbraio al 14 marzo arancione 
for (i in 1513:1534){
  data[i,32]<-1
}

#dal 15 marzo al 11 aprile rossa
for (i in 1535:1562){
  data[i,32]<-1
}

data[1563,32]<-1


#dal 9 marzo 2020 al 18 maggio 2020 lockdown
for (i in 1164:1234){
  data[i,32]<-1
}

#################################################################

B<- data[,c("DATA_4","VENDITE_1")]
B[is.na(B)] <- 0

ts <- xts(B$VENDITE_1, as.Date(B$DATA_4, "%Y-%m-%d"))
ts2 <- xts(data$VENDITE_2, as.Date(data$DATA_4, "%Y-%m-%d"))
ts3 <- xts(data$VENDITE_3, as.Date(data$DATA_4, "%Y-%m-%d"))
ts4 <- xts(data$VENDITE_4, as.Date(data$DATA_4, "%Y-%m-%d"))
ts5 <- xts(data$VENDITE_5, as.Date(data$DATA_4, "%Y-%m-%d"))
ts6 <- xts(data$VENDITE_6, as.Date(data$DATA_4, "%Y-%m-%d"))

ts[is.na(ts)] <- 0
ts2[is.na(ts2)] <- 0
ts3[is.na(ts3)] <- 0
ts4[is.na(ts4)] <- 0
ts5[is.na(ts5)] <- 0
ts6[is.na(ts6)] <- 0



#Creo serie storica settimanale anzichè giornaliera
ts_w = apply.weekly(ts, FUN = sum)

ts_w <- ts_w[-1] #rimosso perchè solo un giorno, non significativo come dato settimanale
ts_w <- ts_w[-224] #rimosso perchè solo un giorno, non significativo come dato settimanale
tsw3 <- ts(ts_w, start=1, frequency=52)

#############################################################################################
##############################################################################################
#################################################################################################


y2 <- ts(ts_w, end= c(2021, 15), frequency= 52)
y2[61] <- 37600
smpl1 <- window(y2, end = c(2019, 1))
smpl2 <- window(y2, start = c(2019, 1), end = c(2019,52))


arima_optimal = auto.arima(y2, seasonal = T , stepwise=FALSE, approximation=FALSE)
arimaorder(arima_optimal)


#############################################################################################
#############################################################################################
#############################################################################################


#plot serie storica, dati aggregati settimanalmente

par(mfrow=c(1,1), cex.axis=1.2, cex.lab=2, cex.main=3, fg="white", bg="black",
    col.axis="white", col.lab="white", col.main="white")
plot(y2, col="yellow", type= "l", main = 'Serie storica settimanale', xlab = '', ylab = '', 
     lwd= 2, axes = FALSE, line= 2)
mtext("Anno", side=1, line=3, cex = 2)
mtext("Vendite", side=2, line=2, cex = 2)
title()
box()
axis(2)
axis(1)




#Decomposizione serie storica doppia stagionalità (settimanale ed annuale)
y <- msts(ts, end=c(2021,95), seasonal.periods=c(7,365.25)) 
y_ts <- msts(y, ts.frequency = 365, start = c(01,2017), seasonal.periods = c(7,365.25))
y_ts_decomp<-mstl(y_ts, s.window = "periodic")
autoplot(y_ts_decomp)



differ <- diff(tsw3) #ci accorgiamo che differenziandola la serie diventa stazionaria
plot(differ)



#stimiamo il modello NUMERO 1

arima_optimal = auto.arima(smpl1, seasonal = T , stepwise=FALSE, approximation=FALSE)
arimaorder(arima_optimal)


est <- Arima(smpl1, order = c(0,1,1), seasonal = list(order=c(1,0,0),period = 52))


res <- est$residuals


#validazione del modello
par(mfrow=c(1,2),cex.axis=1, cex.lab=1, cex.main=1, fg="white",
    col.axis="white", col.lab="white", col.main="white", bg="black")
acf(res, ci.col="white")
pacf(res, ci.col="white")
hist(res)
qqnorm(res)
qqline(res)
shapiro.test(res)
ols_test_normality(res)


# stime
frc <- forecast(est, h=length(smpl2))


autoplot(frc, ts.colour = 'black', size = 0.5) +
  scale_color_manual(labels = c("Actual", "Forecasted"),
                     values=c("black", "blue")) +
  autolayer(frc, series = 'Forecasts', size = 0.8) +
  autolayer(smpl2, series='Data', size = 0.5) +
  guides(colour = guide_legend(title= 'Data series')) +
  ggtitle('Previsione serie storica aggregato settimanale') +
  labs(x='Anni', y='Vendite') +
  theme(legend.position = c(0.15, 0.85), legend.text=element_text(size=rel(3)), legend.title=element_text(size=rel(3.5)),
        axis.title = element_text(size = rel(3.5)), plot.title = element_text(size = rel(2.9)),
        axis.text.x = element_text(size = rel(3)), axis.text.y = element_text(size = rel(3)))

  

MAPE(frc$mean, smpl2) * 100
est$aic


#####################################################################################
#####################################################################################

#modello tbats a doppia stagionalità sui dati giornalieri

ts <- ts[-1]
ts <- ts[-1562]

y <- msts(ts, end=c(2021,95), seasonal.periods=c(7,365.25))
train <- window(y, end = c(2018, 365))
test <- window(y, start = c(2019, 1), end = c(2019,365))
te <- msts(test, start = c(2019,1), end=c(2019,365), seasonal.periods=c(7,365.25))

tr <- msts(train, end=c(2018,365), seasonal.periods=c(7,365.25))
fit <- tbats(tr)
fc <- forecast(fit, h=length(te))

autoplot(fc, ts.colour = 'black') +
  scale_color_manual(labels = c("Actual", "Forecasted"),
                     values=c("black", "blue")) +
  scale_size_manual(labels = c("Actual", "Forecasted"),
                    values = 5) +
  autolayer(fc, series = 'Forecasts') +
  autolayer(te, series='Data') +
  guides(colour = guide_legend(title= 'Data series')) +
  xlim(2018.75, 2019.25) +
  ggtitle('Previsione serie storica aggregato giornaliero') +
  labs(x='Anni', y='Vendite') +
  theme(legend.position = c(0.1, 0.88), legend.text=element_text(size=rel(3)), legend.title=element_text(size=rel(3.5)),
        axis.title = element_text(size = rel(3.5)), plot.title = element_text(size = rel(3)),
        axis.text.x = element_text(size = rel(3)), axis.text.y = element_text(size = rel(3)))


MAPE(fc$mean, te) * 100
fit$AIC

#########################################################################################
#PREVISIONE SU TUTTO IL 2021 DELLE VENDITE
tot <- msts(y, seasonal.periods=c(7,365.25))

fit_tot <- tbats(tot)
fc_tot <- forecast(fit_tot, h= 270)
autoplot(fc_tot)

###########################################################################################
var<-c("WEEKEND", "COVID", "TMEDIA")
pairs.panels(data[,var])


mod1<- lm(data$VENDITE_1 ~ data$TMEDIA+data$WEEKEND+data$COVID,data)
summary(mod1)

mod2 <- lm(data$VENDITE_2 ~ data$TMEDIA+data$WEEKEND+data$COVID,data)
summary(mod2)

mod3 <- lm(data$VENDITE_3 ~ data$TMEDIA+data$WEEKEND+data$COVID,data)
summary(mod3)

mod4 <- lm(data$VENDITE_4 ~ data$TMEDIA+data$WEEKEND+data$COVID,data)
summary(mod4)

mod5 <- lm(data$VENDITE_5 ~ data$TMEDIA+data$WEEKEND+data$COVID,data)
summary(mod5) #TEMPERATURA MEDIA NON SIGNIFICATIVA

mod52<- lm(data$VENDITE_5 ~ data$WEEKEND+data$COVID,data)
summary(mod52)

mod6 <- lm(data$VENDITE_6 ~ data$TMEDIA+data$WEEKEND+data$COVID,data)
summary(mod6)

#MODELLO 1 UNICO CHE SPIEGA PIù DEL 50%
#effetto del covid nettamente più impattante per il ristorante 1 rispetto agli altri,
#è inoltre quello con l'R2 più elevato (0.55).
#stranamente la tmedia influenza negativamente le vendite in tutti i ristoranti

##########################################################################################

#confronto tra i mesi
par(mfrow=c(1,1))
ts_mon = apply.monthly(ts, FUN = sum)
ts_mon <- ts_mon[-52]
month <- ts(ts_mon, end= c(2021, 3), frequency= 12)
plot(month)

ggseasonplot(month, year.labels=TRUE, year.labels.left=FALSE) +
  geom_line(size = 1.5) +
  ggtitle('Andamento vendite mensili negli anni del ristorante 1') + 
  labs(x='Month', y='Vendite') +
  theme(axis.title = element_text(size = rel(3.5)), plot.title = element_text(size = rel(4.8)),
        axis.text.x = element_text(size = rel(3)), axis.text.y = element_text(size = rel(3))) #seasonal plot


ggsubseriesplot(month) +
  geom_line(size = 1.5) +
  ggtitle('Confronto vendite per lo stesso mese nei diversi anni') + 
  labs(x='Mese', y='Vendite') +
  theme(axis.title = element_text(size = rel(3.5)), plot.title = element_text(size = rel(4.8)),
        axis.text.x = element_text(size = rel(3)), axis.text.y = element_text(size = rel(3))) #seasonal plot #confronto mesi


#senza stagionalità
deco_m <- decompose(month)
plot(deco_m)
no_seas_m <- seasadj(deco_m) #serie storica privata della stagionalità
plot(no_seas_m)


ggseasonplot(no_seas_m, year.labels=TRUE, year.labels.left=FALSE) +
  geom_line(size = 1.5) +
  ggtitle('Andamento vendite mensili negli anni privata di stagionalità') + 
  labs(x='Mese', y='Vendite') +
  theme(axis.title = element_text(size = rel(3.5)), plot.title = element_text(size = rel(4.3)),
        axis.text.x = element_text(size = rel(3)), axis.text.y = element_text(size = rel(3))) #seasonal plot


ggsubseriesplot(no_seas_m) +
  geom_line(size = 1.5) +
  ggtitle('Confronto vendite stesso mese nei diversi anni privata di stagionalità') + 
  labs(x='Mese', y='Vendite') +
  theme(axis.title = element_text(size = rel(3.5)), plot.title = element_text(size = rel(3.8)),
        axis.text.x = element_text(size = rel(3)), axis.text.y = element_text(size = rel(3))) #seasonal plot


###################################################################################

##Variazione percentuale vendite dei ristoranti rispetto all'anno precedente

parz <- data[,c('DATA_4','VENDITE_1','VENDITE_2','VENDITE_3','VENDITE_4','VENDITE_5','VENDITE_6')]

v1 <- data[,c('DATA_4', 'VENDITE_1')]
v2 <- data[,c('DATA_4', 'VENDITE_2')]
v3 <- data[,c('DATA_4', 'VENDITE_3')]
v4 <- data[,c('DATA_4', 'VENDITE_4')]
v5 <- data[,c('DATA_4', 'VENDITE_5')]
v6 <- data[,c('DATA_4', 'VENDITE_6')]


v1[is.na(v1)] <- 0
v2[is.na(v2)] <- 0
v3[is.na(v3)] <- 0
v4[is.na(v4)] <- 0
v5[is.na(v5)] <- 0
v6[is.na(v6)] <- 0


vts1 <- xts(v1$VENDITE_1, as.Date(v1$DATA_4, "%Y-%m-%d"))
vts2 <- xts(v2$VENDITE_2, as.Date(v2$DATA_4, "%Y-%m-%d"))
vts3 <- xts(v3$VENDITE_3, as.Date(v3$DATA_4, "%Y-%m-%d"))
vts4 <- xts(v4$VENDITE_4, as.Date(v4$DATA_4, "%Y-%m-%d"))
vts5 <- xts(v5$VENDITE_5, as.Date(v5$DATA_4, "%Y-%m-%d"))
vts6 <- xts(v6$VENDITE_6, as.Date(v6$DATA_4, "%Y-%m-%d"))


v1_m <- apply.yearly(vts1, FUN = mean)
v2_m <- apply.yearly(vts2, FUN = mean)
v3_m <- apply.yearly(vts3, FUN = mean)
v4_m <- apply.yearly(vts4, FUN = mean)
v5_m <- apply.yearly(vts5, FUN = mean)
v6_m <- apply.yearly(vts6, FUN = mean)



cb <- cbind(v1_m, v2_m, v4_m, v5_m, v6_m)
cd <- data.frame(cb)

colnames(cd)[1] <- "Ristorante_1"
colnames(cd)[2] <- "Ristorante_2"
colnames(cd)[3] <- "Ristorante_4"
colnames(cd)[4] <- "Ristorante_5"
colnames(cd)[5] <- "Ristorante_6"

unito2 <- ts(cd, frequency=1, start = c(2017), end = c(2021))

for (i in 1:5){
  unito3[i] <- (unito3[i]/unito2[i-1])*100
  }

unito3 <- diff(unito2)
unito3/unito2

unito3df <- data.frame(unito3)
unito2df <- data.frame(unito2)
unito2df <- unito2df[-5,]
fin <- (unito3df/unito2df)*100
fin2 <- ts(fin, frequency=1, start = c(2018), end = c(2020))
fin2[1,5] <- 0

fin$v1_m

autoplot(fin2) +
  guides(colour = guide_legend(title= 'Data series')) + 
  geom_line(size = 1.5) +
  ggtitle("Variazione % rispetto all'anno precedente") + 
  labs(x='Anni', y='Variazione %') +
  theme(legend.text=element_text(size=rel(2)), legend.title=element_text(size=rel(2.5)),
        axis.title = element_text(size = rel(3.5)), plot.title = element_text(size = rel(3)),
        axis.text.x = element_text(size = rel(3)), axis.text.y = element_text(size = rel(3))) #seasonal plot

#Variazione percentuale vendite dei ristoranti rispetto all'anno precedente
#manca ristorante 3 perchè è stato aperto durante il covid
#riga 1 ristorante 6 sostituita con 0 perchè non significativa (solo 2 mesi nel 2017)


#######################################################################

ts_mon = apply.monthly(ts, FUN = sum)
ts_mon2 = apply.monthly(ts2, FUN = sum)
ts_mon3 = apply.monthly(ts3, FUN = sum)
ts_mon4 = apply.monthly(ts4, FUN = sum)
ts_mon5 = apply.monthly(ts5, FUN = sum)
ts_mon6 = apply.monthly(ts6, FUN = sum)

ts_mon3 <- ts_mon3[-(1:33)]
ts_mon6 <- ts_mon6[-(1:7)]

both_build_df <- merge(ts_mon, ts_mon2, ts_mon3, ts_mon4, ts_mon5, ts_mon6, all = TRUE, suffixes= c("1", "2", "3", "4", "5", "6"))

colnames(both_build_df)[1] <- "Ristorante_1"
colnames(both_build_df)[2] <- "Ristorante_2"
colnames(both_build_df)[3] <- "Ristorante_3"
colnames(both_build_df)[4] <- "Ristorante_4"
colnames(both_build_df)[5] <- "Ristorante_5"
colnames(both_build_df)[6] <- "Ristorante_6"

both_build_ts <- ts(both_build_df, frequency=12, start = c(2017,1), end = c(2021,3))


autoplot(both_build_ts, size = 0.8) +
  guides(colour = guide_legend(title= 'Data series')) +
  ggtitle('Andamento vendite mensili dei sei ristorante') +
  labs(x='Anni', y='Venditi mensili') +
  theme(legend.text=element_text(size=rel(2)), legend.title=element_text(size=rel(2.5)),
        axis.title = element_text(size = rel(3.5)), plot.title = element_text(size = rel(4.8)),
        axis.text.x = element_text(size = rel(3)), axis.text.y = element_text(size = rel(3)))

###############################################################################


#VENDITE

B<-data[,c("DATA_4","VENDITE_1")]
B[is.na(B)]<-0


ts <- xts(B$VENDITE_1, as.Date(B$DATA_4, "%Y-%m-%d"))

ts_mon = apply.monthly(ts, FUN = sum)
ts_mon <- ts_mon[-52]
VENDITE <- ts(ts_mon, end= c(2021, 3), frequency= 12)


#SCONTRINI

B<-data[,c("DATA_4","SCONTRINI_1")]
B[is.na(B)]<-0


ts <- xts(B$SCONTRINI_1, as.Date(B$DATA_4, "%Y-%m-%d"))

ts_mon = apply.monthly(ts, FUN = sum)
ts_mon <- ts_mon[-52]
SCONTRINI <- ts(ts_mon, end= c(2021, 3), frequency= 12)


#COVID

B<-data[,c("DATA_4","COVID")]
B[is.na(B)]<-0


ts <- xts(B$COVID, as.Date(B$DATA_4, "%Y-%m-%d"))

ts_mon = apply.monthly(ts, FUN = sum)
ts_mon <- ts_mon[-52]
COVID <- ts(ts_mon, end= c(2021, 3), frequency= 12)


#WEEKEND

B<-data[,c("DATA_4","WEEKEND")]
B[is.na(B)]<-0


ts <- xts(B$WEEKEND, as.Date(B$DATA_4, "%Y-%m-%d"))

ts_mon = apply.monthly(ts, FUN = sum)
ts_mon <- ts_mon[-52]
WEEKEND <- ts(ts_mon, end= c(2021, 3), frequency= 12)


#TMEDIA

B<-data[,c("DATA_4","TMEDIA")]
B[is.na(B)]<-0


ts <- xts(B$TMEDIA, as.Date(B$DATA_4, "%Y-%m-%d"))

ts_mon = apply.monthly(ts, FUN = sum)
ts_mon <- ts_mon[-52]
TMEDIA <- ts(ts_mon, end= c(2021, 3), frequency= 12)


#MALTEMPO

B<-data[,c("DATA_4","MALTEMPO")]
B[is.na(B)]<-0


ts <- xts(B$MALTEMPO, as.Date(B$DATA_4, "%Y-%m-%d"))

ts_mon = apply.monthly(ts, FUN = sum)
ts_mon <- ts_mon[-52]
MALTEMPO <- ts(ts_mon, end= c(2021, 3), frequency= 12)


mydata<-data.frame(VENDITE,SCONTRINI,COVID,WEEKEND,TMEDIA,MALTEMPO)
colnames(mydata)<-c("VENDITE","SCONTRINI","COVID","WEEKEND","TMEDIA","MALTEMPO" )


#___________________________________SOM_________________________________________

library(aweSOM)


#___________________RISTO1___________________

full.data <- mydata
## Select variables
train.data <- full.data

### Scale training data
train.data <- scale(train.data)

### RNG Seed (for reproducibility)
set.seed(2021)
### Initialization (PCA grid)
init <- somInit(train.data, 4, 4)
## Train SOM
risto1.som <- kohonen::som(train.data, grid = kohonen::somgrid(4, 4, "hexagonal"), 
                           rlen = 100, alpha = c(0.05, 0.01), radius = c(2.65,-2.65), 
                           dist.fcts = "sumofsquares", init = init)

#Clustering 
superclust_pam_1 <- cluster::pam(risto1.som$codes[[1]], 1)
superclasses_pam_1 <- superclust_pam_1$clustering

#Plotting numeric variables on the map
aweSOMplot(som = risto1.som, type = "Circular", data= full.data, superclass = superclasses_pam_1, palvar = "rainbow")

#Plotting Umatrix 
aweSOMplot(som = risto1.som, type = "UMatrix", data=train.data, superclass = superclasses_pam_1)

#Plotting fluid Umatrix 
aweSOMsmoothdist(risto1.som)

#Quality of the map
somQuality(risto1.som, train.data)

#Hitmap
aweSOMplot(som = risto1.som, type = "Hitmap", superclass = superclasses_pam_1)





