#Clase práctica ECONOMETRÍA DE EDWIN RUEDA Y MAYERLI OBANDO

#Librerías
library(fpp2)
library(tidyverse)
library(readxl)
library(forecast)
library(ggplot2)
library(lubridate)
library(car)
library(dplyr)
library(fitdistrplus)
library(stargazer)
library(tseries)
library(lmtest)
library(fitdistrplus)
library(fpp)
library(purrr)
library(timsac)
library(dplyr)
library(tseries)
library(urca)
library(tsm)
library(astsa)
library(timeDate)


#Datos
library(readxl)
datoscons <- read_excel("C:/Users/dell/Desktop/Trabajos econ?micos/datoscons.xlsx")
View(datoscons)
attach(datoscons)
#Base de datos desde github
https://github.com/May3rli/Extra/blob/ff91f9d632de317a2237aeea6d451ff3078ab002/datoscons.xlsx

#Serie
consumo.ts <- ts(Consumo, start = c(2006,1), frequency = 4)
consumo.ts
plot(consumo.ts)

#Serie desestacionalizada y sin error

dects <- decompose(consumo.ts, "multiplicative")
plot(dects)
trendci_consu <- as.ts(dects$trend)
trendci_cons <- na.omit(trendci_consu) 
plot(trendci_cons)
trendci_cons
ac(trendci_cons)


#Pruebas RU

PP <- ur.pp(trendci_cons, type = "Z-tau", model = "trend", lags = "short")
summary(PP)

KPSS <- ur.kpss(trendci_cons, type = "tau", lags =  "short")
summary(KPSS)

##En las dos pruebas realizadas, KPPS y PP encontramos que hay raíz unitaria----- 

#Sacamos la primera diferencia (correción RU)

dtcorr <- diff(trendci_cons)
ts.plot(dtcorr)
dtcorr
dtc <- na.omit(dtcorr) 


#Pruebas RU (tras la correción)

PP1 <- ur.pp(dtc, type = "Z-tau", model = "constant", lags = "short")
summary(PP1)

KPSS1 <- ur.kpss(dtc, type = "tau", use.lag = 1 )
summary(KPSS1)

###En PP, se rechaza Ho y en KPSS no se rechaza Ho. No hay raíz unitaria.


#Serie AR 
##Correlograma----------

ac(dtc, max.lag = 30)

##Podemos ver que no hay procesos con RU mediante el correlograma. La serie es AR1, I1, Q0

##Box test
Box.test(dtc, lag = 1, type = "Ljung-Box")


#MODELO ARIMA 

AR110 <- arima(dtc, order = c(1, 1, 0), include.mean = FALSE)
AR110

q.stat <- rep(0,10)
q.prob <- rep(0,10)
for (i in 1:10) {
  q.stat[i] <- Box.test(dtc,lag = i, type = "Ljung-Box")$statistic
  
  
}

arima311 <- arima(dtc, order =c(3,1,1), include.mean = FALSE)
arima311

arima.tc1 <- na.omit(dtc)

ac(arima311$residuals, max.lag=30)
par(mfrow = c(1,1))
plot(arima311$residuals)

arima201 <- arima(dtc, order =c(2,0,1), include.mean = FALSE)
arima201

ac(arima201$residuals, max.lag=30)
par(mfrow = c(1,1))
plot(arima201$residuals)

