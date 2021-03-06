setwd('C:/Users/Danilo/OneDrive/FIA/P�s Data Science/Aulas/Analytics/06 - abr22/20220413')

options(scipen=999) #Retira nota��o cient�fica

#install.packages("tseries")
library(tseries)
serie <- read.ts("dados22.txt", header = FALSE, sep = "", skip = 0)

# Gr�fico da serie
par(mfrow=c(1,1))
ts.plot(serie)

# Teste de Estacionariedade
adf.test(serie)

#Divis�o em treino e teste
treino <- serie[1:988]
teste <- serie[989:1000]

#Identifica��o
# Gr�fico do ACF e PACF
par(mfrow=c(1,2))
acf(treino, main="ACF")
pacf(treino,main="PACF")

#Modelo AR(2) com intercepto diferente de zero
modelo <- arima(treino, order = c(2,0,0), fixed = c(NA,NA,NA), method = c("ML"))
modelo

#teste de hipotese
#install.packages("lmtest")
library(lmtest)
options(scipen=999)
coeftest(modelo) #apresenta o modelo e os p-valores

#Retirar o interceptor
modelo <- arima(treino, order = c(2,0,0), fixed = c(NA,NA,0), method = c("ML"))
coeftest(modelo) #apresenta o modelo e os p-valores

#An�lise de Res�duos
par(mfrow=c(1,2))
acf(residuals(modelo), main="ACF dos Res�duos")
pacf(residuals(modelo), main="PACF dos Res�duos")

#Predito
par(mfrow=c(1,1))
pred = predict(modelo, n.ahead = 12)
ts.plot(serie, pred$pred, lty = c(1,3), col=c(5,2))

#Erro
library(Metrics)
sse(teste,pred$pred)
mae(teste,pred$pred)
mape(teste,pred$pred)
