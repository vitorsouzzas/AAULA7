                       #Aula 7 - Critérios de Informação

library(readxl)                                                        #Carrega os pacotes
variacao_PIB <- read.table("c:/Econometria/Variacao.xls", header = T)  #Carrega os arquivos
var_PIB <- ts(variacao_PIB$variacao_PIB, start =1951, frequency = 1 )  #Cria a serie temporal var_PIB
View(var_PIB)
plot(var_PIB, main="Variacao do PIB Brasileiro", col="Blue", ylab="%PIB", xlab="Ano")

acf(var_PIB)         #Cria a FAC -Função de Autocorrelação (ACF)
pacf(var_PIB)        #cria a FACP - Funçao de Autocorrelação Parcial (PACF)

AR1 <- arima(var_PIB, order = c(1,0,0))   #Estima um modelo autoreressivo de ordem p=1 ,AR(1)
MA1 <- arima(var_PIB, order = c(0,0,1))   #Estima um modelo de médias móveis ordem q=1 , MA(1)
ARMA11 <- arima(var_PIB, order = c(1,0,1))#Estima um modelo autoregressivo de médias móveis ordem p=1 e q=1 ARMA(1,1)

AIC(AR1) #Extrai a estatística AIC do modelo AR1
BIC(AR1) #Extrai a estatística BIC Ddo modelo AR1


AR2 <- arima(var_PIB, order = c(2,0,0))

MA2 <- arima(var_PIB, order = c(0,0,2))
AIC(MA2)
BIC(MA2)
MA3 <- arima(var_PIB, order = c(0,0,3))
AIC(MA3)
BIC(MA3)
MA4<-arima(var_PIB,order=c(0,0,4))
AIC(MA4)
BIC(MA5)
MA5<-arima(var_PIB,order=c(0,0,5))
AIC(MA5)
BIC(MA5)
MA6<-arima(var_PIB,order=c(0,0,6))
AIC(MA6)
BIC(MA6)
MA7<-arima(var_PIB,order=c(0,0,7))
AIC(MA7)
BIC(MA7)
MA8<-arima(var_PIB,order=c(0,0,8))
AIC(MA8)
BIC(MA8)
MA9<-arima(var_PIB,order=c(0,0,9))
AIC(MA9)
BIC(MA9)
ARMA12<- arima(var_PIB, order = c(1,0,2))
AIC(ARMA12)
BIC(ARMA12)
ARMA13<- arima(var_PIB, order = c(1,0,3))
AIC(ARMA13)
BIC(ARMA13)
ARMA14<- arima(var_PIB, order = c(1,0,4))
AIC(ARMA14)
BIC(ARMA14)
ARMA15<- arima(var_PIB, order = c(1,0,5))
AIC(ARMA15)
BIC(ARMA15)
ARMA16<- arima(var_PIB, order = c(1,0,6))
AIC(ARMA16)
BIC(ARMA16)
ARMA17<- arima(var_PIB, order = c(1,0,7))
AIC(ARMA17)
BIC(ARMA17)
ARMA18<- arima(var_PIB, order = c(1,0,8))
AIC(ARMA18)
BIC(ARMA18)
ARMA19<- arima(var_PIB, order = c(1,0,9))
AIC(ARMA19)
BIC(ARMA19)

ARMA21<- arima(var_PIB, order = c(2,0,1))
AIC(ARMA21)
BIC(ARMA21)
ARMA22<- arima(var_PIB, order = c(2,0,2))
AIC(ARMA22)
BIC(ARMA22)
ARMA23<- arima(var_PIB, order = c(2,0,3))
AIC(ARMA23)
BIC(ARMA23)
ARMA24<- arima(var_PIB, order = c(2,0,4))
AIC(ARMA24)
BIC(ARMA24)
ARMA25<- arima(var_PIB, order = c(2,0,5))
AIC(ARMA25)
BIC(ARMA25)
ARMA26<- arima(var_PIB, order = c(2,0,6))
AIC(ARMA26)
BIC(ARMA26)
ARMA27<- arima(var_PIB, order = c(2,0,7))
AIC(ARMA27)
BIC(ARMA27)
ARMA28<- arima(var_PIB, order = c(2,0,8))
AIC(ARMA28)
BIC(ARMA28)
ARMA29<- arima(var_PIB, order = c(2,0,9))
AIC(ARMA29)
BIC(ARMA29)

#Exemplo aplicação múltipla - Extra (Deve-se completar as estimações antes de executar esse código)

estimacoes <- list(AR1, AR2, MA1, MA2, MA3, MA4, MA5, MA6, MA7, MA8, MA9, 
                   ARMA11,ARMA12, ARMA13, ARMA14,ARMA15, ARMA16,ARMA17,ARMA18,ARMA19,
                   ARMA21,ARMA22,ARMA23,ARMA24,ARMA25,ARMA26,ARMA27,ARMA28,ARMA29)      #Cria uma lista com os estimadores
sapply(estimacoes, AIC)                 #Aplica o comando AIC na lista
sapply(estimacoes, BIC)                 #Aplica o comando BIC na lista

#Exemplo de criação de tabela com resultados - Extra
AIC <- sapply(estimacoes, AIC) 
BIC <- sapply(estimacoes, BIC)
Modelo <- c("AR1", "AR2", "MA1", "MA2", "MA3", "MA4", "MA5", "MA6", "MA7", "MA8", "MA9", "ARMA11","ARMA12", "ARMA13", "ARMA14","ARMA15", "ARMA16","ARMA17","ARMA18","ARMA19","ARMA21","ARMA22","ARMA23","ARMA24","ARMA25","ARMA26","ARMA27","ARMA28","ARMA29")

Resultados <- data.frame(Modelo, AIC, BIC)
View(Resultados)
