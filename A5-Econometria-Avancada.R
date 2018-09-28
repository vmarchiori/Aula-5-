# Econometria Avançada A5

#Revisão de alguns comando + Criação de Série Temporal 

install.packages("readxl") #instalando o pacote readxl
library(readxl) #rodando o pacote readxl
install.packages("urca") #instalando o pacote urca
library(urca) #rodando o pacote urca
IPCA <- read_excel("C:/Econometria/IPCA.xls", col_types = c("date","numeric")) #Abre o arquivo excel IPCA no RSTUDIO
Inflação <- ts(IPCA$IPCA,start = 2008-01, frequency = 12) #constrói uma série temporal com os dados da Inflação
View(Inflação) #Visualizar
write.csv(Inflação,file = "Inflação.csv")

# Comando para realizar o Teste de Estacionariedade

TesteDF <- summary(ur.df(Inflação, type="none", lags=0))
TesteDF

# Criando o Gráfico de Autocorrelação

acf(IPCA$IPCA, main="Inflação Mensal") #FAC função de auto correlação
pacf(IPCA$IPCA, main="Inflação Mensal") #FACP função de auto correlação parcial

# Modelo Autoregressivo

AR1 <- arima(Inflação,order = c(1,0,0)) #modelo AR de ordem 1
AR1
AR2 <- arima(Inflação, order=c(2,0,0)) #modelo AR de ordem 2
AR2

#Salvar CNVAZQUEZ
