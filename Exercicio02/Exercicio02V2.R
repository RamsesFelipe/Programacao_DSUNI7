setwd("C:\\Users\\posgrad\\Downloads\\Programacao_DSUNI7-master\\Exercicio02")
getwd()

install.packages("openair")
install.packages("lubridate")
library(openair)
library(lubridate)

dataset <- read.csv("database.csv", sep = ";",header = TRUE)
View(dataset)
str(dataset)


#----Realizando a analise do ano de 2017 com as colunas DataPedido e Quantidade
dataset02 <- dataset[,c(12,18)]
names(dataset02) <- c("date", "Quantidade")

str(dataset02)

dataset02$date <- as.Date(dataset02$date)

ValoresAgregados <- aggregate(dataset02$Quantidade, by=list(Category=dataset02$date), FUN=sum)

str(ValoresAgregados)

summary(ValoresAgregados)

names(ValoresAgregados) <- c("date", "Quantidade")


calendarPlot(mydata=ValoresAgregados, pollutant = "Quantidade", year = 2017, month = 1:12,
             main = "Quantidade Comprada diariamente 2017",limits = c(0, 700) )



#----Realizando a analise do ano de 2017 com as colunas DataPedido e Valor

dataset03 <- dataset[,c(12,19)]
names(dataset03) <- c("date", "Valor")

str(dataset03)

dataset03$date <- as.Date(dataset03$date)

ValoresAgregados02 <- aggregate(dataset03$Valor, by=list(Category=dataset03$date), FUN=sum)

str(ValoresAgregados02)

summary(ValoresAgregados02)

names(ValoresAgregados02) <- c("date", "Valor")


calendarPlot(mydata=ValoresAgregados02, pollutant = "Valor", year = 2017, month = 1:12,
             main = "Valor Comprado diariamente 2017",limits = c(100, 4500) )



##testando AnomalyDetection com os valores agregados

install.packages("devtools")
devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)

help(AnomalyDetectionTs)


str(ValoresAgregados)
str(raw_data)
names(ValoresAgregados) <- c("timestamp", "count")

ValoresAgregados$timestamp = as.POSIXlt(ValoresAgregados$timestamp)


res = AnomalyDetectionTs(ValoresAgregados, max_anoms=0.02, direction='both', plot=TRUE, title = "Valores Agregados Quantidade")
res$plot

#AnomalyDetectionVec(ValoresAgregados[,2], max_anoms=0.02, period=12, direction='both', only_last=FALSE, plot=TRUE)


names(ValoresAgregados02) <- c("timestamp", "Valor")

ValoresAgregados02$timestamp = as.POSIXlt(ValoresAgregados02$timestamp)


res = AnomalyDetectionTs(ValoresAgregados02, max_anoms=0.02, direction='both', plot=TRUE, title = "Valores Agregados02 Valor")
res$plot

#AnomalyDetectionVec(ValoresAgregados02[,2], max_anoms=0.02, period=12, direction='both', only_last=FALSE, plot=TRUE)


#---------------------Mais algumas analises
packages =  c("ggplot2", "dplyr", "tidyr", "data.table", 'corrplot', 'gridExtra', 'forecast', 'tseries', 'TSA', 'tibble', 'TTR', 'xts', 'dygraphs', 'assertthat')

my.install <- function(pkg, ...){
  if (!(pkg %in% installed.packages()[,1])) {
    install.packages(pkg)
  }
  return (library(pkg, ...))
}

purrr::walk(packages, my.install, character.only = TRUE, warn.conflicts = FALSE)  


sample_num = 5


s_data  <- dataset[,c(12,18,19,4)]
summary(s_data)
str(s_data)

s_data[is.na(s_data)] <- 0
s_data$DataPedido <- as.Date(s_data$DataPedido, format = "%Y-%m-%d")
summary(s_data)

str(s_data)

options(repr.plot.width=12, repr.plot.height=12) 

p1 = ggplot(s_data, aes(Quantidade)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# + xlim(c(0, 1000))

p2 = ggplot(s_data, aes(Valor)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# + xlim(c(0, 1000))

grid.arrange(p1,p2, nrow=1,ncol=2)

tmp <- s_data
length(unique(tmp$Estado))

sample_ticker <- as.character(sample(tmp$Estado, sample_num))
#sample_ticker <- c(sample_ticker, 'GOOGL') 
candidate_ticker <- unique(sample_ticker)
candidate_ticker <- c("RJ", "SP")
candidate_num <- length(candidate_ticker)
stock_list <- vector(mode="list", length=candidate_num)
names(stock_list) <- candidate_ticker
i = 1
for (ticker in candidate_ticker){
  stock_list[[i]] <- filter(s_data, Estado == ticker)
  # print(stock_list[[i]])
  i <- i+1
  # print(ticker)
}
str(stock_list)

xts_list <- vector(mode="list", length=candidate_num)
ts_list <- vector(mode="list", length=candidate_num)

names(xts_list) = candidate_ticker
names(ts_list) = candidate_ticker

for (ticker in candidate_ticker){
  stock = stock_list[[ticker]]
  xts = xts(stock$Quantidade, order.by=stock$DataPedido)
  attr(xts, 'frequency') <- length(xts)/12
  ts = as.ts(xts, start = c(2017))
  xts_list[[ticker]] <- xts
  ts_list[[ticker]] <- ts
}
xts_table= do.call(cbind, xts_list)
dygraph(xts_table, xlab = "DataPedido", ylab = "Quantidade", main = "Time Series") %>%
  # dySeries(labels.default()) %>%
  # dyOptions(colors = c("red")) %>%
  dyRangeSelector()

require(plyr) 

xts1 = xts_list[['SP']]
xts2 = xts_list[['RJ']]
xts = rbind(xts1,xts2)
ts = ts_list
adf.test(xts, alternative = "stationary", k = 0)


tscomponents_add <- decompose(xts, type = "additive")
tscomponents_mul <- decompose(xts, type = "multiplicative")

#graficos interessantes
plot(tscomponents_add, col = "red")

plot(tscomponents_mul, col = "blue")


xtsdiff1 <- diff(xts, differences=1)
#tsdiff1 <- diff(ts, differences=1)
plot.xts(xtsdiff1, col = "blue")



adf.test(tsdiff1, alternative = "stationary", k = 0)

findfrequency(xts) 

findfrequency(xtsdiff1)

Acf(xtsdiff1, lag.max=60)

Pacf(xtsdiff1, lag.max=60)

tsarima30 <- auto.arima(head(xts, -30), max.p = 3, max.q = 3, max.d = 3) # excluding last 30 time series as test data
print(tsarima30)

tsforecasts30 <- forecast(tsarima30, h = 500) # forecast the next 30 time series
autoplot(tsforecasts30)

accuracy(tsforecasts30, head(tail(xts, 30), 30))

ggplot(data.frame(residuals = tsforecasts30$residuals), aes(residuals)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# make a histogram

checkresiduals(tsforecasts30)



#
# ValoresAgregados = qtd
# ValoresAgregados02 = valor
# BreakoutDetection
install.packages("devtools")
devtools::install_github("twitter/BreakoutDetection")
library(BreakoutDetection)
?breakout

names(ValoresAgregados) <- c("timestamp", "count")
data(ValoresAgregados)
res = breakout(ValoresAgregados, min.size=15, method='multi', beta=.001, degree=1, plot=TRUE)
res$plot

names(ValoresAgregados02) <- c("timestamp", "count")
data(ValoresAgregados02)
res = breakout(ValoresAgregados02, min.size=15, method='multi', beta=.001, degree=1, plot=TRUE)
res$plot

str(ValoresAgregados02)


#alguns creditos -> https://www.kaggle.com/szrlee/time-series-analysis-arima-basic-model



