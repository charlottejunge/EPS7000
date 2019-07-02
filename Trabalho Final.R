library(readr)
library(fpp2)
library(ggplot2)
material <- read_csv("~/Documents/Bachelor/6. Semester/Estatística e Modelos de Previsao/material_construcao.csv")
View(material)
y <- ts(material$value, frequency=4, start=c(1960, 1))
autoplot(y)
autoplot(y) + ylab("Vendas em unidade") + xlab("Ano") + ggtitle("Material de construcao")
ggseasonplot(y, year.labels=TRUE, year.labels.left=TRUE) + ylab("Vendas em unidade") +
  ggtitle("Seasonal plot: Material de construcao")
ggseasonplot(y, polar=TRUE) + ylab("Vendas em unidade") + ggtitle("Seasonal polar plot: Material de construcao")
ggsubseriesplot(y) + ylab("Vendas em unidade") + ggtitle("Subseries plot: Material de construcao")
ggAcf(y) + ggtitle("Autoregressao: Material de construcao")
# dados tem tendecia e sesonalidade
# para treinamento usar modelos: ses, ets, tiw (automatizados)
# fazer previsao com esses modelos
ses <- ses(y)
holt <- holt(y)
autoplot(holt)
holt.damped <- holt(y, damped = TRUE)
holt.winters <- hw(y, h = 2, damped=TRUE, seasonal="additive")
AB <- forecast(holt.winters, hi = 2)
autoplot(AB, PI = FALSE)
autoplot(holt.winters)
help(hw)
holt.winters95 <- hw(y, h = 2 * frequency(x), seasonal = c("multiplicative"),
   damped = FALSE, level = NULL, fan = FALSE,
   initial = c("optimal", "simple"), exponential = FALSE,
   alpha = NULL, beta = NULL, gamma = NULL, phi = NULL,
   lambda = NULL, biasadj = FALSE)
help(autoplot)
autoplot(y, PI=FALSE)
# mean - as previsões são iguais ao valor médio da série temporal.
meanf <- meanf(y)
autoplot(meanf)
# Naive - as previsões são iguais ao último valor da série temporal.
naive <- naive(y)
autoplot(naive)
# Random Walk Forecast
''
rwf <- rwf(y)
autoplot(rwf)
# seasonal naive 
'as previsões para determinada estação são iguais ao valor daquela estação um período completo antes, 
por ex. as previsões para janeiro de 2019 são iguais ao valor das séries temporais em janeiro
de 2018'
snaive <- snaive(y)
autoplot(snaive)
rwf.drift <- rwf(y, drift=TRUE) 
autoplot(rwf.drift)
y.short <- window(y, start=1960, end=1970, frequency=4)
decompose(y)$seasonal
decompose(y)$trend
acf(y)
library(tseries)
adf.test(y)
# tendencia: padrão existe quando há um aumento ou diminuição a longo prazo nos dados
# sazonalidade: Preditores de sazonalidade são variáveis dummy que indicam o período (por exemplo, mês, trimestre) para o qual as previsões são feitas.
'suavização exponencial é um método útil para previsão de séries temporais. A ideia básica é prever valores futuros de séries temporais como média ponderada de observações passadas, 
onde os pesos diminuem exponencialmente com o tempo'
y %>% decompose(type="multiplicative") %>% autoplot() + xlab("Ano") +
ggtitle("Materias de construcao")
library(seasonal)
'Sazonalidade é um padrão regular, repetitivo em dados de séries temporais.
Pode ser de natureza aditiva ou multiplicativa'
y %>% forecast(h=2) %>%
  autoplot() +
  ylab("Vendas em unidade") + xlab("Material de construcao")
ets <- ets(y)
summary(ets)
x <- window(y, start=1960, end=c(1969, 4))
?window
l <- window(y, start=1971, end=c(1972, 4))
y.outliners <- ts(material$value, frequency=4, start=c(1960, 1))
y.outliners[3] <- 422
y.outliners
autoplot(y, series="Dados") + autolayer(ma(y,4), series="4-MA") + xlab("Ano") + ylab("Vendas em unidade") +
  ggtitle("Materiais de construcao, Previsao com média móvel de ordem 4") + scale_colour_manual(values = c("Dados"="grey50","4-MA"="red"), breaks = c("Dados","4-MA"))
x11 <- y %>% seas(x11="")
autoplot(x11)
forecast(y, method = c("arima"), etsmodel = "ZZN", forecastfunction = NULL,
         h = frequency(object$time.series) * 2, level = c(80, 95),
         fan = FALSE, lambda = NULL, biasadj = NULL, xreg = NULL,
         newxreg = NULL, allow.multiplicative.trend = FALSE, ...)
plot(y)
fit <- stl(y, s.window="periodic")
lines(trendcycle(fit),col="red")
library(ggplot2)
autoplot(cbind(
  Data=y,
  Seasonal=seasonal(fit),
  Trend=trendcycle(fit),
  Remainder=remainder(fit)),
  facets=TRUE) +
  ylab("Vendas em unidade") + xlab("Year")
y.regressao <- tslm(y ~ trend + season)
forecast.y.regressao <- forecast(y.regressao)
autoplot(forecast.y.regressao) +
  ggtitle("Previsao de materiais de construcao usando regressao linear") +
  xlab("Ano") + ylab("Vendas em unidade")
linear <- lm( ~ displacement + horsepower + weight , data=auto)