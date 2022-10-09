# Separação de dados de cotas comercializadas
cotas_comercializadas_df <- select(dados_summarise, cotas_comercializadas) / 1000

# Criação da série temporal
cotas_comercializadas_ts <- ts(cotas_comercializadas_df, start = 1997, frequency = 12)
# Definição da janela de análise
cotas_comercializadas_ts <- window(cotas_comercializadas_ts, start=c(2017,1), end=c(2022,7))
# Saneamento da série temporal
cotas_comercializadas_ts <- tsclean(cotas_comercializadas_ts, replace.missing=TRUE)
cotas_comercializadas_ts

# Informações sobre a série
summary(cotas_comercializadas_ts)

# Desvio padrão
desvio_padrao <- sd(cotas_comercializadas_ts)
desvio_padrao

# Média
media <- mean(cotas_comercializadas_ts)
media
  
# Coeficiete de variação
coeficiente_variacao <- desvio_padrao / media * 100
coeficiente_variacao

# Gráfico Boxplot
ccplot <- ggplot(cotas_comercializadas_ts, aes(y=cotas_comercializadas_ts)) + geom_boxplot() + labs(y = "Quantidade de cotas (por mil unidades)", title="Cotas Comercializadas")+theme(axis.text.x=element_text(size=rel(1.5)))+theme(axis.text.y=element_text(size=rel(1.5)))
ggplotly(ccplot)

# Descomposição da série
cotas_comercializadas_ts_components <- decompose(cotas_comercializadas_ts,type = "additive")
autoplot(decompose(cotas_comercializadas_ts, type = "additive"))+
  labs(y="Quantidade de cotas (por mil unidades)", x="Meses") + 
  ggtitle("Decomposição Série Temporal - Cotas Comercializadas")

#
# Análise ARIMA
#

# Separação das séries pré e pós COVID
cotas_comercializadas_pre_ts <- window(cotas_comercializadas_ts, start=c(2017,1), end=c(2019,12))
cotas_comercializadas_pos_ts <- window(cotas_comercializadas_ts, start=c(2020,1), end=c(2022,7))

cotas_comercializadas_pre_ts
cotas_comercializadas_pos_ts

# Cálculo do modelo ARIMA para as duas séries
arima_cc_pre = auto.arima(cotas_comercializadas_pre_ts, trace=T)
arima_cc_pos = auto.arima(cotas_comercializadas_pos_ts, trace=T)

arima_cc_pre
arima_cc_pos

#
# Validação e diagnóstico
#

# 1. Teste de Ljung-Box
# p-value > 0.01, aceitamos H0, resíduos não são correlacionados
Box.test(arima_cc_pre$residuals, lag=20, type="Ljung-Box")
Box.test(arima_cc_pos$residuals, lag=20, type="Ljung-Box")

# 2. Normalidade dos resíduos
# p-valor > 0.01 - aceitamos H0, ou seja, resíduos normais
ks.test(arima_cc_pre$residuals, "pnorm", mean(arima_cc_pre$residuals),
        sd(arima_cc_pre$residuals))
ks.test(arima_cc_pos$residuals, "pnorm", mean(arima_cc_pos$residuals),
        sd(arima_cc_pos$residuals))

# 3. Teste Arch
# Confirmada a não existência de autocorrelação serial e normalidade dos resíduos,
# podemos verificar a estacionariedade de variãncia
# p-valor > 0.01, aceita-se H0, não se rejeita a H0, garante não existência
ArchTest(arima_cc_pre$residuals)
ArchTest(arima_cc_pos$residuals)

# Projeção das 2 séries com os modelos obtidos
arima_cc_pre %>% forecast(h = 72) %>% autoplot(ylab="Quantidade de cotas (por mil unidades)", xlab='Meses', main="Forecast - Cotas Comercializadas (Pré-pandemia)")
arima_cc_pos %>% forecast(h = 41) %>% autoplot(ylab="Quantidade de cotas (por mil unidades)", xlab='Meses', main="Forecast - Cotas Comercializadas (Pandemia)")

# Decomposição das 2 séries
autoplot(decompose(cotas_comercializadas_pre_ts, type = "additive"))+
  labs(y="Quantidade de cotas (por mil unidades)", x="Meses") + 
  ggtitle("Decomposição Série Temporal - Cotas Comercializadas - Pré-pandemia")
autoplot(decompose(cotas_comercializadas_pos_ts, type = "additive"))+
  labs(y="Quantidade de cotas (por mil unidades)", x="Meses") + 
  ggtitle("Decomposição Série Temporal - Cotas Comercializadas - Pandemia")

# Regressão linear da tendência das 2 séries para obter os coeficientes
tslm(cotas_comercializadas_pre_ts ~ trend)
tslm(cotas_comercializadas_pos_ts ~ trend)
