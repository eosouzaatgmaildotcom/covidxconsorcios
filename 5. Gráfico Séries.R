#
# Gráficos das séries de cotas ativas e comercializadas juntos
#

# Gráfico cotas comercializadas
plot.ts( cotas_comercializadas_ts
       , plot.type = c('multiple')
       , main='Mercado de Consórcios'
       , xlab = "Meses"
       , ylab = "Quantidade de cotas (por mil unidades)"
       , xy.labels = False
       , col = 'gray'
)

# Marcação do início COVID
abline( v   = c(2020)
      , col = c("red")
)
# Legenda da marcação
text( c(2020)-0.4
    , c(350)
    , labels = c("COVID-19")
    , cex    = .8
    , col    = c("red")
)

# Legenda do gráfico
legend("topleft", legend = c('Comercializadas','Ativas'), col = c('gray','blue'), lty = 1, cex = 0.8)

# Inclusão do gráfico cotas ativas
par(new=TRUE)
plot.ts(cotas_ativas_ts, main='', xlab='', ylab='', axes=F, col="blue")
par(new=FALSE)

# Eixo Y secundário para as cotas ativas
axis(4, at = seq(7000, 8500, by = 500))
