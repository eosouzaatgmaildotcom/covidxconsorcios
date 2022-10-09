# Lista de pacotes utilizados
pacotes <- c("tidyverse","dplyr","zoo","TTR","itsmr","forecast","FinTS", "plotly", "bayesforecast", "knitr", "changepoint")

# Carga dos pacotes necessários
if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}
