# Carga da base de dados disponibilizada pelo Banco Central
#
# URL da base de dados : https://www.bcb.gov.br/estabilidadefinanceira/consorciobd
#
dados_bc <- carrega_csvs("Base de Dados", "*Segmentos_Consolidados.csv")

# Sumarização dos dados carregados, baseada no mês / ano da informação
dados_summarise <- dados_bc %>%
                   group_by(Data_base) %>%
                   summarise( cotas_comercializadas = sum(Quantidade_de_cotas_comercializadas_no_mês)
                            , cotas_ativas          = sum(Quantidade_de_cotas_ativas_em_dia+Quantidade_de_cotas_ativas_contempladas_inadimplentes+Quantidade_de_cotas_ativas_não_contempladas_inadimplentes)
                   )
