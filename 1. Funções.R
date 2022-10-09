#
# Funções para facilitar o tratamento dos arquivos do Banco Central
#

# Carga de dados de um arquivo .CSV
load_df <- function(csv) {
  #print(csv)
  return( read.csv( file=csv
                    , dec=','
                    , sep=";"
                    , encoding="iso-8859-1"
                    , fileEncoding="latin1"
  ) )
}

# Processo para carga de todos os arquivos presentes na pasta (base de dados baixadas do Banco Central)
carrega_csvs <- function(pasta_base, sufixo_csv) {
  for ( pasta in list.files( path       = pasta_base
                             , pattern    = "*"
                             , full.names = TRUE
  ) ) {
    for ( csv in list.files( path       = pasta
                             , pattern    = sufixo_csv
                             , full.names = TRUE
    ) ) {
      if (!exists("df_csv")) {
        df_csv <- load_df(csv)
      }
      else {
        df_csv <- load_df(csv) %>%
          bind_rows(df_csv)
      }
    }
  }
  return(df_csv)
}
