pre_proc_data <- function(data = X, tipo = 'graph', datelim = '2020-06-27'){
  datelim <- as.POSIXct(datelim)
  a_day <- 60*60*24
  data_corte <-  trunc(datelim,"days")+ a_day #as.POSIXct(datelim)
  
  colunas <- colnames(srag)
  colnames(X) <- colunas
  
  X <- X[which(X$Tipo =="Estado" & X$sexo =="Total" & X$escala =="casos"), ]
  colnames(X)[8] <- "ano"
  colnames(X)[9] <- "semana"
  colnames(X)[12] <- "total"
  X$semana <- paste("", formatC(X$semana, width = 2, flag="0"))
  X$datadia <- as.POSIXct(as.Date(paste(X$ano, X$semana, 7, sep = '-'), "%Y-%U-%u"))
  X <- X[X$datadia < data_corte, ]
  
  X_casos <- X[which(X$dado =="srag"), ]
  X_obitos <- X[which(X$dado =="obito"), ]
  
  if(tipo == 'graph'){
    filtro <- c('datadia', "Unidade da Federação", 'total')
  }
  else{
    filtro <- c('ano', 'semana', "Unidade da Federação",
                'total', 'SARS-CoV-2')
  }
  
  dt_casos <- X_casos[filtro]
  dt_obitos <- X_obitos[filtro]
  
  if(tipo == 'graph'){
    lista_serie_casos <- pre_proc_merge(dt_casos, tipo)
    lista_serie_obitos <- pre_proc_merge(dt_obitos, tipo)
    
    serie_total = list("cases" = lista_serie_casos, "deaths" = lista_serie_obitos)
  }
  else{
    lista_serie_casos <- pre_proc_merge(dt_casos, tipo)
    lista_serie_obitos <- pre_proc_merge(dt_obitos, tipo)
    
    serie_total = list("cases" = lista_serie_casos, "deaths" = lista_serie_obitos)
  }
  return (serie_total)
}






#GRAPH
library('readr')
source("./Func_/new_preproc.R")
srag <- read_csv2("./Aux_arqs/dados_semanais_faixa_etaria_sexo_virus-02-07-21.csv")#, sep = ';')

datelim <- "02/07/2021"
datelim <- as.POSIXct(datelim)
a_day <- 60*60*24
data_corte <-  trunc(datelim,"days")+ a_day #as.POSIXct(datelim)

X <- srag
colunas <- colnames(srag)
colnames(X) <- colunas

X <- X[which(X$Tipo =="Estado" & X$sexo =="Total" & X$escala =="casos"), ]

colnames(X)[8] <- "ano"
colnames(X)[9] <- "semana"

X$UF <- NULL
X$Tipo <- NULL
X$`Ano e semana epidemiológica` <- NULL
X$`Situação do dado` <- NULL
X$sexo <- NULL
X$escala <- NULL

X$semana <- paste("", formatC(X$semana, width = 2, flag="0"))
X$datadia <- as.POSIXct(as.Date(paste(X$ano, X$semana, 7, sep = '-'), "%Y-%U-%u"))

colunas_criancas <- c("< 2 anos", "2-4 anos", "5-9 anos")
X[, colunas_criancas] <- sapply(colunas_criancas, as.numeric)
X$total <- rowSums(X[ , colunas_criancas])#colunas_criancas])
#X <- X[X$datadia <= data_corte, ]

X_casos <- X[which(X$dado =="srag"), ]
X_obitos <- X[which(X$dado =="obito"), ]
#FILTROGRAFICO
filtro <- c('datadia', "Unidade da Federação", 'total')

dt_casos <- X_casos[filtro]
dt_obitos <- X_obitos[filtro]

lista_serie_casos <- pre_proc_merge(dt_casos, tipo)
lista_serie_obitos <- pre_proc_merge(dt_obitos, tipo)

serie_total = list("cases" = lista_serie_casos, "deaths" = lista_serie_obitos)





