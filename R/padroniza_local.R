#' padroniza_local
#'
#' Carregar as bases de dados do SIM.
#'
#' @param x, padrao vetor de números.
#'
#' @export


padroniza_local = function(x){
  ###Criação da Variável ano e cdmun com 6 dígitos
  out.file <- x %>% 
    mutate(ano=str_sub(DTOBITO,5,8),
           cdmun=str_sub(CODMUNRES,end=6))
  
  print(table(out.file$ano, exclude = NULL)) # Total de registros no SIM
  
  base <- out.file %>% 
    select(cdmun, SEXO, CAUSABAS, age, idade.cat, ano)
  
  return(base)
}