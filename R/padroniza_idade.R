#' Padroniza Idade
#'
#' Esta função padroniza as idades no formato padrão do SIM em faixas etárias quinquenais.
#'
#' @param x Um data frame contendo dados a serem padronizados.
#' @return Um data frame com idades padronizadas.
#' @examples
#' \dontrun{
#' # Exemplo de uso:
#' dados <- data.frame(IDADE = c("001", "002", "090", NA))
#' dados_padronizados <- padroniza_idade(dados)
#' }
#' @export

padroniza_idade = function(x){
  if (!require("pacman")) install.packages("pacman") #garantir que o pacman está instalado
  pacman::p_load(tidyverse,rio) # pacotes necessários

  ###Função para criar os grupos de idades
  age.cat = function(age){
    result = 5*(age %/% 5)
    nove = age>=90
    na =(is.na(age))
    result[nove] = 90
    result[na]= NA
    return(result)
  }
  #Base
  #arquivo com o código das idades no SIM
  cod.idades <- RedGCSIM::cod.idades
  cod.idades.2 <- cod.idades[,1:2]

  cod.idades.2 <- cod.idades.2 %>%
    mutate(IDADE=str_pad(IDADE,3,"left",pad="0"))


  rm(cod.idades)

  ###Merge Códigos de Idade

  out.file <- left_join(x, cod.idades.2, by='IDADE') # Categoriza todas as idade do SIM em faixas etárias

  ###Tratamento da Variável Idade
  out.file<- out.file %>%
    mutate(age=IDADE_ANOS)%>%
    mutate(age= recode(age, 'MENOR 1 ANO IGN'='Post Neonatal', '0 A 6 DIAS'='Early Neonatal', '7 A 27 DIAS'= 'Late Neonatal','28 A 364 DIAS'='Post Neonatal', '1 ANO'='1 ANOS'))%>%
    mutate(idade.cat=ifelse(grepl("^\\d{1}",perl = T,.$age),
                            age.cat(as.numeric(str_sub(age, end = -6))),.$age))%>%
    mutate(idade.cat=ifelse(idade.cat ==999|is.na(idade.cat),'IGN',idade.cat))

   return(out.file)
}
