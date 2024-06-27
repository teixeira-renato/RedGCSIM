#' tabela_final_1
#'
#' Carregar as bases de dados do SIM.
#'
#' @param x, padrao vetor de números.
#' @return Um data frame com os municípios, regionais de saúde, estados e grandes regiões.
#' @examples
#' \dontrun{
#' # Exemplo de uso:
#' dados <- data.frame(out.line2)
#' dados_municipios <- tabela_final_1(dados)
#' }
#' @export

tabela_final_1 = function(x){
  if (!require("pacman")) install.packages("pacman") #garantir que o pacman está instalado
  pacman::p_load(tidyverse,rio) # pacotes necessários

  ###Arquivo com as categoria das causas e os CID
  causas <- RedGCSIM::mumime
  colnames(causas)[1] <- 'CAUSABAS'

  ###Merge Causas GBD
  base <- left_join(x, causas, by='CAUSABAS')

  ##Arquivo com as micros e as mesos regiões
  mumime <- RedGCSIM::mumime
  mumime <- mumime %>%
    select(municode.6, microcode, mesocode)%>%
    mutate(cdmun = as.factor(municode.6))%>%
    select(-municode.6)

  ###Merge Micro, Meso
  base <- left_join(base, mumime, by='cdmun')

  ###Base por idade, sexo e etc.
  base.2 <- base %>%
    group_by(cdmun,microcode, mesocode,  SEXO, idade.cat, ano, CLASS_GPEAS_EXT) %>%
    summarise(ob=n())

  #Marca registro sem idade definida
  base.2$idade.cat[is.na(base.2$idade.cat)] <- 'IGN'

  #Reclassifica o sexo
  base.2$SEXO <- recode(base.2$SEXO, '1'='Masculino', '2'='Feminino', '0'='IGN', '9'='IGN')

  #Renomeia colunas
  b.raiz <- base.2
  colnames(b.raiz) <- c('cdmun','micro', 'meso',  'sexo', 'idade','ano', 'GBD',  'obitos')
  b.raiz <- b.raiz %>%
    mutate(uf=str_sub(cdmun,1,2))

  return(b.raiz)
}
