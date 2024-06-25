#' tabela_final_1
#'
#' Carregar as bases de dados do SIM.
#'
#' @param x,arq_ICD_map,arq_regioes, padrao vetor de números.
#'
#' @export

tabela_final_1 = function(x,arq_ICD_map,arq_regioes){
  if (!require("pacman")) install.packages("pacman") #garantir que o pacman está instalado
  pacman::p_load(tidyverse,rio) # pacotes necessários
  
  ###Arquivo com as categoria das causas e os CID
  causas <- import(arq_ICD_map, sheet = 1, colClasses="character")
  colnames(causas)[1] <- 'CAUSABAS'
  
  ###Merge Causas GBD 
  base <- left_join(x, causas, by='CAUSABAS')
  
  ##Arquivo com as micros e as mesos regiões
  mumime <- import(arq_regioes, 
                   locale = locale(encoding = "WINDOWS-1252"))
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