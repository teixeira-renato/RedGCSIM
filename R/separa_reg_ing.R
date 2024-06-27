#' separa_reg_ing
#'
#' Carregar as bases de dados do SIM.
#'
#' @param x, padrao vetor de números.
#'
#' @export

separa_reg_ign = function(x){
  if (!require("pacman")) install.packages("pacman") #garantir que o pacman está instalado
  pacman::p_load(tidyverse) # pacotes necessári
  `%notin%` <- Negate(`%in%`)

  #Códigos para municípios ignorados
  notwantedlevels <- c(110000,120000,130000,140000, 150000, 160000, 170000, 210000, 220000,
                       230000,240000,250000,260000,270000,280000,290000, 310000,320000,330000,
                       350000, 410000,420000,430000,500000,510000,520000,000000)

  ##### Dados SIM IGNORADOS

  ign <- x%>%
    ungroup() %>%
    mutate(idade=ifelse(idade ==999,'IGN',idade))%>%
    filter(cdmun %in% notwantedlevels | sexo=='IGN' | idade =='IGN')
  colnames(ign)[8] <- 'ign'

  print(paste0("total de registros com campos em branco: ",sum(ign$ign))) # total de registros com campos ignorados

  ##### Dados SIM completos
  dados_completos <- x %>%
    filter(cdmun %notin% notwantedlevels & sexo!='IGN' & idade!=999 & idade!='IGN')

  if ((sum(nrow(dados_completos),nrow(ign)))==nrow(x)) print("Todos os registros estão separados nas bases de ignorados ou dados completos")

  out.file <- list(ign,dados_completos)

  return(ign)
}
