#' prepara_base_generalizada
#'
#' Carregar as bases de dados do SIM.
#'
#' @param base_dados_completos,dados_municipios,dados_pop, padrao vetor de números.
#'
#' @export

prepara_base_generalizada = function(base_dados_completos){
  `%notin%` <- Negate(`%in%`)
  if (!require("pacman")) install.packages("pacman") #garantir que o pacman está instalado
  pacman::p_load(tidyverse,rio) # pacotes necessários

  causas <- c( "Injuries - Falls"    , "_pneumo",
               "Injuries - Homicide" , "Injuries - Others"  ,
               "Injuries - Road"     , "Injuries - Suicide" ,
               "other_causes_all","other_causes-lri",
               "other_desnutricao_all_ages","Injuries - Other transport injuries",
               "materna_ectopica"    , "materna_hipertensiva",
               "materna_trab_parto"  , "materna_aborto_induzido",
               "materna_tardia"      , "materna_aborto_espontaneo",
               "materna_sepsis"      , "materna_indiretas",
               "materna_outras"      , "materna_hemorragia",
               "trans_dengue"        , "materna_materna_hiv",
               "trans_encefatlite"   , "trans_schistosomiasis",
               "trans_chagas"        , "trans_tuberculose",
               "trans_hiv_aids"      , "trans_doenças_diarreicas",
               "trans_varicela"      , "trans_leishmaniose",
               "trans_zoonoticas"    , "trans_hepatite",
               "trans_meningites"    , "trans_sexualmente_transmissíveis",
               "trans_desnutricao"   , "trans_febre_amarela",
               "trans_infec_urinaria", "trans_malaria",
               "dcnt_neoplasms"      , "dcnt_chronic respiratory",
               "dcnt_diabetes"       , "dcnt_cardiovascular",
               "anom_congenitas"     , "aspiracao_pulmunar",
               "infant_lri_post_neo" , "infant_neonatal_encefalopatia",
               "infant_subita"       , "infant_neonatal_hemolitica",
               "obst_intestinal"     , "infant_neonatal_prematuridade",
               "infant_neonatal_other","infant_neonatal_sepsis")

  #Codigo de municipio

  mumime <- RedGCSIM::mumime
  mumime <- mumime %>%
    select(municode.6, microcode, mesocode)
  colnames(mumime) <- c('cdmun','micro', 'meso')
  mumime$cdmun <- as.character(mumime$cdmun)

  cdmun <- unique(as.character(base_dados_completos$cdmun))

  notwantedlevels <- c(110000,120000,130000,140000, 150000, 160000, 170000, 210000, 220000,
                       230000,240000,250000,260000,270000,280000,290000, 310000,320000,330000,
                       350000, 410000,420000,430000,500000,510000,520000,000000)

  cdmun <- cdmun[cdmun %notin% notwantedlevels]

  ####base Pop
  pop <- RedGCSIM::pop
  pop <- pop %>%
    filter(Ano%in%base_dados_completos$ano)

  colnames(pop) <- c('ano', 'uf', 'sexo', 'cdmun.7', 'nome', as.character(seq(0,90,5)), 'total')

  head(pop)
  cols <- variable.names(pop[,6:25])
  pop <- pop %>%
    mutate_at(all_of(cols), ~ str_replace(., ",", "."))


  pop[cols] <- as.data.frame(lapply(pop[cols],  function(y) as.numeric(y)))
  pop.2 <- pop %>%
    select(-total, -nome, -uf) %>%
    pivot_longer(!(ano:cdmun.7), names_to='idade', values_to='pop') %>%
    mutate(cdmun=str_sub(cdmun.7,1,6),
           uf=str_sub(cdmun.7,1,2)) %>%
    select(-cdmun.7)

  rm(pop)

  pop.2$sexo <- recode(pop.2$sexo, 'f'='Feminino', 'm'='Masculino')
  pop.2$idade <- as.character(pop.2$idade)
  pop.2$ano <- as.character(pop.2$ano)

  pop.2$idade <- as.character(pop.2$idade)

  #####Base todos
  causa <- unique(base_dados_completos$GBD)
  sexo <- unique(base_dados_completos$sexo)
  anos <- unique(base_dados_completos$ano)
  # age <- as.character(c(seq(0,90,5)))
  age <- c(seq(0,90,5), "Early Neonatal", "Post Neonatal", "Late Neonatal")
  mat <-c(causas[grepl("^materna",causas)])
  infant <-c(causas[grepl("^infant",causas)])
  base.1 <- expand.grid(cdmun,anos,age,sexo,causa)
  colnames(base.1) <- c('cdmun','ano','idade','sexo', 'GBD')

  #Limpeza de inconsistencias
  base.1 <- base.1%>%
    mutate(to_exclude= case_when(
      GBD %in% mat & idade %in% c("Early Neonatal","Post Neonatal","Late Neonatal","<1 year","0","5","60","65","70","75","80","85","90") ~ 1,
      GBD %in% mat & sexo == "Masculino"  ~ 1,
      # GBD %in% infant & idade %notin% c("Early Neonatal","Post Neonatal","Late Neonatal","<1 year") ~ 1,
      TRUE ~ 0
    ))%>%
    filter(to_exclude==0)%>%
    select(-to_exclude)

  base.1 <- left_join(base.1,mumime, by='cdmun')

  base.1$idade <- as.character(base.1$idade)
  base_dados_completos$idade <- as.character(base_dados_completos$idade)

  #Junta base real com a de todas as possibilidades
  base.2 <- left_join(base.1, base_dados_completos[1:8], by=c(colnames(base_dados_completos)[1:7]))
  print(c(colnames(base_dados_completos)[1:7]))
  base.2 <- base.2 %>%
    mutate(uf=str_sub(cdmun,1,2))

  #Agrega dados populacionais

  base.2 <- left_join(base.2,pop.2, by=c('cdmun','uf', 'ano', 'sexo', 'idade'))

  base.2$obitos[is.na(base.2$obitos)] <- 0

  if(sum(base.2$obitos, na.rm = T) == sum(base_dados_completos$obitos, na.rm = T)) print("manteve o mesmo total de óbitos")


  return(base.2)
}
