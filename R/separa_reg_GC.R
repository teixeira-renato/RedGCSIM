#' separa_reg_GC
#'
#' Carregar as bases de dados do SIM.
#'
#' @param dados_sem_ign,dados_municipios, padrao vetor de números.
#'
#' @export

separa_reg_GC = function (dados_sem_ign, dados_municipios){
  if (!require("pacman")) install.packages("pacman") #garantir que o pacman está instalado
  pacman::p_load(tidyverse,rio) # pacotes necessários
  
  # Definição de causas targets e códigos garbage para redistribuir
  
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
               "lri_post_neo" , "infant_neonatal_encefalopatia",
               "infant_subita"       , "infant_neonatal_hemolitica",
               "obst_intestinal"     , "infant_neonatal_prematuridade",
               "infant_neonatal_other","infant_neonatal_sepsis")
  
  redis <- c("_injuries" , "_inj (hom,suic, fall,road)", "_all", "_inj (hom,suic,other)" ,  
             "_pneumo","_inj (hom,sui)" , "_inj (hom,sui,transp)","_maternas", "_x59", "_y34","_infant_neonat")
  
  `%notin%` <- Negate(`%in%`)
  
  #Codigo de municipio
  
  mumime <- import( dados_municipios ,encoding = 'Latin-1')
  mumime <- mumime %>% 
    select(municode.6, microcode, mesocode)
  colnames(mumime) <- c('cdmun','micro', 'meso')
  mumime$cdmun <- as.character(mumime$cdmun)
  
  cdmun <- unique(as.character(dados_sem_ign$cdmun))
  
  notwantedlevels <- c(110000,120000,130000,140000, 150000, 160000, 170000, 210000, 220000, 
                       230000,240000,250000,260000,270000,280000,290000, 310000,320000,330000,
                       350000, 410000,420000,430000,500000,510000,520000,000000)
  
  cdmun <- cdmun[cdmun %notin% notwantedlevels]
  
  #Base com o GC
  base.r <- dados_sem_ign  %>% 
    filter(GBD %in% redis) %>% 
    select(cdmun:meso, obitos.2, uf)
  colnames(base.r)[8] <- 'redis'
  colnames(base.r)[5] <- 'c.red'
  
  # Montagem da Base SEM os GC ----
  
  base.4 <- dados_sem_ign %>% 
    filter(GBD %in% causas) %>% 
    select(cdmun:meso, obitos.2, uf)
  
  ####Base cheia, incluindo os casos sem registro, sem os Garbage
  mat <-c(causas[grepl("^materna",causas)])
  infant <-c(causas[grepl("^infant",causas)])
  sexo <- unique(dados_sem_ign$sexo)
  anos <- unique(dados_sem_ign$ano)
  age <- c(seq(0,90,5), "Early Neonatal", "Post Neonatal", "Late Neonatal")
  
  base.1 <- expand.grid(cdmun,anos,age,sexo,causas)
  colnames(base.1) <- c('cdmun','ano','idade','sexo', 'GBD')
  
  base.1 <- base.1%>%
    mutate(to_exclude= case_when(
      GBD %in% mat & idade %in% c("Early Neonatal","Post Neonatal","Late Neonatal","<1 year","0","5","60","65","70","75","80","85","90") ~ 1,
      GBD %in% mat & sexo == "Masculino"  ~ 1,
      GBD %in% mat & idade %in% c("10","15","20","25","30","35","40","45","50","55") ~ 0,
      # GBD %in% infant & idade %notin% c("Early Neonatal","Post Neonatal","Late Neonatal","<1 year") ~ 1,
      TRUE ~ 0
    ))%>%
    filter(to_exclude==0)%>%
    select(-to_exclude)
  
  base.1 <- base.1%>%
    mutate(to_exclude= case_when(
      GBD %in% mat & idade %in% c("Early Neonatal","Post Neonatal","Late Neonatal","<1 year","0","5","60","65","70","75","80","85","90") ~ 1,
      GBD %in% mat & sexo == "Masculino"  ~ 1,
      GBD %in% mat & idade %in% c("10","15","20","25","30","35","40","45","50","55") ~ 0,
      # GBD %in% infant & idade %notin% c("Early Neonatal","Post Neonatal","Late Neonatal","<1 year") ~ 1,
      TRUE ~ 0
    ))%>%
    filter(to_exclude==0)%>%
    select(-to_exclude)
  
  base.5 <- left_join(base.1,mumime, by='cdmun')
  
  base.5 <- base.5 %>% 
    mutate(uf=str_sub(cdmun,1,2),
           reg=str_sub(cdmun,1,1))
  
  ###Merge da base limpa com a base de óbitos, considerando as causas sem GB
  
  base.5 <-   left_join(base.5, base.4, by=c('cdmun','micro','meso','idade','GBD', 'sexo', 'ano', 'uf'))
  
  out.file <- list(redistribuir=base.r,completos=base.5)
  
  return(out.file)
}