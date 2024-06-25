#' redistribuicao_causas_externas
#'
#' Carregar as bases de dados do SIM.
#'
#' @param dados_completos,dados_redis, padrao vetor de números.
#'
#' @export


redistribuicao_causas_externas = function (dados_completos,dados_redis){
  if (!require("pacman")) install.packages("pacman") #garantir que o pacman está instalado
  pacman::p_load(tidyverse) # pacotes necessários
  
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
  
  ##### Injuries  -----
  
  inj <-c(causas[grepl("^Injuries",causas)])
  
  base.5 <- dados_completos %>% 
    mutate(c.red=ifelse(GBD %in% inj, '_injuries', NA)) %>%  
    left_join(dados_redis, by=c('cdmun','micro','meso',  'ano', 'sexo','idade', 'uf', 'c.red'))
  ###Proporções INJ
  
  ###PRMUN
  muni.inj <- base.5 %>% 
    filter(GBD %in% inj) %>% 
    group_by(cdmun,micro,meso, GBD,idade, ano, sexo, uf) %>% 
    summarise(ob=sum(obitos.2, na.rm = T))%>% 
    ungroup() %>% 
    group_by(cdmun,micro,meso,idade, ano, sexo, uf) %>% 
    mutate(pr.mu=ob/sum(ob),
           ob.mu=sum(ob)) %>% 
    select(-ob)
  
  ###PR.MICRO 
  micro.inj <- base.5 %>% 
    filter(GBD %in% inj) %>% 
    group_by(micro,meso, GBD,idade, ano, sexo, uf) %>% 
    summarise(ob=sum(obitos.2, na.rm = T))%>% 
    ungroup() %>% 
    group_by(micro,meso,idade, ano, sexo, uf) %>% 
    mutate(pr.mi=ob/sum(ob),
           ob.mi=sum(ob)) %>% 
    select(-ob)
  
  ###PR.MESO 
  meso.inj <- base.5 %>% 
    filter(GBD %in% inj) %>% 
    group_by(meso, GBD,idade, ano, sexo, uf) %>% 
    summarise(ob=sum(obitos.2, na.rm = T))%>% 
    ungroup() %>% 
    group_by(meso,idade, ano, sexo, uf) %>% 
    mutate(pr.me=ob/sum(ob),
           ob.me=sum(ob)) %>% 
    select(-ob)
  
  ###PR.UF
  uf.inj <- base.5 %>%
    filter(GBD %in% inj) %>% 
    group_by( GBD,idade, ano, sexo, uf) %>% 
    summarise(ob=sum(obitos.2, na.rm = T))%>% 
    ungroup() %>% 
    group_by(idade, ano, sexo, uf) %>% 
    mutate(pr.uf=ob/sum(ob),
           ob.uf=sum(ob)) %>% 
    select(-ob)
  
  ###PR.REG
  reg.inj <- base.5 %>%
    filter(GBD %in% inj) %>% 
    mutate(reg=str_sub(cdmun,1,1)) %>% 
    group_by( GBD,idade, ano, sexo, reg) %>% 
    summarise(ob=sum(obitos.2, na.rm = T))%>% 
    ungroup() %>% 
    group_by(idade, ano, sexo, reg) %>% 
    mutate(pr.rg=ob/sum(ob),
           ob.rg=sum(ob)) %>% 
    select(-ob)
  
  
  base.5 <- base.5 %>% 
    left_join(muni.inj, by=c('cdmun','micro','meso', 'GBD','idade', 'ano', 'sexo', 'uf')) %>% 
    left_join(micro.inj, by=c('micro','meso', 'GBD','idade', 'ano', 'sexo', 'uf')) %>% 
    left_join(meso.inj, by=c('meso', 'GBD','idade', 'ano', 'sexo', 'uf')) %>% 
    left_join(uf.inj, by=c( 'GBD','idade', 'ano', 'sexo', 'uf')) %>% 
    left_join(reg.inj, by=c( 'GBD','idade', 'ano', 'sexo', 'reg')) 
  
  base.5 <- base.5 %>% 
    mutate(inj.1=redis*pr.mu,
           redis.2=ifelse(is.na(inj.1) & ob.mu==0, redis,NA),
           inj.2=redis.2*pr.mi, 
           redis.3=ifelse(is.na(inj.2) & ob.mi==0, redis.2,NA),
           inj.3=redis.3*pr.me, 
           redis.4=ifelse(is.na(inj.3) & ob.me==0, redis.3,NA),
           inj.4=redis.4*pr.uf,
           redis.5=ifelse(is.na(inj.4) & ob.uf==0, redis.4,NA),
           inj.5=redis.5*pr.rg,
           obitos.3= ifelse(!is.na(inj.1), obitos.2+inj.1, 
                            ifelse(!is.na(inj.2), obitos.2+inj.2,
                                   ifelse(!is.na(inj.3), obitos.2+inj.3,
                                          ifelse(!is.na(inj.4), obitos.2+inj.4,
                                                 ifelse(!is.na(inj.5), obitos.2+inj.5, obitos.2))))))
  
  #Validação
  
  # obitos_para_redis <- sum(base.r[grepl("_injuries",base.r$c.red),]$redis, na.rm = T)
  # obitos_pre_redis <- sum(base.5$obitos.2,na.rm = T)
  # round(sum(base.5$obitos.3,na.rm = T))==round(sum(obitos_para_redis,obitos_pre_redis))
  # round(sum(base.5$obitos.3,na.rm = T))-round(sum(obitos_para_redis,obitos_pre_redis))
  
  rm(inj,muni.inj,micro.inj,meso.inj,uf.inj,reg.inj)
  
  ############_Injuries-hom-sui ----
  
  inj.hom.sui <-c("Injuries - Homicide", "Injuries - Suicide" )
  
  base.5 <- base.5 %>% 
    select(!(pr.mu:ob.rg),-redis,-redis.2, -redis.3,-redis.4, -redis.5,-c.red) %>% 
    mutate(c.red=ifelse(GBD %in% inj.hom.sui, '_inj (hom,sui)',NA))%>%
    mutate(c.red=ifelse(GBD %in% "Injuries - Suicide" & idade %in% c("Early Neonatal","Post Neonatal","Late Neonatal","<1 year","0"),NA,c.red))%>% 
    left_join(dados_redis, by=c('cdmun','micro','meso',  'ano', 'sexo','idade', 'uf', 'c.red'))
  
  ###Proporções _Injuries-hom-sui
  
  ###PRMUN
  mu.inj.h.s <- base.5 %>% 
    filter(c.red == '_inj (hom,sui)') %>% 
    group_by(cdmun,micro,meso, GBD,idade, ano, sexo, uf) %>% 
    summarise(ob=sum(obitos.3, na.rm = T))%>% 
    ungroup() %>% 
    group_by(cdmun,micro,meso,idade, ano, sexo, uf) %>% 
    mutate(pr.mu=ob/sum(ob),
           ob.mu=sum(ob)) %>% 
    select(-ob)
  
  ###PR.MICRO 
  mi.inj.h.s <- base.5 %>% 
    filter(c.red == '_inj (hom,sui)') %>% 
    group_by(micro,meso, GBD,idade, ano, sexo, uf) %>% 
    summarise(ob=sum(obitos.3, na.rm = T))%>% 
    ungroup() %>% 
    group_by(micro,meso,idade, ano, sexo, uf) %>% 
    mutate(pr.mi=ob/sum(ob),
           ob.mi=sum(ob)) %>% 
    select(-ob)
  
  ###PR.MESO 
  me.inj.h.s <- base.5 %>% 
    filter(c.red == '_inj (hom,sui)') %>% 
    group_by(meso, GBD,idade, ano, sexo, uf) %>% 
    summarise(ob=sum(obitos.3, na.rm = T))%>% 
    ungroup() %>% 
    group_by(meso,idade, ano, sexo, uf) %>% 
    mutate(pr.me=ob/sum(ob),
           ob.me=sum(ob)) %>% 
    select(-ob)
  
  ###PR.UF
  uf.inj.h.s <- base.5 %>%
    filter(c.red == '_inj (hom,sui)') %>% 
    group_by( GBD,idade, ano, sexo, uf) %>% 
    summarise(ob=sum(obitos.3, na.rm = T))%>% 
    ungroup() %>% 
    group_by(idade, ano, sexo, uf) %>% 
    mutate(pr.uf=ob/sum(ob),
           ob.uf=sum(ob)) %>% 
    select(-ob)
  
  ###PR.REG
  rg.inj.h.s <- base.5 %>%
    filter(c.red == '_inj (hom,sui)') %>% 
    group_by( GBD,idade, ano, sexo, reg) %>% 
    summarise(ob=sum(obitos.3, na.rm = T))%>% 
    ungroup() %>% 
    group_by(idade, ano, sexo, reg) %>% 
    mutate(pr.rg=ob/sum(ob),
           ob.rg=sum(ob)) %>% 
    select(-ob)
  
  base.5 <- base.5 %>% 
    left_join(mu.inj.h.s, by=c('cdmun','micro','meso', 'GBD','idade', 'ano', 'sexo', 'uf')) %>% 
    left_join(mi.inj.h.s, by=c('micro','meso', 'GBD','idade', 'ano', 'sexo', 'uf')) %>% 
    left_join(me.inj.h.s, by=c('meso', 'GBD','idade', 'ano', 'sexo', 'uf')) %>% 
    left_join(uf.inj.h.s, by=c( 'GBD','idade', 'ano', 'sexo', 'uf')) %>% 
    left_join(rg.inj.h.s, by=c( 'GBD','idade', 'ano', 'sexo', 'reg')) 
  
  base.5 <- base.5 %>% 
    mutate(ihs.1=redis*pr.mu,
           redis.2=ifelse(is.na(ihs.1) & ob.mu==0, redis,NA),
           ihs.2=redis.2*pr.mi, 
           redis.3=ifelse(is.na(ihs.2) & ob.mi==0, redis.2,NA),
           ihs.3=redis.3*pr.me,
           redis.4=ifelse(is.na(ihs.3) & ob.me==0, redis.3,NA),
           ihs.4=redis.4*pr.uf,
           redis.5=ifelse(is.na(ihs.4) & ob.uf==0, redis.4,NA),
           ihs.5=redis.5*pr.rg,
           obitos.4= ifelse(!is.na(ihs.1), obitos.3+ihs.1, 
                            ifelse(!is.na(ihs.2), obitos.3+ihs.2,
                                   ifelse(!is.na(ihs.3), obitos.3+ihs.3,
                                          ifelse(!is.na(ihs.4), obitos.3+ihs.4,
                                                 ifelse(!is.na(ihs.5), obitos.3+ihs.5, obitos.3))))))
  
  rm(inj.hom.sui,me.inj.h.s,mi.inj.h.s,mu.inj.h.s,rg.inj.h.s,uf.inj.h.s)
  
  #Validação
  # obitos_para_redis <- sum(base.r[grepl('_inj (hom,sui)',base.r$c.red),]$redis, na.rm = T)
  # obitos_pre_redis <- sum(base.5$obitos.3,na.rm = T)
  # round(sum(base.5$obitos.4,na.rm = T))==round(sum(obitos_para_redis,obitos_pre_redis))
  
  ########_inj (hom,suic, fall,road): -----
  
  inj.hsf <-c("Injuries - Falls", "Injuries - Homicide", "Injuries - Suicide", "Injuries - Road")
  
  base.5 <- base.5 %>% 
    select(!(pr.mu:ob.rg),-redis,-redis.2, -redis.3,-redis.4, -redis.5,-c.red) %>% 
    mutate(c.red=ifelse(GBD %in% inj.hsf, '_inj (hom,suic, fall,road)',NA))%>%
    mutate(c.red=ifelse(GBD %in% "Injuries - Suicide" & idade %in% c("Early Neonatal","Post Neonatal","Late Neonatal","<1 year","0"),NA,c.red))%>% 
    left_join(dados_redis, by=c('cdmun','micro','meso',  'ano', 'sexo','idade', 'uf', 'c.red'))
  
  ###PRMUN
  mu.inj.hsf <- base.5 %>% 
    filter(c.red %in% '_inj (hom,suic, fall,road)')%>% 
    group_by(cdmun,micro,meso, GBD,idade, ano, sexo, uf) %>% 
    summarise(ob=sum(obitos.4, na.rm = T))%>% 
    ungroup() %>% 
    group_by(cdmun,micro,meso,idade, ano, sexo, uf) %>% 
    mutate(pr.mu=ob/sum(ob),
           ob.mu=sum(ob)) %>% 
    select(-ob)
  
  ###PR.MICRO 
  mi.inj.hsf <- base.5 %>% 
    filter(c.red %in% '_inj (hom,suic, fall,road)')%>% 
    group_by(micro,meso, GBD,idade, ano, sexo, uf) %>% 
    summarise(ob=sum(obitos.4, na.rm = T))%>% 
    ungroup() %>% 
    group_by(micro,meso,idade, ano, sexo, uf) %>% 
    mutate(pr.mi=ob/sum(ob),
           ob.mi=sum(ob)) %>% 
    select(-ob)
  
  
  
  ###PR.MESO 
  me.inj.hsf <- base.5 %>% 
    filter(c.red %in% '_inj (hom,suic, fall,road)')%>% 
    group_by(meso, GBD,idade, ano, sexo, uf) %>% 
    summarise(ob=sum(obitos.4, na.rm = T))%>% 
    ungroup() %>% 
    group_by(meso,idade, ano, sexo, uf) %>% 
    mutate(pr.me=ob/sum(ob),
           ob.me=sum(ob)) %>% 
    select(-ob)
  
  
  
  ###PR.UF
  uf.inj.hsf <- base.5 %>%
    filter(c.red %in% '_inj (hom,suic, fall,road)')%>% 
    group_by( GBD,idade, ano, sexo, uf) %>% 
    summarise(ob=sum(obitos.4, na.rm = T))%>% 
    ungroup() %>% 
    group_by(idade, ano, sexo, uf) %>% 
    mutate(pr.uf=ob/sum(ob),
           ob.uf=sum(ob)) %>% 
    select(-ob)
  
  
  ###PR.REG
  rg.inj.hsf <- base.5 %>%
    filter(c.red %in% '_inj (hom,suic, fall,road)')%>% 
    group_by( GBD,idade, ano, sexo, reg) %>% 
    summarise(ob=sum(obitos.4, na.rm = T))%>% 
    ungroup() %>% 
    group_by(idade, ano, sexo, reg) %>% 
    mutate(pr.rg=ob/sum(ob),
           ob.rg=sum(ob)) %>% 
    select(-ob)
  
  
  base.5 <- base.5 %>% 
    left_join(mu.inj.hsf, by=c('cdmun','micro','meso', 'GBD','idade', 'ano', 'sexo', 'uf')) %>% 
    left_join(mi.inj.hsf, by=c('micro','meso', 'GBD','idade', 'ano', 'sexo', 'uf')) %>% 
    left_join(me.inj.hsf, by=c('meso', 'GBD','idade', 'ano', 'sexo', 'uf')) %>% 
    left_join(uf.inj.hsf, by=c( 'GBD','idade', 'ano', 'sexo', 'uf')) %>% 
    left_join(rg.inj.hsf, by=c( 'GBD','idade', 'ano', 'sexo', 'reg')) 
  
  
  
  base.5 <- base.5 %>% 
    mutate(ihsf.1=redis*pr.mu,
           redis.2=ifelse(is.na(ihsf.1) & ob.mu==0, redis,NA),
           ihsf.2=redis.2*pr.mi,
           redis.3=ifelse(is.na(ihsf.2) & ob.mi==0, redis.2,NA),
           ihsf.3=redis.3*pr.me,
           redis.4=ifelse(is.na(ihsf.3) & ob.me==0, redis.3,NA),
           ihsf.4=redis.4*pr.uf,
           redis.5=ifelse(is.na(ihsf.4) & ob.uf==0, redis.4,NA),
           ihsf.5=redis.5*pr.rg,
           obitos.5= ifelse(!is.na(ihsf.1), obitos.4+ihsf.1, 
                            ifelse(!is.na(ihsf.2), obitos.4+ihsf.2,
                                   ifelse(!is.na(ihsf.3), obitos.4+ihsf.3,
                                          ifelse(!is.na(ihsf.4), obitos.4+ihsf.4,
                                                 ifelse(!is.na(ihsf.5), obitos.4+ihsf.5, obitos.4))))))
  
  rm(inj.hsf,me.inj.hsf,mi.inj.hsf,mu.inj.hsf,rg.inj.hsf,uf.inj.hsf)
  #Validação
  # obitos_para_redis <- sum(base.r[grepl('(hom,suic, fall,road)',base.r$c.red),]$redis, na.rm = T)
  # obitos_pre_redis <- sum(base.5$obitos.4,na.rm = T)
  # round(sum(base.5$obitos.5,na.rm = T))==round(sum(obitos_para_redis,obitos_pre_redis))
  # round(sum(base.5$obitos.5,na.rm = T))-round(sum(obitos_para_redis,obitos_pre_redis))
  
  ########	_inj (hom,sui,transp):-----
  
  inj.hst <-c("Injuries - Road", "Injuries - Suicide", "Injuries - Homicide", "Injuries - Other transport injuries")
  
  base.5 <- base.5 %>% 
    select(!(pr.mu:ob.rg),-redis,-redis.2, -redis.3,-redis.4, -redis.5,-c.red) %>% 
    mutate(c.red=ifelse(GBD %in% inj.hst, '_inj (hom,sui,transp)', NA)) %>%
    mutate(c.red=ifelse(GBD %in% "Injuries - Suicide" & idade %in% c("Early Neonatal","Post Neonatal","Late Neonatal","<1 year","0"),NA,c.red))%>% 
    left_join(dados_redis, by=c('cdmun','micro','meso',  'ano', 'sexo','idade', 'uf', 'c.red'))
  
  
  ###Proporções o	_inj (hom,sui,transp): 
  
  ###PRMUN
  mu.inj.hst <- base.5 %>% 
    filter(c.red %in% '_inj (hom,sui,transp)')%>%
    group_by(cdmun,micro,meso, GBD,idade, ano, sexo, uf) %>% 
    summarise(ob=sum(obitos.5, na.rm = T))%>% 
    ungroup() %>% 
    group_by(cdmun,micro,meso,idade, ano, sexo, uf) %>% 
    mutate(pr.mu=ob/sum(ob),
           ob.mu=sum(ob)) %>% 
    select(-ob)
  
  ###PR.MICRO 
  mi.inj.hst <- base.5 %>% 
    filter(c.red %in% '_inj (hom,sui,transp)')%>%
    group_by(micro,meso, GBD,idade, ano, sexo, uf) %>% 
    summarise(ob=sum(obitos.5, na.rm = T))%>% 
    ungroup() %>% 
    group_by(micro,meso,idade, ano, sexo, uf) %>% 
    mutate(pr.mi=ob/sum(ob),
           ob.mi=sum(ob)) %>% 
    select(-ob)
  
  ###PR.MESO 
  me.inj.hst <- base.5 %>% 
    filter(c.red %in% '_inj (hom,sui,transp)')%>%
    group_by(meso, GBD,idade, ano, sexo, uf) %>% 
    summarise(ob=sum(obitos.5, na.rm = T))%>% 
    ungroup() %>% 
    group_by(meso,idade, ano, sexo, uf) %>% 
    mutate(pr.me=ob/sum(ob),
           ob.me=sum(ob)) %>% 
    select(-ob)
  
  ###PR.UF
  uf.inj.hst <- base.5 %>%
    filter(c.red %in% '_inj (hom,sui,transp)')%>%
    group_by( GBD,idade, ano, sexo, uf) %>% 
    summarise(ob=sum(obitos.5, na.rm = T))%>% 
    ungroup() %>% 
    group_by(idade, ano, sexo, uf) %>% 
    mutate(pr.uf=ob/sum(ob),
           ob.uf=sum(ob)) %>% 
    select(-ob)
  
  ###PR.REG
  rg.inj.hst <- base.5 %>%
    filter(c.red %in% '_inj (hom,sui,transp)')%>%
    group_by( GBD,idade, ano, sexo, reg) %>% 
    summarise(ob=sum(obitos.5, na.rm = T))%>% 
    ungroup() %>% 
    group_by(idade, ano, sexo, reg) %>% 
    mutate(pr.rg=ob/sum(ob),
           ob.rg=sum(ob)) %>% 
    select(-ob)
  
  base.5 <- base.5 %>% 
    left_join(mu.inj.hst, by=c('cdmun','micro','meso', 'GBD','idade', 'ano', 'sexo', 'uf')) %>% 
    left_join(mi.inj.hst, by=c('micro','meso', 'GBD','idade', 'ano', 'sexo', 'uf')) %>% 
    left_join(me.inj.hst, by=c('meso', 'GBD','idade', 'ano', 'sexo', 'uf')) %>% 
    left_join(uf.inj.hst, by=c( 'GBD','idade', 'ano', 'sexo', 'uf')) %>% 
    left_join(rg.inj.hst, by=c( 'GBD','idade', 'ano', 'sexo', 'reg')) 
  
  
  base.5 <- base.5 %>% 
    mutate(ihst.1=redis*pr.mu,
           redis.2=ifelse(is.na(ihst.1) & ob.mu==0, redis,NA),
           ihst.2=redis.2*pr.mi,
           redis.3=ifelse(is.na(ihst.2) & ob.mi==0, redis.2,NA),
           ihst.3=redis.3*pr.me,
           redis.4=ifelse(is.na(ihst.3) & ob.me==0, redis.3,NA),
           ihst.4=redis.4*pr.uf,
           redis.5=ifelse(is.na(ihst.4) & ob.uf==0, redis.4,NA),
           ihst.5=redis.5*pr.rg,
           obitos.6= ifelse(!is.na(ihst.1), obitos.5+ihst.1, 
                            ifelse(!is.na(ihst.2), obitos.5+ihst.2,
                                   ifelse(!is.na(ihst.3), obitos.5+ihst.3,
                                          ifelse(!is.na(ihst.4), obitos.5+ihst.4,
                                                 ifelse(!is.na(ihst.5), obitos.5+ihst.5, obitos.5))))))
  
  rm(inj.hst,me.inj.hst,mi.inj.hst,mu.inj.hst,rg.inj.hst,uf.inj.hst)
  
  #Validação
  # obitos_para_redis <- sum(base.r[grepl('(hom,sui,transp)',base.r$c.red),]$redis, na.rm = T)
  # obitos_pre_redis <- sum(base.5$obitos.5,na.rm = T)
  # round(sum(base.5$obitos.6,na.rm = T))==round(sum(obitos_para_redis,obitos_pre_redis))
  # round(sum(base.5$obitos.6,na.rm = T))-round(sum(obitos_para_redis,obitos_pre_redis))
  
  ########	_inj(hom,suic,other):----- 
  
  inj.hso <-c("Injuries - Others", "Injuries - Suicide", "Injuries - Homicide")
  
  base.5 <- base.5 %>% 
    select(!(pr.mu:ob.rg),-redis,-redis.2, -redis.3,-redis.4, -redis.5,-c.red) %>% 
    mutate(c.red=ifelse(GBD %in% inj.hso, '_inj (hom,suic,other)', NA)) %>% 
    mutate(c.red=ifelse(GBD %in% "Injuries - Suicide" & idade %in% c("Early Neonatal","Post Neonatal","Late Neonatal","<1 year","0"),NA,c.red))%>% 
    left_join(dados_redis, by=c('cdmun','micro','meso',  'ano', 'sexo','idade', 'uf', 'c.red'))
  
  
  ###Proporções _inj(hom,suic,other): 
  
  ###PRMUN
  mu.inj.hso <- base.5 %>% 
    filter(c.red %in% '_inj (hom,suic,other)') %>% 
    group_by(cdmun,micro,meso, GBD,idade, ano, sexo, uf) %>% 
    summarise(ob=sum(obitos.6, na.rm = T))%>% 
    ungroup() %>% 
    group_by(cdmun,micro,meso,idade, ano, sexo, uf) %>% 
    mutate(pr.mu=ob/sum(ob),
           ob.mu=sum(ob)) %>% 
    select(-ob)
  
  ###PR.MICRO 
  mi.inj.hso <- base.5 %>% 
    filter(c.red %in% '_inj (hom,suic,other)') %>% 
    group_by(micro,meso, GBD,idade, ano, sexo, uf) %>% 
    summarise(ob=sum(obitos.6, na.rm = T))%>% 
    ungroup() %>% 
    group_by(micro,meso,idade, ano, sexo, uf) %>% 
    mutate(pr.mi=ob/sum(ob),
           ob.mi=sum(ob)) %>% 
    select(-ob)
  
  ###PR.MESO 
  me.inj.hso <- base.5 %>% 
    filter(c.red %in% '_inj (hom,suic,other)') %>% 
    group_by(meso, GBD,idade, ano, sexo, uf) %>% 
    summarise(ob=sum(obitos.6, na.rm = T))%>% 
    ungroup() %>% 
    group_by(meso,idade, ano, sexo, uf) %>% 
    mutate(pr.me=ob/sum(ob),
           ob.me=sum(ob)) %>% 
    select(-ob)
  
  ###PR.UF
  uf.inj.hso <- base.5 %>%
    filter(c.red %in% '_inj (hom,suic,other)') %>% 
    group_by( GBD,idade, ano, sexo, uf) %>% 
    summarise(ob=sum(obitos.6, na.rm = T))%>% 
    ungroup() %>% 
    group_by(idade, ano, sexo, uf) %>% 
    mutate(pr.uf=ob/sum(ob),
           ob.uf=sum(ob)) %>% 
    select(-ob)
  
  ###PR.REG
  rg.inj.hso <- base.5 %>%
    filter(c.red %in% '_inj (hom,suic,other)') %>% 
    group_by( GBD,idade, ano, sexo, reg) %>% 
    summarise(ob=sum(obitos.6, na.rm = T))%>% 
    ungroup() %>% 
    group_by(idade, ano, sexo, reg) %>% 
    mutate(pr.rg=ob/sum(ob),
           ob.rg=sum(ob)) %>% 
    select(-ob)
  
  
  base.5 <- base.5 %>% 
    left_join(mu.inj.hso, by=c('cdmun','micro','meso', 'GBD','idade', 'ano', 'sexo', 'uf')) 
  base.5 <- base.5%>% 
    left_join(mi.inj.hso, by=c('micro','meso', 'GBD','idade', 'ano', 'sexo', 'uf')) 
  base.5 <- base.5%>% 
    left_join(me.inj.hso, by=c('meso', 'GBD','idade', 'ano', 'sexo', 'uf')) 
  base.5 <- base.5%>% 
    left_join(uf.inj.hso, by=c( 'GBD','idade', 'ano', 'sexo', 'uf')) 
  base.5 <- base.5%>% 
    left_join(rg.inj.hso, by=c( 'GBD','idade', 'ano', 'sexo', 'reg')) 
  
  
  base.5 <- base.5 %>% 
    mutate(ihso.1=redis*pr.mu,
           redis.2=ifelse(is.na(ihso.1) & ob.mu==0, redis,NA),
           ihso.2=redis.2*pr.mi,
           redis.3=ifelse(is.na(ihso.2) & ob.mi==0, redis.2,NA),
           ihso.3=redis.3*pr.me,
           redis.4=ifelse(is.na(ihso.3) & ob.me==0, redis.3,NA),
           ihso.4=redis.4*pr.uf,
           redis.5=ifelse(is.na(ihso.4) & ob.uf==0, redis.4,NA),
           ihso.5=redis.5*pr.rg,
           obitos.7= ifelse(!is.na(ihso.1), obitos.6+ihso.1, 
                            ifelse(!is.na(ihso.2), obitos.6+ihso.2,
                                   ifelse(!is.na(ihso.3), obitos.6+ihso.3,
                                          ifelse(!is.na(ihso.4), obitos.6+ihso.4,
                                                 ifelse(!is.na(ihso.5), obitos.6+ihso.5, obitos.6))))))
  
  rm(inj.hso,me.inj.hso,mi.inj.hso,mu.inj.hso,rg.inj.hso,uf.inj.hso)
  #Validação
  # obitos_para_redis <- sum(base.r[grepl('(hom,suic,other)',base.r$c.red),]$redis, na.rm = T)
  # obitos_pre_redis <- sum(base.5$obitos.6,na.rm = T)
  # round(sum(base.5$obitos.7,na.rm = T))==round(sum(obitos_para_redis,obitos_pre_redis))
  # 
  # round(sum(base.5$obitos.7,na.rm = T))-round(sum(obitos_para_redis,obitos_pre_redis))
  print(base.5)
  return(base.5)
}