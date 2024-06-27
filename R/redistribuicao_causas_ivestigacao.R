#' redistribuicao_causas_ivestigacao
#'
#' Carregar as bases de dados do SIM.
#'
#' @param dados_completos,dados_redis,pesos, padrao vetor de números.
#'
#' @export

redistribuicao_causas_ivestigacao = function (dados_completos,dados_redis,pesos){
  if (!require("pacman")) install.packages("pacman") #garantir que o pacman está instalado
  pacman::p_load(tidyverse,rio) # pacotes necessários

  ICD_pesos <- RedGCSIM::ICD_pesos

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
  ##### x59----

  trans <-c(causas[grepl("^trans",causas)])
  inj <- c(causas[grepl("^Injuries",causas)])
  mat <-c(causas[grepl("^materna",causas)])

  ICD_x59 <- ICD_pesos%>%
    filter(CG=="_x59")%>%
    select(target,weight)

  x59 <- data.frame(target = c(inj,trans,mat,"other_causes_all","other_causes-lri","other_desnutricao_all_ages"))

  x59 <- x59%>%
    left_join(ICD_x59,by="target")

  base.5 <- dados_completos %>%
    select(!(pr.mu:ob.rg),-redis,-redis.2, -redis.3,-redis.4,-c.red) %>%
    mutate(c.red=ifelse(GBD %in% x59$target, '_x59', NA)) %>%
    left_join(dados_redis, by=c('cdmun','micro','meso',  'ano', 'sexo','idade', 'uf', 'c.red'))

  ###Proporções x59

  ###PRMUN
  muni.x59 <- base.5 %>%
    filter(GBD %in% c(trans,mat,"other_causes_all","other_causes-lri","other_desnutricao_all_ages")) %>%
    group_by(cdmun,micro,meso, GBD,idade, ano, sexo, uf) %>%
    summarise(ob=sum(obitos.9, na.rm = T))%>%
    ungroup() %>%
    group_by(cdmun,micro,meso,idade, ano, sexo, uf) %>%
    mutate(pr.mu=ob/sum(ob),
           ob.mu=sum(ob)) %>%
    select(-ob)

  ###PR.MICRO
  micro.x59 <- base.5 %>%
    filter(GBD %in% c(trans,mat,"other_causes_all","other_causes-lri","other_desnutricao_all_ages")) %>%
    group_by(micro,meso, GBD,idade, ano, sexo, uf) %>%
    summarise(ob=sum(obitos.9, na.rm = T))%>%
    ungroup() %>%
    group_by(micro,meso,idade, ano, sexo, uf) %>%
    mutate(pr.mi=ob/sum(ob),
           ob.mi=sum(ob)) %>%
    select(-ob)

  ###PR.MESO
  meso.x59 <- base.5 %>%
    filter(GBD %in% c(trans,mat,"other_causes_all","other_causes-lri","other_desnutricao_all_ages")) %>%
    group_by(meso, GBD,idade, ano, sexo, uf) %>%
    summarise(ob=sum(obitos.9, na.rm = T))%>%
    ungroup() %>%
    group_by(meso,idade, ano, sexo, uf) %>%
    mutate(pr.me=ob/sum(ob),
           ob.me=sum(ob)) %>%
    select(-ob)

  ###PR.UF
  uf.x59 <- base.5 %>%
    filter(GBD %in% c(trans,mat,"other_causes_all","other_causes-lri","other_desnutricao_all_ages")) %>%
    group_by( GBD,idade, ano, sexo, uf) %>%
    summarise(ob=sum(obitos.9, na.rm = T))%>%
    ungroup() %>%
    group_by(idade, ano, sexo, uf) %>%
    mutate(pr.uf=ob/sum(ob),
           ob.uf=sum(ob)) %>%
    select(-ob)

  ###PR.REG
  rg.x59 <- base.5 %>%
    filter(GBD %in% c(trans,mat,"other_causes_all","other_causes-lri","other_desnutricao_all_ages")) %>%
    group_by( GBD,idade, ano, sexo, reg) %>%
    summarise(ob=sum(obitos.9, na.rm = T))%>%
    ungroup() %>%
    group_by(idade, ano, sexo, reg) %>%
    mutate(pr.rg=ob/sum(ob),
           ob.rg=sum(ob)) %>%
    select(-ob)

  base.5 <- base.5 %>%
    left_join(muni.x59, by=c('cdmun','micro','meso', 'GBD','idade', 'ano', 'sexo', 'uf')) %>%
    left_join(micro.x59, by=c('micro','meso', 'GBD','idade', 'ano', 'sexo', 'uf')) %>%
    left_join(meso.x59, by=c('meso', 'GBD','idade', 'ano', 'sexo', 'uf')) %>%
    left_join(uf.x59, by=c( 'GBD','idade', 'ano', 'sexo', 'uf')) %>%
    left_join(rg.x59, by=c( 'GBD','idade', 'ano', 'sexo', 'reg'))

  base.5 <- base.5 %>%
    left_join(x59,by=c("GBD"="target"))%>%
    mutate(redis=ifelse(!is.na(weight),redis*weight,redis))%>%
    mutate(x59.1=ifelse(GBD %in% inj,redis,redis*pr.mu),
           redis.2=ifelse(is.na(x59.1)| x59.1 == 0  & ob.mu==0, redis,NA),
           x59.2=ifelse(GBD %in% inj,redis.2,redis.2*pr.mi),
           redis.3=ifelse(is.na(x59.2) & ob.mi==0, redis.2,NA),
           x59.3=ifelse(GBD %in% inj,redis.3,redis.3*pr.me),
           redis.4=ifelse(is.na(x59.3) & ob.me==0, redis.3,NA),
           x59.4=ifelse(GBD %in% inj,redis.4,redis.4*pr.uf),
           redis.5=ifelse(is.na(x59.4) & ob.uf==0, redis.4,NA),
           x59.5=ifelse(GBD %in% inj,redis.5,redis.5*pr.rg),
           obitos.10= ifelse(!is.na(x59.1), obitos.9+x59.1,
                             ifelse(!is.na(x59.2), obitos.9+x59.2,
                                    ifelse(!is.na(x59.3), obitos.9+x59.3,
                                           ifelse(!is.na(x59.4), obitos.9+x59.4,
                                                  ifelse(!is.na(x59.5), obitos.9+x59.5, obitos.9))))))

  rm(trans,inj,mat,ICD_x59)
  gc()
  #Validação

  # obitos_para_redis <- sum(base.r[grepl('_x59',base.r$c.red),]$redis, na.rm = T)
  # obitos_pre_redis <- sum(base.5$obitos.9,na.rm = T)
  # round(sum(base.5$obitos.10,na.rm = T))==round(sum(obitos_para_redis,obitos_pre_redis))
  # round(sum(base.5$obitos.10,na.rm = T))-round(sum(obitos_para_redis,obitos_pre_redis))

  ##### y34----

  trans <-c(causas[grepl("^trans",causas)])
  dcnt<- c(causas[grepl("^dcnt",causas)])
  inj <- c(causas[grepl("^Injuries",causas)])
  mat <-c(causas[grepl("^materna",causas)])

  ICD_y34 <- ICD_pesos%>%
    filter(CG=="_y34")%>%
    select(target,weight)

  y34 <- data.frame(target = c(dcnt,inj,trans,mat,"other_causes_all","other_causes-lri","other_desnutricao_all_ages"))

  y34 <- y34%>%
    left_join(ICD_y34,by="target")

  base.5 <- base.5 %>%
    select(!(pr.mu:ob.rg),-redis,-redis.2, -redis.3,-redis.4,-c.red,-weight) %>%
    mutate(c.red=ifelse(GBD %in% y34$target, '_y34', NA)) %>%
    left_join(dados_redis, by=c('cdmun','micro','meso',  'ano', 'sexo','idade', 'uf', 'c.red'))

  ###Proporções y34

  ###PRMUN
  muni.y34 <- base.5 %>%
    filter(GBD %in% c(trans,mat,"other_causes_all","other_causes-lri","other_desnutricao_all_ages")) %>%
    group_by(cdmun,micro,meso, GBD,idade, ano, sexo, uf) %>%
    summarise(ob=sum(obitos.10, na.rm = T))%>%
    ungroup() %>%
    group_by(cdmun,micro,meso,idade, ano, sexo, uf) %>%
    mutate(pr.mu=ob/sum(ob),
           ob.mu=sum(ob)) %>%
    select(-ob)

  ###PR.MICRO
  micro.y34 <- base.5 %>%
    filter(GBD %in% c(trans,mat,"other_causes_all","other_causes-lri","other_desnutricao_all_ages")) %>%
    group_by(micro,meso, GBD,idade, ano, sexo, uf) %>%
    summarise(ob=sum(obitos.10, na.rm = T))%>%
    ungroup() %>%
    group_by(micro,meso,idade, ano, sexo, uf) %>%
    mutate(pr.mi=ob/sum(ob),
           ob.mi=sum(ob)) %>%
    select(-ob)

  ###PR.MESO
  meso.y34 <- base.5 %>%
    filter(GBD %in% c(trans,mat,"other_causes_all","other_causes-lri","other_desnutricao_all_ages")) %>%
    group_by(meso, GBD,idade, ano, sexo, uf) %>%
    summarise(ob=sum(obitos.10, na.rm = T))%>%
    ungroup() %>%
    group_by(meso,idade, ano, sexo, uf) %>%
    mutate(pr.me=ob/sum(ob),
           ob.me=sum(ob)) %>%
    select(-ob)

  ###PR.UF
  uf.y34 <- base.5 %>%
    filter(GBD %in% c(trans,mat,"other_causes_all","other_causes-lri","other_desnutricao_all_ages")) %>%
    group_by( GBD,idade, ano, sexo, uf) %>%
    summarise(ob=sum(obitos.10, na.rm = T))%>%
    ungroup() %>%
    group_by(idade, ano, sexo, uf) %>%
    mutate(pr.uf=ob/sum(ob),
           ob.uf=sum(ob)) %>%
    select(-ob)

  ###PR.REG
  rg.y34 <- base.5 %>%
    filter(GBD %in% c(trans,mat,"other_causes_all","other_causes-lri","other_desnutricao_all_ages")) %>%
    group_by( GBD,idade, ano, sexo, reg) %>%
    summarise(ob=sum(obitos.10, na.rm = T))%>%
    ungroup() %>%
    group_by(idade, ano, sexo, reg) %>%
    mutate(pr.rg=ob/sum(ob),
           ob.rg=sum(ob)) %>%
    select(-ob)

  base.5 <- base.5 %>%
    left_join(muni.y34, by=c('cdmun','micro','meso', 'GBD','idade', 'ano', 'sexo', 'uf')) %>%
    left_join(micro.y34, by=c('micro','meso', 'GBD','idade', 'ano', 'sexo', 'uf')) %>%
    left_join(meso.y34, by=c('meso', 'GBD','idade', 'ano', 'sexo', 'uf')) %>%
    left_join(uf.y34, by=c( 'GBD','idade', 'ano', 'sexo', 'uf')) %>%
    left_join(rg.y34, by=c( 'GBD','idade', 'ano', 'sexo', 'reg'))


  base.5 <- base.5 %>%
    left_join(y34,by=c("GBD"="target"))%>%
    mutate(redis=ifelse(!is.na(weight),redis*weight,redis))%>%
    mutate(y34.1=ifelse(GBD %in% c(inj,dcnt),redis,redis*pr.mu),
           redis.2=ifelse(is.na(y34.1)| y34.1 == 0  & ob.mu==0, redis,NA),
           y34.2=ifelse(GBD %in% c(inj,dcnt),redis.2,redis.2*pr.mi),
           redis.3=ifelse(is.na(y34.2) & ob.mi==0, redis.2,NA),
           y34.3=ifelse(GBD %in% c(inj,dcnt),redis.3,redis.3*pr.me),
           redis.4=ifelse(is.na(y34.3) & ob.me==0, redis.3,NA),
           y34.4=ifelse(GBD %in% c(inj,dcnt),redis.4,redis.4*pr.uf),
           redis.5=ifelse(is.na(y34.4) & ob.uf==0, redis.4,NA),
           y34.5=ifelse(GBD %in% c(inj,dcnt),redis.5,redis.5*pr.rg),
           obitos.11= ifelse(!is.na(y34.1), obitos.10+y34.1,
                             ifelse(!is.na(y34.2), obitos.10+y34.2,
                                    ifelse(!is.na(y34.3), obitos.10+y34.3,
                                           ifelse(!is.na(y34.4), obitos.10+y34.4,
                                                  ifelse(!is.na(y34.5), obitos.10+y34.5, obitos.10))))))

  rm(trans,inj,mat,dcnt,ICD_y34)
  rm(list = ls()[grepl("^meso|^micro|^muni|^rg|^uf",ls())])
  gc()

  #Validação

  # obitos_para_redis <- sum(base.r[grepl('_y34',base.r$c.red),]$redis, na.rm = T)
  # obitos_pre_redis <- sum(base.5$obitos.10,na.rm = T)
  # round(sum(base.5$obitos.11,na.rm = T))==round(sum(obitos_para_redis,obitos_pre_redis),0)
  # round(sum(base.5$obitos.11,na.rm = T))-round(sum(obitos_para_redis,obitos_pre_redis),0)

  #### Pneumo DCV ----

  trans <-c(causas[grepl("^trans",causas)])
  dcnt<- c(causas[grepl("^dcnt",causas)])
  inj <- c(causas[grepl("^Injuries",causas)])
  mat <-c(causas[grepl("^materna",causas)])

  ICD_pneumo <- ICD_pesos%>%
    filter(CG=="_pneumo")%>%
    select(target,age,weight)

  pneumo <- data.frame(target = c("_pneumo",dcnt,inj,trans,mat,"other_causes_all","other_causes-lri","other_desnutricao_all_ages"))

  pneumo <- pneumo%>%
    left_join(ICD_pneumo,by="target")%>%
    mutate(c.red=case_when(age == "<10" ~ '_pneumo_inf',
                           age == "10 a 59" ~ '_pneumo_adult',
                           age == "60 emais" ~ '_pneumo_idoso',))%>%
    select(-age)

  base.r.pneumo <- dados_redis%>%
    filter(c.red == "_pneumo")%>%
    mutate(c.red=case_when(c.red == "_pneumo" & idade %in% c("Early Neonatal","Post Neonatal","Late Neonatal","<1 year","0","5") ~ '_pneumo_inf',
                           c.red == "_pneumo" & idade %in% c("10","15","20","25","30","35","40","45","50","55") ~ '_pneumo_adult',
                           c.red == "_pneumo" & idade %in% c("60","65","70","75","80","85","90") ~ '_pneumo_idoso'))


  base.5 <- base.5 %>%
    select(!(pr.mu:ob.rg),-redis,-redis.2, -redis.3,-redis.4,-c.red,-weight) %>%
    mutate(c.red=case_when(GBD %in% pneumo$target & idade %in% c("Early Neonatal","Post Neonatal","Late Neonatal","<1 year","0","5") ~ '_pneumo_inf',
                           GBD %in% pneumo$target & idade %in% c("10","15","20","25","30","35","40","45","50","55") ~ '_pneumo_adult',
                           GBD %in% pneumo$target & idade %in% c("60","65","70","75","80","85","90") ~ '_pneumo_idoso')) %>%
    left_join(base.r.pneumo, by=c('cdmun','micro','meso',  'ano', 'sexo','idade', 'uf', 'c.red'))

  ###Proporções PNE

  ###PRMUN
  muni.pne <- base.5 %>%
    filter(GBD %in% c(trans,mat,"other_causes_all","other_causes-lri","other_desnutricao_all_ages")) %>%
    filter(c.red %in% c('_pneumo_inf','_pneumo_adult','_pneumo_idoso'))%>%
    group_by(cdmun,micro,meso,c.red,idade, GBD, ano, sexo, uf) %>%
    summarise(ob=sum(obitos.11, na.rm = T))%>%
    ungroup() %>%
    group_by(cdmun,micro,meso,c.red, idade, ano, sexo, uf) %>%
    mutate(pr.mu=ob/sum(ob),
           ob.mu=sum(ob)) %>%
    select(-ob)

  ###PR.MICRO
  micro.pne <- base.5 %>%
    filter(GBD %in% c(trans,mat,"other_causes_all","other_causes-lri","other_desnutricao_all_ages")) %>%
    filter(c.red %in% c('_pneumo_inf','_pneumo_adult','_pneumo_idoso'))%>%
    group_by(micro,meso, c.red,idade, GBD, ano, sexo, uf) %>%
    summarise(ob=sum(obitos.11, na.rm = T))%>%
    ungroup() %>%
    group_by(micro,meso, c.red,idade, ano, sexo, uf) %>%
    mutate(pr.mi=ob/sum(ob),
           ob.mi=sum(ob)) %>%
    select(-ob)

  ###PR.MESO
  meso.pne <- base.5 %>%
    filter(GBD %in% c(trans,mat,"other_causes_all","other_causes-lri","other_desnutricao_all_ages")) %>%
    filter(c.red %in% c('_pneumo_inf','_pneumo_adult','_pneumo_idoso'))%>%
    group_by(meso, c.red,idade, GBD, ano, sexo, uf) %>%
    summarise(ob=sum(obitos.11, na.rm = T))%>%
    ungroup() %>%
    group_by(meso,c.red, idade, ano, sexo, uf) %>%
    mutate(pr.me=ob/sum(ob),
           ob.me=sum(ob)) %>%
    select(-ob)

  ###PR.UF
  uf.pne <- base.5 %>%
    filter(GBD %in% c(trans,mat,"other_causes_all","other_causes-lri","other_desnutricao_all_ages")) %>%
    filter(c.red %in% c('_pneumo_inf','_pneumo_adult','_pneumo_idoso'))%>%
    group_by( c.red, idade, GBD, ano, sexo, uf) %>%
    summarise(ob=sum(obitos.11, na.rm = T))%>%
    ungroup() %>%
    group_by(c.red, idade, ano, sexo, uf) %>%
    mutate(pr.uf=ob/sum(ob),
           ob.uf=sum(ob)) %>%
    select(-ob)

  ###PR.REG
  rg.pne <- base.5 %>%
    filter(GBD %in% c(trans,mat,"other_causes_all","other_causes-lri","other_desnutricao_all_ages")) %>%
    filter(c.red %in% c('_pneumo_inf','_pneumo_adult','_pneumo_idoso'))%>%
    group_by(c.red, idade, GBD, ano, sexo, reg) %>%
    summarise(ob=sum(obitos.11, na.rm = T))%>%
    ungroup() %>%
    group_by(c.red, idade, ano, sexo, reg) %>%
    mutate(pr.rg=ob/sum(ob),
           ob.rg=sum(ob)) %>%
    select(-ob)

  base.5 <- base.5 %>%
    left_join(muni.pne, by=c('cdmun','micro','meso','c.red', 'idade','GBD', 'ano', 'sexo', 'uf')) %>%
    left_join(micro.pne, by=c('micro','meso', 'c.red', 'idade', 'GBD', 'ano', 'sexo', 'uf')) %>%
    left_join(meso.pne, by=c('meso','c.red', 'idade', 'GBD', 'ano', 'sexo', 'uf')) %>%
    left_join(uf.pne, by=c( 'c.red','idade', 'GBD', 'ano', 'sexo', 'uf')) %>%
    left_join(rg.pne, by=c( 'c.red','idade','GBD', 'ano', 'sexo', 'reg'))

  base.5 <- base.5 %>%
    left_join(pneumo,by=c("GBD"="target","c.red"="c.red"))%>%
    mutate(redis=ifelse(!is.na(weight),redis*weight,redis))%>%
    mutate(pne.1=ifelse(GBD %in% c("_pneumo",inj,dcnt),redis,redis*pr.mu),
           redis.2=ifelse(is.na(pne.1)  & ob.mu==0, redis,NA),
           pne.2=ifelse(GBD %in% c("_pneumo",inj,dcnt),redis.2,redis.2*pr.mi),
           redis.3=ifelse(is.na(pne.2) & ob.mi==0, redis.2,NA),
           pne.3=ifelse(GBD %in% c("_pneumo",inj,dcnt),redis.3,redis.3*pr.me),
           redis.4=ifelse(is.na(pne.3) & ob.me==0, redis.3,NA),
           pne.4=ifelse(GBD %in% c("_pneumo",inj,dcnt),redis.4,redis.4*pr.uf),
           redis.5=ifelse(is.na(pne.4) & ob.uf==0, redis.4,NA),
           pne.5=ifelse(GBD %in% c("_pneumo",inj,dcnt),redis.5,redis.5*pr.rg),
           obitos.12= ifelse(!is.na(pne.1) , obitos.11+pne.1,
                             ifelse(!is.na(pne.2), obitos.11+pne.2,
                                    ifelse(!is.na(pne.3), obitos.11+pne.3,
                                           ifelse(!is.na(pne.4), obitos.11+pne.4,
                                                  ifelse(!is.na(pne.5), obitos.11+pne.5, obitos.11))))))%>%
    mutate(obitos.12=ifelse(GBD%in%"_pneumo",obitos.12-obitos.11,obitos.12))


  rm(trans,inj,mat,dcnt,ICD_pneumo)
  gc()

  #Validação
  #
  # base.r.sp <- base.r %>%
  #   filter(c.red%notin%c("_pneumo","_injuries","_inj (hom,sui)","_inj (hom,suic, fall)","_inj (hom,sui,transp)","_inj (hom,suic,other)","_maternas","_infant_neonat","_x59","_y34")) %>%
  #   group_by(cdmun,sexo,idade) %>%
  #   summarise(redis=sum(redis,na.rm=T))
  #
  # sum(base.r.sp$redis)+sum(base.5$obitos.12,na.rm=T)

  #### Casos redistribuídos entre todas as causas------
  #### Criação da variável com a causa CG para nortear a redistribuição
  base.5 <- base.5 %>%
    select(-redis,-redis.2, -redis.3, -redis.4, -redis.5) %>%
    mutate(c.red=ifelse(GBD%in%"_pneumo",NA,'_all')) %>%
    left_join(dados_redis, by=c('cdmun','micro','meso',  'ano', 'sexo','idade', 'uf', 'c.red'))

  base.5 <- base.5 %>%
    mutate(all.1=redis*pr.mu,
           redis.2=ifelse(is.na(all.1) & ob.mu==0, redis,NA),
           all.2=redis.2*pr.mi,
           redis.3=ifelse(is.na(all.2) & ob.mi==0, redis.2,NA),
           all.3=redis.3*pr.me,
           redis.4=ifelse(is.na(all.3) & ob.me==0, redis.3,NA),
           all.4=redis.4*pr.uf,
           redis.5=ifelse(is.na(all.4) & ob.uf==0, redis.4,NA),
           all.5=redis.5*pr.rg,
           obitos.13=ifelse(!is.na(all.1), obitos.12+all.1,
                            ifelse(!is.na(all.2), obitos.12+all.2,
                                   ifelse(!is.na(all.3), obitos.12+all.3,
                                          ifelse(!is.na(all.4), obitos.12+all.4,
                                                 ifelse(!is.na(all.5), obitos.12+all.5, obitos.12))))))

  #Validação
  # obitos_para_redis <- sum(base.r[grepl("_all",base.r$c.red),]$redis, na.rm = T)
  # obitos_pre_redis <- sum(base.5$obitos.12,na.rm = T)
  # round(sum(base.5$obitos.13,na.rm = T),0)==round(sum(obitos_para_redis,obitos_pre_redis),0)
  #


  return(base.5)
}
