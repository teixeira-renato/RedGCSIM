#' Redistribuição de Causas Maternas e Infantis
#'
#' Esta função redistribui causas de óbitos maternos e infantis (neonatais) nos dados do SIM,
#' utilizando critérios específicos baseados em proporções calculadas.
#'
#' @param dados_completos Data frame contendo os dados completos, incluindo colunas como
#'                        `cdmun`, `idade`, `sexo`, `ano`, e `GBD`.
#' @param dados_redis Data frame contendo as causas garbage a serem redistribuídas,
#'                    com as colunas necessárias para o processo.
#' @return Um data frame atualizado com as redistribuições realizadas, incluindo novas
#'         colunas de redistribuição (`obitos.8`, `obitos.9`, etc.) e validações.
#' @examples
#' \dontrun{
#' # Dados fictícios
#' dados_completos <- data.frame(
#'   cdmun = c("110001", "120001"),
#'   idade = c("25", "30"),
#'   sexo = c("Feminino", "Feminino"),
#'   ano = c(2020, 2021),
#'   GBD = c("materna_hemorragia", "infant_neonatal_prematuridade"),
#'   obitos = c(5, 10)
#' )
#'
#' dados_redis <- data.frame(
#'   cdmun = c("110001", "120001"),
#'   idade = c("25", "30"),
#'   sexo = c("Feminino", "Feminino"),
#'   ano = c(2020, 2021),
#'   c.red = c("_maternas", "_infant_neonat"),
#'   redis = c(1.5, 2.5)
#' )
#'
#' resultado <- redistribuicao_causas_mat_inf(dados_completos, dados_redis)
#' head(resultado)
#' }
#' @export

redistribuicao_causas_mat_inf = function (dados_completos, dados_redis){
  if (!require("pacman")) install.packages("pacman") #garantir que o pacman está instalado
  pacman::p_load(tidyverse) # pacotes necessários

  causas=unique(ICD$CLASS_GPEAS_PRODUCAO)[!grepl(pattern = "^_",x = unique(ICD$CLASS_GPEAS_PRODUCAO))]
  causas=c(causas,"_pneumo")
  
  ##### Óbito Materno----

  mat <-c(causas[grepl("^materna",causas)])

  base.5 <- dados_completos %>%
    select(!(pr.mu:ob.rg),-redis,-redis.2, -redis.3,-redis.4,-c.red) %>%
    mutate(c.red=ifelse(GBD %in% mat & sexo == "Feminino" & idade%in%c("10","15","20","25","30","35","40","45","50"), '_maternas', NA)) %>%
    left_join(dados_redis, by=c('cdmun','micro','meso',  'ano', 'sexo','idade', 'uf', 'c.red'))

  ###Proporções mat

  ###PRMUN
  muni.mat <- base.5 %>% 
    filter(GBD %in% mat & sexo == "Feminino") %>% 
    filter(idade%in%c("10","15","20","25","30","35","40","45","50"))%>%
    group_by(cdmun,micro,meso, GBD,idade, ano, sexo, uf) %>% 
    summarise(ob=sum(obitos.7, na.rm = T))%>% 
    ungroup() %>% 
    group_by(cdmun,micro,meso,idade, ano, sexo, uf) %>% 
    mutate(pr.mu=ob/sum(ob,na.rm=T),
           ob.mu=sum(ob,na.rm=T)) %>% 
    select(-ob)
  
  ###PR.MICRO 
  micro.mat <- base.5 %>% 
    filter(GBD %in% mat & sexo == "Feminino") %>% 
    filter(idade%in%c("10","15","20","25","30","35","40","45","50"))%>%
    group_by(micro,meso, GBD,idade, ano, sexo, uf) %>% 
    summarise(ob=sum(obitos.7, na.rm = T))%>% 
    ungroup() %>% 
    group_by(micro,meso,idade, ano, sexo, uf) %>% 
    mutate(pr.mi=ob/sum(ob,na.rm=T),
           ob.mi=sum(ob,na.rm=T)) %>% 
    select(-ob)
  
  ###PR.MESO 
  meso.mat <- base.5 %>% 
    filter(GBD %in% mat & sexo == "Feminino") %>% 
    filter(idade%in%c("10","15","20","25","30","35","40","45","50"))%>%
    group_by(meso, GBD,idade, ano, sexo, uf) %>% 
    summarise(ob=sum(obitos.7, na.rm = T))%>% 
    ungroup() %>% 
    group_by(meso,idade, ano, sexo, uf) %>% 
    mutate(pr.me=ob/sum(ob, na.rm = T),
           ob.me=sum(ob, na.rm = T)) %>% 
    select(-ob)
  
  ###PR.UF
  uf.mat <- base.5 %>%
    filter(GBD %in% mat & sexo == "Feminino") %>% 
    filter(idade%in%c("10","15","20","25","30","35","40","45","50"))%>%
    group_by( GBD,idade, ano, sexo, uf) %>% 
    summarise(ob=sum(obitos.7, na.rm = T))%>% 
    ungroup() %>% 
    group_by(idade, ano, sexo, uf) %>% 
    mutate(pr.uf=ob/sum(ob, na.rm = T),
           ob.uf=sum(ob, na.rm = T)) %>% 
    select(-ob)
  
  ###PR.REG
  rg.mat <- base.5 %>%
    filter(GBD %in% mat & sexo == "Feminino") %>% 
    filter(idade%in%c("10","15","20","25","30","35","40","45","50"))%>%
    group_by( GBD,idade, ano, sexo, reg) %>% 
    summarise(ob=sum(obitos.7, na.rm = T))%>% 
    ungroup() %>% 
    group_by(idade, ano, sexo, reg) %>% 
    mutate(pr.rg=ob/sum(ob,na.rm=T),
           ob.rg=sum(ob,na.rm=T)) %>% 
    select(-ob)
  
  base.5 <- base.5 %>% 
    left_join(muni.mat, by=c('cdmun','micro','meso', 'GBD','idade', 'ano', 'sexo', 'uf')) 
  base.5 <- base.5%>% 
    left_join(micro.mat, by=c('micro','meso', 'GBD','idade', 'ano', 'sexo', 'uf')) 
  base.5 <- base.5%>% 
    left_join(meso.mat, by=c('meso', 'GBD','idade', 'ano', 'sexo', 'uf')) 
  base.5 <- base.5%>% 
    left_join(uf.mat, by=c( 'GBD','idade', 'ano', 'sexo', 'uf')) 
  base.5 <- base.5%>% 
    left_join(rg.mat, by=c( 'GBD','idade', 'ano', 'sexo', 'reg'))
  
  
  base.5 <- base.5 %>% 
    mutate(mat.1=(redis)*pr.mu,
           redis.2=ifelse(is.na(mat.1) & ob.mu==0, redis,NA),
           mat.2=(redis.2)*pr.mi,
           redis.3=ifelse(is.na(mat.2) & ob.mi==0, redis.2,NA),
           mat.3=(redis.3)*pr.me,
           redis.4=ifelse(is.na(mat.3) & ob.me==0, redis.3,NA),
           mat.4=(redis.4)*pr.uf,
           redis.5=ifelse(is.na(mat.4) & ob.uf==0, redis.4,NA),
           mat.5=(redis.5)*pr.rg,
           obitos.8= ifelse(!is.na(mat.1), obitos.7+mat.1, 
                            ifelse(!is.na(mat.2), obitos.7+mat.2,
                                   ifelse(!is.na(mat.3), obitos.7+mat.3,
                                          ifelse(!is.na(mat.4), obitos.7+mat.4,
                                                 ifelse(!is.na(mat.5), obitos.7+mat.5, obitos.7))))))
  
  #validação

  # obitos_para_redis <- sum(base.r[grepl('_maternas',base.r$c.red),]$redis, na.rm = T)
  # obitos_pre_redis <- sum(base.5$obitos.7,na.rm = T)
  # round(sum(base.5$obitos.8,na.rm = T))==round(sum(obitos_para_redis,obitos_pre_redis))

  ##### Óbito Neonatal----

  infant <-c(causas[grepl("^infant|anom_congenitas|aspiracao_pulmunar|obst_intestinal|lri_post_neo",causas)])

  base.5 <- base.5 %>%
    select(!(pr.mu:ob.rg),-redis,-redis.2, -redis.3,-redis.4,-c.red) %>%
    mutate(c.red=ifelse(GBD %in% infant & idade %in% c("Early Neonatal","Post Neonatal","Late Neonatal","<1 year"), '_infant_neonat', NA)) %>%
    left_join(dados_redis, by=c('cdmun','micro','meso',  'ano', 'sexo','idade', 'uf', 'c.red'))

  ###Proporções infant

  ###PRMUN
  muni.infant <- base.5 %>% 
    filter(GBD %in% infant & idade %in% c("Early Neonatal","Post Neonatal","Late Neonatal","<1 year")) %>% 
    group_by(cdmun,micro,meso, GBD,idade, ano, sexo, uf) %>% 
    summarise(ob=sum(obitos.8, na.rm = T))%>% 
    ungroup() %>% 
    group_by(cdmun,micro,meso,idade, ano, sexo, uf) %>% 
    mutate(pr.mu=ob/sum(ob,na.rm=T),
           ob.mu=sum(ob,na.rm=T)) %>% 
    select(-ob)
  
  ###PR.MICRO 
  micro.infant <- base.5 %>% 
    filter(GBD %in% infant & idade %in% c("Early Neonatal","Post Neonatal","Late Neonatal","<1 year")) %>% 
    group_by(micro,meso, GBD,idade, ano, sexo, uf) %>% 
    summarise(ob=sum(obitos.8, na.rm = T))%>% 
    ungroup() %>% 
    group_by(micro,meso,idade, ano, sexo, uf) %>% 
    mutate(pr.mi=ob/sum(ob,na.rm=T),
           ob.mi=sum(ob,na.rm=T)) %>% 
    select(-ob)
  
  ###PR.MESO 
  meso.infant <- base.5 %>% 
    filter(GBD %in% infant & idade %in% c("Early Neonatal","Post Neonatal","Late Neonatal","<1 year")) %>% 
    group_by(meso, GBD,idade, ano, sexo, uf) %>% 
    summarise(ob=sum(obitos.8, na.rm = T))%>% 
    ungroup() %>% 
    group_by(meso,idade, ano, sexo, uf) %>% 
    mutate(pr.me=ob/sum(ob, na.rm = T),
           ob.me=sum(ob, na.rm = T)) %>% 
    select(-ob)
  
  ###PR.UF
  uf.infant <- base.5 %>%
    filter(GBD %in% infant & idade %in% c("Early Neonatal","Post Neonatal","Late Neonatal","<1 year")) %>% 
    group_by( GBD,idade, ano, sexo, uf) %>% 
    summarise(ob=sum(obitos.8, na.rm = T))%>% 
    ungroup() %>% 
    group_by(idade, ano, sexo, uf) %>% 
    mutate(pr.uf=ob/sum(ob, na.rm = T),
           ob.uf=sum(ob, na.rm = T)) %>% 
    select(-ob)
  
  ###PR.REG
  rg.infant <- base.5 %>%
    filter(GBD %in% infant & idade %in% c("Early Neonatal","Post Neonatal","Late Neonatal","<1 year")) %>% 
    group_by( GBD,idade, ano, sexo, reg) %>% 
    summarise(ob=sum(obitos.8, na.rm = T))%>% 
    ungroup() %>% 
    group_by(idade, ano, sexo, reg) %>% 
    mutate(pr.rg=ob/sum(ob,na.rm=T),
           ob.rg=sum(ob,na.rm=T)) %>% 
    select(-ob)
  
  base.5 <- base.5 %>% 
    left_join(muni.infant, by=c('cdmun','micro','meso', 'GBD','idade', 'ano', 'sexo', 'uf')) 
  base.5 <- base.5%>% 
    left_join(micro.infant, by=c('micro','meso', 'GBD','idade', 'ano', 'sexo', 'uf')) 
  base.5 <- base.5%>% 
    left_join(meso.infant, by=c('meso', 'GBD','idade', 'ano', 'sexo', 'uf')) 
  base.5 <- base.5%>% 
    left_join(uf.infant, by=c( 'GBD','idade', 'ano', 'sexo', 'uf')) 
  base.5 <- base.5%>% 
    left_join(rg.infant, by=c( 'GBD','idade', 'ano', 'sexo', 'reg'))
  
  
  base.5 <- base.5 %>% 
    mutate(infant.1=(redis)*pr.mu,
           redis.2=ifelse(is.na(infant.1) & ob.mu==0, redis,NA),
           infant.2=(redis.2)*pr.mi,
           redis.3=ifelse(is.na(infant.2) & ob.mi==0, redis.2,NA),
           infant.3=(redis.3)*pr.me,
           redis.4=ifelse(is.na(infant.3) & ob.me==0, redis.3,NA),
           infant.4=(redis.4)*pr.uf,
           redis.5=ifelse(is.na(infant.4) & ob.uf==0, redis.4,NA),
           infant.5=(redis.5)*pr.rg,
           obitos.9= ifelse(!is.na(infant.1), obitos.8+infant.1, 
                            ifelse(!is.na(infant.2), obitos.8+infant.2,
                                   ifelse(!is.na(infant.3), obitos.8+infant.3,
                                          ifelse(!is.na(infant.4), obitos.8+infant.4,
                                                 ifelse(!is.na(infant.5), obitos.8+infant.5, obitos.8))))))
  
  # validação
  # obitos_para_redis <- sum(base.r[grepl('_infant_neonat',base.r$c.red),]$redis, na.rm = T) #TODO: Conferir
  # obitos_pre_redis <- sum(base.5$obitos.8,na.rm = T)
  # round(sum(base.5$obitos.9,na.rm = T))==round(sum(obitos_para_redis,obitos_pre_redis))
  # round(sum(base.5$obitos.9,na.rm = T))-round(sum(obitos_para_redis,obitos_pre_redis))
  return(base.5)
}
