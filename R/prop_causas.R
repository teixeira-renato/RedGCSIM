#' prop_causas
#'
#' Carregar as bases de dados do SIM.
#'
#' @param dados, padrao vetor de números.
#'
#' @export

prop_causas = function(dados){
  if (!require("pacman")) install.packages("pacman") #garantir que o pacman está instalado
  pacman::p_load(tidyverse, dtplyr) # pacotes necessários
  
  ####Município
  base.2 <- dados %>%
    lazy_dt()%>%
    group_by(cdmun,micro,meso, GBD, ano, sexo, uf) %>% 
    mutate(mu.id=sum(obitos,na.rm = T),
           pr.mu.id=obitos/sum(obitos,na.rm = T)) %>% 
    ungroup() %>%
    group_by(cdmun, GBD, ano, idade, uf) %>% 
    mutate(mu.s=sum(obitos,na.rm = T),
           pr.mu.s=obitos/sum(obitos,na.rm = T)) %>% 
    ungroup() %>%
    ###Sem sexo, sem idade
    group_by(cdmun, GBD, ano, uf) %>% 
    mutate(mu=sum(obitos,na.rm = T),
           pr.mu=obitos/sum(obitos,na.rm = T)) %>% 
    ungroup() %>% 
    ####Pela População Geral(Município Conhecido)
    group_by(cdmun, ano,GBD, idade,sexo, uf) %>%
    mutate(pop.id.s=pop/sum(pop)) %>%
    ungroup() %>%
    ####Pela População sem ID(Município Conhecido)
    group_by(cdmun, ano,GBD, sexo, uf) %>%
    mutate(pop.id=pop/sum(pop)) %>%
    ungroup() %>%
    ####Pela População sem sexo(Município Conhecido)
    group_by(cdmun, ano,GBD, idade, uf) %>%
    mutate(pop.s=pop/sum(pop)) %>%
    ungroup() %>%
    ####Pela População sem ID e sexo(Município Conhecido)
    group_by(cdmun, ano,GBD,  uf) %>%
    mutate(pop.t=pop/sum(pop)) %>%
    ungroup() %>%
    ####Pela População Geral(Em Município)
    group_by(ano,GBD, idade,sexo, uf) %>%
    mutate(pmu.id.s=pop/sum(pop)) %>%
    ungroup() %>%
    ####Pela População sem ID
    group_by(ano,GBD, sexo, uf) %>%
    mutate(pmu.id=pop/sum(pop)) %>%
    ungroup() %>%
    ####Pela População sem sexo
    group_by(ano,GBD, idade, uf) %>%
    mutate(pmu.s=pop/sum(pop)) %>%
    ungroup() %>%
    ####Pela População sem ID e sexo
    group_by(ano,GBD,  uf) %>%
    mutate(pmu.t=pop/sum(pop)) %>%
    ungroup()%>%
    as_tibble()
  gc()
  
  ###base.micro
  micro <- base.2 %>% 
    lazy_dt()%>%
    group_by(micro,meso,idade, GBD, ano, sexo, uf) %>% 
    summarise(ob=sum(obitos,na.rm = T))%>% 
    ungroup() %>% 
    group_by(micro,meso, GBD, ano, sexo, uf) %>% 
    mutate(pr.mi.id=ob/sum(ob,na.rm = T),
           ob.mi.id=sum(ob,na.rm = T))%>%   
    ungroup() %>% 
    group_by(micro,meso, GBD, ano, idade, uf) %>% 
    mutate(pr.mi.s=ob/sum(ob,na.rm = T),
           ob.mi.s=sum(ob,na.rm = T))%>% 
    ungroup() %>%     
    group_by(micro,meso, GBD, ano, uf) %>% 
    mutate(pr.mi.id.s=ob/sum(ob,na.rm = T),
           ob.mi.id.s=sum(ob,na.rm = T))%>% 
    ungroup()%>% 
    select(-ob)%>%
    as_tibble()
  gc()
  
  ###base.meso
  
  meso <- base.2 %>% 
    lazy_dt()%>%
    group_by(meso,idade, GBD, ano, sexo, uf) %>% 
    summarise(ob=sum(obitos,na.rm = T))%>% 
    ungroup() %>% 
    group_by(meso, GBD, ano, sexo, uf) %>% 
    mutate(pr.me.id=ob/sum(ob,na.rm = T),
           ob.me.id=sum(ob,na.rm = T))%>% 
    ungroup() %>% 
    group_by(meso, GBD, ano, idade, uf) %>% 
    mutate(pr.me.s=ob/sum(ob,na.rm = T),
           ob.me.s=sum(ob,na.rm = T))%>%
    ungroup() %>%     
    group_by(meso, GBD, ano, uf) %>% 
    mutate(pr.me.id.s=ob/sum(ob,na.rm = T),
           ob.me.id.s=sum(ob,na.rm = T))%>%
    ungroup()  %>% 
    select(-ob)%>%
    as_tibble()
  gc()
  
  ###base.uf
  
  uf <- base.2 %>% 
    lazy_dt()%>%
    group_by(idade, GBD, ano, sexo, uf) %>% 
    summarise(ob.uf=sum(obitos,na.rm = T))%>% 
    ungroup() %>% 
    group_by(GBD, ano, sexo, uf) %>% 
    mutate(pr.uf.id=ob.uf/sum(ob.uf,na.rm = T),
           ob.uf.id=sum(ob.uf,na.rm = T))%>%  
    ungroup() %>% 
    group_by( GBD, ano, idade, uf) %>% 
    mutate(pr.uf.s=ob.uf/sum(ob.uf,na.rm = T),
           ob.uf.s=sum(ob.uf,na.rm = T))%>%
    ungroup() %>%     
    group_by( GBD, ano, uf) %>% 
    mutate(pr.uf.id.s=ob.uf/sum(ob.uf,na.rm = T),
           ob.uf.id.s=sum(ob.uf,na.rm = T))%>%
    ungroup()%>%
    as_tibble()
  
  gc()
  
  ## Agragando dados calculados na base original
  
  micro$idade <- as.character(micro$idade)
  
  base.3 <- base.2 %>%  
    left_join( micro, by=c('micro','idade','sexo', 'meso', 'GBD', 'ano', 'uf')) 
  
  rm(base.2, micro)
  gc();gc()
  
  meso$idade <- as.character(meso$idade)
  base.3 <- base.3 %>% 
    left_join( meso, by=c('meso','idade','sexo', 'GBD', 'ano',  'uf')) 
  
  rm(meso)
  gc();gc()
  
  uf$idade <- as.character(uf$idade)
  
  base.3<- base.3 %>%
    left_join( uf, by=c( 'GBD', 'idade','sexo', 'ano','uf'))
  
  rm(uf)
  gc();gc()
  
  return(base.3)
}