#' prop_causas_temp
#'
#' Carregar as bases de dados do SIM.
#'
#' @param dados, padrao vetor de números.
#'
#' @export

prop_causas_temp = function(dados){
  if (!require("pacman")) install.packages("pacman") #garantir que o pacman está instalado
  pacman::p_load(tidyverse, dtplyr) # pacotes necessários

  ####Município
  base.2 <- dados %>%
    lazy_dt()%>%
    group_by(cdmun,micro,meso, GBD, ano, sexo, uf) %>%
    mutate(mu.id=sum(obitos,na.rm = TRUE),
           pr.mu.id=obitos/sum(obitos,na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(cdmun, GBD, ano, idade, uf) %>%
    mutate(mu.s=sum(obitos,na.rm = TRUE),
           pr.mu.s=obitos/sum(obitos,na.rm = TRUE)) %>%
    ungroup() %>%
    ###Sem sexo, sem idade
    group_by(cdmun, GBD, ano, uf) %>%
    mutate(mu=sum(obitos,na.rm = TRUE),
           pr.mu=obitos/sum(obitos,na.rm = TRUE)) %>%
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
  return(base.2)
}
