#' Redistribuição de Dados Faltantes
#'
#' Esta função redistribui dados faltantes (como idade, sexo, município, ou combinações desses) nos registros
#' do SIM, utilizando proporções pré-calculadas para completar os valores ausentes.
#'
#' @param base_prop Data frame contendo as proporções pré-calculadas usadas para redistribuição.
#'                  Deve incluir informações como município (`cdmun`), idade, sexo, e proporções (`pr.*`).
#' @param dados_ign Data frame contendo os registros com dados faltantes, incluindo colunas como `idade`,
#'                  `sexo`, `cdmun`, e informações de óbitos.
#' @return Um data frame com os dados redistribuídos, incluindo colunas adicionais para diferentes níveis de redistribuição
#'         (por idade, sexo, município e combinações).
#' @examples
#' \dontrun{
#' # Exemplo de uso com dados fictícios
#' base_prop <- data.frame(
#'   cdmun = c("110001", "120001"),
#'   idade = c("30", "25"),
#'   sexo = c("Masculino", "Feminino"),
#'   pr.mu.id = c(0.8, 0.6),
#'   pr.mi.id = c(0.2, 0.4)
#' )
#'
#' dados_ign <- data.frame(
#'   cdmun = c("110001", "120001"),
#'   idade = c("IGN", "25"),
#'   sexo = c("Masculino", "IGN"),
#'   obitos = c(5, 10)
#' )
#'
#' resultado <- redistribuicao_dados_faltantes(base_prop, dados_ign)
#' head(resultado)
#' }
#' @export

redistribuicao_dados_faltantes = function(base_prop,dados_ign){
  if (!require("pacman")) install.packages("pacman") #garantir que o pacman está instalado
  pacman::p_load(tidyverse) # pacotes necessários

  `%notin%` <- Negate(`%in%`)

  notwantedlevels <- c(110000,120000,130000,140000, 150000, 160000, 170000, 210000, 220000,
                       230000,240000,250000,260000,270000,280000,290000, 310000,320000,330000,
                       350000, 410000,420000,430000,500000,510000,520000,000000)

  #Redistribuição ign----

  ####Sem Idade
  ##No município

  ig.id <- dados_ign %>%
    filter(idade=='IGN'& cdmun %notin% notwantedlevels & sexo!='IGN') %>%
    select(-idade)

  base.3 <- base_prop
  ig.id <- ig.id

  base.3 <- left_join(base.3, ig.id, by=c('cdmun','micro', 'meso', 'uf', 'GBD', 'sexo', 'ano'))

  base.3 <- mutate(base.3,id.1=ign*pr.mu.id,
                   ign.2=ifelse(is.na(id.1) & mu.id==0, ign,NA),
                   id.2=ign.2*pr.mi.id,
                   ign.3=ifelse(is.na(id.2) & ob.mi.id==0, ign,NA),
                   id.3=ign.3*pr.me.id,
                   ign.4=ifelse(is.na(id.3) & ob.me.id==0, ign,NA),
                   id.4=ign.4*pr.uf.id,
                   ign.5=ifelse(is.na(id.4) & ob.uf.id==0, ign,NA),
                   id.5=ign.5*pop.id,
                   obi.2= ifelse(!is.na(id.1), obitos+id.1,
                                 ifelse(!is.na(id.2), obitos+id.2,
                                        ifelse(!is.na(id.3), obitos+id.3,
                                               ifelse(!is.na(id.4), obitos+id.4,
                                                      ifelse(!is.na(id.5), obitos+id.5,obitos)))))) %>%
    select(-ign,-ign.2, -ign.3, -ign.4, -ign.5)
  gc();gc()

  ###########Sem sexo

  ig.sex <- dados_ign %>%
    filter(idade!='IGN'& !cdmun %in% notwantedlevels & sexo=='IGN') %>%
    select(-sexo)

  ig.sex$idade<- as.character(ig.sex$idade)

  base.3 <- left_join(base.3, ig.sex, by=c('cdmun','micro', 'meso', 'uf', 'GBD', 'idade', 'ano'))%>%
    mutate(s.1=ign*pr.mu.s,
           ign.2=ifelse(is.na(s.1) & mu.s==0, ign,NA),
           s.2=ign.2*pr.mi.s,
           ign.3=ifelse(is.na(s.2) & ob.mi.s==0, ign,NA),
           s.3=ign.3*pr.me.s,
           ign.4=ifelse(is.na(s.3) & ob.me.s==0, ign,NA),
           s.4=ign.4*pr.uf.s,
           ign.5=ifelse(is.na(s.4) & ob.uf.s==0, ign,NA),
           s.5=ign.5*pop.s,
           obi.3=ifelse(!is.na(s.1), obi.2+s.1,
                        ifelse(!is.na(s.2), obi.2+s.2,
                               ifelse(!is.na(s.3), obi.2+s.3,
                                      ifelse(!is.na(s.4), obi.2+s.4,
                                             ifelse(!is.na(s.5), obi.2+s.5,obi.2)))))) %>%
    select(-ign,-ign.2, -ign.3, -ign.4, -ign.5)


  #### Sem sexo e idade
  ig.id.sex <- dados_ign %>%
    filter(idade=='IGN'& !cdmun %in% notwantedlevels & sexo=='IGN') %>%
    select(-c(idade,sexo))



  base.3 <- left_join(base.3, ig.id.sex, by=c('cdmun','micro', 'meso', 'uf', 'GBD',  'ano')) %>%
    mutate(id.s.1=ign*pr.mu,
           ign.2=ifelse(is.na(id.s.1) & mu==0, ign,NA),
           id.s.2=ign.2*pr.mi.id.s,
           ign.3=ifelse(is.na(id.s.2) & ob.mi.id.s==0, ign,NA),
           id.s.3=ign.3*pr.me.id.s,
           ign.4=ifelse(is.na(id.s.3) & ob.me.id.s==0, ign,NA),
           id.s.4=ign.4*pr.uf.id.s,
           ign.5=ifelse(is.na(id.s.4) & ob.uf.id.s==0, ign,NA),
           id.s.5=ign.5*pop.id.s,
           obi.4=ifelse(!is.na(id.s.1),obi.3+id.s.1,
                        ifelse(!is.na(id.s.2),obi.3+id.s.2,
                               ifelse(!is.na(id.s.3),obi.3+id.s.3,
                                      ifelse(!is.na(id.s.4),obi.3+id.s.4,
                                             ifelse(!is.na(id.s.5),obi.3+id.s.5,obi.3)))))) %>%
    select(-ign,-ign.2, -ign.3, -ign.4, -ign.5)


  ##### Sem MUN

  ig.mun <- dados_ign %>%
    filter(idade!='IGN'& cdmun %in% notwantedlevels & sexo!='IGN') %>%
    select(!(cdmun:meso))


  ig.mun$idade<- as.character(ig.mun$idade)


  base.3 <- left_join(base.3, ig.mun, by=c( 'uf', 'GBD', 'ano','idade','sexo')) %>%
    mutate(ig.mu=ign*(obitos/ob.uf),
           ig.pop=ifelse(is.na(ig.mu) & ob.uf==0, ign*pmu.id.s, NA),
           obi.5= ifelse(!is.na(ig.mu),  obi.4+ig.mu,
                         ifelse(!is.na(ig.pop), obi.4+ig.pop, obi.4)))  %>%
    select(-ign)



  ##### Sem MUN  e id

  ig.mun.id <- dados_ign %>%
    filter(idade=='IGN'& cdmun %in% notwantedlevels & sexo!='IGN') %>%
    select(!(cdmun:meso), -idade)



  base.3 <- left_join(base.3, ig.mun.id, by=c( 'uf', 'GBD', 'ano','sexo')) %>%
    mutate(ig.mu.id=ign*(obitos/ob.uf.id),
           ig.id.pop=ifelse(is.na(ig.mu.id) & ob.uf.id==0, ign*pmu.id, NA),
           obi.6=ifelse(!is.na(ig.mu.id), obi.5+ig.mu.id,
                        ifelse(!is.na(ig.id.pop),obi.5+ig.id.pop, obi.5)))  %>%
    select(-ign)


  ##### Sem MUN  e sexo

  ig.mun.s <- dados_ign %>%
    filter(idade!='IGN'& cdmun %in% notwantedlevels & sexo=='IGN') %>%
    select(!(cdmun:sexo))

  ig.mun.s$idade<- as.character(ig.mun.s$idade)


  base.3 <- left_join(base.3, ig.mun.s, by=c( 'uf', 'GBD', 'ano','idade')) %>%
    mutate(ig.mu.s=ign*(obitos/ob.uf.s),
           ig.s.pop=ifelse(is.na(ig.mu.s) & ob.uf.s==0, ign*pmu.s, NA),
           obi.7=ifelse(!is.na(ig.mu.s),  obi.6+ig.mu.s,
                        ifelse(!is.na(ig.s.pop), obi.6+ig.s.pop, obi.6)))  %>%
    select(-ign)


  ##### Sem mun, idade e sexo
  rm(ig.id,ig.id.sex,ig.mun,ig.mun.id,ig.mun.s,ig.sex)
  gc();gc()

  ig.mun.s.id <- dados_ign %>%
    filter(idade=='IGN'& cdmun %in% notwantedlevels & sexo=='IGN') %>%
    select(!(cdmun:idade))



  base.3 <- base.3%>%
    left_join(ig.mun.s.id, by=c( 'uf', 'GBD', 'ano'))

  base.3 <- base.3 %>%
    mutate(ig.mu.s.id=ign*(obitos/ob.uf.id.s))
  base.3 <- base.3%>%
    mutate(ig.s.id.pop=ifelse(is.na(ig.mu.s.id) & ob.uf.id.s==0, ign*pmu.t, NA))
  base.3 <- base.3%>%
    mutate(obitos.2=ifelse(!is.na(ig.mu.s.id),  obi.7+ig.mu.s.id,
                           ifelse(!is.na(ig.s.id.pop), obi.7+ig.s.id.pop, obi.7)))
  base.3 <- base.3%>%
    select(-ign)

  rm(ig.mun.s.id)
  gc();gc()

  return(base.3)

}
