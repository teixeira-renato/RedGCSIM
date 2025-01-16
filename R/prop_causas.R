#' Calcula Proporções de Causas
#'
#' Esta função calcula as proporções de causas de óbito nos dados fornecidos.
#'
#' @param dados Data frame contendo os dados de óbitos com as colunas necessárias.
#' @return Data frame com as proporções calculadas para cada causa.
#' @examples
#' \dontrun{
#' dados <- data.frame(
#'   cdmun = c(1, 2),
#'   micro = c("A", "B"),
#'   meso = c("X", "Y"),
#'   GBD = c("Causa1", "Causa2"),
#'   ano = c(2020, 2021),
#'   sexo = c("M", "F"),
#'   uf = c("SP", "RJ"),
#'   obitos = c(10, 20),
#'   pop = c(1000, 2000)
#' )
#' resultado <- prop_causas(dados)
#' }
#' @export


prop_causas <- function(dados) {
  if (!require("pacman")) install.packages("pacman") # garantir que o pacman está instalado
  pacman::p_load(tidyverse) # pacotes necessários

  #### Município
  base.2 <- dados %>%
    group_by(cdmun, micro, meso, GBD, ano, sexo, uf) %>%
    mutate(mu.id = sum(obitos, na.rm = TRUE),
           pr.mu.id = obitos / sum(obitos, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(cdmun, GBD, ano, idade, uf) %>%
    mutate(mu.s = sum(obitos, na.rm = TRUE),
           pr.mu.s = obitos / sum(obitos, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(cdmun, GBD, ano, uf) %>%
    mutate(mu = sum(obitos, na.rm = TRUE),
           pr.mu = obitos / sum(obitos, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(cdmun, ano, GBD, idade, sexo, uf) %>%
    mutate(pop.id.s = pop / sum(pop, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(cdmun, ano, GBD, sexo, uf) %>%
    mutate(pop.id = pop / sum(pop, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(cdmun, ano, GBD, idade, uf) %>%
    mutate(pop.s = pop / sum(pop, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(cdmun, ano, GBD, uf) %>%
    mutate(pop.t = pop / sum(pop, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(ano, GBD, idade, sexo, uf) %>%
    mutate(pmu.id.s = pop / sum(pop, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(ano, GBD, sexo, uf) %>%
    mutate(pmu.id = pop / sum(pop, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(ano, GBD, idade, uf) %>%
    mutate(pmu.s = pop / sum(pop, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(ano, GBD, uf) %>%
    mutate(pmu.t = pop / sum(pop, na.rm = TRUE)) %>%
    ungroup()

  #### Base micro
  micro <- base.2 %>%
    group_by(micro, meso, idade, GBD, ano, sexo, uf) %>%
    summarise(ob = sum(obitos, na.rm = TRUE)) %>%
    group_by(micro, meso, GBD, ano, sexo, uf) %>%
    mutate(pr.mi.id = ob / sum(ob, na.rm = TRUE),
           ob.mi.id = sum(ob, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(micro, meso, GBD, ano, idade, uf) %>%
    mutate(pr.mi.s = ob / sum(ob, na.rm = TRUE),
           ob.mi.s = sum(ob, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(micro, meso, GBD, ano, uf) %>%
    mutate(pr.mi.id.s = ob / sum(ob, na.rm = TRUE),
           ob.mi.id.s = sum(ob, na.rm = TRUE)) %>%
    ungroup() %>%
    select(-ob)

  #### Base meso
  meso <- base.2 %>%
    group_by(meso, idade, GBD, ano, sexo, uf) %>%
    summarise(ob = sum(obitos, na.rm = TRUE)) %>%
    group_by(meso, GBD, ano, sexo, uf) %>%
    mutate(pr.me.id = ob / sum(ob, na.rm = TRUE),
           ob.me.id = sum(ob, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(meso, GBD, ano, idade, uf) %>%
    mutate(pr.me.s = ob / sum(ob, na.rm = TRUE),
           ob.me.s = sum(ob, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(meso, GBD, ano, uf) %>%
    mutate(pr.me.id.s = ob / sum(ob, na.rm = TRUE),
           ob.me.id.s = sum(ob, na.rm = TRUE)) %>%
    ungroup() %>%
    select(-ob)

  #### Base uf
  uf <- base.2 %>%
    group_by(idade, GBD, ano, sexo, uf) %>%
    summarise(ob.uf = sum(obitos, na.rm = TRUE)) %>%
    group_by(GBD, ano, sexo, uf) %>%
    mutate(pr.uf.id = ob.uf / sum(ob.uf, na.rm = TRUE),
           ob.uf.id = sum(ob.uf, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(GBD, ano, idade, uf) %>%
    mutate(pr.uf.s = ob.uf / sum(ob.uf, na.rm = TRUE),
           ob.uf.s = sum(ob.uf, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(GBD, ano, uf) %>%
    mutate(pr.uf.id.s = ob.uf / sum(ob.uf, na.rm = TRUE),
           ob.uf.id.s = sum(ob.uf, na.rm = TRUE)) %>%
    ungroup()

  ## Agregando dados calculados na base original
  base.3 <- base.2 %>%
    left_join(micro, by = c('micro', 'idade', 'sexo', 'meso', 'GBD', 'ano', 'uf')) %>%
    left_join(meso, by = c('meso', 'idade', 'sexo', 'GBD', 'ano', 'uf')) %>%
    left_join(uf, by = c('GBD', 'idade', 'sexo', 'ano', 'uf'))

  return(base.3)
}

