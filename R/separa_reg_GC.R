#' Separa Registros de Garbage Code (GC)
#'
#' Esta função processa as bases de dados do Sistema de Informações sobre Mortalidade (SIM),
#' separando os registros de Garbage Code (GC) e preparando uma base completa para redistribuição.
#'
#' @param dados_sem_ign Data frame contendo os dados sem registros ignorados,
#'                      incluindo colunas como `cdmun`, `GBD`, `sexo`, `ano`, etc.
#' @return Uma lista com duas bases de dados:
#' \itemize{
#'   \item `redistribuir`: Data frame contendo os registros de Garbage Code (GC) a serem redistribuídos.
#'   \item `completos`: Data frame com todos os registros, incluindo os dados processados sem GC.
#' }
#' @examples
#' \dontrun{
#' # Exemplo de uso com dados fictícios
#' dados_sem_ign <- data.frame(
#'   cdmun = c("110001", "120001"),
#'   GBD = c("_injuries", "Injuries - Falls"),
#'   sexo = c("Masculino", "Feminino"),
#'   ano = c(2020, 2021),
#'   obitos.2 = c(5, 10)
#' )
#'
#' resultado <- separa_reg_GC(dados_sem_ign)
#' head(resultado$redistribuir)
#' head(resultado$completos)
#' }
#' @export

separa_reg_GC = function (dados_sem_ign){
  if (!require("pacman")) install.packages("pacman") #garantir que o pacman está instalado
  pacman::p_load(tidyverse,rio) # pacotes necessários

  # Definição de causas targets e códigos garbage para redistribuir

causas=unique(ICD$CLASS_GPEAS_PRODUCAO)[!grepl(pattern = "^_",x = unique(ICD$CLASS_GPEAS_PRODUCAO))]
  causas=c(causas,"_pneumo")


redis=unique(ICD$CLASS_GPEAS_PRODUCAO)[grepl(pattern = "^_",x = unique(ICD$CLASS_GPEAS_PRODUCAO))]

  `%notin%` <- Negate(`%in%`)

  #Codigo de municipio

  mumime <- RedGCSIM::mumime
  mumime <- mumime %>%
    select(municode.6, microcode, mesocode)
  colnames(mumime) <- c('cdmun','micro', 'meso')
  mumime$cdmun <- as.character(mumime$cdmun)

  cdmun <- unique(as.character(dados_sem_ign$cdmun))

  notwantedlevels <- c(110000,120000,130000,140000, 150000, 160000, 170000, 210000, 220000,
                       230000,240000,250000,260000,270000,280000,290000, 310000,320000,330000,
                       350000, 410000,420000,430000,500000,510000,520000,000000)

  cdmun <- cdmun[cdmun %notin% notwantedlevels]

  #Base sem dados de Covid

  base_covid <- dados_sem_ign %>%
    select(cdmun:pop,obitos,obitos.2) %>%
    filter(GBD == "covid_19")

  assign("base_covid", base_covid, envir = .GlobalEnv)

  #Base com o GC
  base.r <- dados_sem_ign  %>%
    filter(GBD %in% redis) %>%
    select(cdmun:meso, obitos.2, uf)
  colnames(base.r)[8] <- 'redis'
  colnames(base.r)[5] <- 'c.red'

  # Montagem da Base SEM os GC ----

  base.4 <- dados_sem_ign %>%
    filter(GBD != "covid_19")%>%
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
