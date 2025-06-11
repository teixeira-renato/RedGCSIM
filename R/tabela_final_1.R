#' Geração de Tabela Final com Dados do SIM
#'
#' Esta função processa os dados do Sistema de Informações sobre Mortalidade (SIM), integrando informações de causas de óbitos, regiões administrativas e outras variáveis para produzir uma tabela consolidada.
#'
#' @param x Data frame contendo os dados do SIM, incluindo colunas como `CAUSABAS`, `SEXO`, `idade.cat`, `ano`, entre outras.
#' @return Um data frame com informações agregadas por município, regionais de saúde, estados e grandes regiões, incluindo as seguintes colunas:
#' \itemize{
#'   \item `cdmun`: Código do município.
#'   \item `micro`: Código da microrregião.
#'   \item `meso`: Código da mesorregião.
#'   \item `sexo`: Sexo reclassificado (Masculino, Feminino ou IGN).
#'   \item `idade`: Categoria de idade, com valores ignorados marcados como 'IGN'.
#'   \item `ano`: Ano dos óbitos.
#'   \item `GBD`: Classificação Global Burden of Disease (GBD) das causas.
#'   \item `obitos`: Número de óbitos agregados.
#'   \item `uf`: Unidade Federativa correspondente ao município.
#' }
#' @examples
#' \dontrun{
#' # Exemplo de uso:
#' dados <- data.frame(
#'   CAUSABAS = c("A01", "B02"),
#'   SEXO = c("1", "2"),
#'   idade.cat = c("20-24", "25-29"),
#'   ano = c(2020, 2021),
#'   cdmun = c("110001", "120001")
#' )
#'
#' dados_municipios <- tabela_final_1(dados)
#' head(dados_municipios)
#' }
#' @export



tabela_final_1 = function(x){
  if (!require("pacman")) install.packages("pacman") #garantir que o pacman está instalado
  pacman::p_load(tidyverse,rio) # pacotes necessários

  ###Arquivo com as categoria das causas e os CID
  causas <- RedGCSIM::ICD
  colnames(causas)[1] <- 'CAUSABAS'

  ###Merge Causas GBD
  base <- left_join(x, causas, by='CAUSABAS')

  ##Arquivo com as micros e as mesos regiões
  mumime <- RedGCSIM::mumime
  mumime <- mumime %>%
    select(municode.6, microcode, mesocode)%>%
    mutate(cdmun = as.factor(municode.6))%>%
    select(-municode.6)

  ###Merge Micro, Meso
  base <- left_join(base, mumime, by='cdmun')

  ###Base por idade, sexo e etc.
  base.2 <- base %>%
    group_by(cdmun,microcode, mesocode,  SEXO, idade.cat, ano, CLASS_GPEAS_PRODUCAO) %>%
    summarise(ob=n())

  #Marca registro sem idade definida
  base.2$idade.cat[is.na(base.2$idade.cat)] <- 'IGN'

  #Reclassifica o sexo
  base.2$SEXO <- recode(base.2$SEXO, '1'='Masculino', '2'='Feminino', '0'='IGN', '9'='IGN')

  #Renomeia colunas
  b.raiz <- base.2
  colnames(b.raiz) <- c('cdmun','micro', 'meso',  'sexo', 'idade','ano', 'GBD',  'obitos')
  b.raiz <- b.raiz %>%
    mutate(uf=str_sub(cdmun,1,2))

  return(b.raiz)
}
