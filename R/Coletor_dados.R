#' Coletor de Dados
#'
#' Esta função coleta e combina dados do SIM de arquivos em um diretório especificado.
#' Os arquivos podem ser em DBF e DBC.
#' Deve-se inserir aspas nos dois parâmetros.
#'
#' @param caminho O caminho para o diretório contendo os arquivos.
#' @param padrao Um padrão de expressão regular para selecionar os arquivos.
#' @return Um data frame combinado de todos os arquivos que correspondem ao padrão.
#' @examples
#' \dontrun{
#' # Exemplo de uso:
#' caminho <- "caminho/para/seus/arquivos"
#' padrao <- "\\.csv$"
#' dados <- Coletor_dados(caminho, padrao)
#' }
#' @export


Coletor_dados=function(caminho,padrao){
  if (!require("pacman")) install.packages("pacman") #garantir que o pacman está instalado
  pacman::p_load(dplyr,rio) # pacotes necessários

  out.file <- data.frame()
  gc();gc();gc()
  setwd(caminho)

  file.names <- list.files(pattern = padrao,recursive = T)

  for(i in 1:length(file.names)){
    print(file.names[i])
    if(grepl(".dbc|.DBC",file.names[i])){
      file <- read.dbc(file.names[i])
    }else{
      file <- import(file.names[i])
    }
    out.file <- bind_rows(out.file, file)
    rm(file)
  }
  return(out.file)
}
