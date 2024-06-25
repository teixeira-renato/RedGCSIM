#' Coletor_dados
#'
#' Carregar as bases de dados do SIM.
#'
#' @param caminho, padrao vetor de números.
#'
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