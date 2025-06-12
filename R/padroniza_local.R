#' Padroniza Local
#'
#' Esta função padroniza informações de local e ano em um conjunto de dados.
#'
#' @param x Um data frame contendo dados a serem padronizados.
#' @return Um data frame com as colunas `cdmun`, `SEXO`, `CAUSABAS`, `age`, `idade.cat` e `ano`.
#' @examples
#' \dontrun{
#' # Exemplo de uso:
#' dados <- data.frame(DTOBITO = c("20230615", "20221230"),
#'                     CODMUNRES = c("1234567", "2345678"),
#'                     SEXO = c("M", "F"),
#'                     CAUSABAS = c("A00", "B00"),
#'                     age = c(30, 25),
#'                     idade.cat = c("25-29", "20-24"))
#' dados_padronizados <- padroniza_local(dados)
#' }
#' @export


padroniza_local = function(x){
  ###Criação da Variável ano e cdmun com 6 dígitos
  out.file <- x %>%
    mutate(ano=str_sub(DTOBITO,5,8),
           cdmun=str_sub(CODMUNRES,end=6)) 
  
  print(table(out.file$ano, exclude = NULL)) # Total de registros no SIM

  base <- out.file %>%
    select(cdmun, SEXO, CAUSABAS, age, idade.cat, ano)

  return(base)
}
