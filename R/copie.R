#
#' copie
#'
#' Cette fonction copie dans le clipboard un dataframe
#' @param df Un dataframe
#' @export
#' @examples copie(df = iris)
#' copie(df = iris)
#'

copie <- function(df){
  write.table(df,'clipboard', row.names = F, sep = '\t')
  cat('Données copiées')
}

