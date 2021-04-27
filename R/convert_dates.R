#
#' convert.dates
#'
#' Cette fonction identifie les vecteur de date de différent format character et les transforme en date
#' @param df un dataframe pour lequel des transformation sont requises
#' @return Retourne un dataframe avec les dates dans le bon format
#' @export
#' @examples df <- convert.dates(df)
#' convert.dates()
#'


convert.dates <- function(df){

  var.dates <- find.dates(df)

  for (date in var.dates) {
    df[,date] <- as.Date(df[,date], format = date.format(df[,date]))

  }

  return(df)
}


