#
#' resumedf_sql
#'
#' This function mets a jour les données dans une table si l'id existe dans la table sql server ou alors ajoute les données a partir d'un dataframe
#' @param df un dataframe pour lequel un résumé est désiré
#' @param conn A DBI connection
#' @param Schema The sql schema
#' @param nom.table The sql Table
#' @return Retourne un dataframe contenant le nombre de valeurs manquantes (NA, Zero, Vide), la part qu'elles représentent sur l'ensemble du dataframe.
#' @export
#' @examples resumedf_sql(conn = conn, Schema = 'dbo' , nom.table = 'mtcars')
#' resumedf_sql()

resumedf_sql <- function(conn, Schema, nom.table) {
  q1 <- dplyr::tbl(conn, dbplyr::in_schema(Schema, nom.table) )
  q2 <- dplyr::collect(q1) # lancer la requête SQL et charger dans R
  #nom <- paste0(nom.table,"_", "Sql") # constuire le nom de l'objet à partir du nom de la table et de la date
  #assign(nom ,q2, envir = .GlobalEnv) # assigner le fruit de la requête (q2) au nom = création de l'objet nommé
  return(resumedf(q2))
}
