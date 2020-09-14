#
#' update_or_insert2
#'
#' This function mets a jour les données PAR BATCH de 100 dans une table si l'id existe dans la table sql server ou alors ajoute les données a partir d'un dataframe
#' @param df A dataframe containing the data. Default is null
#' @param conn A DBI connection
#' @param BDD The sql database name
#' @param Schema The sql schema
#' @param table_name The sql Table
#' @param id a unique identifier per row.
#' @examples update_or_insert2(df = mtcars, conn = conn, BDD = 'STATPRODTEMP',Schema = 'GEN',table_name = 'MTCARS',id = rownames(mtcars))
#' update_or_insert2()



update_or_insert2 <- function(df,conn, BDD, Schema, table_name, id) {
  .Defunct("upsert")
}
