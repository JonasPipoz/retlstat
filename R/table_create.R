#
#' table_create
#'
#' This function crée une table a partir d'un dataframe
#' @param df A dataframe containing the data. Default is null
#' @param conn A DBI connection
#' @param BDD The sql database name
#' @param Schema The sql schema
#' @param table_name The sql Table

#' @export
#' @examples table_create(df = mtcars, conn = conn, BDD = 'STATPRODTEMP',Schema = 'GEN',table_name = 'MTCARS')
#' table_create()
table_create <- function(df = NULL, conn = NULL, BDD = NULL, Schema = NULL, table_name = NULL){
  df <- num_to_int(df)
  table_id <- DBI::Id(schema = Schema,
                 table = table_name)
  DBI::dbWriteTable(conn = conn,
                    name = table_id,
                    value = df,
                    append = T,
                    overwrite = F)

cat(green(paste('Table', table_name, 'créée avec succès.', as.character(dim(df)[1]),'observations de ',as.character(dim(df)[2]),'variables ont été ajoutées à la table.\n')))
return(NULL)
}
