#
#' stat_update
#'
#' This function mets a jour les données dans une table sql server a partir d'un dataframe
#' @param df A dataframe containing the data. Default is null
#' @param conn A DBI connection
#' @param BDD The sql database name
#' @param Schema The sql schema
#' @param table_name The sql Table
#' @param id a unique identifier per row.
#' @export
#' @examples stat_update(df = mtcars, conn = conn, BDD = 'STATPRODTEMP',Schema = 'GEN',table_name = 'MTCARS',id = rownames(mtcars))
#' stat_update()
stat_update <- function(df = NULL, conn, BDD, Schema, table_name, id){
  df <- rep_guillemets(df)

  for(row in 1:dim(df)[1]){
    for(col in 1:dim(df)[2]){
      if (is.na(df[row,col])){
        odbc::dbGetQuery(conn,paste0("UPDATE [", BDD,"].[",Schema, "].[" ,table_name,"] SET ", names(df[col]), " = NULL WHERE ",id," = '",df[row,id],"'", sep=''))

      } else {
        odbc::dbGetQuery(conn,paste0("UPDATE [", BDD,"].[",Schema, "].[" ,table_name,"] SET ", names(df[col]), " = '", df[row,col], "' WHERE ",id," = '",df[row,id],"'", sep=''))
      }
    }
  }
  cat('data updated\n')
  return(NULL)
}
