#
#' col_store
#'
#' This function create a columns stored index on a specified table
#' @param conn A DBI connection
#' @param bdd The sql database name
#' @param schema The sql schema name
#' @param table The sql Table name
#' @param drop default is FALSE, if True, drop the existing index first to create the new one.
#' @export
#' @examples col_store(conn = conn, BDD = 'STATPRODTEMP',Schema = 'GEN',table_name = 'MTCARS',drop = FALSE)
#' col_store()
#'

col_store <- function(conn, bdd, schema, table_name, drop = FALSE ){
  nom.indexe <- paste0("I_",table_name)
  if(drop == FALSE){
    dr <- 'OFF'
  }else{
    dr <- 'ON'
  }
  query <- paste0("CREATE CLUSTERED COLUMNSTORE INDEX [",nom.indexe ,"] ON [",bdd,"].[",schema,"].[",table_name,"] WITH  (DROP_EXISTING = ",dr,") ;")
  #--default is OFF, Si il existe d?j? un index sur la table, il est possible de le remplacer par celui-ci ( DROP_EXISTING = ON)

  odbc::dbSendQuery(conn,query)
}
