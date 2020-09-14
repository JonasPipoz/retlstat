#
#' drop_table
#'
#' Cette fonction supprime une table spécifiée.
#' @param conn A DBI connection
#' @param bdd The sql database name
#' @param schema The sql schema name
#' @param table The sql Table name
#' @export
#' @examples drop_table(conn = conn, BDD = 'STATPRODTEMP',Schema = 'GEN',table_name = 'MTCARS')
#' col_store()
#'

drop_table <- function(conn, bdd, schema, table_name){

  query <- paste0("DROP TABLE [",bdd,"].[",schema,"].[",table_name,"] ;")

  areyousure <- readline(prompt= paste0("Etes-vous sûre de vouloir supprimer la table [",bdd,"].[",schema,"].[",table_name,"]? o/n : "))

  if (areyousure == 'o'){

    print(query)
    cat(paste0("Table: [",bdd,"].[",schema,"].[",table_name,"] supprimée.") )
    odbc::dbSendQuery(conn,query)

  }
  else{
    cat('Oppération annulé')
  }


}


