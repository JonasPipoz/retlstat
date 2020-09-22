#
#' drop_replace
#'
#' This function crée une table a partir d'un dataframe
#' @param df A dataframe containing the data. Default is null
#' @param conn A DBI connection
#' @param BDD The sql database name
#' @param Schema The sql schema
#' @param table_name The sql Table

#' @export
#' @examples drop_replace(df = mtcars, conn = conn, BDD = 'STATPRODTEMP',Schema = 'GEN',table_name = 'MTCARS')
#' drop_replace()
drop_replace <- function(df = NULL, conn = NULL, BDD = NULL, Schema = NULL, table_name = NULL){
  areyousure <- readline(prompt= paste0("Etes-vous sûre de vouloir supprimer les données contenues dans la table [",bdd,"].[",schema,"].[",table_name,"]? o/n : "))

  if (areyousure == 'o'){
    df <- num_to_int(df)
    print('Formats de nombre modifiés.')
    table_id <- DBI::Id(schema = Schema,
                        table = table_name)
    DBI::dbWriteTable(conn = conn,
                      name = table_id,
                      value = df,
                      append = F,
                      overwrite = T)

  cat(paste('Table', table_name, 'mise-à-jour par écrasement.', as.character(dim(df)[1]),'observations de ',as.character(dim(df)[2]),'variables ont été ajoutées à la table.\n'))
  }
  else{
    cat('Oppération annulé')
  }

  return(NULL)
}
