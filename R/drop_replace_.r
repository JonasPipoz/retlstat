#
#' drop_replace
#'
#' cette fonction remplace les données d'une table existante
#' @param df Un data frame contenant les nouvelles données
#' @param conn Une connection DBI
#' @param BDD Le nom de la base de donnée
#' @param Schema Le schéma SQL server dans lequel la table se trouve
#' @param table_name Le nom de la table sql

#' @export
#' @examples drop_replace(df = mtcars, conn = conn, BDD = 'STATPRODTEMP',Schema = 'GEN',table_name = 'MTCARS')
#' drop_replace_()
drop_replace_ <- function(df = NULL, conn = NULL, BDD = NULL, Schema = NULL, table_name = NULL){
  require(crayon)
  require(dplyr)
  areyousure <- 'o'

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

  cat(paste('Table', table_name, 'mise-à-jour par écrasement.',
            as.character(dim(df)[1]),'observations de ',
            as.character(dim(df)[2]),'variables ont été ajoutées à la table.\n'))
  logs(methode = 'drop_replace', bdd = BDD, schema = Schema, table_name = table_name)
  }
  else{
    cat('Oppération annulée')
  }

  return(NULL)
}
