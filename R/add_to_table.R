# ADD to table!
#
# Cette fonction ajoute des données à partir dans dataframe dans une table SQL existante
#
#' stat_add
#'
#' This function add a dataframe to an existing sql table.
#' @param df un dataframe contenant les données
#' @param conn une connexion DBI
#' @param BDD Le nom de la base de données sql
#' @param Schema Le schema sql
#' @param table_name Le nom de la table sql
#' @export
#' @examples stat_add(mtcars, conn, 'STATPRODTEMP','GEN','MTCARS')
#' stat_add()

stat_add <- function(df = NULL, conn = NULL, BDD = NULL, Schema = NULL, table_name = NULL, ask = TRUE){
  if (ask == T) {
    areyousure <- readline(prompt= paste0("Etes-vous sûre de vouloir ajouter ",
                                          nrow(df),"lignes dans la table [",BDD,"].[",Schema,"].[",
                                          table_name,"]? o/n : "))

  }else{
    areyousure <- 'o'
  }


  if (areyousure == 'o'){
    df <- num_to_int(df)
    print('Formats de nombre modifiés.')
    table_id <- DBI::Id(schema = Schema,
                        table = table_name)
    DBI::dbWriteTable(conn = conn,
                      name = table_id,
                      value = df,
                      append = T,
                      overwrite = F)

    cat(paste('Table', table_name,
              'mise-à-jour par ajout.',
              as.character(dim(df)[1]),
              'observations de '
              ,as.character(dim(df)[2]),
              'variables ont été ajoutées à la table.\n'))
    logs(methode = 'add_to_table',bdd = BDD,schema = Schema,table_name = table_name)
  }
  else{
    cat('Oppération annulée')
  }

  return(NULL)
}


