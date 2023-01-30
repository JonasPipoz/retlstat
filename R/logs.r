#
#' logs
#'
#' Cette fonction ajoute une ligne dans la table de logs des modifications apportées aux tables
#' @param methode la méthode de mise à jour: 'upsert','table_create','drop_replace'
#' @param bdd The sql database name
#' @param schema The sql schema
#' @param table_name The sql Table
#' @export
#' @examples logs(methode, bdd, schema, table_name, user)
#' logs()

logs <- function(methode, bdd, schema, table_name){
  require(lubridate)
  require(DBI)
  require(odbc)

  conn <- DBI::dbConnect(odbc::odbc(),
                         Driver = "SQL Server",
                         Server = "NESTATSQL1",
                         Database = "STATPRODTEMP",
                         encoding = "latin1")

  time <- lubridate::now()
  user <- Sys.info()[7]
  project <- basename(getwd())

  log <- data.frame(methode = toupper(methode),
                    bdd = toupper(bdd),
                    schema = toupper(schema),
                    table = toupper(table_name),
                    time = time,
                    user = user,
                    project = project ,row.names = NULL)
  print(log)

  query <- paste0("INSERT INTO [STATPRODTEMP].[DBO].[MAJ_TABLE_LOGS] (methode_,bdd_,schema_,table_,time_,user_,project_) VALUES ('",
                  toupper(methode),
                  "','",toupper(bdd),
                  "','",toupper(schema),
                  "','",toupper(table_name),
                  "','",time,
                  "','",user,
                  "','",project,"')")
  print(query)
  DBI::dbSendQuery(conn = conn, statement = query)

}


################# TEST ##################################

#logs(methode = 'upsert', bdd = 'statprodtemp', schema = 'GEN', table_name = 'nom_de_ma_table')
