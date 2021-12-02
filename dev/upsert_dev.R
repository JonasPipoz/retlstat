#
#' upsert
#'
#' This function mets a jour les données dans une table si l'id existe dans la table sql server ou alors ajoute les données a partir d'un dataframe
#' @param df A dataframe containing the data. Default is null
#' @param conn A DBI connection
#' @param BDD The sql database name
#' @param Schema The sql schema
#' @param table_name The sql Table
#' @param id a unique identifier per row.
#' @export
#' @examples upsert(df = mtcars, conn = conn, BDD = 'STATPRODTEMP',Schema = 'GEN',table_name = 'MTCARS',id = rownames(mtcars))
#' upsert()


upsert <- function(df,conn, BDD, Schema, table_name, id) {
  # Create tempory table
  tempname <- paste0('temp_', random::randomStrings(1,upperalpha = T,len = 5,digits = F),'_', stringr::str_replace_all(Sys.time(),'[ :-]','') )
  table_create(conn=conn,BDD=BDD,Schema=Schema,table_name=tempname,df=df)
  cat(paste0('Table temporaire : [',BDD,'].[',Schema,'].[',tempname,'] créé.'))

  df <- rep_guillemets(df)
  df <- num_to_int(df)
  id <- paste0("[",id,"]")
  print('num to int ok!')
  col_data <- names(df)
  queries <- ''

  # Merge tempory table with target table

  pairs <- ''
  pairsup <- ''
  pairs = paste0(pairs, "[Target].", names(df), " = [Source].", names(df), sep = '')
  for (i in 1:length(pairs)){
    if (i == 1) {
      pairsup <- paste0(pairsup,pairs[i], sep='')
    } else {
      pairsup <- paste0(pairsup,', ',pairs[i], sep='')
    }
  }
  pairs2 <- ''
  pairsint <- ''
  pairs = paste0(pairs2, names(df),sep = '')
  for (i in 1:length(pairs)){
    if (i == 1) {
      pairsint <- paste0(pairsint,pairs[i], sep='')
    } else {
      pairsint <- paste0(pairsint,', ',pairs[i], sep='')
    }
  }
  pairs3 <- ''
  pairsins <- ''
  pairs3 = paste0(pairs3, "[Source].", names(df),sep = '')
  for (i in 1:length(pairs3)){
    if (i == 1) {
      pairsins <- paste0(pairsins,pairs3[i], sep='')
    } else {
      pairsins <- paste0(pairsins,', ',pairs3[i], sep='')
    }
  }

  query = paste0("MERGE ", '[',BDD,'].[',Schema,'].[',table_name,']', " AS [Target] USING ", '[',BDD,'].[',Schema,'].[',tempname,']', " AS [Source] ON [Target].",id, " = [Source].",id," WHEN MATCHED THEN UPDATE SET ",pairsup," WHEN NOT MATCHED THEN INSERT (",pairsint,") VALUES (",pairsins,");")
  cat("------------------------------------------------------- \n")
  res <- odbc::dbSendStatement(conn,query)
  cat(paste0("Nombre de ligne modifiés ou ajoutées :",odbc::dbGetRowsAffected(res), '\n'))
  odbc::dbClearResult(res)
  # Drop temporary table
  query <- paste0("DROP TABLE ",'[',BDD,'].[',Schema,'].[',tempname,']')
  res2 <- odbc::dbSendStatement(conn,query)
  cat(paste0("Table temporaire supprimée. lignes contenues :",odbc::dbGetRowsAffected(res2), '\n'))
  odbc::dbClearResult(res2)
  cat("------------------------------------------------------- \n")
}


################ TEST #######################
#df1 <- data.frame("SN" = 1:5, "AGE" = c(21,15,12,15,17), "NAME" = c("John","Dora","Tony","Stephane","Sophie"))
#df2 <- data.frame("SN" = 1:7, "AGE" = c(29,14,12,15,17,2,11), "NAME" = c("John","Dora","Tony","Stephane","Sophie","Max","James"))
#bdd<- "STATTEST"
#schema <- "dbo"
#table <- "_test_upsert"
#
#conn <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "NESTATSQL1",
#                       Database = bdd, encoding = "latin1")
#
#library(dplyr)
#table_create(table_name = table, conn = conn, Schema=schema, BDD = bdd, df = df1)
#
#upsert(df = df2,conn = conn ,BDD = bdd,Schema = schema ,table_name = table,id = 'SN')


########################

#df <- data.frame("SN" = 1:2, "Age" = c(21,15), "Name" = c("John","Dora"))
#pairs2 <- ''
#pairsint <- ''
#pairs2 = paste0(pairs2, "[Target].", names(df),sep = '')
#for (i in 1:length(pairs2)){
#  if (i == 1) {
#    pairsint <- paste0(pairsint,pairs2[i], sep='')
#  } else {
#    pairsint <- paste0(pairsint,', ',pairs2[i], sep='')
#  }
#}
#pairsint
#
#pairs3 <- ''
#pairsins <- ''
#pairs3 = paste0(pairs3, "[Source].", names(df),sep = '')
#for (i in 1:length(pairs3)){
#  if (i == 1) {
#    pairsins <- paste0(pairsins,pairs3[i], sep='')
#  } else {
#    pairsins <- paste0(pairsins,', ',pairs3[i], sep='')
#  }
#}
#pairsins
#
