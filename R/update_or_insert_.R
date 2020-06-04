#
#' update_or_insert
#'
#' This function mets a jour les données dans une table si l'id existe dans la table sql server ou alors ajoute les données a partir d'un dataframe
#' @param df A dataframe containing the data. Default is null
#' @param conn A DBI connection
#' @param BDD The sql database name
#' @param Schema The sql schema
#' @param table_name The sql Table
#' @param id a unique identifier per row.
#' @export
#' @examples update_or_insert(df = mtcars, conn = conn, BDD = 'STATPRODTEMP',Schema = 'GEN',table_name = 'MTCARS',id = rownames(mtcars))
#' update_or_insert()


update_or_insert <- function(df,conn, BDD, Schema, table_name, id) {
  .Deprecated("upsert")
  df <- rep_guillemets(df)
  df <- num_to_int(df)
  id <- paste0("[",id,"]")
  print('num to int ok!')
  col_data <- names(df)
  queries <- ''
  print(head(df))
  for (row in 1:dim(df)[1]) {
    if(row%%1000 == 0 | row == 1){
      #cat(paste("création query:  ajout ligne : ",row,"/",nrow(df),"                                           \r"))
    }

    values <- ''

    for (i in 1:dim(df)[2]) {
      if (i == 1) {
        if (is.na(df[row, i])) {
          values = paste0(values, 'NULL', sep = '')
        }else {
          values = paste0(values, "'", as.character(df[row, i]), "'", sep = '')
        }
      }else {
        if (is.na(df[row, i])) {
          values = paste0(values, ',NULL', sep = '')
        }else {
          values = paste0(values, ",'", as.character(df[row, i]), "'", sep = '')
        }
      }
    }

    pairs <- ''

    for (i in 1:dim(df)[2]) {
      if (i == 1) {
        if (is.na(df[row, i])) {
          pairs = paste0(pairs, ' ', names(df[i]), ' = NULL', sep = '')
        }else {
          pairs = paste0(pairs, ' ', names(df[i]), " = '", as.character(df[row, i]), "'", sep = '')
        }
      }else {
        if (is.na(df[row, i])) {
          pairs = paste0(pairs, ',', names(df[i]), ' = NULL', sep = '')
        }else {
          pairs = paste0(pairs, ",", names(df[i]), " = '", as.character(df[row, i]), "'", sep = '')
        }
      }
    }

    query <- sprintf("if exists (select * from [%s].[%s].[%s] where %s = '%s') begin update [%s].[%s].[%s] set %s where %s = '%s' end; else begin insert into [%s].[%s].[%s] (%s) values (%s) end ;", BDD, Schema, table_name, id, df[row, id], BDD, Schema, table_name, pairs, id, df[row, id], BDD, Schema, table_name, paste0(col_data, collapse = ','), values)
    #query <- sprintf("begin tran if exists (select * from [%s].[%s].[%s] where %s = '%s') begin update [%s].[%s].[%s] set %s where %s = '%s' end else begin insert into [%s].[%s].[%s] (%s) values (%s) end commit tran", BDD, Schema, table_name, id, df[row, id], BDD, Schema, table_name, pairs, id, df[row, id], BDD, Schema, table_name, paste0(col_data, collapse = ','), values)
    queries <- paste(queries,query, sep=' ')


    if(nchar(queries) > 100000){ #(row%%200 == 0){
      #cat(paste("ligne : ",row,"/",nrow(df),"Envoi de la requete. Peut durer un certain temps.                                    \r"))
      dbBegin(conn)
      res<-odbc::dbSendStatement(conn,queries)
      print("-----------------------------")
      print(odbc::dbHasCompleted(res))
      print("-----------------------------")
      cat(paste("nombre de ligne affectées: ",odbc::dbGetRowsAffected(res),'\n'))
      odbc::dbCommit(conn)
      odbc::dbClearResult(res)
      #print(queries)
      #cat(paste(res,'\r'))
      queries <- ""
    }

  }

  #cat(paste("ligne : ",row,"/",nrow(df),"Envoi de la requete. Peut durer un certain temps.                                    \r"))
  #res <- odbc::dbGetQuery(conn,queries)
  dbBegin(conn)
  res <- odbc::dbSendStatement(conn, queries)
  cat(odbc::dbHasCompleted(res))
  print("-----------------------------")
  print(odbc::dbHasCompleted(res))
  print("-----------------------------")
  cat(paste("nombre de ligne affectées",odbc::dbGetRowsAffected(res),'\n'))
  odbc::dbCommit(conn)
  odbc::dbClearResult(res)

  #print(queries)

  #cat(paste("ligne : ",row,"/",nrow(df)," Ajouté ou mise à jour                                                               \n"))
  cat(sprintf('Data updated or inserted into %s.%s.%s                           \n',BDD,Schema,table_name))

  return(NULL)
}

### TEST
#library(dplyr)
#df <- mtcars %>% mutate(id = row.names(mtcars))
#dch <- read.csv("P:/STAT/Cockpit CE/Cockpits/MAJ/0_Données/SOURCES/LAMDA/donnees_chomage_benchmark.csv",
#                sep = ';',
#                stringsAsFactors = F)
## Ajout de la varible ID
#
#dch2 <- dch %>% mutate(id = paste0(as.character(Mois),'-',Canton))
#
## Connexion à la base de donnée sql
#conn <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "NESTATSQL1",
#                       Database = "STATPRODTEMP", encoding = "latin1")
#
## Varibles d'attribut de la table
#
#schema <- 'REV'
#table <- '12005086_CHOMAGE'
#bdd <- 'STATPRODTEMP'
#
#
#::table_create(conn = conn, table_name = table, Schema = schema, df = dch2, BDD = bdd )


#update_or_insert(df = head(dch2,1), conn = NULL, Schema = schema,BDD = bdd,table_name = table, id = id)
