#
#' update_or_insert2
#'
#' This function mets a jour les données PAR BATCH de 100 dans une table si l'id existe dans la table sql server ou alors ajoute les données a partir d'un dataframe
#' @param df A dataframe containing the data. Default is null
#' @param conn A DBI connection
#' @param BDD The sql database name
#' @param Schema The sql schema
#' @param table_name The sql Table
#' @param id a unique identifier per row.

#' @examples update_or_insert2(df = mtcars, conn = conn, BDD = 'STATPRODTEMP',Schema = 'GEN',table_name = 'MTCARS',id = rownames(mtcars))
#' update_or_insert2()



update_or_insert2 <- function(df,conn, BDD, Schema, table_name, id) {
  .Deprecated("upsert")
  df <- rep_guillemets(df)
  df <- num_to_int(df)
  rowlist<-c()
  batchsize <- 20
  done <- 0
  ncharquery <-0
  ncharqueries1 <- 0
  n.batch <- nrow(df)/batchsize - (nrow(df)/batchsize) %% 1
  residurow <- ((nrow(df)/batchsize) %% 1 )*batchsize

  cat('num to int ok! \n')

  col_data <- names(df)
  n<-1
  for (b in seq(0,n.batch,1)){

    #print(paste('b=',b))
    #print(paste('n_batch = ',n.batch))
    #print(seq(b*100+1,min(((b+1)*100),((n.batch*100)+residurow)),1))                                                                          #
    queries <- ''
    #cat(paste(done,n.batch,'\n'))

    for (row in seq(n,min(((b+1)*batchsize+1),round(((n.batch*batchsize+1)+residurow-1))),1)) {
      n<-row

      #print(paste(row,n.batch,b*batchsize+1,'min(',((b+1)*batchsize),((n.batch*batchsize)+residurow),residurow-1,')','min=',min(((b+1)*batchsize),round(((n.batch*batchsize)+residurow)))))
      #print(paste('row: ',row))                                                                          #
      values <- ''
      for (i in 1:dim(df)[2]) {
        if (i == 1) {
          if (is.na(df[row, i])) {
            #print('1')                                                                          #
            values = paste0(values, 'NULL', sep = '')
          }else {
            values = paste0(values, "'", as.character(df[row, i]), "'", sep = '')
          }
        }else {
          #print('2')                                                                          #
          if (is.na(df[row, i])) {
            #print('2')                                                                          #
            values = paste0(values, ',NULL', sep = '')
          }else {
            values = paste0(values, ",'", as.character(df[row, i]), "'", sep = '')
          }
        }
      }

      pairs <- ''

      for (i in 1:dim(df)[2]) {
        if (i == 1) {
          #print('3')                                                                          #
          if (is.na(df[row, i])) {
            pairs = paste0(pairs, ' ', names(df[i]), ' = NULL', sep = '')
          }else {
            pairs = paste0(pairs, ' ', names(df[i]), " = '", as.character(df[row, i]), "'", sep = '')
          }
        }else {
          if (is.na(df[row, i])) {
            #print('4')                                                                          #
            pairs = paste0(pairs, ',', names(df[i]), ' = NULL', sep = '')
          }else {
            pairs = paste0(pairs, ",", names(df[i]), " = '", as.character(df[row, i]), "'", sep = '')
          }
        }
      }

      query <- sprintf("if exists (select * from [%s].[%s].[%s] where %s = '%s') begin update [%s].[%s].[%s] set %s where %s = '%s' end else begin insert into [%s].[%s].[%s] (%s) values (%s) end", BDD, Schema, table_name, id, df[row, id], BDD, Schema, table_name, pairs, id, df[row, id], BDD, Schema, table_name, paste0(col_data, collapse = ','), values)
      #query <- sprintf("begin tran if exists (select * from [%s].[%s].[%s] where %s = '%s') begin update [%s].[%s].[%s] set %s where %s = '%s' end else begin insert into [%s].[%s].[%s] (%s) values (%s) end commit tran", BDD, Schema, table_name, id, df[row, id], BDD, Schema, table_name, pairs, id, df[row, id], BDD, Schema, table_name, paste0(col_data, collapse = ','), values)
      ncharqueries1 <- ncharqueries1 + nchar(query)+1
      queries <- paste(queries,query, sep=' ')
      ncharquery <- c(ncharquery,nchar(query))
      batchsize <- round(200000 / mean(ncharquery))
      cat(paste(as.character(batchsize),'\r'))
      n.batch <- nrow(df)/batchsize - (nrow(df)/batchsize) %% 1
      residurow <- ((nrow(df)/batchsize) %% 1 )*batchsize


    }
    print(ncharqueries1 == nchar(queries))
    print(paste(ncharqueries1,nchar(queries)))
    ncharqueries1 <- 0
    odbc::dbGetQuery(conn,queries)
    #print(paste('nombre de caractère:',nchar(queries)))

    queries <- ""
    done <- done + 1


    cat(paste('Mise à jour et ajout des données batch:',as.character(done),'/', as.character(n.batch+1) ,' terminé \n'))
    #print(queries)
    if(done==n.batch+1){

      break
    }
  }

  return(NULL)
}


# test
#library(dplyr)
#dch <- read.csv("P:/STAT/Cockpit CE/Cockpits/MAJ/0_Données/SOURCES/LAMDA/donnees_chomage_benchmark.csv",
#                sep = ';',
#                stringsAsFactors = F)
## Ajout de la varible ID
#
#dch2 <- dch %>% mutate(id = paste0(as.character(Mois),'-',Canton))
#dch2$Mois <- as.Date(dch2$Mois, format = '%d.%m.%Y')
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
## Mise à jour des données existantes et ajout des nouvelles.
#update_or_insert2(df=dch2, conn = conn, BDD = bdd, Schema = schema, table_name = table, id = 'id')

