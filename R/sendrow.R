

sendrow <- function(conn, BDD, Schema, table_name, col_data, data) {
  query = paste0("INSERT INTO [", BDD, "].[", Schema, "].[", table_name, "] (", paste0(col_data, collapse = ','), ") VALUES (")
  for (i in 1:length(data)) {
    if (i==1){
      if (is.na(data[i])) {
        query = paste0(query, 'NULL', sep = '')
      }else {
        query = paste0(query, "'", as.character(data[i]), "'", sep = '')
      }
    }else{
      if (is.na(data[i])) {
        query = paste0(query, ',NULL', sep = '')
      }else {
        query = paste0(query, ",'", as.character(data[i]), "'", sep = '')
      }
    }
  }
  query = paste(query, ");")
  odbc::dbGetQuery(conn,query)
  cat('data inserted \n')
  return(NULL)
  }




#sendrow <- function(conn,BDD,Schema,table_name,col_data,data){
#  if (is.na(data)){
#    odbc::dbGetQuery(conn,paste0("INSERT INTO [", BDD,"].[",Schema, "].[" ,table_name,"] (",paste0(col_data,collapse=','),") VALUES ('",paste0(data,collapse="','"),"');"))
#  } else {
#    odbc::dbGetQuery(conn,paste0("INSERT INTO [", BDD,"].[",Schema, "].[" ,table_name,"] (",paste0(col_data,collapse=','),") VALUES ('",paste0(data,collapse="','"),"');"))
#  }
#    print('data inserted')
#  return(NULL)}
