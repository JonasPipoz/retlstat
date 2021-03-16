#' modif_df
#'
#' Cette fonction ouvre un df dans excel pour apporter des modifications et retourner le df modifié
#' @param df Un dataframe
#' @export
#' @examples df <- modif_df(df = iris)
#' df <- modif_df(df = iris)
#'
modif_df <- function(df, file.type = 'excel'){
  require(xlsx)
  require(lubridate)
  require(stringr)
  # Créer un ficher temp
  dir <- getwd()
  if (tolower(file.type) == 'excel') {
    stop("La fonction 'modifier dans excel ne fonctionne pas encore. Il faut utiliser file.type = 'csv'\n")

    #file <- paste0('/file_temp_',str_replace_all(as_datetime(lubridate::now()),"[^0-9]",""),'.xlsx')
    #write.xlsx(df,paste0( dir , file ), row.names = F)
  }else{
    file <- paste0('/file_temp_',str_replace_all(as_datetime(lubridate::now()),"[^0-9]",""),'.csv')
    write.table(df,paste0( dir , file ), sep = ';',row.names = F)
  }


  #write.table(df , paste0( dir , '/temp1234.csv' ) , sep=';',row.names = F)
  sn <- fileSnapshot(path = dir, file.info = TRUE, timestamp = tempfile("timestamp"),
                     md5sum = T)
  # Ouvrir le fichier dans excel
  excel_file_with_path <- paste0( dir , file )
  command <- paste("open excel", excel_file_with_path)
  system(command)
  sn <- fileSnapshot(path = dir, file.info = TRUE, timestamp = tempfile("timestamp"),
                     md5sum = T)

  quest <- readline('Le documents CSV est enregistré et fermé? (o/n) :')
  if (quest == 'o') {
    if (tolower(file.type)=='excel') {
      df <- read.xlsx(paste0( dir , file ), sheetIndex = 1)
    }else{
      df <- read.table(paste0( dir , file ), sep = ';',header = T)
    }

    file.remove(paste0( dir , file ))
    cat("Données chargées")
    return(df)
  }else{
    file.remove(paste0( dir , file ))
    cat("Les données n'ont pas été chargées")
    return(NULL)
  }

  file.remove(paste0( dir , file ))
  return(NULL)
}
