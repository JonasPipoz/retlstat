# Fonction en construction...
# But: ouvrir un df dans excel pour apporter des modifications et retourner le df modifié

modif_df <- function(df, file.type = 'excel'){
  require(xlsx)
  require(lubridate)
  require(stringr)
  # Créer un ficher temp
  dir <- getwd()
  if (tolower(file.type) == 'excel') {
    file <- paste0('/file_temp_',as_date(lubridate::now()),'.xlsx')
    write.xlsx(df,paste0( dir , file ), row.names = F)
  }else{
    file <- paste0('/file_temp_',as_date(lubridate::now()),'.csv')
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
  # Attendre que les modifs soient enregistrés
  #
  #while(subset(changedFiles(sn)$changes, rownames(changedFiles(sn)$changes) == stringr::str_replace(file,'/',''))[6] == F){
  while (file.opened(paste0(dir,file))==T) {

    Sys.sleep(1)
  }
  if (tolower(file.type)=='excel') {
    df <- read.xlsx(paste0( dir , file ), sheetIndex = 1)
  }else{
    df <- read.table(paste0( dir , file ), sep = ';')
  }

  file.remove(paste0( dir , file ))
  return(df)
}

file.opened <- function(path) {
  suppressWarnings(
    "try-error" %in% class(
      try(file(path,
               open = "w"),
          silent = TRUE
      )
    )
  )
}

modif_df(df,file.type = 'csv')

cbind(table(unlist(df.mat2[,c(a:b)])))

library(stringr)
setwd('c:/temp')
getwd()

dir <- getwd()


df <- read.csv2(paste0(dir,'/Codes_etat.csv'))

write.table(df , paste0( dir , '/temp1234.csv' ) , sep=';',row.names = F)

sn <- fileSnapshot(path = dir, file.info = TRUE, timestamp = tempfile("timestamp"),
                   md5sum = T)
write.table(head(df),paste0(dir,'/temp1234.csv'), sep=';',row.names = F)


if (changedFiles(sn)$changes == F) {
  print('file as changed')
}
changedFiles(sn)$changes

changedFiles(before, after, path = before$path, timestamp = before$timestamp,
             check.file.info = c("size", "isdir", "mode", "mtime"),
             md5sum = before$md5sum, digest = before$digest,
             full.names = before$full.names, ...)

