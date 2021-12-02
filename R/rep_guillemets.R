# Fonction pour remplacer les apostrophes par des doubles apostrophes
# Sql server n'accept pas les apostrophe simple dans les string

rep_guillemets <- function(df){
  names(df)<- unlist(strsplit(  paste0("[",paste(names(df), collapse = "], ["),"]"), split=", "  ))
  df <- as.data.frame(df)
  for(col in 1:dim(df)[2]){
    if(is.factor(df[,col])){
      df[,col] <- as.character(df[,col])
    }
    if(is.character(df[,col])){
      for (el in 1:dim(df)[1]){
        if(grepl("'",df[el,col], fixed = T)){
          df[el,col]<- gsub("'","''",df[el,col])
          df[el,col] <- gsub("’","''",df[el,col])
        }

      }
    }
  }
  cat(green('\nGuillemets remplacés \n'))
  return(df)
}

