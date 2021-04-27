





dates.periode <- function(vecdate){
  require(lubridate)
  require(dplyr)
# Fonction qui mesure les intervals de date et défini si l'interval est mensuel, annuel, trimestriel et constant
  dates <- as.Date(as.character(vecdate),format=date.format(vecdate))
  if (is.na(dates)) {
    cat('Transformation des dates')
    dates <- as.Date(as.character(vecdate),format="%d.%m.%Y")

  }

  inter <- c()
  for (prem in seq(length(dates) -1)) {
    sec = prem + 1
    # vecteur d'interval de date
    inter <- c(inter, unique(sort(dates))[sec]-unique(sort(dates))[prem] )


  }

  medin <- median(inter, na.rm = T)
  meanin <- mean(inter, na.rm = T)
  maxin <- max(inter, na.rm = T)
  minin <- min(inter, na.rm = T)


  constance <- case_when(abs((meanin - medin) / medin) < 0.05 ~ "Ecart de date constant",
                         abs((meanin - medin) / medin) > 0.05  ~ "Ecart de date variable"

  )

  period <- case_when( 29 <= meanin & meanin <= 31  ~ "Données mensuelles",
                       1 == meanin  ~ "Données quotidienne",
                       360 <= meanin & meanin <= 366  ~ "Données annuelles",
                       88 <= meanin & meanin <= 95  ~ "Données trimestrielles",
                       TRUE ~ 'Aucune périodicité')


return(c(constance,period))
}


find.dates <- function(df){
  dates.found = c()

  for (col in colnames(df)) {
    if (class(df[,col]) %in% c('Date','character')) {
      if(is.date(df[,col]) == T){
        dates.found <- c(dates.found, col)
      }
    }
  }
return(dates.found)


}



is.date <- function(string){
  dixok = F
  nbchiffre = F
  string <- as.character(string)
  x <- string[1]
  i = 1
  n = 0
  while(string[i] == ''){
    n = i+1
    i = i + 1
  }
  if (n != 0) {
    x <- string[n]
  }
  if (nchar(x) == 10) {
    dixok = T
  }
  if (nchar(gsub(x ,pattern = '[^0-9]',replacement = "")) == 8) {
    nbchiffre = T
  }
if (dixok == T & nbchiffre == T) {
  return(T)
}else
  return(F)
}

date.format <- function(stringdate){
  stringdate <- as.character(stringdate)
  x <- gsub(stringdate[1] ,pattern = '[0-9]',replacement = "")
  i = 1
  n = 0
  while(stringdate[i] == ''){
    n = i+1
    i = i + 1
  }
  if (n != 0) {
    x <- gsub(stringdate[n] ,pattern = '[0-9]',replacement = "")
  }
  if (x == '..') {
    dateformat = '%d.%m.%Y'
  }else if (x == '//') {
    dateformat = '%Y/%m/%d'
  }else if (x == '--') {
    dateformat = '%Y-%m-%d'
  }else{
    dateformat = 'Unknown'
  }

  return(dateformat)
}

df$d3 <- as.Date(df$d3)


dates.periode(df$d1)


find.dates(df)

date.format(df$d3)
is.date(df$d3)

df <- read.csv2("./data/test_dates.csv")
find.dates(df)
is.date('2020/01/02')
gsub('01.01.2020' ,pattern = '[0-9]',replacement = "")





gsub(df$d1 ,pattern = '[0-9]',replacement = "")

class(df$d1)
class('01-01-2020')
typeof('01-01-2020')
ti <- '01.01.2020'
ti[1]
i = 1
while(is.na(stringdate[i])){
  print('toto')
  i = i + 1
}
