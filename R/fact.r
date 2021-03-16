#
#' fact
#'
#' Cette fonction affiche toutes les valeurs et le nombre d'occurence des facteurs d'un dataframe
#' @param df Un dataframe
#' @param n.distinct Dans le cas où les facteurs sont en classe caractère, n.distinct permet de définir le nombre
#' de valeurs distincte maximale qu'une variable caractère peut prendre en étant considérée comme une variable de type facteur.
#' @export
#' @examples fact(df = iris, n.distinct = 10)
#' fact(df = iris, n.distinct = 5)
#'

fact <- function(df, n.distinct = 10){
  require(dplyr)
  n = 0
  for (col in colnames(df)) {
    if (class(df[,col])%in%c("character","factor")) {
      if (df[,col]%>% as.factor() %>% levels() %>% length() < n.distinct) {
        cat("\n")
        cat(paste(col,":\n"))
        print(table(df[,col], useNA = 'always') %>% cbind())
        n = n+1
      }
    }
  }
  if (n == 0) {
    cat("Aucune variable n'a pu être considérée comme un facteur.\n
        Vérifier la valeur de 'n.distinct' (défaut: 10)")
  }
}

