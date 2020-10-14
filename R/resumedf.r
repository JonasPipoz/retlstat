#
#' resumedf
#'
#' This function mets a jour les données dans une table si l'id existe dans la table sql server ou alors ajoute les données a partir d'un dataframe
#' @param df un dataframe pour lequel un résumé est désiré
#' @export
#' @examples resumedf(df = mtcars)
#' resumedf()

resumedf <- function(df) {
  options(scipen = 999)
  nad <- data.frame(na = t(t(sapply(df, function(x) sum(is.na(x))))))

  zerod <-  data.frame(zero = t(t(sapply(df, function(x) sum(x == 0)) ) ) )

  vided <-  data.frame(vide = t(t(sapply(df, function(x) sum(as.character(x) == '')) ) ) )

  suppressWarnings(meand <-  data.frame(moyenne = t(t(sapply(df, function(x) round(mean(as.numeric(x)),2)) ) ) ))
  suppressWarnings(mind <-  data.frame(min = t(t(sapply(df, function(x) round(min(as.numeric(x)),2)) ) ) ))
  suppressWarnings(maxd <-  data.frame(max = t(t(sapply(df, function(x) round(max(as.numeric(x)),2)) ) ) ))

  typed <- data.frame(type = t(t(sapply(df, function(x) typeof(x)) ) ) )


  ret <- cbind(typed, nad, zerod, vided, meand, mind, maxd)
  #ret <- sapply(ret, function(x) replace_na(x, 0))
  ret
}


