#
#' resumedf
#'
#' This function mets a jour les données dans une table si l'id existe dans la table sql server ou alors ajoute les données a partir d'un dataframe
#' @param df un dataframe pour lequel un résumé est désiré
#' @return Retourne un dataframe contenant le nombre de valeurs manquantes (NA, Zero, Vide), la part qu'elles représentent sur l'ensemble du dataframe.
#' @export
#' @examples resumedf(df = iris)
#' resumedf()

resumedf <- function(df) {
  options(scipen = 999)
  cat(paste0("\n\n##########################################\n"))
  cat(paste0("##    n. lignes         :       ",nrow(df),"\n"))
  cat(paste0("##    n. colonnes       :       ",ncol(df),"\n"))
  cat(paste0("##########################################\n"))
  cat(paste0("##########################################\n"))
  cat(paste0("##########################################\n\n"))

  formatdf <- sapply(df, function(x) !all(is.na(as.Date(as.character(x),format="%Y-%m-%d"))))
  if (T %in% formatdf) {

  cat(paste0(" ---- Valeurs maximums des dates: ----- \n"))
  i <- 1
  for (var in formatdf) {
    if (var == T) {
      cat(paste0("La valeur maximum de la variable : '",names(formatdf)[i],"' est :\n"))
      cat(paste0(max(as.Date(as.character(df[[i]]),format="%Y-%m-%d")), na.rm= T), "\n")
      df[[i]] <- as.Date(df[[i]])
      }
    i = i + 1
    }
  }
  nad <- data.frame(na = t(t(sapply(df, function(x) sum(is.na(x))))))
  unicited <- data.frame(unicite = t(t(sapply(df, function(x) ifelse(sum(duplicated(x))==0,TRUE,FALSE)))))


  zerod <-  data.frame(zeros = t(t(sapply(df, function(x) sum(x == 0)) ) ) )

  vided <-  data.frame(vide = t(t(sapply(df, function(x) sum(as.character(x) == '')) ) ) )

  suppressWarnings(meand <-  data.frame(moyenne = t(t(sapply(df, function(x) round(mean(as.numeric(x), na.rm = T),2)) ) ) ))
  suppressWarnings(mind <-  data.frame(min = t(t(sapply(df, function(x) round(min(as.numeric(x), na.rm = T),2)) ) ) ))
  suppressWarnings(maxd <-  data.frame(max = t(t(sapply(df, function(x) round(max(as.numeric(x), na.rm = T),2)) ) ) ))

  vided <- transform(vided, pourcent.manquant = round((replace(nad$na, NA, 0) + replace(vided$vide, NA, 0))/nrow(df)*100,2))

  typed <- data.frame(type = t(t(sapply(df, function(x) typeof(x)) ) ) )

  ret <- cbind(typed, nad, vided, zerod, meand, mind, maxd, unicited)
  ret
}

