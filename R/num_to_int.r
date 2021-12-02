#' A numeric to integer
#'
#' This function transform data type that are trully integer but stored in numeric format into integer format.
#' @param df A dataframe containing numeric variables that as to be stored as integer. Default is null
#' @keywords cats
#' @export
#' @examples
#' num_to_int()


# Check l'existance de la table et crée une nouvelle si elle n'existe pas.

# test le type des variables et crée la table en conséquence... ex: double --> Test si la variable est composé que d'entier.
num_to_int <- function(df = NULL){
  # Mutate des variables numériques en entier si nécessaire.
  # Si la somme de la retenu de la division par 1 == 0 --> Transforme la variable en entier.
  suppressWarnings(df <- dplyr::mutate_if(df, ~ (is.numeric(.) & (sum(as.numeric(.) %% 1, na.rm = T) == 0)),
                            dplyr::funs(as.integer(.))))
  return(df)

}

