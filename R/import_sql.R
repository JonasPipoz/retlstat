#
#' import_sql
#'
#' Cette fonctio charge dans l'environnement R la une table sql dont le nom et le schéma sont passé en argument.
#' @param conn Une connexion DBI
#' @param Schema Le nom du schéma sur lequel la table se trouve
#' @param TableSQL Le nom de la table SQL
#' @return charge dans l'environnement R la table SQL
#' @export
#' @examples import_sql(conn = conn, Schema = 'GEN',TableSQL = '[11001041_EGID]')
#' import_sql()
#'
import_sql  <- function(conn, Schema, TableSQL) {
q1 <- dplyr::tbl(conn, dbplyr::in_schema(Schema, TableSQL) )
q2 <- dplyr::collect(q1) # lancer la requête SQL et charger dans R
nom <- paste0(TableSQL,"_", "Sql") # constuire le nom de l'objet à partir du nom de la table et de la date
assign(nom ,q2, envir = .GlobalEnv) # assigner le fruit de la requête (q2) au nom = création de l'objet nommé
return(c(dim(q2)[1], nom ))

}
