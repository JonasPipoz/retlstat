# ADD to table!
#
# This function add a dataframe to a table sql
#
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
# Pour installer le package
#path <- "P:/INSTALL/Outils/R/Packages_fait_maison/retlstat_0.1.0.tar.gz"
#install.packages(path, repos = NULL, type = "source")

#' add_to_table
#'
#' This function add a dataframe to an existing sql table.
#' @param dataframe A dataframe containing the data. Default is null
#' @param conn A DBI connection
#' @param BDD The sql database name
#' @param Schema The sql schema
#' @param Table The sql Table
#' @export
#' @examples stat_add(mtcars, conn, 'STATPRODTEMP','GEN','MTCARS')
#' stat_add()

stat_add <- function(dataframe = NULL,conn,BDD,Schema,Table){
  col_data <- names(dataframe)
  apply(dataframe, 1, sendrow,
        conn = conn,
        BDD = BDD,
        Schema = Schema,
        table_name = Table,
        col_data = col_data)

}

