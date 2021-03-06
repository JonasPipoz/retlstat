% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update_or_insert.R, R/update_or_insert_.R
\name{update_or_insert}
\alias{update_or_insert}
\title{update_or_insert}
\usage{
update_or_insert(df, conn, BDD, Schema, table_name, id)

update_or_insert(df, conn, BDD, Schema, table_name, id)
}
\arguments{
\item{df}{A dataframe containing the data. Default is null}

\item{conn}{A DBI connection}

\item{BDD}{The sql database name}

\item{Schema}{The sql schema}

\item{table_name}{The sql Table}

\item{id}{a unique identifier per row.}
}
\description{
This function mets a jour les données dans une table si l'id existe dans la table sql server ou alors ajoute les données a partir d'un dataframe

This function mets a jour les données dans une table si l'id existe dans la table sql server ou alors ajoute les données a partir d'un dataframe
}
\examples{
update_or_insert(df = mtcars, conn = conn, BDD = 'STATPRODTEMP',Schema = 'GEN',table_name = 'MTCARS',id = rownames(mtcars))
update_or_insert()
update_or_insert(df = mtcars, conn = conn, BDD = 'STATPRODTEMP',Schema = 'GEN',table_name = 'MTCARS',id = rownames(mtcars))
update_or_insert()
}
