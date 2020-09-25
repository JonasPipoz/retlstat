#
#' changes
#'
#' This function compare deux dataframes à partir du package daff et trie les comparaisons
#' retourne une liste de 4 dataframes,
#' updated: un df avec toutes les lignes mises à jour
#' added: un df avec toutes les lignes ajoutés
#' deleted: un df avec toutes les lignes supprimées
#' @param df_original A dataframe containing the original data. Default is null
#' @param df_nouveau A dataframe containing the data to compare with
#' @export
#' @examples
#' changes()
#'dep = stringr, dplyr, daff


change <- function(df_original, df_nouveau, id){
  names(df_nouveau) <-  stringr::str_replace(names(df_nouveau),id,'id')
  names(df_original) <-  stringr::str_replace(names(df_original),id,'id')

  stopifnot(nrow(unique(df_original$id)) == nrow(df_original))
  stopifnot(nrow(unique(df_nouveau$id)) == nrow(df_nouveau))



  dif <- daff::diff_data(df_original, df_nouveau, ids = 'id')
  chf <- dif$get_data()
  if (length(chf)==0){
    cat('Les deux datasets sont identiques.')
    return(NULL)
  }else{
    cat('Il existe des différences.\n\n')
    names(chf) <- stringr::str_replace(names(chf),"@@", 'ch')

    chf <- dplyr::select(chf,ch,id)

    ad <-  dplyr::filter(chf,ch == '+++')

    del <- dplyr::filter(chf,ch == '---')

    up <- dplyr::filter(chf,ch == '->')

    update_new <- dplyr::filter(df_nouveau, id %in% up$id)
    update_old <- dplyr::filter(df_nouveau, id %in% up$id)
    add <- dplyr::filter(df_nouveau, id %in% ad$id)
    delete <- dplyr::filter(df_original, id %in% del$id)


    summary <- data.frame(added = nrow(add),
                          updated = nrow(update_new),
                          deleted = nrow(delete))
    names(update_new) <-  stringr::str_replace(names(update_new),'id',id)
    names(update_old) <-  stringr::str_replace(names(update_old),'id',id)
    names(add) <-  stringr::str_replace(names(add) ,'id',id)
    names(delete) <- stringr::str_replace(names(delete),'id',id)

    changes <- list("added" = add, "deleted"= delete, "updated_new" = update_new,"updated_old" = update_old, "summary"=summary)

    cat('résumé des différences entre les deux datasets:\n\n' )
    print(summary)

  return(changes)
  }


}
### Solution tester

#d1 <- data.frame(ID = c(1  , 2 , 3),
#                 x1 = c('a','b','c'),
#                 x2 = c(21 ,32 ,34))
#
#d2 <- data.frame(ID = c(4  , 2 , 3),
#                 x1 = c('d','b','c'),
#                 x2 = c(34 ,32 ,35))
#d3 <- change(d1,d2,'ID')
#
#d3
#
#library(readxl)
#
#df_nouveau <- readxl::read_excel('c:/temp/testChanges.xlsx', sheet = 'EGID1')
#df_original <- readxl::read_excel('c:/temp/testChanges.xlsx', sheet = 'EGID2')
#id <- 'NUM_EGID'
#names(df_nouveau) <-  stringr::str_replace(names(df_nouveau),id,'id')
#names(df_original) <-  stringr::str_replace(names(df_original),id,'id')
#
#stopifnot(nrow(unique(df_original$id)) == nrow(df_original))
#stopifnot(nrow(unique(df_nouveau$id)) == nrow(df_nouveau))
#
#
#
#dif <- daff::diff_data(df_original, df_nouveau, ids = 'id')
#chf <- dif$get_data()
#names(chf) <- stringr::str_replace(names(chf),"@@", 'ch')
#
#chf <- dplyr::select(chf,ch,id)
#
#ad <-  dplyr::filter(chf,ch == '+++')
#
#del <- dplyr::filter(chf,ch == '---')
#
#up <- dplyr::filter(chf,ch == '->')
#
#update_new <- dplyr::filter(df_nouveau, id %in% up$id)
#update_old <- dplyr::filter(df_nouveau, id %in% up$id)
#add <- dplyr::filter(df_nouveau, id %in% ad$id)
#delete <- dplyr::filter(df_original, id %in% del$id)
#
#
#
#summary <- data.frame(added = nrow(add),
#                      updated = nrow(update),
#                      deleted = nrow(delete))
#
#changes <- list("added" = add, "deleted"= delete, "updated_new" = update_new,"updated_old" = update_old, "summary"=summary)
#
#
#
#d3 <- change(d1,d2,'id')
#
#d3$summary
#d3$deleted
#
#names(d2) <-  stringr::str_replace(names(d2),'Contrib','id')
#names(d1) <-  stringr::str_replace(names(d1),'Contrib','id')
#dif <- daff::diff_data(d1, d2,ids = 'id')
#chf <- dif$get_data()
#
#names(chf) <- stringr::str_replace(names(chf),"@@", 'ch')
#chf <- dplyr::select(chf,ch,id)
#
#
#ad <-  dplyr::filter(chf,ch == '+++')
#
#del <- dplyr::filter(chf,ch == '---')
#
#up <- dplyr::filter(chf,ch == '->')
#
#update <- dplyr::filter(d2, id %in% up$id)
#add <- dplyr::filter(d2, id %in% ad$id)
#delete <- dplyr::filter(d1, id %in% del$id)
#
#names(update) <-  stringr::str_replace(names(update),'id',id)
#names(add) <-  stringr::str_replace(names(update),'id',id)
#names(delete) <- stringr::str_replace(names(update),'id',id)#
