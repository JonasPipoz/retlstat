#
#' plotdf
#'
#' Cette fonction prend un dataframe en argument et propose diverses options pour produire facilement un graphique avec ggplo2
#' @param df un dataframe pour lequel un graphique est désiré
#' @return Retourne un objet ggplot
#' @export
#' @examples plotdf(df = mtcars)
#' plotdf()

plotdf <- function(df){
  types <- c("colonnes" = 1, 'points'=2, "lignes" = 3, 'histogramme' = 5, "boxplot" = 6, 'violons' = 7 )

  print(types)
  types_selected <- readline(paste0("Quel type de graphique voulez-vous? (ex: pour des bars tappez 1) :"))
  print(types_selected)
  cat(paste0("Le type de graphique selectionné est '", names(types)[as.numeric(types_selected)], "'\n"))

  cat("Les variables disponibles sont :\n")
  cat(paste0(names(df),'\n'))



  # Points
  if (types_selected == '2') {
    x <- readline("Quelle variable en x ? (tapez en toutes lettre) :")
    y <- readline("Quelle variable en y ? (tapez en toutes lettre) :")
    cate <- readline("Souhaitez-vous ajouter une variable catégorielle? (o/n) :")
    if (cate == 'o') {
      v.cat <- readline("Quelle est la variable catégorielle? :")

    }
    if (cate == 'o') {
      g <- ggplot2::ggplot(df, ggplot2::aes(df[[x]],df[[y]], color = df[[v.cat]])) + ggplot2::geom_point()+ ggplot2::scale_x_discrete()+  ggplot2::guides(col=ggplot2::guide_legend(v.cat))
    }else{
      g <- ggplot2::ggplot(df, ggplot2::aes(df[[x]],df[[y]])) + ggplot2::geom_point()
    }
    g <- g + ggplot2::xlab(x) + ggplot2::ylab(y) + ggplot2::theme_light()
  }

  # histogramme
  if (types_selected == '5') {
    x <- readline("Quelle variable en x ? (Variable continue) :")
    bin <- readline("Quelle est la taille des classes ? :")


    g <- ggplot2::ggplot(df, ggplot2::aes(df[[x]])) + ggplot2::geom_histogram(binwidth = as.numeric(bin))+  ggplot2::guides(fill=ggplot2::guide_legend(v.cat))


    g <- g + ggplot2::xlab(x) + ggplot2::theme_light()
  }

  # boxplot
  if (types_selected == '6') {
    x <- readline("Quelle variable en x ? (Variable continue) :")
    cate <- readline("Souhaitez-vous ajouter une variable catégorielle? (o/n) :")
    if (cate == 'o') {
      v.cat <- readline("Quelle est la variable catégorielle? :")

    }
    if (cate == 'o') {
      g <- ggplot2::ggplot(df, ggplot2::aes(df[[v.cat]],df[[x]], fill = df[[v.cat]],group = df[[v.cat]])) +
        ggplot2::geom_boxplot()+
        ggplot2::guides(fill=ggplot2::guide_legend(v.cat))
    }else{
      g <- ggplot2::ggplot(df, ggplot2::aes(df[[x]])) + ggplot2::geom_boxplot()
    }
    g <- g + ggplot2::xlab(v.cat) + ggplot2::ylab(x) + ggplot2::theme_light()
  }

  # violon
  if (types_selected == '7') {
    x <- readline("Quelle variable en x ? (Variable continue) :")
    cate <- readline("Souhaitez-vous ajouter une variable catégorielle? (o/n) :")
    if (cate == 'o') {
      v.cat <- readline("Quelle est la variable catégorielle? :")

    }
    if (cate == 'o') {
      g <- ggplot2::ggplot(df, ggplot2::aes(df[[v.cat]],df[[x]], fill = df[[v.cat]],group = df[[v.cat]])) +
        ggplot2::geom_violin()+
        ggplot2::guides(fill=ggplot2::guide_legend(v.cat))
    }else{
      g <- ggplot2::ggplot(df, ggplot2::aes(df[[x]])) + ggplot2::geom_violin()
    }
    g <- g + ggplot2::xlab(v.cat) + ggplot2::ylab(x) + ggplot2::theme_light()
  }

  # Lines
  if (types_selected == '3') {
    x <- readline("Quelle variable en x ? (tapez en toutes lettre) :")
    y <- readline("Quelle variable en y ? (tapez en toutes lettre) :")
    cate <- readline("Souhaitez-vous ajouter une variable catégorielle? (o/n) :")
    if (cate == 'o') {
      v.cat <- readline("Quelle est la variable catégorielle? :")

    }
    if (cate == 'o') {
      g <- ggplot2::ggplot(df, ggplot2::aes(df[[x]],df[[y]], color = df[[v.cat]])) + ggplot2::geom_line()+  ggplot2::guides(col=ggplot2::guide_legend(v.cat))
    }else{
      g <- ggplot2::ggplot(df, ggplot2::aes(df[[x]],df[[y]])) + ggplot2::geom_line()
    }
    g <- g + ggplot2::xlab(x) + ggplot2::ylab(y) + ggplot2::theme_light()
  }

  # Colonnes
  if (types_selected == '1') {
    x <- readline("Quelle variable en x ? (tapez en toutes lettre) :")
    y <- readline("Quelle variable en y ? (tapez en toutes lettre) :")
    cate <- readline("Souhaitez-vous ajouter une variable catégorielle? (o/n) :")
    if (cate == 'o') {
      v.cat <- readline("Quelle est la variable catégorielle? :")

    }
    if (cate == 'o') {
      g <- ggplot2::ggplot(df, ggplot2::aes(df[[x]],df[[y]],fill = df[[v.cat]])) + ggplot2::geom_col() + ggplot2::scale_x_discrete() + ggplot2::guides(fill=ggplot2::guide_legend(v.cat))
    }else{
      g <- ggplot2::ggplot(df, ggplot2::aes(df[[x]],df[[y]])) + ggplot2::geom_col()
    }
    g <- g + ggplot2::xlab(x) + ggplot2::ylab(y) + ggplot2::theme_light()
  }



  titreon <- readline("Voulez-vous ajouter un titre ? (o/n) :")
  if (titreon == 'o') {
    titre <- readline('Tapez le titre : ')
    g <- g + ggplot2::labs(title = titre)
  }
  return(g)
}
