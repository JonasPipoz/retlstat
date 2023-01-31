#
#' resumedf
#'
#' This function mets a jour les données dans une table si l'id existe dans la table sql server ou alors ajoute les données a partir d'un dataframe
#' @param g Un objet ggplot
#' @param title Le titre du graphique à passer comme argument à la fonction ggplot2::labs()
#' @param subtitle Le sous-titre du graphique à passer comme argument à la fonction ggplot2::labs()
#' @param x.label Le titre de l'axe x
#' @param y.label Le titre de l'axe y
#' @param caption ...
#' @param tag ...
#' @param xlab.angle Angle en degré pour une rotation des graduation de l'axe x
#' @param ylab.angle Angle en degré pour une rotation des graduation de l'axe y
#' @return Retourne un objet ggplot mis en forme
#' @export
#' @examples ggformat(g)
#'
#' g <- mtcars %>%
#' ggplot(aes(x  = carb, y = ws))+
#' geom_bar(stat = 'identity')
#'
#' ggformat(g,
#'    title = 'Titre de mon graphique',
#'    subtitle = 'sous - titre',
#'    caption = 'fig. n',
#'    x.label = 'carburant',
#'    y.label = 'weigth')
#'
#'
#'

ggformat <- function(g,
                     title = NULL,
                     subtitle = NULL,
                     x.label = NULL,
                     y.label = NULL,
                     caption = NULL,
                     tag = NULL,
                     xlab.angle=0,
                     ylab.angle=0){
  require(ggplot2)
  require(ggthemes)
  g <- g +
    labs(title = title,
         subtitle = subtitle,
         y = y.label,
         x = x.label,
         caption = caption,
         tag = tag
         )+
    guides(alpha="none")+
    theme_hc()+
    theme(axis.text = element_text(angle = xlab.angle, hjust = 1),
          axis.text.y = element_text(angle = ylab.angle),
          legend.position = "none",
          text=element_text(size=10)
    )
  return(g)

}
