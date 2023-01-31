#
#' palcol
#'
#' Cette fonction retourne un vecteur de code de couleur en hexa. Il existe quelques palette à choix:
#' @param n Le nombre de couleur souhaité. Si n > lenght palette --> n = length(palette)
#' @param show Logique. Si True, plot la palette.
#' @param type la palette de couleur choisie: "cat20", "flash12", "ws10", "gender2"
#' @export
#' @examples palcol(n = 12, show = F, type = 'cat20')
#' palcol()

palcol <- function(n = 12, show = F, type = 'cat20'){

  if (type == 'cat20') {
    pal <- c(
      "#52582C",
      "#A2AE57",
      "#A94D53",
      "#527167",
      "#132520",
      "#61B861",
      "#607F60",
      "#AE7244",
      "#4A5BAC",
      "#001FBD",
      "#76A1EA",
      "#45639A",
      "#B59646",
      "#613881",
      "#BE57FF",
      "#392886",
      "#5D4F99",
      "#BEBE2D",
      "#90375D",
      "#25131B"
    )
  }else if (type == 'flash12') {
    pal <- c(
      "#4c1a57",
      "#ff3cc7",
      "#f0f600",
      "#00e5e8",
      "#007c77",
      "#009ddc",
      "#f26430",
      "#009b72",
      "#2a2d34",
      "#d81e5b",
      "#a379c9",
      "#00FF37"
    )
  }else if (type == "ws10"){
    pal <- c(
      "#3b2515",
      "#4d2f13",
      "#854918",
      "#dd5629",
      "#486273",
      "#5b6c70",
      "#e89546",
      "#838e90",
      "#b1a495",
      "#d6baa3"
    )
  }else if (type == "gender2"){
    pal <- c("#d8b365",
             "#5ab4ac")
  }


  n = ifelse(n> length(pal),length(pal),n)

  pal <- pal[1:n]

  if (show == T) {
    barplot(rep(1, length(pal)), col = pal ,axes = F,legend.text = paste(seq(1,length(pal)),pal))


  }
  return(pal)
}
