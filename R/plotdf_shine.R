#
#' plotdf_shine
#'
#' Cette fonction ouvre une application shiny et propose différents graphiques à partir du dataframe passé en argument
#' @param df un data frame
#' @return Retourne une shiny application
#' @export
#' @examples plotdf_shine(df)
#' plotdf_shine()



plotdf_shine <- function(df) {
    require(shiny)
    df$n <- 1
    var <- names(df)
    shinyApp(
        ui = fluidPage(
            sidebarLayout(
                sidebarPanel(
                    radioButtons('type', 'Type de graphique', c("colonnes", 'points', "lignes", 'histogramme' , "boxplot" , 'violons' )),
                    selectInput('x', "Variables en ordonnée", var ),
                    selectInput('y','Variable en absysse', var),
                    selectInput('g','Variable catégorielle', c("null",var))
                    ),
                mainPanel(plotOutput("hist"))
            )
        ),
        server = function(input, output) {
            output$hist <- renderPlot(
                if (input$type == 'points') {
                    x <- input$x#readline("Quelle variable en x ? (tapez en toutes lettre) :")
                    y <- input$y #readline("Quelle variable en y ? (tapez en toutes lettre) :")
                    #cate <-  #readline("Souhaitez-vous ajouter une variable catégorielle? (o/n) :")
                    if (input$g != "null") {
                        v.cat <- input$g# readline("Quelle est la variable catégorielle? :")
                        df[[v.cat]] <- as.factor(df[[v.cat]])
                    }
                    if (input$g != "null") {
                        g <- ggplot2::ggplot(df, ggplot2::aes(df[[x]],df[[y]], color = df[[v.cat]])) + ggplot2::geom_point()+ ggplot2::scale_x_discrete()+  ggplot2::guides(col=ggplot2::guide_legend(v.cat))
                        g <- g + ggplot2::xlab(x) + ggplot2::ylab(y) + ggplot2::theme_light()
                    }else{
                        g <- ggplot2::ggplot(df, ggplot2::aes(df[[x]],df[[y]])) + ggplot2::geom_point()
                        g <- g + ggplot2::xlab(x) + ggplot2::ylab(y) + ggplot2::theme_light()
                    }
                    g
                }else if (input$type == 'histogramme') {
                    x <- input$x#readline("Quelle variable en x ? (Variable continue) :")
                    #bin <- readline("Quelle est la taille des classes ? :")


                    g <- ggplot2::ggplot(df, ggplot2::aes(df[[x]])) + ggplot2::geom_histogram()


                    g <- g + ggplot2::xlab(x) + ggplot2::theme_light()
                    g
                }else if (input$type == 'boxplot') {
                    x <- input$x#readline("Quelle variable en x ? (Variable continue) :")
                    cate <- input$g#readline("Souhaitez-vous ajouter une variable catégorielle? (o/n) :")
                    if (input$g != "null") {
                        v.cat <- input$g #readline("Quelle est la variable catégorielle? :")
                        df[[v.cat]] <- as.factor(df[[v.cat]])
                    }
                    if (input$g != "null") {
                        g <- ggplot2::ggplot(df, ggplot2::aes(df[[v.cat]],df[[x]], fill = df[[v.cat]],group = df[[v.cat]])) +
                            ggplot2::geom_boxplot()+
                            ggplot2::guides(fill=ggplot2::guide_legend(v.cat))
                        g <- g + ggplot2::xlab(v.cat) + ggplot2::ylab(x) + ggplot2::theme_light()
                    }else{
                        g <- ggplot2::ggplot(df, ggplot2::aes(df[[x]])) + ggplot2::geom_boxplot()
                        g <- g + ggplot2::xlab(v.cat) + ggplot2::ylab(x) + ggplot2::theme_light()
                    }
                    g
                }else if (input$type == 'violons') {
                    x <- input$x #readline("Quelle variable en x ? (Variable continue) :")
                    cate <- input$g # readline("Souhaitez-vous ajouter une variable catégorielle? (o/n) :")
                    if (input$g != "null") {
                        v.cat <- input$g#readline("Quelle est la variable catégorielle? :")
                        df[[v.cat]] <- as.factor(df[[v.cat]])
                    }
                    if (input$g != "null") {

                        g <- ggplot2::ggplot(df, ggplot2::aes(df[[v.cat]],df[[x]], fill = df[[v.cat]],group = df[[v.cat]])) +
                            ggplot2::geom_violin()+
                            ggplot2::guides(fill=ggplot2::guide_legend(v.cat))
                        g <- g + ggplot2::xlab(v.cat) + ggplot2::ylab(x) + ggplot2::theme_light()
                    }else{
                        g <- ggplot2::ggplot(df, ggplot2::aes(df[[x]])) + ggplot2::geom_violin()
                        g <- g + ggplot2::xlab(v.cat) + ggplot2::ylab(x) + ggplot2::theme_light()
                    }
                    g
                }else if (input$type == 'lignes') {
                    x <- input$x #readline("Quelle variable en x ? (tapez en toutes lettre) :")
                    y <- input$y #readline("Quelle variable en y ? (tapez en toutes lettre) :")
                    cate <- input$g #readline("Souhaitez-vous ajouter une variable catégorielle? (o/n) :")
                    if (input$g != "null") {
                        v.cat <- input$g#readline("Quelle est la variable catégorielle? :")
                        df[[v.cat]] <- as.factor(df[[v.cat]])
                    }
                    if (input$g != "null") {
                        g <- ggplot2::ggplot(df, ggplot2::aes(df[[x]],df[[y]], color = df[[v.cat]])) + ggplot2::geom_line()+  ggplot2::guides(col=ggplot2::guide_legend(v.cat))
                        g <- g + ggplot2::xlab(x) + ggplot2::ylab(y) + ggplot2::theme_light()
                    }else{
                        g <- ggplot2::ggplot(df, ggplot2::aes(df[[x]],df[[y]])) + ggplot2::geom_line()
                        g <- g + ggplot2::xlab(x) + ggplot2::ylab(y) + ggplot2::theme_light()
                    }
                    g
                }else if (input$type == 'colonnes') {
                    x <- input$x #readline("Quelle variable en x ? (tapez en toutes lettre) :")
                    y <- input$y #readline("Quelle variable en y ? (tapez en toutes lettre) :")
                    cate <- input$g #readline("Souhaitez-vous ajouter une variable catégorielle? (o/n) :")
                    if (cate != 'null') {
                        v.cat <- input$g #readline("Quelle est la variable catégorielle? :")
                        df[[v.cat]] <- as.factor(df[[v.cat]])
                    }
                    if (cate != 'null') {
                        g <- ggplot2::ggplot(df, ggplot2::aes(df[[x]],df[[y]],fill = df[[v.cat]])) + ggplot2::geom_col() + ggplot2::scale_x_discrete() + ggplot2::guides(fill=ggplot2::guide_legend(v.cat))
                        g <- g + ggplot2::xlab(x) + ggplot2::ylab(y) + ggplot2::theme_light()
                    }else{
                        g <- ggplot2::ggplot(df, ggplot2::aes(df[[x]],df[[y]])) + ggplot2::geom_col()
                        g <- g + ggplot2::xlab(x) + ggplot2::ylab(y) + ggplot2::theme_light()
                    }
                    g
                }
            )
        }
    )
}

