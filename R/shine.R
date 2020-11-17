#
#' plotdf
#'
#' Cette fonction prend un vecteur de variable continue et retourne un histogramme de distribtion selon le nombre de classe choisie
#' @param var une variable contine
#' @return Retourne une shiny application
#' @export
#' @examples shine(df$continuesVar)
#' plotdf()

shinee <- function(df) {
    require(shiny)
    var <- names(df)
    shinyApp(
        ui = fluidPage(
            sidebarLayout(
                radioButtons('type', 'Type de graphique', c("colonnes", 'points', "lignes", 'histogramme' , "boxplot" , 'violons' )),
                selectInput('x', "Variables en ordonnée", var ),
                selectInput('x','Variable en absysse', var),
                selectInput('g','Variable catégorielle', c("null",var)),
                mainPanel(plotOutput("plot"))
            )
        ),
        server = function(input, output) {
            output$plot <- renderPlot(
                # Points
                if (input$type == 'points') {
                    x <- input$x#readline("Quelle variable en x ? (tapez en toutes lettre) :")
                    y <- input$y #readline("Quelle variable en y ? (tapez en toutes lettre) :")
                    #cate <-  #readline("Souhaitez-vous ajouter une variable catégorielle? (o/n) :")
                    if (input$g != "null") {
                        v.cat <- input$g# readline("Quelle est la variable catégorielle? :")

                    }
                    if (input$g != "null") {
                        g <- ggplot2::ggplot(df, ggplot2::aes(df[[x]],df[[y]], color = df[[v.cat]])) + ggplot2::geom_point()+ ggplot2::scale_x_discrete()+  ggplot2::guides(col=ggplot2::guide_legend(v.cat))
                    }else{
                        g <- ggplot2::ggplot(df, ggplot2::aes(df[[x]],df[[y]])) + ggplot2::geom_point()
                    }
                    g <- g + ggplot2::xlab(x) + ggplot2::ylab(y) + ggplot2::theme_light()
                },

                # histogramme
                if (input$type == 'histogramme') {
                    x <- input$x#readline("Quelle variable en x ? (Variable continue) :")
                    #bin <- readline("Quelle est la taille des classes ? :")


                    g <- ggplot2::ggplot(df, ggplot2::aes(df[[x]])) + ggplot2::geom_histogram()


                    g <- g + ggplot2::xlab(x) + ggplot2::theme_light()
                },

                # boxplot
                if (input$type == 'boxplot') {
                    x <- input$x#readline("Quelle variable en x ? (Variable continue) :")
                    cate <- input$g#readline("Souhaitez-vous ajouter une variable catégorielle? (o/n) :")
                    if (input$g != "null") {
                        v.cat <- input$g #readline("Quelle est la variable catégorielle? :")

                    }
                    if (input$g != "null") {
                        g <- ggplot2::ggplot(df, ggplot2::aes(df[[v.cat]],df[[x]], fill = df[[v.cat]],group = df[[v.cat]])) +
                            ggplot2::geom_boxplot()+
                            ggplot2::guides(fill=ggplot2::guide_legend(v.cat))
                    }else{
                        g <- ggplot2::ggplot(df, ggplot2::aes(df[[x]])) + ggplot2::geom_boxplot()
                    }
                    g <- g + ggplot2::xlab(v.cat) + ggplot2::ylab(x) + ggplot2::theme_light()
                },

                # violon
                if (input$type == 'violon') {
                    x <- input$x #readline("Quelle variable en x ? (Variable continue) :")
                    cate <- input$g # readline("Souhaitez-vous ajouter une variable catégorielle? (o/n) :")
                    if (input$g != "null") {
                        v.cat <- input$g#readline("Quelle est la variable catégorielle? :")

                    }
                    if (input$g != "null") {
                        g <- ggplot2::ggplot(df, ggplot2::aes(df[[v.cat]],df[[x]], fill = df[[v.cat]],group = df[[v.cat]])) +
                            ggplot2::geom_violin()+
                            ggplot2::guides(fill=ggplot2::guide_legend(v.cat))
                    }else{
                        g <- ggplot2::ggplot(df, ggplot2::aes(df[[x]])) + ggplot2::geom_violin()
                    }
                    g <- g + ggplot2::xlab(v.cat) + ggplot2::ylab(x) + ggplot2::theme_light()
                },

                # Lines
                if (input$type == 'lines') {
                    x <- input$x #readline("Quelle variable en x ? (tapez en toutes lettre) :")
                    y <- input$y #readline("Quelle variable en y ? (tapez en toutes lettre) :")
                    cate <- input$g #readline("Souhaitez-vous ajouter une variable catégorielle? (o/n) :")
                    if (input$g != "null") {
                        v.cat <- input$g#readline("Quelle est la variable catégorielle? :")

                    }
                    if (input$g != "null") {
                        g <- ggplot2::ggplot(df, ggplot2::aes(df[[x]],df[[y]], color = df[[v.cat]])) + ggplot2::geom_line()+  ggplot2::guides(col=ggplot2::guide_legend(v.cat))
                    }else{
                        g <- ggplot2::ggplot(df, ggplot2::aes(df[[x]],df[[y]])) + ggplot2::geom_line()
                    }
                    g <- g + ggplot2::xlab(x) + ggplot2::ylab(y) + ggplot2::theme_light()
                },

                # Colonnes
                if (input$type == 'lines') {
                    x <- input$x #readline("Quelle variable en x ? (tapez en toutes lettre) :")
                    y <- input$y #readline("Quelle variable en y ? (tapez en toutes lettre) :")
                    cate <- input$g #readline("Souhaitez-vous ajouter une variable catégorielle? (o/n) :")
                    if (cate != 'null') {
                        v.cat <- input$g #readline("Quelle est la variable catégorielle? :")

                    }
                    if (cate != 'null') {
                        g <- ggplot2::ggplot(df, ggplot2::aes(df[[x]],df[[y]],fill = df[[v.cat]])) + ggplot2::geom_col() + ggplot2::scale_x_discrete() + ggplot2::guides(fill=ggplot2::guide_legend(v.cat))
                    }else{
                        g <- ggplot2::ggplot(df, ggplot2::aes(df[[x]],df[[y]])) + ggplot2::geom_col()
                    }
                    g <- g + ggplot2::xlab(x) + ggplot2::ylab(y) + ggplot2::theme_light()
                },
                g
            )
        }
    )
}
