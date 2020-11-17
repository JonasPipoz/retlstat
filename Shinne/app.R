#
#' plotdf
#'
#' Cette fonction prend un vecteur de variable continue et retourne un histogramme de distribtion selon le nombre de classe choisie
#' @param var une variable contine
#' @return Retourne une shiny application
#' @export
#' @examples shine(df$continuesVar)
#' plotdf()

shinee <- function(var) {
    require(shiny)
    shinyApp(
        ui = fluidPage(
            sidebarLayout(
                sidebarPanel(sliderInput("n", "Bins", 5, 100, 20)),
                mainPanel(plotOutput("hist"))
            )
        ),
        server = function(input, output) {
            output$hist <- renderPlot(
                hist(var, breaks = input$n,
                     col = "skyblue", border = "white")
            )
        }
    )
}
