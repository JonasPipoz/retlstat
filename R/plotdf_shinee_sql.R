#
#' plotdf_shinesql
#'
#' Cette fonction ouvre une application shiny et propose différents graphiques à partir d'une table SQL à choix dans l'application
#' @param conn une connexion DBI
#' @return Retourne une shiny application
#' @export
#' @examples plotdf_shinesql(conn)
#' plotdf_shinesql()

plotsql <- function(conn) {
  require(shiny)

  shinyApp(
    ui = fluidPage(
      sidebarLayout(
        sidebarPanel(
          selectInput('table','Tables SQL', c("11001033_PAYS")),
          checkboxInput('aff.table','Afficher les données', F),
          radioButtons('type', 'Type de graphique', c('null',"colonnes", 'points', "lignes", 'histogramme' , "boxplot" , 'violons', 'table' )),
          selectInput('x', "Variables en abscisse (x)", c("null")),
          selectInput('y','Variable en ordonnée (y)', c("null")),
          selectInput('g','Variable catégorielle', c("null")),
          textInput('filtre', 'Filtrer les données (dplyr), ex: ANNEE == 2020')
        ),
        mainPanel(plotOutput("hist"),
                  DT::dataTableOutput('tableau'))
      )
    ),
    server = function(input, output,session) {
      #cat(input$aff.table)
      tables <- reactive({
        query <- "SELECT [Table Name] as Tab
        ,[Schéma] as Sch
        FROM [STATPRODTEMP].[VIEW].[22007010_TABLE]
        order by [Schéma]  desc"
        DBI::dbGetQuery(conn, query)

      })

      donnee <- reactive({

        t.sel <- dplyr::filter(tables(), Tab == input$table)
        schema <- as.character(t.sel$Sch[1])
        table <- paste0("[",t.sel$Tab[1],"]")

        query <- paste0(" SELECT * FROM [",schema,"].", table)

        as.data.frame(DBI::dbGetQuery(conn, query))



        })
      var <- reactive({names(donnee())})
      NewDonnee <- reactive({
        if (input$filtre != '') {
          NewDonnee <- dplyr::filter(donnee(),!! rlang::parse_expr(input$filtre))
          return(NewDonnee)
        }else{
          return(donnee())
        }

        })
      observe({
        input$tables
        updateSelectInput(session, "x",
                          choices = var())
        updateSelectInput(session, "y",
                          choices = var())
        updateSelectInput(session, "g",
                          choices = var())
        })
      observe({
        updateSelectInput(session, "table",
                          choices = tables()$Tab)
      })
      output$tableau <- DT::renderDataTable({
        if (input$aff.table == T) {
          NewDonnee()
        }
      })
      output$hist <- renderPlot({
        input$table
        if (input$type == 'points') {
          x <- input$x
          y <- input$y

          if (input$g != "null") {
            v.cat <- input$g
          }
          if (input$g != "null") {
            g <- ggplot2::ggplot(NewDonnee(), ggplot2::aes(NewDonnee()[[x]],NewDonnee()[[y]], color = NewDonnee()[[v.cat]])) + ggplot2::geom_point()+ ggplot2::scale_x_discrete()+  ggplot2::guides(col=ggplot2::guide_legend(v.cat))
            g <- g + ggplot2::xlab(x) + ggplot2::ylab(y) + ggplot2::theme_light()
          }else{
            g <- ggplot2::ggplot(NewDonnee(), ggplot2::aes(NewDonnee()[[x]],NewDonnee()[[y]])) + ggplot2::geom_point()
            g <- g + ggplot2::xlab(x) + ggplot2::ylab(y) + ggplot2::theme_light()
          }
          g
        }else if (input$type == 'histogramme') {
          x <- input$x


          g <- ggplot2::ggplot(NewDonnee(), ggplot2::aes(NewDonnee()[[x]])) + ggplot2::geom_histogram()


          g <- g + ggplot2::xlab(x) + ggplot2::theme_light()
          g
        }else if (input$type == 'boxplot') {
          x <- input$x
          cate <- input$g
          if (input$g != "null") {
            v.cat <- input$g

          }
          if (input$g != "null") {
            g <- ggplot2::ggplot(NewDonnee(), ggplot2::aes(NewDonnee()[[v.cat]],NewDonnee()[[x]], fill = NewDonnee()[[v.cat]],group = NewDonnee()[[v.cat]])) +
              ggplot2::geom_boxplot()+
              ggplot2::guides(fill=ggplot2::guide_legend(v.cat))
            g <- g + ggplot2::xlab(v.cat) + ggplot2::ylab(x) + ggplot2::theme_light()
          }else{
            g <- ggplot2::ggplot(NewDonnee(), ggplot2::aes(NewDonnee()[[x]])) + ggplot2::geom_boxplot()
            g <- g + ggplot2::xlab(v.cat) + ggplot2::ylab(x) + ggplot2::theme_light()
          }
          g
        }else if (input$type == 'violons') {
          x <- input$x
          cate <- input$g
          if (input$g != "null") {
            v.cat <- input$g

          }
          if (input$g != "null") {

            g <- ggplot2::ggplot(NewDonnee(), ggplot2::aes(NewDonnee()[[v.cat]],NewDonnee()[[x]], fill = NewDonnee()[[v.cat]],group = NewDonnee()[[v.cat]])) +
              ggplot2::geom_violin()+
              ggplot2::guides(fill=ggplot2::guide_legend(v.cat))
            g <- g + ggplot2::xlab(v.cat) + ggplot2::ylab(x) + ggplot2::theme_light()
          }else{
            g <- ggplot2::ggplot(NewDonnee(), ggplot2::aes(NewDonnee()[[x]])) + ggplot2::geom_violin()
            g <- g + ggplot2::xlab(v.cat) + ggplot2::ylab(x) + ggplot2::theme_light()
          }
          g
        }else if (input$type == 'lignes') {
          x <- input$x
          y <- input$y
          cate <- input$g
          if (input$g != "null") {
            v.cat <- input$g
          }
          if (input$g != "null") {
            g <- ggplot2::ggplot(NewDonnee(), ggplot2::aes(NewDonnee()[[x]],NewDonnee()[[y]], color = NewDonnee()[[v.cat]])) + ggplot2::geom_line()+  ggplot2::guides(col=ggplot2::guide_legend(v.cat))
            g <- g + ggplot2::xlab(x) + ggplot2::ylab(y) + ggplot2::theme_light()
          }else{
            g <- ggplot2::ggplot(NewDonnee(), ggplot2::aes(NewDonnee()[[x]],NewDonnee()[[y]])) + ggplot2::geom_line()
            g <- g + ggplot2::xlab(x) + ggplot2::ylab(y) + ggplot2::theme_light()
          }
          g
        }else if (input$type == 'colonnes') {
          x <- input$x
          y <- input$y
          cate <- input$g

          if (cate != 'null') {

            v.cat <- input$g
          }
          if (cate != 'null') {

            g <- ggplot2::ggplot(NewDonnee(), ggplot2::aes(NewDonnee()[[x]],NewDonnee()[[y]],fill = NewDonnee()[[v.cat]])) + ggplot2::geom_col() + ggplot2::scale_x_discrete() + ggplot2::guides(fill=ggplot2::guide_legend(v.cat))
            g <- g + ggplot2::xlab(x) + ggplot2::ylab(y) + ggplot2::theme_light()
          }else{

            g <- ggplot2::ggplot(NewDonnee(), ggplot2::aes(NewDonnee()[[x]],NewDonnee()[[y]])) + ggplot2::geom_col()
            g <- g + ggplot2::xlab(x) + ggplot2::ylab(y) + ggplot2::theme_light()
          }


          g

        }

          }
        )

      }
      )
    }
#plotdf_shinesql(conn)
