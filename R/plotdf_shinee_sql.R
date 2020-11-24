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
  require(shinythemes)
  require(shinyFiles)

  shinyApp(
    ui = fluidPage(theme = shinytheme("superhero"),
      sidebarLayout(
        sidebarPanel('Plot',
          selectInput('table','Tables SQL', c("11001033_PAYS")),
          radioButtons('type', 'Type de graphique', c('-',"colonnes", 'points', "lignes", 'histogramme' , "boxplot" , 'violons' )),
          selectInput('x', "Variables en abscisse (x)", c("-")),
          selectInput('y','Variable en ordonnée (y)', c("-")),
          selectInput('g','Variable catégorielle', c("-"))


        ),
        mainPanel(plotOutput("hist"))
      ),
      sidebarLayout(
        sidebarPanel('Filtrer les données:',
                     textInput('filtre', 'Filtrer les données (dplyr), ex: ANNEE == 2020'),
                     checkboxInput('aff.table','Afficher les données', F)

        ),
        mainPanel(

          DT::dataTableOutput('tableau')
        )
      ),
      sidebarLayout(
        sidebarPanel('Exporter',
                     textInput('nom.df','Nommer les données:',value = "df_sql"),
                     actionButton('load.in.r',"Charger dans R"),
                     verbatimTextOutput("dir", placeholder = TRUE),
                     shinyDirButton("dir", "Changer le dossier d'export", "Exporter donnée en CSV - Choix du dossier"),
                     actionButton('export.csv',"Exporter en csv")
      ),
      mainPanel(
        tableOutput('toto')
                )
      )
    ),
    ###############################################################
    ############ SERVER   #########################################
    ###############################################################

    server = function(input, output,session) {
      shinyDirChoose(
        input,
        'dir',
        roots = c(home = substr(getwd(),1,3)),
        filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw")
      )
      global <- reactiveValues(datapath = getwd())

      dir <- reactive(input$dir)

      output$dir <- renderText({
        global$datapath
      })

      observeEvent(ignoreNULL = TRUE,
                   eventExpr = {
                     input$dir
                   },
                   handlerExpr = {
                     if (!"path" %in% names(dir())) return()
                     home <- normalizePath("~")
                     global$datapath <-
                       file.path(substr(getwd(),1,3), paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
                   })


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
      var <- reactive({names(NewDonnee())})
      # Selection des données filtrees
      NewDonnee <- reactive({
        if (input$filtre != '') {

          tryCatch({
            dplyr::filter(donnee(),!! rlang::parse_expr(input$filtre))

          },
           error = function(cond){

             return(dplyr::mutate(donnee(), n = 1))
           }
          )

        }else{
          return(dplyr::mutate(donnee(), n = 1))
        }

        })
      #Chargement dans R
      observeEvent(input$load.in.r, assign(paste0(isolate(input$nom.df)) ,NewDonnee(), envir = .GlobalEnv))

      #export CSV
      observeEvent(input$export.csv, write.table(x = NewDonnee(), file = paste0(global$datapath,"/",isolate(input$nom.df),".csv"),sep= ';', row.names = F))
      # Modification des variables pour les graphs selon la table sql choisie
      observe({
        input$tables
        updateSelectInput(session, "x",
                          choices =  c('-',var()))
        updateSelectInput(session, "y",
                          choices = c('-',var()))
        updateSelectInput(session, "g",
                          choices = c('-',var()))
        })
      observe({
        updateSelectInput(session, "table",
                          choices = tables()$Tab)
      })

      ############
      ############ RENDER
      ############
      ## table
      output$tableau <- DT::renderDataTable({

        if (input$aff.table == T) {
          DT::datatable(data = NewDonnee(),
                        options = list(
                          initComplete = DT::JS(
                            "function(settings, json) {",
                            "$(this.api().table().header()).css({'color': '#eee'});",
                            "}")))


        }
      })
      ## Plot
      output$hist <- renderPlot({
        input$table
        if (input$type == 'points') {
          x <- input$x
          y <- input$y

          if (input$g != "-") {
            v.cat <- input$g
          }
          if (input$g != "-") {
            g <- ggplot2::ggplot(NewDonnee(), ggplot2::aes(NewDonnee()[[x]],NewDonnee()[[y]], color = NewDonnee()[[v.cat]])) + ggplot2::geom_point()+ ggplot2::scale_x_discrete()+  ggplot2::guides(col=ggplot2::guide_legend(v.cat))
            g <- g + ggplot2::xlab(x) + ggplot2::ylab(y) + ggplot2::theme_dark() + ggplot2::theme(plot.background = ggplot2::element_rect(fill = "darkgrey"))
          }else{
            g <- ggplot2::ggplot(NewDonnee(), ggplot2::aes(NewDonnee()[[x]],NewDonnee()[[y]])) + ggplot2::geom_point()
            g <- g + ggplot2::xlab(x) + ggplot2::ylab(y) + ggplot2::theme_dark() + ggplot2::theme(plot.background = ggplot2::element_rect(fill = "darkgrey"))
          }
          g
        }else if (input$type == 'histogramme') {
          x <- input$x


          g <- ggplot2::ggplot(NewDonnee(), ggplot2::aes(NewDonnee()[[x]])) + ggplot2::geom_histogram()


          g <- g + ggplot2::xlab(x) + ggplot2::theme_dark() + ggplot2::theme(plot.background = ggplot2::element_rect(fill = "darkgrey"))
          g
        }else if (input$type == 'boxplot') {
          x <- input$x
          cate <- input$g
          if (input$g != "-") {
            v.cat <- input$g

          }
          if (input$g != "-") {
            g <- ggplot2::ggplot(NewDonnee(), ggplot2::aes(NewDonnee()[[v.cat]],NewDonnee()[[y]], fill = NewDonnee()[[v.cat]],group = NewDonnee()[[v.cat]])) +
              ggplot2::geom_boxplot()+
              ggplot2::guides(fill=ggplot2::guide_legend(v.cat))
            g <- g + ggplot2::xlab(v.cat) + ggplot2::ylab(y) + ggplot2::theme_dark() + ggplot2::theme(plot.background = ggplot2::element_rect(fill = "darkgrey"))
          }else{
            g <- ggplot2::ggplot(NewDonnee(), ggplot2::aes(NewDonnee()[[y]])) + ggplot2::geom_boxplot()
            g <- g + ggplot2::xlab(v.cat) + ggplot2::ylab(y) + ggplot2::theme_dark() + ggplot2::theme(plot.background = ggplot2::element_rect(fill = "darkgrey"))
          }
          g
        }else if (input$type == 'violons') {
          x <- input$x
          cate <- input$g
          if (input$g != "-") {
            v.cat <- input$g

          }
          if (input$g != "-") {

            g <- ggplot2::ggplot(NewDonnee(), ggplot2::aes(NewDonnee()[[v.cat]],NewDonnee()[[y]], fill = NewDonnee()[[v.cat]],group = NewDonnee()[[v.cat]])) +
              ggplot2::geom_violin()+
              ggplot2::guides(fill=ggplot2::guide_legend(v.cat))
            g <- g + ggplot2::xlab(v.cat) + ggplot2::ylab(y) + ggplot2::theme_dark() + ggplot2::theme(plot.background = ggplot2::element_rect(fill = "darkgrey"))
          }else{
            g <- ggplot2::ggplot(NewDonnee(), ggplot2::aes(NewDonnee()[[y]])) + ggplot2::geom_violin()
            g <- g + ggplot2::xlab(v.cat) + ggplot2::ylab(y) + ggplot2::theme_dark() + ggplot2::theme(plot.background = ggplot2::element_rect(fill = "darkgrey"))
          }
          g
        }else if (input$type == 'lignes') {
          x <- input$x
          y <- input$y
          cate <- input$g
          if (input$g != "-") {
            v.cat <- input$g
          }
          if (input$g != "-") {
            g <- ggplot2::ggplot(NewDonnee(), ggplot2::aes(NewDonnee()[[x]],NewDonnee()[[y]], group = NewDonnee()[[v.cat]], color = NewDonnee()[[v.cat]])) + ggplot2::geom_line()+  ggplot2::guides(col=ggplot2::guide_legend(v.cat))
            g <- g + ggplot2::xlab(x) + ggplot2::ylab(y) + ggplot2::theme_dark() + ggplot2::theme(plot.background = ggplot2::element_rect(fill = "darkgrey"))
          }else{
            g <- ggplot2::ggplot(NewDonnee(), ggplot2::aes(NewDonnee()[[x]],NewDonnee()[[y]])) + ggplot2::geom_line()
            g <- g + ggplot2::xlab(x) + ggplot2::ylab(y) + ggplot2::theme_dark() + ggplot2::theme(plot.background = ggplot2::element_rect(fill = "darkgrey"))
          }
          g
        }else if (input$type == 'colonnes') {
          x <- input$x
          y <- input$y
          cate <- input$g

          if (cate != '-') {

            v.cat <- input$g
          }
          if (cate != '-') {

            g <- ggplot2::ggplot(NewDonnee(), ggplot2::aes(NewDonnee()[[x]],NewDonnee()[[y]],fill = NewDonnee()[[v.cat]])) + ggplot2::geom_col() + ggplot2::scale_x_discrete() + ggplot2::guides(fill=ggplot2::guide_legend(v.cat))
            g <- g + ggplot2::xlab(x) + ggplot2::ylab(y) + ggplot2::theme_dark() + ggplot2::theme(plot.background = ggplot2::element_rect(fill = "darkgrey"))
          }else{

            g <- ggplot2::ggplot(NewDonnee(), ggplot2::aes(NewDonnee()[[x]],NewDonnee()[[y]])) + ggplot2::geom_col()
            g <- g + ggplot2::xlab(x) + ggplot2::ylab(y) + ggplot2::theme_dark() + ggplot2::theme(plot.background = ggplot2::element_rect(fill = "darkgrey"))
          }


          g

        }else if (input$type == 'null'){
         return(ggplot2::ggplot() + ggplot2::geom_blank() + ggplot2::theme(panel.background = ggplot2::element_rect(fill = "#2c3f50"), plot.background = ggplot2::element_rect(fill = "#2c3f50", colour = 'darkgrey', size = 1)))
        }

          }
        )

      }
      )
    }
plotsql(conn)
