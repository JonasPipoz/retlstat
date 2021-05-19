#
#' plotdf2
#'
#' Cette fonction ouvre une application shiny et propose différents graphiques à partir d'une table SQL à choix dans l'application
#' @param df un dataframe
#' @return Retourne une shiny application
#' @export
#' @examples plotdf2(df)
#' plotdf2()

plotdf2 <- function(df) {
  require(shiny)
  require(shinythemes)
  require(shinyFiles)
  require(dplyr)

  shinyApp(
    ui = fluidPage(theme = shinytheme("superhero"),
                   sidebarLayout(
                     sidebarPanel('Plot',
                                  radioButtons('type', 'Type de graphique', c('-',"colonnes", 'points', "lignes", 'histogramme' , "boxplot" , 'violons' )),
                                  selectInput('x', "Variables en abscisse (x)", c("-")),
                                  selectInput('y','Variable en ordonnée (y)', c("-")),
                                  selectInput('g','Variable catégorielle', c("-"))


                     ),
                     mainPanel(plotOutput("hist"))
                   ),
                   sidebarLayout(
                     sidebarPanel('Manipuler les données:',
                                  checkboxInput('aff.table','Afficher les données', F),
                                  textInput('filtre', 'Filtrer les données (dplyr), ex: ANNEE == 2020'),
                                  selectInput('select',
                                              'Selectionner les colonnes (dplyr), ex: CANTON, ANNEE',
                                              c(""),
                                              multiple = T),
                                  selectInput('group_by',
                                              'Aggrèger les données (dplyr::group_by), ex: CANTON, ANNEE',
                                              c(""),
                                              multiple = T),
                                  selectInput('summerise',
                                              "Fonction d'agrégat",
                                              c("Somme",'Moyenne','Max','Min','Nombre de ligne')),
                                  selectInput('varagg',
                                              "Valeur d'aggrégat",
                                              c(""),
                                              multiple = F),

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



      donnee <- reactive(df)
      var <- reactive({names(NewDonnee())})
      ## Selection des données filtrees####
      NewDonnee <- reactive({

        df <- mutate(donnee(),n = 1)
        if (input$filtre != '') {
          tryCatch({

            df <- dplyr::filter(
              df,!! rlang::parse_expr(input$filtre)
            )

          },
          error = function(cond){
            df <- df
          }
          )
        }


        if (!is.null(input$select)) {
          tryCatch({
            df <- dplyr::select(df,input$select)

          },
          error = function(cond){
            df <- df
          }
          )

        }

        if (!is.null(input$group_by) & input$varagg == '-') {
          tryCatch({
            if (input$summerise == 'Nombre de ligne') {
              df <- dplyr::group_by_at(df,dplyr::vars(input$group_by)) %>%
                dplyr::summarise(N = n())
            }
            },
            error = function(cond){
              df <- df
            }
          )
        }

        if (!is.null(input$group_by) & input$varagg != '-') {
          tryCatch({
            #df <- dplyr::summarise(
            #  dplyr::group_by(df,!! rlang::parse_expr(input$group_by))
            #  ,
            #  !! rlang::parse_expr(input$group_by) = sum(!! rlang::parse_expr(input$varagg)
            #            )
            #  )
            if (input$summerise == 'Somme') {
              df <- dplyr::group_by_at(df,dplyr::vars(input$group_by)) %>%
                dplyr::summarise(Somme = sum(!! rlang::parse_expr(input$varagg)))

            }
            if (input$summerise == 'Moyenne') {
              df <- dplyr::group_by_at(df,dplyr::vars(input$group_by)) %>%
                dplyr::summarise(Moyenne = mean(!! rlang::parse_expr(input$varagg)))
            }
            if (input$summerise == 'Min') {
              df <- dplyr::group_by_at(df,dplyr::vars(input$group_by)) %>%
                dplyr::summarise(Min = min(!! rlang::parse_expr(input$varagg)))
            }
            if (input$summerise == 'Max') {
              df <- dplyr::group_by_at(df,dplyr::vars(input$group_by)) %>%
                dplyr::summarise(Max = max(!! rlang::parse_expr(input$varagg)))
            }

          },
          error = function(cond){
            df <- df
          }
          )

        }

        df
      }
      )

      #  if (input$filtre != '' & !is.null(input$select)) {
      #
      #
      #   tryCatch({
      #     df <- dplyr::mutate(
      #       dplyr::select(
      #         dplyr::filter(
      #           donnee(),!! rlang::parse_expr(input$filtre)
      #         )
      #         ,input$select
      #       ), n = 1)
      #     df
      #
      #
      #   },
      #
      #   error = function(cond){
      #
      #     return(dplyr::mutate(donnee(), n = 1))
      #   }
      #   )
      #
      #
      #  }else if(input$filtre != '' & is.null(input$select)){
      #    tryCatch({
      #      df <-  dplyr::mutate(
      #        dplyr::filter(
      #          donnee(),
      #          !! rlang::parse_expr(input$filtre)
      #        ),n = 1)
      #      df
      #
      #    },
      #
      #    error = function(cond){
      #
      #      return(dplyr::mutate(donnee(), n = 1))
      #    }
      #    )
      #  }else if(input$filtre == '' & !is.null(input$select)){
      #    tryCatch({
      #      df <- dplyr::mutate(
      #        dplyr::select(
      #          donnee(),
      #          input$select
      #        ), n= 1)
      #      df
      #    },
      #
      #    error = function(cond){
      #
      #      return(dplyr::mutate(donnee(), n = 1))
      #    }
      #    )
      #
      #  }else if(input$filtre == '' & !is.null(input$select)){
      #    tryCatch({
      #      df <- dplyr::mutate(
      #        dplyr::select(
      #          donnee(),
      #          input$select
      #        ), n= 1)
      #      df
      #    },
      #
      #    error = function(cond){
      #
      #      return(dplyr::mutate(donnee(), n = 1))
      #    }
      #    )
      #
      #  }else{
      #    return(dplyr::mutate(donnee(), n = 1))
      #  }
      #

      #Chargement dans R
      observeEvent(input$load.in.r, assign(paste0(isolate(input$nom.df)) ,NewDonnee(), envir = .GlobalEnv))

      #export CSV
      observeEvent(input$export.csv, write.table(x = NewDonnee(), file = paste0(global$datapath,"/",isolate(input$nom.df),".csv"),sep= ';', row.names = F))
      ## Modification des variables pour les graphs selon la table sql choisie####
      observe({
        isolate(input$tables)
        updateSelectInput(session, "x",
                          choices =  c('-',var()))
        updateSelectInput(session, "y",
                          choices = c('-',var()))
        updateSelectInput(session, "g",
                          choices = c('-',var()))
      })

      observe({
        updateSelectInput(session, "select",
                          choices =  c("-", names(donnee())))
        updateSelectInput(session, "group_by",
                          choices =  c("-", names(donnee())))
        updateSelectInput(session, "varagg",
                          choices =  c("-", names(donnee())))

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

