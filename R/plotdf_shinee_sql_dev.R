#
#' plotdf_shinesql
#'
#' Cette fonction ouvre une application shiny et propose différents graphiques à partir d'une table SQL à choix dans l'application
#' @param conn une connexion DBI
#' @return Retourne une shiny application
#' @export
#' @examples plotdf_shinesql(conn)
#' plotdf_shinesql()

plotsql_ <- function(conn) {
  require(shiny)
  require(shinythemes)
  require(shinyFiles)
  require(dplyr)
  require(shinybusy)
  require(shinydashboard)
  require(dashboardthemes)
  customTheme <- shinyDashboardThemeDIY(

    ### general
    appFontFamily = "Arial"
    ,appFontColor = "rgb(100,100,100)"
    ,primaryFontColor = "rgb(0,0,0)"
    ,infoFontColor = "rgb(0,0,0)"
    ,successFontColor = "rgb(0,0,0)"
    ,warningFontColor = "rgb(0,0,0)"
    ,dangerFontColor = "rgb(0,0,0)"
    ,bodyBackColor = "rgb(80, 80, 90)"

    ### header
    ,logoBackColor = "rgb(80, 80, 90)"

    ,headerButtonBackColor = "rgb(238,238,238)"
    ,headerButtonIconColor = "rgb(75,75,75)"
    ,headerButtonBackColorHover = "rgb(210,210,210)"
    ,headerButtonIconColorHover = "rgb(0,0,0)"

    ,headerBackColor = "rgb(238,238,238)"
    ,headerBoxShadowColor = "#aaaaaa"
    ,headerBoxShadowSize = "2px 2px 2px"

    ### sidebar
    ,sidebarBackColor = cssGradientThreeColors(
      direction = "down"
      ,colorStart = "rgb(180, 180, 190)"
      ,colorMiddle = "rgb(200, 200, 220)"
      ,colorEnd = "rgb(220, 220, 230)"
      ,colorStartPos = 0
      ,colorMiddlePos = 50
      ,colorEndPos = 100
    )
    ,sidebarPadding = 0

    ,sidebarMenuBackColor = "transparent"
    ,sidebarMenuPadding = 0
    ,sidebarMenuBorderRadius = 0

    ,sidebarShadowRadius = "3px 5px 5px"
    ,sidebarShadowColor = "#aaaaaa"

    ,sidebarUserTextColor = "rgb(255,255,255)"

    ,sidebarSearchBackColor = "rgb(55,72,80)"
    ,sidebarSearchIconColor = "rgb(153,153,153)"
    ,sidebarSearchBorderColor = "rgb(55,72,80)"

    ,sidebarTabTextColor = "rgb(255,255,255)"
    ,sidebarTabTextSize = 13
    ,sidebarTabBorderStyle = "none none solid none"
    ,sidebarTabBorderColor = "rgb(35,106,135)"
    ,sidebarTabBorderWidth = 1

    ,sidebarTabBackColorSelected = cssGradientThreeColors(
      direction = "right"
      ,colorStart = "rgba(165, 165, 115,.5)"
      ,colorMiddle = "rgba(175, 175, 131,.5)"
      ,colorEnd = "rgba(185, 185, 146,.5)"
      ,colorStartPos = 0
      ,colorMiddlePos = 30
      ,colorEndPos = 100
    )
    ,sidebarTabTextColorSelected = "rgb(0,0,0)"
    ,sidebarTabRadiusSelected = "0px 20px 20px 0px"

    ,sidebarTabBackColorHover = cssGradientThreeColors(
      direction = "right"
      ,colorStart = "rgba(180, 180, 190,1)"
      ,colorMiddle = "rgba(180, 180, 195,1)"
      ,colorEnd = "rgba(200, 200, 230,1)"
      ,colorStartPos = 0
      ,colorMiddlePos = 30
      ,colorEndPos = 100
    )
    ,sidebarTabTextColorHover = "rgb(50,50,50)"
    ,sidebarTabBorderStyleHover = "none none solid none"
    ,sidebarTabBorderColorHover = "rgb(75,126,151)"
    ,sidebarTabBorderWidthHover = 1
    ,sidebarTabRadiusHover = "0px 20px 20px 0px"

    ### boxes
    ,boxBackColor = "rgb(180, 180, 190)"
    ,boxBorderRadius = 5
    ,boxShadowSize = "0px 1px 1px"
    ,boxShadowColor = "rgba(0,0,0,.1)"
    ,boxTitleSize = 16
    ,boxDefaultColor = "rgb(100, 100, 110)"
    ,boxPrimaryColor = "rgba(44,222,235,1)"
    ,boxInfoColor = "rgb(210,214,220)"
    ,boxSuccessColor = "rgba(0,255,213,1)"
    ,boxWarningColor = "rgb(244,156,104)"
    ,boxDangerColor = "rgb(255,88,55)"

    ,tabBoxTabColor = "rgb(80, 80, 90)"
    ,tabBoxTabTextSize = 14
    ,tabBoxTabTextColor = "rgb(0,0,0)"
    ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
    ,tabBoxBackColor = "rgb(255,255,255)"
    ,tabBoxHighlightColor = "rgba(44,222,235,1)"
    ,tabBoxBorderRadius = 5

    ### inputs
    ,buttonBackColor = "rgb(220,220,220)"
    ,buttonTextColor = "rgb(25,25,25)"
    ,buttonBorderColor = "rgb(240,240,240)"
    ,buttonBorderRadius = 5

    ,buttonBackColorHover = "rgb(165,165,165)"
    ,buttonTextColorHover = "rgb(200,200,200)"
    ,buttonBorderColorHover = "rgb(240,240,240)"


    ,textboxBackColor = "rgb(220,220,220)"
    ,textboxBorderColor = "rgb(200,200,200)"
    ,textboxBorderRadius = 5
    ,textboxBackColorSelect = "rgb(65,65,65)"
    ,textboxBorderColorSelect = "rgb(240,240,240)"

    ### tables
    ,tableBackColor = "rgb(255,255,255)"
    ,tableBorderColor = "rgb(240,240,240)"
    ,tableBorderTopSize = 1
    ,tableBorderRowSize = 1

  )

  header <-dashboardHeader(
    title = 'Plot from SQL'
  )

  sidebar <-dashboardSidebar(width = 450,
    selectInput('table','Tables SQL', c("11001033_PAYS")),
    sidebarMenu(
      menuSubItem('Selection graphique',
               tabName = "plot",
               icon = icon('chart-bar'),
               newtab = F
      ),

      menuSubItem(
        'Manipuler les données:',
        tabName = 'handledata',
        icon = icon('calculator'),
        newtab = F


      ),
      menuSubItem('Exporter',
               tabName = "export",
               icon = icon('arrow-alt-circle-down'),
               newtab = F
      )
    ),

    tabItems(
      tabItem(tabName = "plot",
              radioButtons('type', 'Type de graphique', c('-',"colonnes", 'points', "lignes", 'histogramme' , "boxplot" , 'violons' )),
              selectInput('x', "Variables en abscisse (x)", c("-")),
              selectInput('y','Variable en ordonnée (y)', c("-")),
              selectInput('g','Variable catégorielle', c("-"))

      ),
      tabItem(tabName = 'handledata',
              checkboxInput('aff.table','Afficher les données', T),
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
      tabItem(tabName = 'export',
              textInput('nom.df','Nommer les données:',value = "df_sql"),
              actionButton('load.in.r',"Charger dans R"),
              verbatimTextOutput("dir", placeholder = TRUE),
              shinyDirButton("dir", "Changer le dossier d'export", "Exporter donnée en CSV - Choix du dossier"),
              actionButton('export.csv',"Exporter en csv")

      )


    )

  )
  body <- dashboardBody(
 customTheme,
    add_busy_spinner(spin = "semipolar",
                     position = "top-left",
                     height = "150px",
                     width = "150px",
                     margins = c(100, 700)),



    fluidRow(

        plotOutput("hist")
      ),
    fluidRow(

        DT::dataTableOutput('tableau')

    )

  )
  shinyApp(

      ui = dashboardPage(
        header,
        sidebar,
        body
    ),
#    ui = fluidPage(theme = shinytheme("superhero"),
#                   add_busy_spinner(spin = "semipolar",
#                                    position = "top-left",
#                                    height = "150px",
#                                    width = "150px",
#                                    margins = c(100, 700)),
#                   sidebarLayout(
#                     sidebarPanel('Plot',
#                                  selectInput('table','Tables SQL', c("11001033_PAYS")),
#                                  radioButtons('type', 'Type de graphique', c('-',"colonnes", 'points', "lignes", 'histogramme' , "boxplot" , 'violons' )),
#                                  selectInput('x', "Variables en abscisse (x)", c("-")),
#                                  selectInput('y','Variable en ordonnée (y)', c("-")),
#                                  selectInput('g','Variable catégorielle', c("-"))
#
#                     ),
#                     mainPanel(plotOutput("hist"))
#                   ),
#                   sidebarLayout(
#                     sidebarPanel('Manipuler les données:',
#                                  checkboxInput('aff.table','Afficher les données', F),
#                                  textInput('filtre', 'Filtrer les données (dplyr), ex: ANNEE == 2020'),
#                                  selectInput('select',
#                                              'Selectionner les colonnes (dplyr), ex: CANTON, ANNEE',
#                                              c(""),
#                                              multiple = T),
#                                  selectInput('group_by',
#                                              'Aggrèger les données (dplyr::group_by), ex: CANTON, ANNEE',
#                                              c(""),
#                                              multiple = T),
#                                  selectInput('summerise',
#                                              "Fonction d'agrégat",
#                                              c("Somme",'Moyenne','Max','Min','Nombre de ligne')),
#                                  selectInput('varagg',
#                                              "Valeur d'aggrégat",
#                                              c(""),
#                                              multiple = F),
#
#                     ),
#                     mainPanel(
#
#                       DT::dataTableOutput('tableau')
#                     )
#                   ),
#                   sidebarLayout(
#                     sidebarPanel('Exporter',
#                                  textInput('nom.df','Nommer les données:',value = "df_sql"),
#                                  actionButton('load.in.r',"Charger dans R"),
#                                  verbatimTextOutput("dir", placeholder = TRUE),
#                                  shinyDirButton("dir", "Changer le dossier d'export", "Exporter donnée en CSV - Choix du dossier"),
#                                  actionButton('export.csv',"Exporter en csv")
#                     ),
#                     mainPanel(
#                       tableOutput('toto')
#                     )
#                   )
#    ),
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
        updateSelectInput(session, "table",
                          choices = isolate(tables()$Tab))
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
plotsql_(conn)
