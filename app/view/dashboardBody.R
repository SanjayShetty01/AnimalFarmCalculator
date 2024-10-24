box::use(./setupModule,
         ./dashboardModule,
         ./overviewModule)

#' @export
mainBody <- function(ns) {
  bs4Dash::tabItems(
    bs4Dash::tabItem(
      tabName = 'profitablity_overveiw',
      shiny::br(),
      shiny::br(),
      overviewModule$yieldTabUI(ns("tab1"))
    ),
    bs4Dash::tabItem(
      "portfolio_setup",
      shiny::br(),
      shiny::br(),
      setupModule$incomeTabUI(ns("tab2")),

      shiny::fixedRow(
        shiny::column(
          width = 6,
          offset = 3,
          shinyWidgets::actionBttn(
            inputId = "submit_portfolio",
            label = "Calculate Results",
            style = "material-flat",
            color = "primary"
          ) |>
            htmltools::tagAppendAttributes(style = "width: inherit;")
        ))

    ),

    bs4Dash::tabItem(
      "dashboard",
      shiny::br(),
      shiny::br(),
      dashboardModule$shopAnalysisUI(ns("tab3"))
    )
  )
}

#' @export
mainBodyServer <- function(id) {
  shiny::moduleServer(id,  function(input, output, server){
    shiny::observeEvent(input$submit_portfolio, {
      bs4Dash::updateTabItems(session = server, inputId = "sidebar-tabs",
                               selected = "dashboard")
    })
  })
}
