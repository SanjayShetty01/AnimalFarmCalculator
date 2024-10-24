box::use(shiny, plotly, bs4Dash)
box::use(
  ./view/setupModule,
  ./view/dashboardModule,
  ./view/overviewModule,
  ./view/sidebarMenu,
  ./view/dashboardBody,
  ./view/dashBrand
)

#' @export
ui <- function(id) {
  ns <- shiny::NS(id)
  header <- bs4Dash::dashboardHeader(title = dashBrand$title)
  sidebar <- bs4Dash::dashboardSidebar(sidebarMenu$sidebarMenu(ns),
                                       minified = F)

  body <- bs4Dash::dashboardBody(dashboardBody$mainBody(ns))

  bs4Dash::dashboardPage(
    header = header,
    sidebar = sidebar,
    body = body,
    fullscreen = T,
    help = NULL
  )
}

#' @export
server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    overviewModule$yieldTabServer("tab1")

    levelData <- setupModule$incomeTabServer("tab2")

    dashboardBody$mainBodyServer("submit_portfolio")

    dashboardModule$shopAnalysisServer("tab3", data = levelData)
  })
}
