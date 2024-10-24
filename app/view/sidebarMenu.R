#' @export
sidebarMenu <- function(ns) {
  bs4Dash::sidebarMenu(
    id = ns("sidebar-tabs"),
    bs4Dash::menuItem(
      text = "Profitablity Overveiw",
      tabName = "profitablity_overveiw",
      icon = shiny::icon("eye")
    ),
    bs4Dash::menuItem(
      text = "Portfolio Setup",
      tabName = "portfolio_setup",
      icon = shiny::icon("table")
    ),
    bs4Dash::menuItem(
      text = "Portfolio Dashboard",
      tabName = "dashboard",
      icon = shiny::icon("chart-simple")
    )
  )
}
