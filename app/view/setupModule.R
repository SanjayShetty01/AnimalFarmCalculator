box::use(shiny)
box::use(shinyWidgets)
box::use(htmltools)
box::use(./components/sliderInputs)

#' @export
incomeTabUI <- function(id){
  ns <- shiny::NS(id)
  sliderInputsUI <- sliderInputs$generateSliderInputs(
    ns, "tab2-level", 20, 0 ,6, 0)

  shiny::fluidPage(

    bs4Dash::box(
      title = "Enter Portfolio Details",
      status = "primary",
      icon = shiny::icon("pencil"),
      width = 12,

      shiny::fluidRow(class = "slider-row",
                      do.call(shiny::tagList, sliderInputsUI),
                      style = "justify-content: space-between;")
    ),

    # shiny::fixedRow(
    #   shiny::column(
    #   width = 6,
    #   offset = 3,
    #   shinyWidgets::actionBttn(
    #     inputId = "submit_portfolio",
    #     label = "Calculate Results",
    #     style = "material-flat",
    #     color = "primary"
    #   ) |>
    #     htmltools::tagAppendAttributes(style = "width: inherit;")
    # ))
)
}


#' @export
incomeTabServer <- function(id){
  shiny::moduleServer(id, function(input, output, server){
    shiny::observeEvent(input$submit_portfolio, {
      shiny::updateTabsetPanel(inputId = "sidebar-tabs",
                               selected = "dashboard")
    })

    return(
      shiny::reactive({
        inputName <- "tab2-level"
        levels <- paste0(inputName, 1:20)
        level <- 1:20
        numberOfAnimals <- sapply(levels, \(level) input[[level]])

        levelData <- data.frame(Level = level,
                                NofAnimals = numberOfAnimals,
                                check.names = F)
        levelData
      })
    )
  })
}
