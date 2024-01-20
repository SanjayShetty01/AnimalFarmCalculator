box::use(shiny, plotly)

getAnimalShopSliderInputs <- function(start, end, max_value, ns) {
  lapply(start:end, function(i) {
    shiny::sliderInput(
      ns(paste0('shop', i)),
      paste('Enter the number of the Animal you have bought in Level', i),
      min = 0,
      max = max_value[i],
      value = 0
    )
  })
}


shopAnalysisUI <- function(id){
  ns <- shiny::NS(id)

  plotly::plotlyOutput('PlotOfDays')
  shiny::br()
  animalShopSliderInputs <-  getAnimalShopSliderInputs(1, 20,
                                                      c(30, 25, 25, 25, 25, 25,
                                                        20, 20, 20, 15, 15, 15,
                                                        15, 15, 15, 10, 10, 10,
                                                        10, 10), ns)
  shiny::fluidPage(do.call(shiny::tagList, animalShopSliderInputs))

}


shopAnalysisServer <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
    }
  )
}

