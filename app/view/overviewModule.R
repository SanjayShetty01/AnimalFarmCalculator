box::use(shiny)
box::use(../../app/logic/calculateIncome)
box::use(../../app/logic/calculateYield)
box::use(../../app/logic/investmentRecoveryTime)
box::use(../../app/logic/calculateCost)


#' @export
yieldTabUI <- function(id){
  ns <- shiny::NS(id)

  shiny::fluidPage(
    bs4Dash::box(
      title = "User Input",
      status = "primary",
      icon = shiny::icon("pencil"),
      width = 12,
      shiny::fixedRow(
        shiny::column(width = 5, shiny::selectInput(
          ns('storeType'),
          'Store or Daily Store?',
          c("Evo Store" = 'Price_in_Stores', 'Daily Shop' = 'DailyPrice')
        )),
        shiny::column(
          offset = 2,
          width = 5,
          shiny::numericInput(
            ns('Nbought'),
            "Enter the number of animals already
                            bought in the shop",
            0
          )
        ),
        shiny::column(
          offset = 7,
          width = 5,
          shiny::tags$small(shiny::em('*Only applicable for Evolution Store'))
        )
      ),

      shiny::br(),
      shiny::br(),
      shiny::fixedRow(
        shiny::column(
          width = 5,
          shiny::sliderInput(
            ns('Nanimal'),
            'Number of Animals',
            min = 1,
            max = 10,
            value = 1
          )
        ),

        shiny::column(
          width = 5,
          offset = 2,
          shiny::sliderInput(
            ns('Level'),
            'Enter the Level of the Animal',
            min = 1,
            max = 20,
            value = 2
          )
        )
      )
    ),

    bs4Dash::box(
      title = "Summary",
      status = "primary",
      icon = shiny::icon("cow"),
      width = 12,

      shiny::fluidRow(
        bs4Dash::bs4InfoBoxOutput(ns("costBox")),
        bs4Dash::bs4InfoBoxOutput(ns("yieldBox")),
        bs4Dash::bs4InfoBoxOutput(ns("reqDaysBox"))
      )
    )

  )
}

#' @export
yieldTabServer <- function(id){
  shiny::moduleServer(id, function(input, output, server){

    output$costBox <- bs4Dash::renderbs4InfoBox({
      cost <- calculateCost$calculateCost(level = input$Level,
                                          nAnimal = input$Nanimal,
                                          store_type = input$storeType,
                                          nAnimalBought = input$Nbought)

      bs4Dash::valueBox(
        subtitle = shiny::em(shiny::strong("Cost")),
        value = shiny::h2(cost),
        icon = shiny::icon("money-bill"),
        color = "success",
        width = 4
      )
    })


    output$yieldBox <- bs4Dash::renderbs4InfoBox({
      yield <- calculateYield$calculateYield(level = input$Level,
                                             nAnimal = input$Nanimal,
                                             store_type = input$storeType,
                                             nAnimalBought = input$Nbought)
      bs4Dash::valueBox(
        subtitle = shiny::em(shiny::strong("Yield")),
        value = shiny::h2(paste0(yield, "%")),
        icon = shiny::icon("percent"),
        color = "orange",
        width = 4
      )
    })

    output$reqDaysBox <- bs4Dash::renderbs4InfoBox({
      reqDays <- investmentRecoveryTime$investmentRecoveryTime(
        level = input$Level, nAnimal = input$Nanimal,
        store_type = input$storeType, nAnimalBought = input$Nbought)

      bs4Dash::valueBox(
        subtitle = shiny::em(shiny::strong("Days Required to recover")),
        value = shiny::h2(reqDays),
        icon = shiny::icon("calendar"),
        color = "teal",
        width = 4
      )
    })
  })
}
