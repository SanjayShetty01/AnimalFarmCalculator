box::use(./calculateCost)
box::use(./calculateIncome)

#' Calculate the annual profit yield
#'
#' @param level The level of the farm.
#' @param nAnimal The number of animals.
#' @param store_type The type of store.
#' @param nAnimalBought The number of animals bought.
#'
#' @return Annual profit yield
#' @export

calculateYield <- function(level, nAnimal, store_type = "DailyPrice",
                           nAnimalBought = 0){

 cost <- calculateCost$calculateCost(level = level, nAnimal = nAnimal,
                                     store_type = store_type,
                                     nAnimalBought = nAnimalBought)

 income <- calculateIncome$calculateIncome(level = level, nAnimal = nAnimal)

 yield <- income |>
   (\(x) x * 24)() |> # Daily Revenue
   (\(x) x * 365)() |> # Yearly Revenue
   (\(x) x - cost)() |> # Total Profit
   (\(x) (x / cost)/100)() # Annual Profit Yield

 yield <- round(yield, 2)
 return(yield)
}

#' Calculate the total yield percentage for a farm.
#'
#' This function calculates the total yield percentage for a farm based on the
#' total yearly income and total shop cost. The yield is calculated as the
#' percentage of total yearly income relative to the total shop cost.
#'
#' @param inputName The prefix of the input names representing different levels.
#' @return The total yield percentage for the farm.
#'
#' @examples
#' # Example usage:
#' # calculateTotalYield("level")
#'
#' @export

calculateTotalYield <- function(level, nAnimals){
  totalShopCost <- calculateCost$calculateTotalShopCost(nAnimals)
  totalYearlyIncome <- calculateIncome$calculateYearlyIncome(level, nAnimals)

  totalYield <- ifelse(totalShopCost == 0, 0,
                       (totalYearlyIncome/totalShopCost) * 100)
  return(round(totalYield, 2))
}
