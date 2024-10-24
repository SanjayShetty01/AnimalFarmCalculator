box::use(./calculateCost)
box::use(./calculateIncome)

#' Calculate Investment Recovery Time
#'
#' This function calculates the number of days required to recover the investment based on the cost and income.
#'
#' @param level The level of the animal.
#' @param nAnimal The number of animals.
#' @param store_type The type of store ('DailyPrice' or 'Price_in_Stores').
#' @param nAnimalBought The number of animals already bought.
#'
#' @return The investment recovery time in days (rounded to the nearest whole number).
#'
#' @examples
#' investmentRecoveryTime(level = 3, nAnimal = 5, store_type = 'DailyPrice', nAnimalBought = 2)
#'
#' @export

investmentRecoveryTime <- function(level, nAnimal, store_type = "DailyPrice",
                                   nAnimalBought = 0){

  cost <- calculateCost$calculateCost(level = level, nAnimal = nAnimal,
                                      store_type = store_type,
                                      nAnimalBought = nAnimalBought)

  income <- calculateIncome$calculateIncome(level = level, nAnimal = nAnimal)

  recoverDays <- {cost/(income*24)} |>
    round()

  return(recoverDays)
}

#' Calculate the investment recovery time for a farm.
#'
#' This function calculates the investment recovery time for a farm based on the
#' total shop cost and total daily income. The investment recovery time is
#' calculated as the ratio of total shop cost to total daily income.
#'
#' @param inputName The prefix of the input names representing different levels.
#' @return The investment recovery time for the farm.
#'
#' @examples
#' # Example usage:
#' # investmentRecoveryTimeFarm("level")
#'
#' @export
investmentRecoveryTimeFarm <- function(level, nAnimals){
  totalShopCost <- calculateCost$calculateTotalShopCost(nAnimals)
  totalDailyIncome <- calculateIncome$calculateDailyIncome(level, nAnimals)

  totalYield <- round(ifelse(totalShopCost == 0, 0,
                             (totalShopCost/totalDailyIncome)), 2)

  return(totalYield)
}


