box::use(./readFarmInfoData)

#' Calculate the cost of animals based on farm level and store type.
#'
#' This function calculates the cost of animals based on the specified farm
#' level,
#' store type, number of animals, and number of animals bought.
#'
#' @param level The farm level.
#' @param store_type The type of store (e.g., 'DailyPrice').
#' @param nAnimal The number of animals.
#' @param nAnimalBought The number of animals bought.
#'
#' @return The calculated cost of animals.
#'
#' @examples
#' # Example usage:
#' calculateCost(level = 1, store_type = 'DailyPrice', nAnimal = 5,
#' nAnimalBought = 2)
#'
#'@export

calculateCost <- function(level, nAnimal, store_type = "DailyPrice",
                          nAnimalBought = 0) {
  data <- readFarmInfoData$readFarmInfoData()

    if (identical(store_type,'DailyPrice')) {
    prices = (data[[level, store_type]]) * nAnimal
  } else {
    prices <- sum(sapply(1:nAnimal, \(i) data[[level, store_type]] +
                           (300 * (i - 1 + nAnimalBought))))
  }

  return(prices)
}

calculateShopCostPerLevel <- function(nAnimals, price){
  shopCost <- ifelse(nAnimals == 0, 0, (nAnimals * 300) + price)

  return(shopCost)
}

#' Calculate the total cost of the shop based on input levels
#'
#' This function takes the input levels for different animal types in the shop
#' and calculates the total cost of the shop by considering the prices of
#' each animal type in the stores.
#'
#' @param inputName The prefix of the input names representing different levels
#' @return The total cost of the shop
#'
#' @examples
#' # Example usage:
#' # calculateTotalShopCost("level")
#'
#' @export
calculateTotalShopCost <- function(nAnimals){
  prices <- readFarmInfoData$readFarmInfoData() |>
    (\(x) x$Price_in_Stores)()

  shopTotalCost <- calculateShopCostPerLevel(nAnimals = nAnimals,
                                             price = prices) |>
    sum() |>
    round(2)

  return(shopTotalCost)
}

