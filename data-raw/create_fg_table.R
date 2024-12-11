# Create a food groups reference table -----------------------------------------

fgtable <- data.frame(
  indicator = c(
    "Food Consumption Score",
    "Food Consumption Score for Nutrition Quality Analysis",
    "Household Dietary Diversity Score",
    "Minimum Dietary Diversity for Women",
    "Women's Dietary Diversity Score",
    "Infant and Young Child Feeding"
  ),
  indicator_code = c("FCS", "FCS-N", "HDDS", "MDD-W", "WDDS", "IYCF")
)

foodgroups <- list(
  FCS = c("cereals", "roots_tubers", "vegetables", "fruits", "meat", "eggs", 
          "fish", "pulses", "milk", "oil", "sugar", "condiments"),
  "FCS-N" = c(),
  HDDS = c(),
  "MDD-W" = c("staples", "pulses", "nuts", "milk", "meat_fish", "eggs",
              "green_leafy", "other_vita", "other_vegetables", "other_fruits"),
  WDDS = c(),
  IYCF = c()
)