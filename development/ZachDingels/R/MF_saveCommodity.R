saveCommodity <- function(commodity) {
  print(paste0("Saving Commodity: ", commodity$Meta$Commodity, " to ", commodity$Meta$FilePath))
  assign(commodity$Meta$Commodity, commodity)
  save(commodity, file=commodity$Meta$FilePath)
}