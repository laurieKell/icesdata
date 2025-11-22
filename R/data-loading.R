#' Load ICES Stock Assessment Data
#' 
#' @param filePath Path to ICES data file
#' @param speciesName Species name to filter (default: "Nephrops norvegicus")
#' @param stockIndices Indices of stocks to select (default: 3:12)
#' @return Processed stock data
#' @export
loadIcesStockData <- function(filePath, 
                             speciesName = "Nephrops norvegicus",
                             stockIndices = 3:12) {
  
  # Read data
  data = read.csv(filePath)
  
  # Filter by species
  neph = subset(data, SpeciesName == speciesName)[,
    c("FishStock", "Year",
      "Low_StockSize", "StockSize", "High_StockSize",
      "Landings", "Catches",
      "Low_FishingPressure", "FishingPressure", "High_FishingPressure",
      "FishingPressureDescription",
      "Fpa", "Blim", "FMSY", "MSYBtrigger")]
  
  # Select specific stocks
  neph = subset(neph, FishStock %in% sort(unique(neph$FishStock))[stockIndices])
  
  # Rename columns
  names(neph) = c(".id", "year", "stockLow", "stock", "stockHigh", 
                   "landings", "catch",
                   "fLow", "f", "fHigh", "fType",
                   "fpa", "blim", "fmsy", "msybtrigger")
  
  return(neph)
}

#' Load ICES Data from RData File
#' 
#' @param filePath Path to ICES RData file
#' @return Loaded ICES data
#' @export
loadIcesRdata <- function(filePath = "data/icesdata.RData") {
  if (file.exists(filePath)) {
    load(filePath)
    cat("ICES data loaded successfully from:", filePath, "\n")
    return(TRUE)
  } else {
    warning("ICES data file not found:", filePath)
    return(FALSE)
  }
}

#' Load Stock Categories from Excel
#' 
#' @param filePath Path to Excel file
#' @param sheetName Sheet name (default: "cats")
#' @return Stock categories data
#' @export
loadStockCategories <- function(filePath = "data/stocks.xls", 
                               sheetName = "cats") {
  if (requireNamespace("readxl", quietly = TRUE)) {
    stockCats = readxl::read_excel(filePath, sheet = sheetName)
    return(stockCats)
  } else {
    stop("readxl package is required to read Excel files")
  }
}

#' Create Stock ID Vector
#' 
#' @return Vector of stock IDs
#' @export
getNephropsStockIds <- function() {
  c("nep.fu.22", "nep.fu.19", "nep.fu.2021", "nep.fu.17", "nep.fu.16",
    "nep.fu.11", "nep.fu.12", "nep.fu.13", "nep.fu.14", "nep.fu.15")
}

#' Create Catch Multiplier Vector
#' 
#' @return Vector of catch multipliers
#' @export
getNephropsCatchMultipliers <- function() {
  c(1e9, 1e6, 1e9, 1e9, 1e9,
    1e9, 1e9, 1e9, 1e6, 1e9)
}

#' Process ICES Stock Data
#' 
#' Loads and processes ICES stock data with standard cleaning operations
#' 
#' @param dataPath Path to the ICES data file
#' @param infoPath Path to the info file
#' @return Processed ICES stock data
#' @export
#' @examples
#' icesData = processIcesData("path/to/data.RData", "path/to/info.RData")
processIcesData <- function(dataPath, infoPath) {
  # Load data
  load(dataPath)
  load(infoPath)
  
  icesData = ICESStocks
  rm(ICESStocks)
  
  stockNames = names(icesData)
  
  # Process SMS stocks
  for (stockId in subset(info, Model == "SMS")$stockId) {
    icesData[[stockId]] = qapply(icesData[[stockId]], function(x) {
      dimnames(x)[3:5] = dimnames(stock.n(icesData[[stockId]]))[3:5]
      dimnames(x)[[3]] = "unique"
      dimnames(x)[[4]] = "all"
      dimnames(x)[[5]] = "unique"
      x
    })
  }
  
  # Clean catch data
  for(stockId in names(icesData)) {
    smallValue = min(catch.n(icesData[[stockId]]), na.rm = TRUE) * 1e-6
    catch.n(icesData[[stockId]])[is.na(catch.n(icesData[[stockId]]))] = smallValue
    landings.n(icesData[[stockId]])[is.na(landings.n(icesData[[stockId]]))] = smallValue
    discards.n(icesData[[stockId]])[is.na(catch.n(icesData[[stockId]]))] = 0.0
  }
  
  return(icesData)
}
