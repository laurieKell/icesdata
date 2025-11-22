#' Process ICES Stock Data
#' 
#' Loads and processes ICES stock data with standard cleaning operations
#' 
#' @param dataPath Path to the ICES data file
#' @param infoPath Path to the info file
#' @return Processed ICES stock data
#' @export
#' @examples
#' Icesdata = processIcesdata("path/to/data.RData", "path/to/info.RData")
processIcesdata <- function(dataPath, infoPath) {
  # Load data
  load(dataPath)
  load(infoPath)
  
  Icesdata = ICESStocks
  rm(ICESStocks)
  
  stks = names(Icesdata)
  
  # Process SMS stocks
  for (.id in subset(info, Model == "SMS")$.id) {
    Icesdata[[.id]] = FLCore::qapply(Icesdata[[.id]], function(x) {
      dimnames(x)[3:5] = dimnames(stock.n(Icesdata[[.id]]))[3:5]
      dimnames(x)[[3]] = "unique"
      dimnames(x)[[4]] = "all"
      dimnames(x)[[5]] = "unique"
      x})}
  
  # Clean catch data
  for(.id in names(Icesdata)) {
    small = min(catch.n(Icesdata[[.id]]), na.rm = TRUE) * 1e-6
    catch.n(Icesdata[[.id]])[is.na(catch.n(Icesdata[[.id]]))] = small
    landings.n(Icesdata[[.id]])[is.na(landings.n(Icesdata[[.id]]))] = small
    discards.n(Icesdata[[.id]])[is.na(catch.n(Icesdata[[.id]]))] = 0.0}
  
  return(Icesdata)}
