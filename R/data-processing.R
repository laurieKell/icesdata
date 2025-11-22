#' Process ICES Stock Data (Backwards Compatibility)
#' 
#' Loads and processes ICES stock data with standard cleaning operations.
#' This function provides backwards compatibility with the original name.
#' For new code, use \code{processIcesData} (with capital D) from data-loading.R
#' 
#' @param dataPath Path to the ICES data file
#' @param infoPath Path to the info file
#' @return Processed ICES stock data
#' @export
#' @examples
#' Icesdata = processIcesdata("path/to/data.RData", "path/to/info.RData")
processIcesdata <- function(dataPath, infoPath) {
  # Call the standardized function from data-loading.R
  if (!exists("processIcesData")) {
    stop("processIcesData function not found. Please load the data-loading.R functions.")
  }
  return(processIcesData(dataPath, infoPath))
}
