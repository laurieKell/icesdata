#' Load ICES Stock Data
#'
#' Loads ICES stock data from the organized data directory structure.
#' This function handles the subdirectory organization.
#'
#' @return Logical indicating success
#' @export
#' @examples
#' \dontrun{
#' load_ices_stocks()
#' # Now icesdata is available in global environment
#' }
load_ices_stocks <- function() {
  data_file <- system.file("data/stocks/icesdata.RData", package = "icesdata")
  
  if (file.exists(data_file)) {
    load(data_file, envir = .GlobalEnv)
    return(TRUE)
  }
  
  # Fallback: try root data directory
  root_data_file <- system.file("data/icesdata.RData", package = "icesdata")
  if (file.exists(root_data_file)) {
    load(root_data_file, envir = .GlobalEnv)
    return(TRUE)
  }
  
  warning("Could not find icesdata.RData in data/stocks/ or data/")
  return(FALSE)
}

#' Load ICES Metadata
#'
#' Loads ICES stock metadata from the organized data directory structure.
#'
#' @return Logical indicating success
#' @export
#' @examples
#' \dontrun{
#' load_ices_metadata()
#' # Now info and spp are available in global environment
#' }
load_ices_metadata <- function() {
  info_file <- system.file("data/metadata/info.RData", package = "icesdata")
  spp_file <- system.file("data/metadata/spp.RData", package = "icesdata")
  
  success <- TRUE
  
  if (file.exists(info_file)) {
    load(info_file, envir = .GlobalEnv)
  } else {
    # Fallback: try root data directory
    root_info_file <- system.file("data/info.RData", package = "icesdata")
    if (file.exists(root_info_file)) {
      load(root_info_file, envir = .GlobalEnv)
    } else {
      warning("Could not find info.RData in data/metadata/ or data/")
      success <- FALSE
    }
  }
  
  if (file.exists(spp_file)) {
    load(spp_file, envir = .GlobalEnv)
  } else {
    # Fallback: try root data directory
    root_spp_file <- system.file("data/spp.RData", package = "icesdata")
    if (file.exists(root_spp_file)) {
      load(root_spp_file, envir = .GlobalEnv)
    } else {
      warning("Could not find spp.RData in data/metadata/ or data/")
      success <- FALSE
    }
  }
  
  return(success)
}

#' Load All ICES Input Data
#'
#' Convenience function to load all ICES input data (stocks and metadata).
#'
#' @return Logical indicating success
#' @export
#' @examples
#' \dontrun{
#' load_all_ices_data()
#' # Now icesdata, info, and spp are available
#' }
load_all_ices_data <- function() {
  stocks_ok <- load_ices_stocks()
  metadata_ok <- load_ices_metadata()
  return(stocks_ok && metadata_ok)
}

