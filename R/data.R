#' ICES Stock Assessment Data
#'
#' A dataset containing ICES Category 1 stock assessments (FLStocks object)
#' with analytical age-based assessments that have been benchmarked.
#'
#' @format An \code{FLStocks} object containing multiple \code{FLStock} objects.
#'   Each stock contains:
#'   \itemize{
#'     \item Stock numbers at age (\code{stock.n})
#'     \item Catch numbers at age (\code{catch.n})
#'     \item Fishing mortality (\code{harvest})
#'     \item Natural mortality (\code{m})
#'     \item Stock weights (\code{stock.wt})
#'     \item Catch weights (\code{catch.wt})
#'     \item Maturity ogive (\code{mat})
#'     \item Selectivity (\code{harvest})
#'     \item Spawning stock biomass (\code{ssb})
#'     \item Catch (\code{catch})
#'   }
#' @source ICES Working Group on Stock Assessment Methods
#' @usage 
#' # Standard R data() function (recommended)
#' data(icesdata)
#' 
#' # Or load using helper function (also works with subfolder structure)
#' load_ices_stocks()
#' 
#' # Or load directly from file path
#' load(system.file("data/stocks/icesdata.RData", package = "icesdata"))
#' @examples
#' \dontrun{
#' data(icesdata)
#' class(icesdata)  # Should be FLStocks
#' names(icesdata)  # List of stock names
#' }
"icesdata"

#' ICES Stock Metadata
#'
#' Metadata information for ICES stocks including stock identification,
#' assessment models, and other stock characteristics.
#'
#' @format A data frame with stock information including:
#'   \itemize{
#'     \item .id - Stock identifier
#'     \item Model - Assessment model type (e.g., "SMS", "XSA")
#'     \item Additional metadata columns
#'   }
#' @source ICES Working Group on Stock Assessment Methods
#' @usage 
#' # Standard R data() function (recommended)
#' data(info)
#' 
#' # Or load using helper function (also works with subfolder structure)
#' load_ices_metadata()
#' 
#' # Or load directly from file path
#' load(system.file("data/metadata/info.RData", package = "icesdata"))
#' @examples
#' \dontrun{
#' data(info)
#' head(info)
#' }
"info"

#' ICES Species Information
#'
#' Species information for ICES stocks including taxonomic and biological
#' characteristics.
#'
#' @format A data frame with species information including:
#'   \itemize{
#'     \item Species identifiers
#'     \item Taxonomic information
#'     \item Biological characteristics
#'   }
#' @source ICES Working Group on Stock Assessment Methods
#' @usage 
#' # Standard R data() function (recommended)
#' data(spp)
#' 
#' # Or load using helper function (also works with subfolder structure)
#' load_ices_metadata()
#' 
#' # Or load directly from file path
#' load(system.file("data/metadata/spp.RData", package = "icesdata"))
#' @examples
#' \dontrun{
#' data(spp)
#' head(spp)
#' }
"spp"

