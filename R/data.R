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
#' data(icesdata)
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
#' data(info)
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
#' data(spp)
#' @examples
#' \dontrun{
#' data(spp)
#' head(spp)
#' }
"spp"

#' ICES Length-Weight Relationships
#'
#' Length-weight relationship parameters for ICES fish species.
#' Parameters a and b in the equation W = a * L^b where W is weight and L is length.
#'
#' @format A data frame with species length-weight relationships:
#'   \itemize{
#'     \item name - Common name of species
#'     \item latin - Latin/scientific name
#'     \item a - Length-weight parameter a
#'     \item b - Length-weight parameter b
#'     \item Source - Data source (e.g., "FishBase", "ResearchGate")
#'   }
#' @source Various sources including FishBase and ResearchGate
#' @usage 
#' data(lw)
#' @examples
#' \dontrun{
#' data(lw)
#' head(lw)
#' }
"lw"

