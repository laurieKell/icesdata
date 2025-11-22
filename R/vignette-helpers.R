# Vignette Helper Functions
# These functions are available for use in vignettes

#' Update Reference Points for FLBRP Object
#'
#' Updates reference points for an FLBRP object by computing various reference points
#' including virgin, MSY, crash, SPR, and benchmark reference points.
#' 
#' @note This function requires the FLBRP package and may require additional FLR packages
#' depending on the functions used (refCreate, rmax, rmsy, rvirgin, computeRefpts).
#'
#' @param x An FLBRP object
#' @return An FLBRP object with updated reference points
#' @export
#' @importFrom FLBRP refpts refpts<-
#' @examples
#' \dontrun{
#' # Assuming you have an FLBRP object
#' if (requireNamespace("FLBRP", quietly = TRUE)) {
#'   brp <- updateRefs(brp)
#' }
#' }
updateRefs <- function(x) {
  # Note: These functions may be from FLBRP or other FLR packages
  # This assumes they're available when the function is called
  refpts(x) <- rbind(
    refCreate(c("virgin", "msy", "crash", "spr.30", "spr.20")),
    rmax(x, 1.0),
    rmax(x, 0.3),
    rmsy(x, 0.5),
    rvirgin(x, 0.3),
    refCreate(attributes(x)$benchmark))
  refpts(x) <- computeRefpts(x)
  refpts(x) <- refpts(x)[sort(dimnames(refpts(x))$refpt)]
  return(x)
}

#' Load ICES Data from Package
#' 
#' Helper function to load ICES data files from the package or vignette directories.
#' This is primarily intended for use in vignettes.
#' 
#' @param filename Name of the RData file to load (e.g., "icesdata.RData")
#' @return Logical indicating success
#' @export
#' @examples
#' \dontrun{
#' # In a vignette
#' load_ices_data("icesdata.RData")
#' }
load_ices_data <- function(filename, subdir = NULL) {
  # Try package data directory with subdirectories
  if (is.null(subdir)) {
    # Try common locations
    data_locations <- c(
      "data/stocks",
      "data/metadata",
      "data"
    )
  } else {
    data_locations <- c(
      paste("data", subdir, sep = "/"),
      "data"
    )
  }
  
  for (loc in data_locations) {
    data_dir <- system.file(loc, package = "icesdata")
    data_file <- file.path(data_dir, filename)
    
    if (file.exists(data_file)) {
      load(data_file, envir = .GlobalEnv)
      return(TRUE)
    }
  }
  
  # Try vignette data directory with subdirectories
  vignette_subdirs <- c("analysis", "reference-points", "derived", "")
  for (subdir in vignette_subdirs) {
    if (subdir == "") {
      vignette_data_dir <- system.file("vignettes/data", package = "icesdata")
    } else {
      vignette_data_dir <- system.file(paste("vignettes/data", subdir, sep = "/"), package = "icesdata")
    }
    data_file <- file.path(vignette_data_dir, filename)
    
    if (file.exists(data_file)) {
      load(data_file, envir = .GlobalEnv)
      return(TRUE)
    }
  }
  
  # Try local data directory (for development)
  if (file.exists(file.path("data", filename))) {
    load(file.path("data", filename), envir = .GlobalEnv)
    return(TRUE)
  }
  
  warning(paste("Could not find", filename))
  return(FALSE)
}

#' Check if FLR Packages are Available
#' 
#' Checks for availability of commonly used FLR packages.
#' Useful for conditional code execution in vignettes.
#' 
#' @return Named logical vector indicating availability of each package
#' @export
#' @examples
#' \dontrun{
#' pkgs <- check_flr_packages()
#' if (pkgs["FLCandy"]) {
#'   # Use FLCandy functions
#' }
#' }
check_flr_packages <- function() {
  packages <- c(
    "FLCore",
    "FLBRP",
    "FLife",
    "FLasher",
    "FLCandy"
  )
  
  availability <- sapply(packages, requireNamespace, quietly = TRUE)
  names(availability) <- packages
  return(availability)
}

#' Get Vignette Data Directory
#' 
#' Returns the path to the vignette data directory, with fallbacks
#' to package data directory or local data directory.
#' 
#' @return Character vector with data directory path, or NULL if not found
#' @export
#' @examples
#' \dontrun{
#' data_dir <- get_vignette_data_dir()
#' if (!is.null(data_dir)) {
#'   load(file.path(data_dir, "data.RData"))
#' }
#' }
get_vignette_data_dir <- function(subdir = NULL) {
  # Try vignette data directory with optional subdirectory
  if (is.null(subdir)) {
    vignette_data_dir <- system.file("vignettes/data", package = "icesdata")
  } else {
    vignette_data_dir <- system.file(paste("vignettes/data", subdir, sep = "/"), package = "icesdata")
  }
  
  if (dir.exists(vignette_data_dir)) {
    return(vignette_data_dir)
  }
  
  # Fallback to package data directory
  data_dir <- system.file("data", package = "icesdata")
  if (dir.exists(data_dir)) {
    return(data_dir)
  }
  
  # Final fallback to local data directory
  if (dir.exists("data")) {
    return("data")
  }
  
  return(NULL)
}

