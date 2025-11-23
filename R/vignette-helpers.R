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
  # Try package data directory
  data_dir <- system.file("data", package = "icesdata")
  data_file <- file.path(data_dir, filename)
  
  if (file.exists(data_file)) {
    load(data_file, envir = .GlobalEnv)
    return(TRUE)
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

#' Get Vignette Output Directory
#' 
#' Returns the path to the vignette output directory (data/vignette-outputs).
#' Creates the directory if it doesn't exist. This is a simplified version
#' specifically for saving vignette outputs.
#' 
#' @return Character vector with output directory path
#' @export
#' @examples
#' \dontrun{
#' vignetteDataDir <- get_vignette_output_dir()
#' save(myData, file = file.path(vignetteDataDir, "myData.RData"))
#' }
get_vignette_output_dir <- function() {
  outputDir = file.path("data", "vignette-outputs")
  if (!dir.exists(outputDir)) {
    dir.create(outputDir, recursive = TRUE)
  }
  return(outputDir)
}

#' Load All ICES Package Data
#' 
#' Convenience function to load all standard ICES data objects at once.
#' Loads \code{icesdata}, \code{info}, \code{spp}, and \code{lw} using \code{data()}.
#' 
#' @return Invisibly returns TRUE on success
#' @export
#' @examples
#' \dontrun{
#' # In a vignette
#' load_all_ices_data()
#' # Now icesdata, info, spp, and lw are available
#' }
load_all_ices_data <- function() {
  data(icesdata, envir = parent.frame())
  data(info, envir = parent.frame())
  data(spp, envir = parent.frame())
  data(lw, envir = parent.frame())
  invisible(TRUE)
}

#' Load Optional Packages
#' 
#' Attempts to load multiple optional packages, only loading those that are available.
#' Useful for vignettes where some packages may not be installed.
#' 
#' @param packages Character vector of package names to load
#' @param quietly Logical, if TRUE (default) suppresses messages and warnings
#' @return Logical vector indicating which packages were successfully loaded
#' @export
#' @examples
#' \dontrun{
#' # In a vignette
#' load_optional_packages(c("ggplotFL", "FLCandy", "rpart"))
#' }
load_optional_packages <- function(packages, quietly = TRUE) {
  loaded = sapply(packages, function(pkg) {
    if (requireNamespace(pkg, quietly = quietly)) {
      library(pkg, character.only = TRUE, quietly = quietly)
      TRUE
    } else {
      FALSE
    }
  })
  names(loaded) = packages
  return(loaded)
}

