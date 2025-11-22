#' Calculate Benchmark Reference Points
#' 
#' Calculates benchmark reference points for ICES stocks
#' 
#' @param icesdata Processed ICES stock data
#' @return Data frame with benchmark reference points
#' @export
#' @examples
#' bm = calculateBenchmarks(icesData)
calculateBenchmarks <- function(icesData) {
  if (!requireNamespace("dplyr", quietly = TRUE) || !requireNamespace("tidyr", quietly = TRUE)) {
    stop("dplyr and tidyr packages are required for this function")
  }
  
  benchmarkData = benchmark(icesData) %>%
    tidyr::pivot_longer(
      cols = c(fmsy, flim, fpa, blim, bpa, btrigger),
      names_to = "rfpt",
      values_to = "Value"
    ) %>%
    dplyr::filter(!is.na(Value))
  
  benchmarkData = merge(benchmarkData, 
              data.frame(rfpt = c("blim", "bpa", "btrigger", "flim", "fpa", "fmsy"),
                         grp = c(rep("SSB", 3), rep("F", 3))), 
              by = "rfpt")[, c(4, 1:3)]
  
  benchmarkData = transform(benchmarkData, 
                  rfpt = factor(rfpt, 
                               levels = c("flim", "fpa", "fmsy", "blim", "bpa", "btrigger"),
                               labels = c(expression(F[lim]),
                                        expression(F[pa]),
                                        expression(F[MSY]),
                                        expression(B[lim]),
                                        expression(B[pa]),
                                        expression(B[trigger]))))
  
  return(benchmarkData)
}

#' Calculate Time Series Data
#' 
#' Calculates time series data for ICES stocks
#' 
#' @param icesData Processed ICES stock data
#' @param benchmarkData Benchmark reference points
#' @return Merged time series and benchmark data
#' @export
#' @examples
#' tsData = calculateTimeseries(icesData, benchmarkData)
calculateTimeseries <- function(icesData, benchmarkData) {
  timeSeriesData = merge(tseries(icesData), benchmarkData)
  return(timeSeriesData)
}

#' Calculate Management Plan Statistics
#' 
#' Calculates management plan statistics for time series data
#' 
#' @param timeSeriesData Time series data
#' @return Data frame with management plan statistics
#' @export
#' @examples
#' mpStats = calculateMpStats(timeSeriesData)
calculateMpStats <- function(timeSeriesData) {
  if (!requireNamespace("plyr", quietly = TRUE)) {
    stop("plyr package is required for this function")
  }
  
  # Extract numeric values from FLR objects if needed
  if (inherits(timeSeriesData$ssb, "FLQuant")) {
    timeSeriesData$ssb = as.numeric(timeSeriesData$ssb)
  }
  if (inherits(timeSeriesData$blim, "FLQuant")) {
    timeSeriesData$blim = as.numeric(timeSeriesData$blim)
  }
  if (inherits(timeSeriesData$bpa, "FLQuant")) {
    timeSeriesData$bpa = as.numeric(timeSeriesData$bpa)
  }
  if (inherits(timeSeriesData$btrigger, "FLQuant")) {
    timeSeriesData$btrigger = as.numeric(timeSeriesData$btrigger)
  }
  
  mpStats = plyr::ddply(timeSeriesData, .(year), with, 
              data.frame(blim = mean(ssb < blim, na.rm = TRUE),
                         bpa = mean(ssb < bpa, na.rm = TRUE),
                         btrigger = mean(ssb < btrigger, na.rm = TRUE)))
  
  mpStatsFinal = data.frame(
    year = rep(mpStats$year, 3),
    ymin = c(rep(0, nrow(mpStats)), mpStats$blim, mpStats$bpa),
    ymax = c(mpStats$blim, mpStats$bpa, rep(1, nrow(mpStats))),
    Status = factor(
      rep(c("Blim", "Bpa", "Above Bpa"), each = nrow(mpStats)),
      levels = c("Blim", "Bpa", "Above Bpa")))
  
  return(mpStatsFinal)
}

#' Calculate Forward FMSY Projections
#' 
#' Calculates forward projections using FMSY reference points
#' 
#' @param stock FLStock object
#' @param years Years to project
#' @param fmsy FMSY value to use
#' @return Projected stock
#' @export
fwdFmsy <- function(stock, years, fmsy) {
  # Implementation would depend on FLR package availability
  # This is a placeholder for the consolidated function
  warning("This function requires FLR package and is not yet implemented")
  return(NULL)
}
