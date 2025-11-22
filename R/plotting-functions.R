#' Plot Stock Status
#' 
#' Creates a plot showing stock status over time
#' 
#' @param data Stock data
#' @param x Column name for x-axis (default: "year")
#' @param y Column name for y-axis (default: "stock")
#' @param group Column name for grouping (default: ".id")
#' @return ggplot object
#' @export
plotStockStatus <- function(data, x = "year", y = "stock", group = ".id") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for this function")
  }
  
  ggplot2::ggplot(data, ggplot2::aes_string(x = x, y = y, color = group)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Stock Status Over Time",
                   x = "Year",
                   y = "Stock Size")
}

#' Plot Fishing Mortality
#' 
#' Creates a plot showing fishing mortality over time
#' 
#' @param data Stock data
#' @param x Column name for x-axis (default: "year")
#' @param y Column name for y-axis (default: "f")
#' @param group Column name for grouping (default: ".id")
#' @return ggplot object
#' @export
plotFishingMortality <- function(data, x = "year", y = "f", group = ".id") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for this function")
  }
  
  ggplot2::ggplot(data, ggplot2::aes_string(x = x, y = y, color = group)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Fishing Mortality Over Time",
                   x = "Year",
                   y = "Fishing Mortality (F)")
}

#' Plot Management Plan Statistics
#' 
#' Creates a stacked area plot showing management plan statistics
#' 
#' @param managementPlanStats Management plan statistics data
#' @return ggplot object
#' @export
plotManagementPlanStats <- function(managementPlanStats) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for this function")
  }
  
  ggplot2::ggplot(managementPlanStats, ggplot2::aes(x = year, ymin = ymin, ymax = ymax, fill = Status)) +
    ggplot2::geom_ribbon(alpha = 0.7) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Management Plan Statistics",
                   x = "Year",
                   y = "Proportion of Stocks",
                   fill = "Status") +
    ggplot2::scale_fill_brewer(palette = "Set2")
}

#' Plot Benchmark Reference Points
#' 
#' Creates a plot showing benchmark reference points
#' 
#' @param benchmarkData Benchmark data
#' @return ggplot object
#' @export
plotBenchmarks <- function(benchmarkData) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for this function")
  }
  
  ggplot2::ggplot(benchmarkData, ggplot2::aes(x = rfpt, y = Value, fill = grp)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Benchmark Reference Points",
                   x = "Reference Point",
                   y = "Value",
                   fill = "Group") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}
