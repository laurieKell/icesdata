#' Standard ICES ggplot2 Theme
#'
#' A standardized ggplot2 theme for ICES stock assessment visualizations.
#' This theme provides a clean, publication-ready appearance suitable for
#' scientific papers and presentations.
#'
#' @param base_size Base font size (default: 12)
#' @param base_family Base font family (default: "sans")
#' @param base_line_size Base size for line elements (default: base_size/22)
#' @param base_rect_size Base size for rect elements (default: base_size/22)
#' @param legend_position Position of legend ("none", "left", "right", "bottom", "top", or coordinates)
#' @param panel_grid Logical indicating whether to show panel grid lines (default: TRUE)
#' @param panel_border Logical indicating whether to show panel border (default: TRUE)
#'
#' @return A ggplot2 theme object
#'
#' @details
#' This theme is based on `theme_minimal()` with customizations for ICES publications:
#' - Clean, minimal appearance suitable for scientific publications
#' - Adjustable font sizes for different publication contexts
#' - Configurable grid lines and borders
#' - Proper spacing and margins for time series plots
#'
#' @import ggplot2
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(icesdata)
#'
#' # Basic usage
#' ggplot(data, aes(x = year, y = catch)) +
#'   geom_line() +
#'   theme_ices()
#'
#' # With custom font size
#' ggplot(data, aes(x = year, y = catch)) +
#'   geom_line() +
#'   theme_ices(base_size = 16)
#'
#' # Without grid lines
#' ggplot(data, aes(x = year, y = catch)) +
#'   geom_line() +
#'   theme_ices(panel_grid = FALSE)
#' }
theme_ices <- function(base_size = 12,
                       base_family = "sans",
                       base_line_size = base_size/22,
                       base_rect_size = base_size/22,
                       legend_position = "right",
                       panel_grid = TRUE,
                       panel_border = TRUE) {
  
  # ggplot2 is now in Imports, so it should be available
  
  # Start with theme_minimal
  half_line <- base_size / 2
  
  theme <- ggplot2::theme_minimal(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  )

  # Customize the theme
  theme <- theme + ggplot2::theme(
    # Panel styling
    panel.background = ggplot2::element_rect(fill = "white", colour = NA),
    panel.border = if (panel_border) {
      ggplot2::element_rect(fill = NA, colour = "grey70", size = base_line_size)
    } else {
      ggplot2::element_blank()
    },
    
    # Grid lines
    panel.grid.major = if (panel_grid) {
      ggplot2::element_line(colour = "grey90", size = base_line_size/2)
    } else {
      ggplot2::element_blank()
    },
    panel.grid.minor = if (panel_grid) {
      ggplot2::element_line(colour = "grey95", size = base_line_size/3)
    } else {
      ggplot2::element_blank()
    },
    
    # Axis styling
    axis.line = ggplot2::element_line(colour = "grey70", size = base_line_size),
    axis.ticks = ggplot2::element_line(colour = "grey70", size = base_line_size),
    axis.ticks.length = ggplot2::unit(base_size/22, "pt"),
    axis.text = ggplot2::element_text(size = ggplot2::rel(0.9), colour = "grey30"),
    axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = base_size/4), vjust = 1),
    axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = base_size/4), hjust = 1),
    axis.title = ggplot2::element_text(size = ggplot2::rel(1.0), face = "bold"),
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = half_line, b = half_line)),
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = half_line, l = half_line), angle = 90),
    
    # Legend
    legend.position = legend_position,
    legend.background = ggplot2::element_rect(fill = "white", colour = NA),
    legend.key = ggplot2::element_rect(fill = "white", colour = NA),
    legend.key.size = ggplot2::unit(1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = ggplot2::element_text(size = ggplot2::rel(0.9)),
    legend.title = ggplot2::element_text(size = ggplot2::rel(1.0), face = "bold"),
    legend.margin = ggplot2::margin(6, 6, 6, 6),
    legend.box = NULL,
    legend.box.just = NULL,
    legend.box.margin = ggplot2::margin(0, 0, 0, 0),
    legend.box.background = ggplot2::element_blank(),
    legend.box.spacing = ggplot2::unit(2 * half_line, "pt"),
    
    # Strip (facet) labels
    strip.background = ggplot2::element_rect(fill = "grey85", colour = "grey70", size = base_line_size),
    strip.text = ggplot2::element_text(size = ggplot2::rel(0.9), face = "bold", 
                                       margin = ggplot2::margin(base_size/5, base_size/5, base_size/5, base_size/5)),
    strip.text.x = ggplot2::element_text(margin = ggplot2::margin(b = base_size/4)),
    strip.text.y = ggplot2::element_text(angle = -90, margin = ggplot2::margin(l = base_size/4)),
    
    # Plot area
    plot.background = ggplot2::element_rect(fill = "white", colour = NA),
    plot.title = ggplot2::element_text(size = ggplot2::rel(1.2), face = "bold",
                                       margin = ggplot2::margin(b = half_line)),
    plot.subtitle = ggplot2::element_text(size = ggplot2::rel(1.0),
                                          margin = ggplot2::margin(b = half_line)),
    plot.caption = ggplot2::element_text(size = ggplot2::rel(0.9), colour = "grey50",
                                         hjust = 1, margin = ggplot2::margin(t = half_line)),
    plot.margin = ggplot2::margin(half_line, half_line, half_line, half_line),
    plot.tag = ggplot2::element_text(size = ggplot2::rel(1.2), face = "bold"),
    plot.tag.position = "topleft",
    
    # Complete theme
    complete = TRUE
  )
  
  return(theme)
}

#' ICES Theme for Publication Figures
#'
#' A variant of theme_ices() optimized for publication figures with larger fonts
#' and minimal grid lines.
#'
#' @param base_size Base font size (default: 14 for publications)
#' @param ... Additional arguments passed to theme_ices()
#'
#' @return A ggplot2 theme object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(icesdata)
#'
#' ggplot(data, aes(x = year, y = catch)) +
#'   geom_line() +
#'   theme_ices_publication()
#' }
theme_ices_publication <- function(base_size = 14, ...) {
  theme_ices(
    base_size = base_size,
    panel_grid = FALSE,
    panel_border = TRUE,
    legend_position = "bottom",
    ...
  )
}

#' ICES Theme for Presentations
#'
#' A variant of theme_ices() optimized for presentations with larger fonts
#' and more prominent grid lines.
#'
#' @param base_size Base font size (default: 16 for presentations)
#' @param ... Additional arguments passed to theme_ices()
#'
#' @return A ggplot2 theme object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(icesdata)
#'
#' ggplot(data, aes(x = year, y = catch)) +
#'   geom_line() +
#'   theme_ices_presentation()
#' }
theme_ices_presentation <- function(base_size = 16, ...) {
  theme_ices(
    base_size = base_size,
    panel_grid = TRUE,
    panel_border = FALSE,
    legend_position = "bottom",
    ...
  )
}

#' ICES Color Palette
#'
#' Standard color palette for ICES visualizations. Provides a set of
#' color-blind friendly colors suitable for multiple data series.
#'
#' @param n Number of colors needed (default: 5)
#' @param type Type of palette: "default", "cool", "warm", "diverging"
#'
#' @return A vector of hex color codes
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(icesdata)
#'
#' colors <- ices_colors(5)
#' ggplot(data, aes(x = year, y = value, color = stock)) +
#'   geom_line() +
#'   scale_color_manual(values = ices_colors(length(unique(data$stock)))) +
#'   theme_ices()
#' }
ices_colors <- function(n = 5, type = "default") {
  # Color-blind friendly palettes
  
  if (type == "default") {
    # Default palette - suitable for most uses
    palette <- c(
      "#1f77b4", # blue
      "#ff7f0e", # orange
      "#2ca02c", # green
      "#d62728", # red
      "#9467bd", # purple
      "#8c564b", # brown
      "#e377c2", # pink
      "#7f7f7f", # gray
      "#bcbd22", # olive
      "#17becf"  # cyan
    )
  } else if (type == "cool") {
    # Cool colors (blues, greens)
    palette <- c(
      "#08519c", "#3182bd", "#6baed6", "#9ecae1", "#c6dbef",
      "#006d2c", "#238b45", "#41ae76", "#66c2a4", "#99d8c9"
    )
  } else if (type == "warm") {
    # Warm colors (reds, oranges)
    palette <- c(
      "#a50f15", "#de2d26", "#fb6a4a", "#fcae91", "#fee5d9",
      "#cb181d", "#ef3b2c", "#fb6a4a", "#fc9272", "#fcbba1"
    )
  } else if (type == "diverging") {
    # Diverging palette (for positive/negative values)
    palette <- c(
      "#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0",
      "#f7f7f7", # white/neutral center
      "#fddbc7", "#f4a582", "#d6604d", "#b2182b", "#67001f"
    )
  } else {
    palette <- c(
      "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
      "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"
    )
  }
  
  # Return the requested number of colors
  if (n <= length(palette)) {
    return(palette[1:n])
  } else {
    # If more colors needed, interpolate
    warning(paste("Requested", n, "colors but palette only has", length(palette),
                  "colors. Colors will be interpolated."))
    return(grDevices::colorRampPalette(palette)(n))
  }
}

#' ICES Color Scales for ggplot2
#'
#' Convenience functions for adding ICES color scales to ggplot2 plots.
#' These functions automatically provide enough colors for discrete scales.
#'
#' @param n Number of colors (default: 10, provides enough for most uses)
#' @param type Type of palette: "default", "cool", "warm", "diverging"
#' @param ... Additional arguments passed to scale functions
#'
#' @return A ggplot2 scale object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(icesdata)
#'
#' # For discrete scales - automatically handles number of levels
#' ggplot(data, aes(x = year, y = catch, color = stock)) +
#'   geom_line() +
#'   scale_color_ices(type = "default") +
#'   theme_ices()
#'
#' # For continuous scales
#' ggplot(data, aes(x = year, y = catch, color = value)) +
#'   geom_line() +
#'   scale_color_gradient_ices(low = "#2166ac", high = "#d6604d") +
#'   theme_ices()
#' }
scale_color_ices <- function(n = 10, type = "default", ...) {
  ggplot2::scale_color_manual(values = ices_colors(n = n, type = type), ...)
}

#' @rdname scale_color_ices
#' @export
scale_fill_ices <- function(n = 10, type = "default", ...) {
  ggplot2::scale_fill_manual(values = ices_colors(n = n, type = type), ...)
}

#' @rdname scale_color_ices
#' @export
scale_color_gradient_ices <- function(low = "#2166ac", high = "#d6604d", ...) {
  ggplot2::scale_color_gradient(low = low, high = high, ...)
}

#' @rdname scale_color_ices
#' @export
scale_fill_gradient_ices <- function(low = "#2166ac", high = "#d6604d", ...) {
  ggplot2::scale_fill_gradient(low = low, high = high, ...)
}

