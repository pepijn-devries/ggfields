#' Key glyphs for 'radius' legends
#' 
#' @inherit ggplot2::draw_key_linerange description
#' @details
#' The layer [geom_fields()] allows for a special aesthetic `radius`. This function
#' draws a key glyph for this aesthetics, where the radius of the arrow corresponds
#' with the scalar value listed with this radius. Note that the width of the key glyph
#' cannot be adjusted by the aesthetic itself. Therefore, if your `max_radius` parameter
#' exceeds the glyph width, you need to change the width of the guides yourself,
#' see `vignette("radius_aes")` for more details.
#' @inheritParams ggplot2::draw_key_linerange
#' @returns A grid grob
#' @examples
#' if (requireNamespace("ggplot2")) {
#'   library(ggplot2)
#'   p <- ggplot(economics, aes(date, psavert, color = "savings rate"))
#'   p + geom_line(key_glyph = "fields")
#' }
#' @author Pepijn de Vries
#' @export
draw_key_fields <- function(data, params, size) {
  params$max_radius <- params$max_radius %||% ggplot2::unit(0.5, "cm")
  data$radius       <- data$radius %||% 1
  lwd               <- data$linewidth*ggplot2::.pt
  if (is.null(data$alpha) || is.na(data$alpha)) data$alpha <- 1
  grid::linesGrob(x = grid::unit(0, "npc")  + c(0, data$radius)*params$max_radius,
                  y = grid::unit(0.5, "npc"),
                  arrow = params$arrow,
                  gp = grid::gpar(
                    col      = data$colour,
                    lwd      = lwd,
                    lty      = data$linetype,
                    alpha    = data$alpha,
                    linejoin = params$linejoin,
                    lineend  = params$lineend
                  ))
}