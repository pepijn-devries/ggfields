#' Annotate a ggplot with vector fields
#' 
#' Functions exactly the same as [geom_fields()], with that difference
#' that this function does not train the `x` and `y` scales. This
#' makes the data central, and uses this layer to support it. Consequently,
#' `annotation_fields()` does not accept a `stat` argument.
#' @include stat_fields.r geom_fields.r
#' @inheritParams geom_fields
#' @returns A [ggplot2::layer_sf()].
#' @examples
#' if (requireNamespace("stars") && requireNamespace("ggplot2")) {
#'   library(stars)
#'   library(ggplot2)
#'
#'   data("seawatervelocity")
#'   sw_sub <- seawatervelocity[,8:13,1:5]
#'   
#'   ## Note that the `seawatervelocity` spans a much larger area,
#'   ## but the plot only focuses on `sw_sub`
#'   ggplot() +
#'     geom_stars(data = sw_sub) +
#'     annotation_fields(data = seawatervelocity,
#'                       aes(angle = as.numeric(atan2(vo, uo)),
#'                           radius = as.numeric(pythagoras(uo, vo)))) +
#'     labs(radius = "v [m/s]")
#' }
#' @author Pepijn de Vries
#' @export
annotation_fields <-
  function(mapping     = NULL, data = NULL,
           position    = "identity", na.rm = FALSE, show.legend = NA,
           max_radius  = ggplot2::unit(0.5, "cm"),
           .angle_correction = angle_correction,
           arrow       = grid::arrow(length = ggplot2::unit(0.2, "cm")),
           inherit.aes = TRUE, ...) {
    geom_fields(
      mapping = mapping, data = data, stat = StatSfAnnotation, position = position, na.rm = na.rm,
      show.legend = show.legend, max_radius = max_radius, .angle_correction = .angle_correction,
      arrow = arrow, inherit.aes = inherit.aes, ...)
  }

StatSfAnnotation <- ggplot2::ggproto(
  "StatSfAnnotation",
  StatFields,
  compute_panel = function(data, scales, coord) {
    data |>
      .compute_panel_stat_fields(scales, coord) |>
      dplyr::mutate(xmin = NA_real_, xmax = NA_real_, ymin = NA_real_, ymax = NA_real_)
  }
)