#' Vector field radius scales
#' 
#' Scales to set up the visualisation of the `radius` aesthetic. These
#' scales are also automatically used in plot guides.
#' Note that `scale_radius_identity()` does *not* exist as it would be impossible to
#' relate such a scale to the `max_radius` parameter. For more details see
#' `vignette("radius_aes")`.
#' @examples
#' if (requireNamespace("ggplot2")) {
#'   library(ggplot2)
#'   data(seawatervelocity)
#'   
#'   g_num <-
#'     ggplot() +
#'     geom_fields(data = seawatervelocity,
#'                 aes(radius = as.numeric(v), angle = as.numeric(angle)))
#'   g_discr <-
#'     ggplot() +
#'     geom_fields(data = seawatervelocity,
#'                 aes(radius = cut(as.numeric(v), 4), angle = as.numeric(angle)))
#'                 
#'   g_num + scale_radius_continuous()
#'   g_num + scale_radius_binned()
#'   g_discr + scale_radius_discrete()
#' }
#' @param ... Arguments passed on to underpinning `ggplot2::scale_*` functions.
#' @param range Relative output range of radii. Must lie between 0 and 1.
#' @returns An object of class [Scale][ggplot2::Scale].
#' @author Pepijn de Vries
#' @name scale_radius_continuous
#' @rdname scale
#' @export
scale_radius_continuous <- function(..., range = c(1e-8, 1)) {
  ggplot2::continuous_scale(
    "radius",
    palette = scales::rescale_pal(range = range),
    rescaler = function(x, to = range, from = c(0, max(x, na.rm = TRUE))) {
      x <- as.numeric(x)
      if (any(stats::na.omit(x) < 0)) rlang::abort(c(
        x = "Cannot scale negative values to the 'radius' aesthetic",
        i = "Only positive radii are meaningful, perhaps try discretising your values"
      ))
      x <- x/max(from, na.rm = TRUE)
      x <- x*diff(to) + min(to)
      x
    }, ...)
}

#' @name scale_radius_binned
#' @rdname scale
#' @export
scale_radius_binned <- function(..., range = c(1e-8, 1)) {
  ggplot2::binned_scale("radius", palette = scales::rescale_pal(range), ...)
}

#' @name scale_radius_discrete
#' @rdname scale
#' @export
scale_radius_discrete <- function(..., range = c(1e-8, 1)) {
  ggplot2::discrete_scale("radius", palette = function(n)
    seq(range[[1]], range[[2]], length.out = n), ...)
}
