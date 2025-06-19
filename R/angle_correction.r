#' Calculate correction for angle in the plot coordinate system
#' 
#' The angle of a vector may be distorted when your plot uses a different
#' coordinate system than the one for which the angle is specified. If `data`
#' is a simple feature object ([sf][sf::st_sf()]), the angle will be corrected
#' for the displayed coordinate reference system ([crs][sf::st_crs()]). When
#' the crs is missing, an aspect ratio of 1 is assumed. For any other data,
#' the angle is corrected for the aspect ratio in the plot.
#' 
#' This function is used by default by [geom_fields()]. For more details on
#' why this correction is required and how to customize corrections please see
#' `vignette("angle_correction")`.
#' @param data fortified data used in a [geom_fields()]. Should at least contain `numeric` columns `x`, `y` and
#' `angle`.
#' @param panel_params panel parameters as returned by [GeomFields$setup_params()][GeomFields]
#' @param coord A [coord][ggplot2::coord_cartesian] object.
#' @returns A `data.frame` with an additional `angle_correction` column. The corrected angle is given
#' by `angle_correction + angle`.
#' @examples
#' if (requireNamespace("ggplot2") && requireNamespace("stars")) {
#'   library(ggplot2)
#'   library(stars)
#' 
#'   ggplot() +
#'     geom_fields(
#'       data    = seawatervelocity,
#'       mapping = aes(radius = as.numeric(v),
#'                     angle  = as.numeric(angle),
#'                     col    = as.numeric(v)),
#'  ## You can provide the `angle_correction()` as argument explicitly
#'  ## (it is already the default). Note that the plotted region requires
#'  ## hardly any correction for the angles.
#'                 .angle_correction = angle_correction)
#' }
#' @author Pepijn de Vries
#' @export
angle_correction <- function(data, panel_params, coord) {
  .default     <- function(data) { data |> dplyr::mutate(angle_correction = 0)}
  crs          <- sf::st_crs(data$geometry)
  if (!is.null(data$geometry) && !is.na(crs)) {
    if (!all(sf::st_is(data$geometry, "POINT"))) {
      rlang::abort(c(x = "All geometries should be of type 'POINT'",
                     i = "Try casting your geometry with `sf::st_cast()`"))
    }
  }
  guides       <- coord$train_panel_guides(panel_params)

  if (is.null(guides$x.range %||% guides$x_range))
    rlang::abort(c(x = "Cannot correct angle for this axis coordinate system.",
                   i = "Try using a different coordinate system."))
  coord_aspect <- diff(guides$y.range %||% guides$y_range) /
    diff(guides$x.range %||% guides$x_range)
  if ("crs" %in% names(guides) && is.na(crs)) true_aspect <- 1 else
    true_aspect  <- coord$ratio %||% coord_aspect
  if (is.na(crs)) {
    rlang::inform("CRS is not specified, correcting for aspect ratio only.")
    ref <- data.frame(angle = atan2(true_aspect*sin(data$angle), cos(data$angle))) |>
      dplyr::mutate(angle = .data$angle - data$angle,
                    angle = atan2(sin(.data$angle), cos(.data$angle)))
  } else {
    requireNamespace("sf", quietly = TRUE)
    ref          <- sf::st_transform(data$geometry, 4326) |> sf::st_coordinates() |>
      as.data.frame() |> dplyr::rename(x = "X", y = "Y")
    offset       <- 0.001
    if (any(ref$y > 90 - offset)) {
      rlang::warn("Too close to the North Pole. Showing uncorrected angles.")
      return(data |> .default())
    }
    north_of_ref <- ref |> dplyr::mutate(y = .data$y + offset)
    ref          <- ggplot2::sf_transform_xy(ref, crs, 4326)
    north_of_ref <- ggplot2::sf_transform_xy(north_of_ref, crs, 4326)
    ref          <- (north_of_ref - ref) |>
      dplyr::mutate(y = .data$y*true_aspect, angle = -atan2(.data$y, .data$x) + pi/2)
  }
  rlang::inform(sprintf("Angle correction between %0.2f and %0.2f radials",
                        min(ref$angle), max(ref$angle)), frequency = "regularly")
  
  data |>
    dplyr::mutate(
      angle_correction = ref$angle
    )
}
