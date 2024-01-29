.mapping_prep_fields <- function(data, mapping) {
  if (!"geometry" %in% names(mapping) && inherits(data, "sf")) {
    geometry <- NULL
    geometry <- ggplot2::aes(geometry = geometry)
    geometry$geometry <- rlang::set_env(geometry$geometry, rlang::global_env())
    mapping <- utils::modifyList(geometry, mapping)
  }
  mapping
}

.draw_panel_fields <- function(
    self, data, panel_params, coord, na.rm, max_radius, arrow, .angle_correction,
    linejoin, lineend) {
  .angle_correction <- .angle_correction %||%
    function(data, ...) { data |> dplyr::mutate(angle_correction = 0) }
  if (!all(sf::st_is(data$geometry, "POINT"))) {
    rlang::abort(c(x = "All geometries should be of type 'POINT'",
                   i = "Try casting your geometry with `sf::st_cast()`"))
  }
  data <-
    data |>
    .angle_correction(panel_params, coord) |>
    dplyr::mutate(
      angle_correction = ifelse(is.na(.data$angle_correction), 0, .data$angle_correction)
    ) |>
    dplyr::mutate(
      x = sf::st_coordinates(.data$geometry)[,1],
      y = sf::st_coordinates(.data$geometry)[,2]
    ) |>
    coord$transform(panel_params = panel_params) |>
    dplyr::mutate(
      xend = mapply(function(x, l, a)
        ggplot2::unit(x, "npc") + max_radius*l*sin(a),
        x  = .data$x, l = .data$radius, a = .data$angle + .data$angle_correction, SIMPLIFY = FALSE),
      yend = mapply(function(y, l, a)
        ggplot2::unit(y, "npc") + max_radius*l*cos(a),
        y  = .data$y, l = .data$radius, a = .data$angle + .data$angle_correction, SIMPLIFY = FALSE),
      x    = lapply(.data$x, ggplot2::unit, units = "npc"),
      y    = lapply(.data$y, ggplot2::unit, units = "npc"),
      grob = mapply(
        grid::segmentsGrob, x0 = .data$x, y0 = .data$y, x1 = .data$xend, y1 = .data$yend,
        arrow = lapply(seq_len(nrow(data)), function(x) arrow),
        gp = mapply(
          grid::gpar,
          col      = data$colour,
          lwd      = data$linewidth*ggplot2::.pt,
          lty      = data$linetype,
          alpha    = data$alpha,
          linejoin = linejoin,
          lineend  = lineend,
          SIMPLIFY = FALSE
        ), SIMPLIFY = FALSE)
    )
  do.call(grid::grobTree, data$grob)
}

.data_prep_fields <- function(data) {
  if (inherits(data, "stars")) {
    requireNamespace("stars", quietly = TRUE)
    data    <- sf::st_as_sf(data, as_points = TRUE)
  }
  data
}

.compute_panel_stat_fields <- function (data, scales, coord) {
  if (!"geometry" %in% names(data)) {
    if (!"y" %in% names(data)) stop("both x and y aes are required when no geometry is available.")
    data <- data |> sf::st_as_sf(coords = c("x", "y"), remove = FALSE)
  }
  ## Perhaps the units can be used in the scales
  data$radius <- if (inherits(data$radius, "units"))
    as.numeric(data$radius) else
      data$radius
  data$angle <- if(is.null(data$angle)) NULL else as.numeric(data$angle)
  data
}

.setup_params_fields <- function(data, params) {
  params$linejoin <- params$linejoin %||% "mitre"
  params$lineend  <- params$lineend %||% "butt"
  params
}