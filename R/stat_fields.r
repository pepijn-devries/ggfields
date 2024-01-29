#' @rdname stat_fields
#' @name StatFields
#' @export
StatFields <-
  ggplot2::ggproto(
    "StatFields", ggplot2::StatSf,
    compute_panel = function(data, scales, coord) {
      .compute_panel_stat_fields(data, scales, coord) |>
        ggplot2::StatSf$compute_panel(scales, coord)
    },
    required_aes = c("geometry|x")
  )

#' Stat method for geom_fields
#' 
#' Prepares data before being handled by [geom_fields()]
#' @include helpers.r
#' @inheritParams geom_fields
#' @param geom The layer type for which the data is prepared. In this case `"fields"`.
#' @returns Returns a [layer][ggplot2::layer()] that can be further modified by [geom_fields()].
#' @examples
#' stat_fields()
#' @author Pepijn de Vries
#' @rdname stat_fields
#' @name stat_fields
#' @export
stat_fields <-
  function(mapping = NULL, data = NULL, geom = "fields",
           position = "identity", na.rm = FALSE, show.legend = NA,
           inherit.aes = TRUE, ...) {
    ggplot2::layer_sf(
      stat     = StatFields, data = data, mapping = mapping, geom = geom,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params   = rlang::list2(na.rm = na.rm, ...)
    )
  }