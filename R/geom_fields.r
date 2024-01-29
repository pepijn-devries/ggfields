#' @include draw_key_fields.r angle_correction.r stat_fields.r helpers.r
#' @rdname geom_fields
#' @name GeomFields
#' @export
GeomFields <-
  ggplot2::ggproto(
    "GeomFields",
    ggplot2::GeomSegment,
    required_aes = c("geometry|x", "radius", "angle"),
    optional_aes = c("y"),
    extra_params = c("na.rm", "max_radius", "arrow", ".angle_correction"),
    draw_key     = draw_key_fields,
    default_aes  = list(colour = "black", linewidth = 0.5, linetype = 1, alpha = 1,
                        radius = 1, angle = 0),
    draw_panel   = .draw_panel_fields,
    setup_params = .setup_params_fields
  )

#' Arrows depicting a vector field
#' 
#' Visualise vector fields (such as, electric/magnetic fields, wind speed, or water currents)
#' with arrows as a layer in a [ggplot][ggplot2::ggplot()].
#' 
#' Adds a layer with vector fields to a [ggplot][ggplot2::ggplot()]. In order to achieve this
#' two special aesthetic are required: `radius` and `angle`.
#' 
#' @section Aesthetics:
#'
#'   * **`geometry|x`**: Either a `geometry` column or `x` coordinate.
#'     In case of `geometry` the column should be of class [sf::sfc_POINT].
#'     In case of `x`, it should be a `numeric` `vector`, and the aesthetic
#'     `y` needs to be specified as well. It specifies the location of
#'     the origin of each vector.
#'   * **`radius`**: This aesthetic will be used to scale the radius of the
#'     vector arrows in the field you wish to display. The maximum radius of the
#'     arrows is given by parameter `max_radius`. See `vignette("radius_aes")` for
#'     more details.
#'   * **`angle`**: This aesthetic represent the angles of the vectors in your
#'     field in radians. Contrary to the mathematical definition, an angle of 0 radians will
#'     point upwards (instead of to the right). This was chosen such because in most geographical
#'     applications an angle of zero degrees points Northwards. Before plotting these angles are
#'     corrected by the function passed to the `.angle_correction` argument.
#'     See `vignette("angle_corrections)` for more details.
#'   * `y`: This aesthetic needs to be used in combination with the `x` aesthetic.
#'     It needs to be a `numeric` `vector`.
#'   * `fill`: See `vignette("ggplot2-specs", "ggplot2")`
#'   * `colour`: See `vignette("ggplot2-specs", "ggplot2")`
#'   * `linetype`: See `vignette("ggplot2-specs", "ggplot2")`
#'   * `linewidth`: See `vignette("ggplot2-specs", "ggplot2")`
#'   * `alpha`: A variable to control the opacity of an element.
#' @inheritParams ggplot2::geom_segment
#' @param data Can be one of four things:
#'   * `NULL`: in that case data from the parent [ggplot][ggplot2::ggplot()] call is
#'     inherited.
#'   * `data.frame`: you need to assign the `x` and `y` aesthetics.
#'   * [sf object][sf::st_as_sf()]: it should contain a `geometry` column with only `POINT` geometries.
#'   * [stars object][stars::st_as_stars()]: it will be converted automatically to an `sf` object.
#' @param stat The statistical transformation to use on the data for this layer. By default it is
#' set to `GeomFields()` (`"fields"`).
#' @param max_radius Maximum radius to which the `radius` aesthetic is scaled in the plot.
#' You can use absolute ("e.g., "cm", "in", "pt") and relative ("npc") [units][ggplot2::unit()] to set
#' its value. Default is `0.5 cm`.
#' @param .angle_correction Function to correct the angle in the aesthetics for the projection and/or
#' aspect ratio used in the plot. When set to `NULL` the angle is not corrected and is treated as the angle
#' in the final plot. A custom function can be provided which should accept at least three arguments
#' (`data`, `panel_params` and `coord`). See [angle_correction()] and `vignette("angle_correction")` for
#' more details.
#' @returns A [layer][ggplot2::layer()] which can be added to a [ggplot][ggplot2::ggplot()].
#' @examples
#' data(seawatervelocity)
#' 
#' if (requireNamespace("ggplot2") && requireNamespace("stars") &&
#'       requireNamespace("scales")) {
#'   library(ggplot2)
#'   library(stars)
#'   
#'   sw_df <- as.data.frame(seawatervelocity)
#'   ggplot(sw_df, aes(x = x, y = y, radius = as.numeric(v), angle = as.numeric(angle))) +
#'     geom_fields(max_radius = unit(0.5, "cm"), na.rm = TRUE)
#'   
#'   ggplot() +
#'     geom_fields(data    = seawatervelocity,
#'                 mapping = aes(radius = as.numeric(v),
#'                               angle  = as.numeric(angle),
#'                               col    = as.numeric(v)),
#'                 max_radius = unit(0.5, "cm")) +
#'     scale_colour_viridis_c()
#' }
#' @author Pepijn de Vries
#' @rdname geom_fields
#' @name geom_fields
#' @export
geom_fields <-
  function(mapping     = NULL, data = NULL,
           stat        = "fields",
           position    = "identity", na.rm = FALSE, show.legend = NA,
           max_radius  = ggplot2::unit(0.5, "cm"),
           .angle_correction = angle_correction,
           arrow       = grid::arrow(length = ggplot2::unit(0.2, "cm")),
           inherit.aes = TRUE, ...) {
    
    coord <- NULL
    
    if (!is.null(data)) {
      data    <- .data_prep_fields(data)
      mapping <- .mapping_prep_fields(data, mapping)
    }
    if (rlang::inherits_any(data, c("sf", "stars"))) {
      coord <- ggplot2::coord_sf(default = TRUE)
    }
    c(
      ggplot2::layer_sf(
        geom     = GeomFields, mapping = mapping, data = data, stat = stat,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params   = rlang::list2(na.rm = na.rm, max_radius = max_radius, arrow = arrow,
                                .angle_correction = .angle_correction, ...)
      ),
      coord
    )
  }