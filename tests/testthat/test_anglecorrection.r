coord <- ggplot2::coord_sf(crs = 4326)

params_mockup <- ggplot2::ggplot_build(
  ggplot2::ggplot(NULL, ggplot2::aes(1, 1)) +
    ggplot2::lims(x = c(1, 2), y = c(50, 51)) +
    coord
)$layout$panel_params[[1]]

test_that(
  "Angle correction won't work on geometries other then point", {
    expect_error({
      data <-
        data.frame(
          geometry = sf::st_sfc(sf::st_polygon())
        ) |>
        sf::st_as_sf(crs = 4326)

      angle_correction(data, params_mockup, coord)
    })
  }) |> suppressMessages()

test_that(
  "Missing CRS is signalled", {
    expect_message({
      data <-
        data.frame(
          angle = 0,
          geometry = sf::st_sfc(sf::st_polygon())
        ) |>
        sf::st_as_sf()

      angle_correction(data, params_mockup, coord)
    })
  }) |> suppressMessages()

test_that(
  "Expect warning for proximity to North Pole", {
    expect_warning({
      data <-
        data.frame(
          x = seq(1, 2, 0.1),
          y = seq(98.999, 99.999, 0.1),
          angle = 0
        ) |>
        sf::st_as_sf(coords = c("x", "y"), crs = 4326, remove = FALSE)

      angle_correction(data, params_mockup, coord)
    })
  }) |> suppressMessages()
