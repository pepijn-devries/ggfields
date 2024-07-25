test_that(
  "Seawater velocity is visualised correctly", {
    library(ggplot2) |> suppressWarnings()
    data("seawatervelocity")
    seawater_plot <-
      ggplot() +
      geom_fields(aes(radius = as.numeric(v), angle = as.numeric(angle)),
                  seawatervelocity)
    vdiffr::expect_doppelganger("ggfields seawater", seawater_plot)
  }
)

test_that(
  "Angle corrections work as expected", {
    library(ggplot2) |> suppressWarnings()
    north_arrows <-
      expand.grid(
        x = seq(-5, 15, length.out = 10),
        y = seq(89.85, 89.9, length.out = 10)
      ) |>
      sf::st_as_sf(coords = c("x", "y"), crs = 4326) |>
      sf::st_make_grid() |>
      stars::st_as_stars(nx = 10, ny = 10) |>
      dplyr::mutate(angle = 0*(2*pi/360))
    
    no_correction <-
    
    north_plot <-
      ggplot() +
      theme(legend.position = "top") +
      labs(colour = NULL) +
      geom_fields(data = north_arrows, aes(angle = angle, col = "no correction"), radius = 1,
                  .angle_correction = NULL,
                  max_radius = ggplot2::unit(0.7, "cm")) +
      geom_fields(data = north_arrows, aes(angle = angle, col = "corrected"), radius = 1,
                  max_radius = ggplot2::unit(0.7, "cm")) +
      scale_colour_manual(values = c(`no correction` = "red", corrected = "green")) +
      coord_sf(crs = 32631)
    
    vdiffr::expect_doppelganger("ggfields north", north_plot)
  }
)