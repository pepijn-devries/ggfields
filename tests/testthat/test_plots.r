library(ggplot2, quietly = TRUE) |> suppressWarnings()
library(stars, quietly = TRUE) |> suppressWarnings()

data("seawatervelocity")
sw_sub <- seawatervelocity[,8:13,1:5]

test_that(
  "Seawater velocity is visualised correctly", {
    seawater_plot <-
      ggplot() +
      geom_fields(aes(radius = as.numeric(v), angle = as.numeric(angle)),
                  seawatervelocity) +
      stat_fields()
    vdiffr::expect_doppelganger("ggfields seawater", seawater_plot)
  }
) |> suppressMessages()

test_that(
  "Angle corrections work as expected", {
    north_arrows <-
      expand.grid(
        x = seq(-5, 15, length.out = 10),
        y = seq(89.85, 89.9, length.out = 10)
      ) |>
      sf::st_as_sf(coords = c("x", "y"), crs = 4326) |>
      sf::st_make_grid() |>
      stars::st_as_stars(nx = 10, ny = 10) |>
      dplyr::mutate(angle = 0*(2*pi/360))
    
    north_plot <-
      ggplot() +
      theme(legend.position = "top") +
      labs(colour = NULL) +
      geom_fields(data = north_arrows, aes(angle = angle, col = "no correction"), radius = 1,
                  .angle_correction = NULL,
                  key_glyph = draw_key_fields,
                  max_radius = ggplot2::unit(0.7, "cm")) +
      geom_fields(data = north_arrows, aes(angle = angle, col = "corrected"), radius = 1,
                  .angle_correction = angle_correction,
                  key_glyph = draw_key_fields,
                  max_radius = ggplot2::unit(0.7, "cm")) +
      scale_colour_manual(values = c(`no correction` = "red", corrected = "green")) +
      coord_sf(crs = 32631)
    
    vdiffr::expect_doppelganger("ggfields north", north_plot)
  }
) |> suppressMessages()

test_that(
  "Annotation is visualised correctly", {
    
    ## Note that the `seawatervelocity` spans a much larger area,
    ## but the plot only focuses on `sw_sub`
    annot_plot <- ggplot() +
      geom_stars(data = sw_sub) +
      annotation_fields(data = seawatervelocity,
                        aes(angle = as.numeric(atan2(vo, uo)),
                            radius = as.numeric(pythagoras(uo, vo)))) +
      labs(radius = "v [m/s]")
    vdiffr::expect_doppelganger("ggfields annot", annot_plot)
  }
) |> suppressMessages()

test_that(
  "Negative radii throws error in continuous scales", {
    expect_error({
      on.exit({grDevices::dev.off(); closeAllConnections()})
      f <- tempfile(fileext = ".pdf")
      p <- ggplot() +
        geom_stars(data = sw_sub) +
        geom_fields(data = sw_sub,
                    aes(angle = as.numeric(atan2(vo, uo)),
                        radius = -as.numeric(pythagoras(uo, vo))))
      grDevices::pdf(f)
      print(p) |> suppressMessages()
    })
  })

test_that(
  "Binned scales work without error", {
    expect_no_error({
      on.exit({grDevices::dev.off(); closeAllConnections()})
      f <- tempfile(fileext = ".pdf")
      p <- ggplot() +
        geom_stars(data = sw_sub) +
        geom_fields(data = sw_sub,
                    aes(angle = as.numeric(atan2(vo, uo)),
                        radius = as.numeric(pythagoras(uo, vo)))) +
        scale_radius_binned()
      grDevices::pdf(f)
      print(p) |> suppressMessages()
    })
  })

test_that(
  "Discrete scales work without error", {
    expect_no_error({
      on.exit({grDevices::dev.off(); closeAllConnections()})
      f <- tempfile(fileext = ".pdf")
      p <- ggplot() +
        geom_stars(data = sw_sub) +
        annotation_fields(data = sw_sub,
                          aes(angle = as.numeric(atan2(vo, uo)),
                              radius = cut(as.numeric(pythagoras(uo, vo)), 3))) +
        scale_radius_discrete()
      grDevices::pdf(f)
      print(p) |> suppressMessages()
    })
  })
