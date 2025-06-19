{
  library(dplyr, quietly = TRUE)
  library(sf, quietly = TRUE)
  library(stars, quietly = TRUE)
  library(ggplot2, quietly = TRUE)
} |>
  suppressWarnings() |>
  suppressMessages()

self <- GeomFields
data <- data.frame(
  colour = "black",
  geometry = st_point(c(1,50)) |> st_sfc(crs = 4326),
  angle = 0,
  PANEL = 1,
  group = 1,
  xmin = 1,
  xmax = 1,
  ymin = 50,
  ymax = 50,
  linewidth = 1,
  linetype = 1,
  alpha = 1,
  radius = 1
) |>
  st_as_sf()

coord <- ggplot2::coord_sf(crs = 4326, default_crs = 4326)
params_mockup <- ggplot2::ggplot_build(
  ggplot2::ggplot(NULL, ggplot2::aes(1, 1)) +
    ggplot2::lims(x = c(1, 2), y = c(50, 51)) +
    coord
)$layout$panel_params[[1]]

test_that(
  "Prep fields coerces stars to sf", {
    expect_s3_class({
      system.file("tif/L7_ETMs.tif", package = "stars") |> read_stars() -> x
      ggfields:::.data_prep_fields(x)
    }, "sf")
  }
)

test_that(
  "Setup params add linejoin and lineend when missing", {
    expect_true({
      params <- ggfields:::.setup_params_fields(params = list())
      typeof(params) == "list" &&
        all(c("linejoin", "lineend") %in% names(params))
    })
  }
)

test_that(
  "Error when `x` aesthetic is not combined with `y`", {
    expect_error({
      data <- data |> st_drop_geometry() |>
        mutate(x = 0)
      ggfields:::.compute_panel_stat_fields(data = data)
    })
  }
)

test_that(
  "radius and angle are coercible to numerics", {
    expect_true({
      test <- ggfields:::.compute_panel_stat_fields(data = data)
      is.numeric(test$angle) && is.numeric(test$radius)
    })
  })

test_that(
  "Geometry is added to mapping of sf objects when missing", {
    expect_true({
      test <- ggfields:::.mapping_prep_fields(data, aes())
      ("geometry" %in% names(test)) && inherits(test$geometry, "quosure")
    })
  })

test_that(
  "Panel draw function returns a gTree object", {
    testthat::expect_s3_class({
      
      ggfields:::.draw_panel_fields(
        self, data, params_mockup, coord,
        FALSE, grid::unit(0.7, "cm"),
        grid::arrow(), angle_correction, "mitre", "butt"
      ) |> suppressMessages()
    }, c("gTree", "grob", "gDesc"))
  }
)