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

params_mockup <-
  c(
    ggplot() + geom_fields(),
    list(
      x_range = c(1, 2),
      y_range = c(50, 51),
      crs = st_crs(4326),
      default_crs = 4326
    )
  )

coord <- coord_sf()

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
      params <- .setup_params_fields(params = list())
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