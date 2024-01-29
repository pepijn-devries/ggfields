if (requireNamespace("CopernicusMarine") &
    !is.null(getOption("CopernicusMarine_uid"))) {
  library(CopernicusMarine)
  library(ggfields)
  library(units)
  destination <- tempfile("copernicus", fileext = ".nc")
  
  result <- cms_download_subset(
    destination   = destination,
    product       = "GLOBAL_ANALYSISFORECAST_PHY_001_024",
    layer         = "cmems_mod_glo_phy-cur_anfc_0.083deg_P1D-m",
    variable      = "sea_water_velocity",
    region        = c(4, 52.5, 5, 53.5),
    timerange     = c("2021-01-01", "2021-01-02"),
    verticalrange = c(0, 2)
  )
  
  if (result) {
    seawatervelocity        <- stars::read_stars(destination)
    seawatervelocity$angle  <- as.numeric(atan2(seawatervelocity$vo,
                                                seawatervelocity$uo))
    units(seawatervelocity$angle) <- "radians"
    seawatervelocity$v      <- pythagoras(seawatervelocity$vo, seawatervelocity$uo)
    sf::st_crs(seawatervelocity) <- 4326
    usethis::use_data(seawatervelocity, overwrite = TRUE)
  }
}
