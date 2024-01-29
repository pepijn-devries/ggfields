#' A small subset of the global ocean physics analysis and forecast product
#' 
#' A small subset of ocean currents data retrieved with [CopernicusMarine][CopernicusMarine::CopernicusMarine]
#' from the source listed below serving as an example.
#' 
#' @references E.U. Copernicus Marine Service Information; Global Ocean Physics Analysis and Forecast - GLOBAL_ANALYSISFORECAST_PHY_001_024 (2016-10-14). \doi{DOI:10.48670/moi-00016}
#' @docType data
#' @name seawatervelocity
#' @format A [stars][stars::st_as_stars()] object with `x`, `y`, `depth` and `time` dimensions. It has the
#' attributes `vo` (northward seawater velocity \[m/s\]) and `uo` (eastward seawater velocity \[m/s\]).
#' @examples
#' data("seawatervelocity")
NULL