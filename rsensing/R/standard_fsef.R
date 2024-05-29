#' @title Calculate the fuel specific emission factor for a pollutant using a fuel
#' mass value of 14.000
#'
#' @description The standard_fsef_x function calculates the fuel specific emission factor for
#' the pollutant x using the measured pollutant ratios to carbon dioxide, assuming
#' a fuel mass of 14.000.
#'
#' @param ratio_co_co2 The ratio of carbon monoxide to carbon dioxide
#' @param ratio_hc_co2 The ratio of un-burnt hydrocarbons to carbon dioxide
#' @param ratio_no_co2 The ratio of nitrogen monoxide to carbon dioxide
#' @param ratio_no2_co2 The ratio of nitrogen dioxide to carbon dioxide
#' @param ratio_nh3_co2 The ratio of ammonia to carbon dioxide
#'
#' @return The fuel specific emission factor for the pollutant calculated using
#' a fuel mass value of 14.000
#'
#' @rdname standard_fsef_co
#'
#' @export

# Carbon dioxide

# Carbon monoxide ----
standard_fsef_co <- function(ratio_co_co2,
                             ratio_hc_co2) {
  standard_co_fsef = ((28.010 * 1000) / 14.000) *
    (ratio_co_co2 / (1 + ratio_co_co2 + 6 * (ratio_hc_co2)))
  return(standard_co_fsef)}

#' @rdname standard_fsef_co
#' @export

# Hydrocarbon ----
standard_fsef_hc <- function(ratio_hc_co2,
                             ratio_co_co2) {
  standard_fsef_hc = ((2 * 44.100 * 1000) / 14.000) *
    (ratio_hc_co2 / (1 + ratio_co_co2 + 6 * (ratio_hc_co2)))
  return(standard_fsef_hc)}

#' @rdname standard_fsef_co
#' @export

# Nitrogen monoxide as nitrogen dioxide ----
standard_fsef_no_as_no2 <- function(ratio_no_co2,
                                    ratio_co_co2,
                                    ratio_hc_co2) {
  standard_fsef_no_as_no2 = ((46.006 * 1000) / 14.000)*
    (ratio_no_co2 / (1 + ratio_co_co2 + 6 * (ratio_hc_co2)))
  return(standard_fsef_no_as_no2)}
# The mass of nitrogen dioxide is used to give NO as NO2 for NOX equivalents.

#' @rdname standard_fsef_co
#' @export

# Nitrogen monoxide ----
standard_fsef_no <- function(ratio_no_co2,
                             ratio_co_co2,
                             ratio_hc_co2) {
  standard_fsef_no = ((30.010 * 1000) / 14.000)*
    (ratio_no_co2 / (1 + ratio_co_co2 + 6 * (ratio_hc_co2)))
  return(standard_fsef_no)}

#' @rdname standard_fsef_co
#' @export

# Nitrogen dioxide ----
standard_fsef_no2 <- function(ratio_no2_co2,
                              ratio_co_co2,
                              ratio_hc_co2) {
  standard_fsef_no2 = ((46.006 * 1000) / 14.000)*
    (ratio_no2_co2 / (1 + ratio_co_co2 + 6 * (ratio_hc_co2)))
  return(standard_fsef_no2)}

#' @rdname standard_fsef_co
#' @export

# Ammonia ----
standard_fsef_nh3 <- function(ratio_nh3_co2,
                              ratio_co_co2,
                              ratio_hc_co2) {
  standard_fsef_nh3 = ((17.031 * 1000) / 14.000)*
    (ratio_nh3_co2 / (1 + ratio_co_co2 + 6 * (ratio_hc_co2)))
  return(standard_fsef_nh3)}

