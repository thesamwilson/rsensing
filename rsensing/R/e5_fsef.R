#' @title Calculate the fuel specific emission factor for a pollutant using a fuel
#' mass value of 14.384
#'
#' @description The e5_fsef_x functions calculate the fuel specific emission factor for
#' the pollutant x using the measured pollutant ratios to carbon dioxide, assuming
#' a fuel mass of 14.384 (E5 petrol).
#'
#' @param ratio_co_co2 The ratio of carbon monoxide to carbon dioxide
#' @param ratio_hc_co2 The ratio of un-burnt hydrocarbons to carbon dioxide
#' @param ratio_no_co2 The ratio of nitrogen monoxide to carbon dioxide
#' @param ratio_no2_co2 The ratio of nitrogen dioxide to carbon dioxide
#' @param ratio_nh3_co2 The ratio of ammonia to carbon dioxide
#'
#' @return The fuel specific emission factor for the pollutant calculated using
#' a fuel mass value of 14.384
#'
#' @rdname e5_fsef_co
#'
#' @export

# Carbon monoxide ----
e5_fsef_co <- function(ratio_co_co2,
                             ratio_hc_co2) {
  e5_co_fsef = ((28.010 * 1000) / 14.384) *
    (ratio_co_co2 / (1 + ratio_co_co2 + 6 * (ratio_hc_co2)))
  return(e5_co_fsef)}

#' @rdname e5_fsef_co
#' @export

# Hydrocarbon ----
e5_fsef_hc <- function(ratio_hc_co2,
                             ratio_co_co2) {
  e5_fsef_hc = ((2 * 44.100 * 1000) / 14.384) *
    (ratio_hc_co2 / (1 + ratio_co_co2 + 6 * (ratio_hc_co2)))
  return(e5_fsef_hc)}

#' @rdname e5_fsef_co
#' @export

# Nitrogen monoxide as nitrogen dioxide ----
e5_fsef_no_as_no2 <- function(ratio_no_co2,
                                    ratio_co_co2,
                                    ratio_hc_co2) {
  e5_fsef_no_as_no2 = ((46.006 * 1000) / 14.384)*
    (ratio_no_co2 / (1 + ratio_co_co2 + 6 * (ratio_hc_co2)))
  return(e5_fsef_no_as_no2)}
# The mass of nitrogen dioxide is used to give NO as NO2 for NOX equivalents.

#' @rdname e5_fsef_co
#' @export

# Nitrogen monoxide ----
e5_fsef_no <- function(ratio_no_co2,
                             ratio_co_co2,
                             ratio_hc_co2) {
  e5_fsef_no = ((30.010 * 1000) / 14.384)*
    (ratio_no_co2 / (1 + ratio_co_co2 + 6 * (ratio_hc_co2)))
  return(e5_fsef_no)}

#' @rdname e5_fsef_co
#' @export

# Nitrogen dioxide ----
e5_fsef_no2 <- function(ratio_no2_co2,
                              ratio_co_co2,
                              ratio_hc_co2) {
  e5_fsef_no2 = ((46.006 * 1000) / 14.384)*
    (ratio_no2_co2 / (1 + ratio_co_co2 + 6 * (ratio_hc_co2)))
  return(e5_fsef_no2)}

#' @rdname e5_fsef_co
#' @export

# Ammonia ----
e5_fsef_nh3 <- function(ratio_nh3_co2,
                              ratio_co_co2,
                              ratio_hc_co2) {
  e5_fsef_nh3 = ((17.031 * 1000) / 14.384)*
    (ratio_nh3_co2 / (1 + ratio_co_co2 + 6 * (ratio_hc_co2)))
  return(e5_fsef_nh3)}
