#' @title Calculate the fuel specific emission factor for a pollutant using a fuel
#' mass value of 14.544
#'
#' @description The e10_fsef_x functions calculate the fuel specific emission factor for
#' the pollutant x using the measured pollutant ratios to carbon dioxide, assuming
#' a fuel mass of 14.544 (E10 petrol).
#'
#' @param ratio_co_co2 The ratio of carbon monoxide to carbon dioxide
#' @param ratio_hc_co2 The ratio of un-burnt hydrocarbons to carbon dioxide
#' @param ratio_no_co2 The ratio of nitrogen monoxide to carbon dioxide
#' @param ratio_no2_co2 The ratio of nitrogen dioxide to carbon dioxide
#' @param ratio_nh3_co2 The ratio of ammonia to carbon dioxide
#'
#' @return The fuel specific emission factor for the pollutant calculated using
#' a fuel mass value of 14.544
#'
#' @rdname e10_fsef_co
#'
#' @export

# Carbon monoxide ----
e10_fsef_co <- function(ratio_co_co2,
                             ratio_hc_co2) {
  e10_co_fsef = ((28.010 * 1000) / 14.544) *
    (ratio_co_co2 / (1 + ratio_co_co2 + 6 * (ratio_hc_co2)))
  return(e10_co_fsef)}

#' @rdname e10_fsef_co
#' @export

# Hydrocarbon ----
e10_fsef_hc <- function(ratio_hc_co2,
                             ratio_co_co2) {
  e10_fsef_hc = ((2 * 44.100 * 1000) / 14.544) *
    (ratio_hc_co2 / (1 + ratio_co_co2 + 6 * (ratio_hc_co2)))
  return(e10_fsef_hc)}

#' @rdname e10_fsef_co
#' @export

# Nitrogen monoxide as nitrogen dioxide ----
e10_fsef_no_as_no2 <- function(ratio_no_co2,
                                    ratio_co_co2,
                                    ratio_hc_co2) {
  e10_fsef_no_as_no2 = ((46.006 * 1000) / 14.544)*
    (ratio_no_co2 / (1 + ratio_co_co2 + 6 * (ratio_hc_co2)))
  return(e10_fsef_no_as_no2)}
# The mass of nitrogen dioxide is used to give NO as NO2 for NOX equivalents.

#' @rdname e10_fsef_co
#' @export

# Nitrogen monoxide ----
e10_fsef_no <- function(ratio_no_co2,
                             ratio_co_co2,
                             ratio_hc_co2) {
  e10_fsef_no = ((30.010 * 1000) / 14.544)*
    (ratio_no_co2 / (1 + ratio_co_co2 + 6 * (ratio_hc_co2)))
  return(e10_fsef_no)}

#' @rdname e10_fsef_co
#' @export

# Nitrogen dioxide ----
e10_fsef_no2 <- function(ratio_no2_co2,
                              ratio_co_co2,
                              ratio_hc_co2) {
  e10_fsef_no2 = ((46.006 * 1000) / 14.544)*
    (ratio_no2_co2 / (1 + ratio_co_co2 + 6 * (ratio_hc_co2)))
  return(e10_fsef_no2)}

#' @rdname e10_fsef_co
#' @export

# Ammonia ----
e10_fsef_nh3 <- function(ratio_nh3_co2,
                              ratio_co_co2,
                              ratio_hc_co2) {
  e10_fsef_nh3 = ((17.031 * 1000) / 14.544)*
    (ratio_nh3_co2 / (1 + ratio_co_co2 + 6 * (ratio_hc_co2)))
  return(e10_fsef_nh3)}
