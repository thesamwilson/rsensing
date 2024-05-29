#' @title Calculate the fuel specific emission factors for all pollutants using
#' e5 petrol parameters.
#'
#' @description The calculate_fsef_std calculates the fuel specific emission
#' factor for the pollutants CO2, CO, HC, NO, NO2, NOx, and NH3 using the
#' following parameters: molar mass of fuel MF = 14.384, response factor RF =
#' 2.0, scaling factor SF = 3.0.
#'
#' @param df The data frame containing remote sensing pollutant emission ratios
#'
#' @return The fuel specific emission factors for all pollutants using e5 petrol
#' parameters.
#'
#' @rdname calculate_fsef_e5
#'
#' @export

calculate_fsef_e5 <- function (df) {
  new_df <- df %>%
    dplyr::mutate(co2_gpkg_e5 = ((44.009 * 1000)/14.384) * (1/(1 + ratio_co_co2 + (2.0 * 3.0 * ratio_hc_co2))),
                  co_gpkg_e5 = ((28.010 * 1000)/14.384) * (ratio_co_co2/(1 + ratio_co_co2 + (2.0 * 3.0 * ratio_hc_co2))),
                  hc_gpkg_e5 = ((44.096 * 1000)/14.384) * ((2.0 * ratio_hc_co2)/(1 + ratio_co_co2 + (2.0 * 3.0 * ratio_hc_co2))),
                  no_gpkg_e5 = ((30.010 * 1000)/14.384) * (ratio_no_co2/(1 + ratio_co_co2 + (2.0 * 3.0 * ratio_hc_co2))),
                  no_gpkg_as_no2_e5 = ((46.006 * 1000)/14.384) * (ratio_no_co2/(1 + ratio_co_co2 + (2.0 * 3.0 * ratio_hc_co2))),
                  no2_gpkg_e5 = ((46.006 * 1000)/14.384) * (ratio_no2_co2/(1 + ratio_co_co2 + (2.0 * 3.0 * ratio_hc_co2))),
                  nox_gpkg_e5 = no_gpkg_e5 * (46.006/30.010) + no2_gpkg_e5,
                  nh3_gpkg_e5 = ((17.031 * 1000)/14.384) * (ratio_nh3_co2/(1 + ratio_co_co2 + (2.0 * 3.0 * ratio_hc_co2)))
    )
  return(new_df)}
