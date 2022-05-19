#' Calculate the growth rate of plants in each condition
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @return Data frame
#' @export

GetGrowthRate <- function(

){

  ## Calculate mean weight from raw data ----------

  utils::data("RawWeightData", envir=environment())

  MeanWeightData <- RawWeightData %>%
    ## Group by
    dplyr::group_by(
      .data$genotype,
      .data$photoperiod,
      .data$intensity,
      .data$age
    ) %>%
    ## Calculate mean
    dplyr::summarise(
      mean_weight = mean(.data$weight)
    )

  ## Calculate growth rate ----------

  ## In LD data is taken every 4 days
  ## Photon per day = intensity * hours * seconds * minutes * normalization for μmol s-1 m-2
  LD <- MeanWeightData %>%
    dplyr::filter(.data$photoperiod == "LD") %>%
    ## 4 day interval between measurement
    dplyr::mutate(instant_growth_rate = (.data$mean_weight - dplyr::lag(.data$mean_weight))/4) %>%
    ## Life long
    dplyr::mutate(avg_growth_rate = .data$mean_weight/.data$age) %>%
    dplyr::mutate(photon_per_day = .data$intensity*16*60*60*10e-6)

  LD_ <- LD %>%
    dplyr::mutate(instant_growth_rate_per_photon = .data$instant_growth_rate/.data$photon_per_day) %>%
    dplyr::mutate(average_growth_rate_per_photon = .data$avg_growth_rate/.data$photon_per_day)

  ## In SD data is taken every 14 days
  SD <- MeanWeightData %>%
    dplyr::filter(.data$photoperiod == "SD") %>%
    ## 14 day interval between measurement
    dplyr::mutate(instant_growth_rate = (.data$mean_weight - dplyr::lag(.data$mean_weight))/14) %>%
    ## Life long
    dplyr::mutate(avg_growth_rate = .data$mean_weight/.data$age) %>%
    dplyr::mutate(photon_per_day = .data$intensity*8*60*60*10e-6)

  SD_ <- SD %>%
    dplyr::mutate(instant_growth_rate_per_photon = .data$instant_growth_rate/.data$photon_per_day) %>%
    dplyr::mutate(average_growth_rate_per_photon = .data$avg_growth_rate/.data$photon_per_day)

  ## Combine the data
  GrowthRateData <- dplyr::bind_rows(LD_, SD_)

  return(GrowthRateData)
}



