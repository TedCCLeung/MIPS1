#' Raw Data of an Experiment Aimed to Determine Plant Growth Rate
#'
#' @format A data frame with 360 observations and 5 variables:
#' \describe{
#'   \item{genotype}{Genotype}
#'   \item{photoperiod}{Photoperiod}
#'   \item{intensity}{Light intensity (μmol photon per second per squared meter)}
#'   \item{age}{Plant age (day)}
#'   \item{weight}{Weight (g)}
#' }
"RawWeightData"

#' Growth rate calculation from RawWeightData
#'
#' @format A dataframe with 10 variables and 72 obervations
#' \describe{
#'   \item{genotype}{Genotype}
#'   \item{photoperiod}{Photoperiod}
#'   \item{intensity}{Light intensity (μmol per second per squared meter)}
#'   \item{age}{Plant age (day)}
#'   \item{mean_weight}{Mean weight (g)}
#'   \item{instant_growth_rate}{Instantaneous growth rate (g per day)}
#'   \item{avg_growth_rate}{Average growth rate since germination (g per day)}
#'   \item{photon_per_day}{Photon intensity per day (mol per day per squared meter)}
#'   \item{instant_growth_rate_per_photon}{Instantaneous growth per light intensity (g m2 per mol)}
#'   \item{averageg_growth_rate_per_photon}{Average growth per light intensity (g m2 per mol)}
#' }
"GrowthRateData"

