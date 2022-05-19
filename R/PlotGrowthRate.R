#' Plot a faceted plot of the growth rate data
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @param instant_or_average "instant" or "average". Whether instantaneous or average growth rate should be plotted.
#' @param facet_scale "free", "free_x", "free_y" or "fixed".
#' @param log_rate Logical. If TRUE then the growth rate is log10 transformed.
#' @param perPhoton Logical. If TRUE then the per photon growth rate is plotted.
#'
#' @return ggplot2 object
#' @export


PlotGrowthRate <- function(
  instant_or_average = "instant",
  facet_scale = "fixed",
  log_rate = FALSE,
  perPhoton = FALSE
){

  ## Check arguments -----------

  if (!instant_or_average %in% c("instant", "average")){
    stop("Enter either 'instant' or 'average' for argument instant_or_average.")
  }

  if (!facet_scale %in% c("fixed", "free_y", "free_x", "free")){
    stop("Enter 'fixed' 'free_y' 'free_x' 'free' for argument instant_or_average.")
  }


  # Get growth rate data frame -----------

  utils::data("GrowthRateData", envir=environment())

  # Date processing ------------

  if (log_rate){
    GrowthRateData$instant_growth_rate <- log10(GrowthRateData$instant_growth_rate)
    GrowthRateData$avg_growth_rate <- log10(GrowthRateData$avg_growth_rate)

    GrowthRateData$instant_growth_rate_per_photon <- log10(GrowthRateData$instant_growth_rate_per_photon)
    GrowthRateData$mean_growth_rate_per_photon <- log10(GrowthRateData$mean_growth_rate_per_photon)
  }

  if (instant_or_average == "instant"){

    if (perPhoton){

      plot <- ggplot2::ggplot(data = GrowthRateData, ggplot2::aes_string(x = "age", y = "instant_growth_rate_per_photon", color = "genotype")) +
        ggplot2::geom_line() +
        ggplot2::geom_point() +
        ggplot2::facet_wrap(~ .data$photoperiod + .data$intensity, scales = facet_scale) +
        theme_Prism()

    } else {

      plot <- ggplot2::ggplot(data = GrowthRateData, ggplot2::aes_string(x = "age", y = "instant_growth_rate", color = "genotype")) +
        ggplot2::geom_line() +
        ggplot2::geom_point() +
        ggplot2::facet_wrap(~ .data$photoperiod + .data$intensity, scales = facet_scale) +
        theme_Prism()

    }

  } else if (instant_or_average == "average"){

    if (perPhoton){

      plot <- ggplot2::ggplot(data = GrowthRateData, ggplot2::aes_string(x = "age", y = "mean_growth_rate_per_photon", color = "genotype")) +
        ggplot2::geom_line() +
        ggplot2::geom_point() +
        ggplot2::facet_wrap(~ .data$photoperiod + .data$intensity, scales = facet_scale) +
        theme_Prism()

    } else {

      plot <- ggplot2::ggplot(data = GrowthRateData, ggplot2::aes_string(x = "age", y = "avg_growth_rate", color = "genotype")) +
        ggplot2::geom_line() +
        ggplot2::geom_point() +
        ggplot2::facet_wrap(~ .data$photoperiod + .data$intensity, scales = facet_scale) +
        theme_Prism()

    }

  }

  return(plot)
}
