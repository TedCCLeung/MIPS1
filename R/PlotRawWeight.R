#' Plot a faceted plot of the raw weight data
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @param facet_scale "free", "free_x", "free_y" or "fixed".
#' @param log_rate Logical. If TRUE then the growth rate is log10 transformed.
#'
#' @return ggplot2 object
#' @export

PlotRawWeight <- function(

  facet_scale = "fixed",
  log_rate = FALSE

){

  # Get raw weight data frame
  utils::data("RawWeightData", envir=environment())

  if (log_rate){
    RawWeightData$weight <- log10(RawWeightData$weight)
  }

  plot <- ggplot2::ggplot(data = RawWeightData, ggplot2::aes_string(x = "age", y = "weight", color = "genotype")) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~ .data$photoperiod + .data$intensity, scales = facet_scale) +
    theme_Prism()

  return(plot)
}




