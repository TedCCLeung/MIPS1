#' Plot a faceted plot of the growth rate data
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @param log_rate Logical. If TRUE then the growth rate is log10 transformed.
#' @param select_genotype Character. One of "WT", "co9", "ft10", "mips1".
#' @param value Character. One of "weight", "average_rate", "instant_rate".
#'
#' @return ggplot2 object
#' @export

RatePlot <- function(
  log_rate = FALSE,
  select_genotype = "WT",
  value = "weight"
){

  ## Check arguments -----------

  if (!select_genotype %in% c("WT", "co9", "ft10", "mips1")){stop("Enter the correct genotype.")}
  if (!value %in% c("weight", "average_rate", "instant_rate")){stop("Enter the correct value.")}

  # Get raw weight and growth rate data frame -----------

  utils::data("RawWeightData", envir=environment())
  utils::data("GrowthRateData", envir=environment())

  # Log transformation ------------

  if (log_rate){

    RawWeightData <- RawWeightData %>%
      dplyr::mutate(weight = log10(.data$weight))

    GrowthRateData <- GrowthRateData %>%
      dplyr::mutate(instant_growth_rate = log10(.data$instant_growth_rate)) %>%
      dplyr::mutate(avg_growth_rate = log10(.data$avg_growth_rate)) %>%
      dplyr::mutate(mean_weight = log10(.data$mean_weight))
  }

  # Filter for genotype ---------------

  Weight <- RawWeightData %>%
    dplyr::filter(.data$genotype == select_genotype) %>%
    dplyr::mutate(intensity = as.factor(.data$intensity))

  Rate <- GrowthRateData %>%
    dplyr::filter(.data$genotype == select_genotype) %>%
    dplyr::mutate(intensity = as.factor(.data$intensity))

  # Plotting --------

  if (value == "weight"){

    plot <- ggplot2::ggplot(data = Rate, ggplot2::aes_string(x = "age", y = "mean_weight", color = "photoperiod", linetype = "intensity")) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::scale_linetype_manual(values=c("dotted", "longdash", "solid")) +
      ggplot2::scale_color_manual(values=c("#EE0000", "#3B4992")) +
      ggplot2::facet_wrap(~ .data$photoperiod, scales = "free_x") +
      theme_Prism()

  } else if (value == "instant_rate"){

    plot <- ggplot2::ggplot(data = Rate, ggplot2::aes_string(x = "age", y = "instant_growth_rate", color = "photoperiod", linetype = "intensity")) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::scale_linetype_manual(values=c("dotted", "longdash", "solid")) +
      ggplot2::scale_color_manual(values=c("#EE0000", "#3B4992")) +
      ggplot2::facet_wrap(~ .data$photoperiod, scales = "free_x") +
      theme_Prism()

  } else if (value == "average_rate"){

    plot <- ggplot2::ggplot(data = Rate, ggplot2::aes_string(x = "age", y = "avg_growth_rate", color = "photoperiod", linetype = "intensity")) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::scale_linetype_manual(values=c("dotted", "longdash", "solid")) +
      ggplot2::scale_color_manual(values=c("#EE0000", "#3B4992")) +
      ggplot2::facet_wrap(~ .data$photoperiod, scales = "free_x") +
      theme_Prism()
  }

  return(plot)
}




