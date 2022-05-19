#' Plot a parallel coordinated plot of the photon efficiency data
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @return ggplot2 object
#' @export


PhotonEfficiencyPlot <- function(

){

  # Get growth rate data frame -----------
  utils::data("GrowthRateData", envir=environment())
  #extrafont::loadfonts()

  ## Day 20 photon usage plot

  data_d20 <- GrowthRateData %>%
    #dplyr::filter(.data$genotype == x) %>%
    dplyr::filter(.data$age == 20) %>%
    ## Make sure the intensites are characters
    dplyr::mutate(intensity = as.character(.data$intensity)) %>%
    tidyr::pivot_wider(names_from = .data$photon_per_day, values_from = .data$average_growth_rate_per_photon) %>%
    dplyr::group_by(.data$photoperiod, .data$genotype, .data$intensity) %>%
    dplyr::select(.data$`23.04`, .data$`57.6`, .data$`115.2`, .data$photoperiod, .data$intensity) %>%
    dplyr::summarise(dplyr::across(tidyr::everything(), ~ dplyr::first(na.omit(.)))) %>%
    tidyr::pivot_longer(!c(.data$photoperiod, .data$genotype, .data$intensity), names_to = "photon", values_to = "rate", values_drop_na = FALSE) %>%
    dplyr::mutate(photon = as.numeric(.data$photon)) %>%
    tidyr::drop_na() %>%
    ## Convert to micro-gram
    dplyr::mutate(rate = .data$rate*1000*1000) %>%
    ## Make sure the plots are in the correct order
    dplyr::mutate(genotype = factor(.data$genotype, levels = c("WT", "mips1", "co9", "ft10")))


  day20_plot <- ggplot2::ggplot(data_d20, ggplot2::aes_string(x = "photon", y = "rate", color = "photoperiod", label = "intensity")) +
    ggplot2::geom_line() +
    ggplot2::geom_vline(xintercept = 120, color = "#000000") +
    ggplot2::geom_vline(xintercept = 15, color = "#000000") +
    ggplot2::geom_vline(xintercept = 60, color = "#CDCDCD", size = 0.5) +
    ggplot2::geom_text(family = "Arial", fontface = "bold", size = 2.5) +
    theme_Prism() +
    ggplot2::scale_y_continuous(limits = c(0, 320), name = "Weight gain per photon (ug m2 mol-1)", expand = c(0, 0), breaks = c(0, 100, 200, 300)) +
    ggplot2::scale_x_continuous(limits = c(15, 120), name = "Daily photon flux (mol m-2)", breaks=c(15, 120)) +
    ggplot2::theme(line = ggplot2::element_blank(), legend.position = "none") +
    ggplot2::scale_color_manual(values=c("#FF0000", "#0000FF")) +
    ggplot2::facet_wrap(~ .data$genotype, scales = "fixed", nrow = 1)

  ## Day 28 photon usage plot

  data_d28 <- GrowthRateData %>%
    #dplyr::filter(.data$genotype == x) %>%
    dplyr::filter(.data$age == 28) %>%
    ## Make sure the intensites are characters
    dplyr::mutate(intensity = as.character(.data$intensity)) %>%
    tidyr::pivot_wider(names_from = .data$photon_per_day, values_from = .data$average_growth_rate_per_photon) %>%
    ## To make sure these labels do not get messed up in summarise we need to group them
    dplyr::group_by(.data$photoperiod, .data$genotype, .data$intensity) %>%
    dplyr::select(.data$`23.04`, .data$`28.8`, .data$`57.6`, .data$photoperiod, .data$intensity) %>%
    dplyr::summarise(dplyr::across(tidyr::everything(), ~ dplyr::first(na.omit(.)))) %>%
    tidyr::pivot_longer(!c(.data$photoperiod, .data$genotype, .data$intensity), names_to = "photon", values_to = "rate", values_drop_na = FALSE) %>%
    ## This would have been factor
    dplyr::mutate(photon = as.numeric(.data$photon)) %>%
    tidyr::drop_na() %>%
    ## Convert to micro-gram
    dplyr::mutate(rate = .data$rate*1000*1000) %>%
    ## Make sure the plots are in the correct order
    dplyr::mutate(genotype = factor(.data$genotype, levels = c("WT", "mips1", "co9", "ft10")))

  day28_plot <- ggplot2::ggplot(data_d28, ggplot2::aes_string(x = "photon", y = "rate", color = "photoperiod", label = "intensity")) +
    ggplot2::geom_line() +
    ggplot2::geom_vline(xintercept = 120, color = "#000000") +
    ggplot2::geom_vline(xintercept = 15, color = "#000000") +
    ggplot2::geom_vline(xintercept = 60, color = "#CDCDCD", size = 0.5) +
    ggplot2::geom_text(family = "Arial", fontface = "bold", size = 2.5) +
    ggplot2::scale_y_continuous(limits = c(0, 320), name = "Weight gain per photon (ug m2 mol-1)", expand = c(0, 0), breaks=c(0, 100, 200, 300)) +
    ggplot2::scale_x_continuous(limits = c(15, 120), breaks=c(15, 120), name = "Daily photon flux (mol m-2)") +
    theme_Prism() +
    ggplot2::theme(line = ggplot2::element_blank(), legend.position = "none") +
    ggplot2::scale_color_manual(values=c("#EE0000", "#0000FF")) +
    ggplot2::facet_wrap(~ .data$genotype, scales = "fixed", nrow = 1)

  plot <- ggpubr::ggarrange(plotlist = list(day20_plot, day28_plot), ncol = 1)

  return(plot)

}

