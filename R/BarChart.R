#' Plot a bar chart the photon efficiency data
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @return ggplot2 object
#' @export


BarChart <- function(

){

  # Get growth rate data frame -----------
  utils::data("GrowthRateData", envir=environment())

  ## Day 28 -----------

  ## Get day 28 data
  data_d28 <- GrowthRateData %>%
    ## Get only the day 28 data
    dplyr::filter(.data$age == 28) %>%
    ## Make sure the intensities are characters
    dplyr::mutate(intensity = as.character(.data$intensity)) %>%
    dplyr::mutate(photon_per_day = as.character(.data$photon_per_day)) %>%
    dplyr::group_by(.data$photoperiod, .data$genotype, .data$intensity) %>%
    ## Make sure the plots are in the correct order
    dplyr::mutate(genotype = factor(.data$genotype, levels = c("WT", "mips1", "co9", "ft10"))) %>%
    dplyr::mutate(sample_tag = paste0(.data$photoperiod, "-", .data$intensity))

  ## We are only interested in LD-100 / SD-100 / SD-200

  day28_barchart <- data_d28 %>%
    dplyr::filter((.data$photon_per_day == "57.6") | (.data$intensity == "100")) %>%
    dplyr::mutate(average_growth_rate_per_photon = .data$average_growth_rate_per_photon*1000*1000) %>%
    ggplot2::ggplot(ggplot2::aes_string(y = "average_growth_rate_per_photon", x = "sample_tag")) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_y_continuous(expand = c(0,0), name = "Average growth rate per photon (ug m2 mol-1)") +
    ggplot2::facet_wrap(~ .data$genotype, scales = "fixed", nrow = 1) +
    theme_Prism()

  ## Day 20 -----------

  ## Get day 28 data
  data_d20 <- GrowthRateData %>%
    ## Get only the day 28 data
    dplyr::filter(.data$age == 20) %>%
    ## Make sure the intensities are characters
    dplyr::mutate(intensity = as.character(.data$intensity)) %>%
    dplyr::mutate(photon_per_day = as.character(.data$photon_per_day)) %>%
    dplyr::group_by(.data$photoperiod, .data$genotype, .data$intensity) %>%
    ## Make sure the plots are in the correct order
    dplyr::mutate(genotype = factor(.data$genotype, levels = c("WT", "mips1", "co9", "ft10"))) %>%
    dplyr::mutate(sample_tag = paste0(.data$photoperiod, "-", .data$intensity))

  ## We are only interested in LD-100 / LD-200

  day20_barchart <- data_d20 %>%
    dplyr::filter((.data$intensity %in% c("100", "200")) & (.data$photoperiod == "LD")) %>%
    dplyr::mutate(average_growth_rate_per_photon = .data$average_growth_rate_per_photon*1000*1000) %>%
    ggplot2::ggplot(ggplot2::aes_string(y = "average_growth_rate_per_photon", x = "sample_tag")) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_y_continuous(expand = c(0,0), name = "Average growth rate per photon (ug m2 mol-1)") +
    ggplot2::facet_wrap(~ .data$genotype, scales = "fixed", nrow = 1) +
    theme_Prism()

  return(ggpubr::ggarrange(plotlist = list(day20_barchart, day28_barchart), ncol = 1))
}


