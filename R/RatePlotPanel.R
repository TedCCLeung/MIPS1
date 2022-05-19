#' Plot all three types of rate plots at once.
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @param log_rate Logical. If TRUE then the growth rate is log10 transformed.
#' @param select_genotype Character. One of "WT", "co9", "ft10", "mips1".
#'
#' @return ggplot2 object
#' @export


RatePlotPanel <- function(

  select_genotype = "WT",
  log_rate = FALSE

){

  rate_plot_list <- list(
    RatePlot(
      log_rate = log_rate,
      select_genotype = select_genotype,
      value = "weight"
    ),
    RatePlot(
      log_rate = log_rate,
      select_genotype = select_genotype,
      value = "average_rate"
    ),
    RatePlot(
      log_rate = log_rate,
      select_genotype = select_genotype,
      value = "instant_rate"
    )
  )

  return(ggpubr::ggarrange(plotlist = rate_plot_list, ncol = 1))

}


