#' @title plot_selectivities
#'
#' @description
#' A Generic plotting function for selectivities that are derived from get_selectivities function()
#'
#' @author Craig Marsh
#' @param selectivity_df data.frame
#' @importFrom ggplot2 ggplot geom_line aes
#' @return a ggplot
#' @rdname plot_selectivities
#' @export plot_selectivities


"plot_selectivities" <- function(selectivity_df) {
  plt = ggplot(full_DF, aes(x = bins, group = label, col = label)) +
    geom_line(aes(y = values), size = 2)
  return(full_DF)
  invisible()
}
