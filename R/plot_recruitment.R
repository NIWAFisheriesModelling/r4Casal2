#' @title plot_recruitment plot recuitment
#'
#' @description
#' A plotting function to plot recuitment for 'casal2TAB' and 'casal2MPD' objects.
#'
#' @author Craig Marsh
#' @param model <casal2MPD, casal2TAB> object that are generated from one of the extract.mpd() and extract.tabular() functions using the Casal2 base library
#' @param report_label <string> optional if you want to plot a specific recruitement report
#' @param quantity
#'  \itemize{
#'   \item ycs_values
#'   \item Recruits
#'   \item true_ycs
#'   \item standardised_ycs
#' }
#' @return generate a plot over time uses get_BH_recruitment
#' @rdname plot_recruitment
#' @export plot_recruitment
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot geom_line aes theme facet_wrap aes_string
#' @details
#' If you have multiple time-steps and Recruitment

"plot_recruitment" <-
  function(model, report_label = NULL, quantity = "ycs_values") {
    if(!quantity %in% c("ycs_values", "Recruits", "true_ycs", "standardised_ycs"))
      stop("quantity, has incorrect values please check ?plot_recruitment")
    UseMethod("plot_recruitment", model)
  }

#' @return \code{NULL}
#'
#' @rdname plot_recruitment
#' @method plot_recruitment casal2MPD
#' @export
"plot_recruitment.casal2MPD" = function(model, report_label = NULL, quantity = "ycs_values") {
  recruit_df = get_BH_recruitment(model)
  if(!is.null(report_label)) {
    recruit_df = subset(recruit_df, subset = label %in% report_label)
  }
  ## create a plot
  plt = ggplot(recruit_df, aes_string(x = "ycs_years", y = quantity)) +
    geom_line(size = 2) +
    facet_wrap(~label)
  return(plt)
}

## method for class casal2TAB
#' @return \code{NULL}
#'
#' @rdname plot_recruitment
#' @method plot_recruitment casal2TAB
#' @export
"plot_recruitment.casal2TAB" = function(model, report_label = "", quantity = "ycs_values") {

  invisible()
}
