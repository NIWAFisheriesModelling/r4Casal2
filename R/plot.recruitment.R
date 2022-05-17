#' @title plot.recruitment plot recuitment
#'
#' @description
#' A plotting function to plot recuitment for 'casal2TAB' and 'casal2MPD' objects.
#'
#' @author Craig Marsh
#' @param model <casal2MPD, casal2TAB> object that are generated from one of the extract.mpd() and extract.tabular() functions using the Casal2 base library
#' @param report_label <string>
#' @param quantity
#'  \itemize{
#'   \item ycs_values
#'   \item Recruits
#'   \item true_ycs
#'   \item standardised_ycs
#' }
#' @param plot.it Whether to generate a default plot or return the values as a dataframe for personal plots
#' @param ... remaining plotting options
#' @return generate a plot over time if plot.it = T, if plot.it = F it will return a matrix of values.
#' @rdname plot.recruitment
#' @export plot.recruitment
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot geom_line aes theme facet_wrap aes_string
#' @details
#' If you have multiple time-steps and Recruitment

"plot.recruitment" <-
  function(model, report_label = "", quantity = "ycs_values", plot.it = T, ...) {
    if(!quantity %in% c("ycs_values", "Recruits", "true_ycs", "standardised_ycs"))
      stop("quantity, has incorrect values please check ?plot.recruitment")
    UseMethod("plot.recruitment", model)
  }

#' @return \code{NULL}
#'
#' @rdname plot.recruitment
#' @method plot.recruitment casal2MPD
#' @export
"plot.recruitment.casal2MPD" = function(model, report_label = "", quantity = "ycs_values", plot.it = T, ...) {
  muliple_iterations_in_a_report = FALSE
  N_runs = 1
  temp_DF = NULL

  check_report = check_report_label(report_label = report_label, model = model)
  if(!check_report$check)
    stop(check_report$msg)

  ## get the report out
  this_report = get(report_label, model)
  ## check that the report label is of type "process"
  if (any(names(this_report) == "type")) {
    if (this_report$type != "process")
      stop(paste0("The report label '", report_label, "' is not a process. Please check that the correct report_label was specified."))
    if (!(this_report$sub_type %in% c("recruitment_beverton_holt")))
      stop(paste0("The report label '", report_label, "' is a process that should be type 'recruitment_beverton_holt'."))

  } else {
    print("multi iteration report found")
    muliple_iterations_in_a_report = TRUE
    N_runs = length(this_report)
    if (this_report$'1'$type != "process")
      stop(paste0("The report label '", report_label, "' is not a process. Please check that the correct report_label was specified."))
    if (!(this_report$'1'$sub_type %in% c("recruitment_beverton_holt")))
      stop(paste0("The report label '", report_label, "' is a process that should be type 'recruitment_beverton_holt'."))
  }
  full_df = NULL;
  if (!muliple_iterations_in_a_report) {
    ## only a single trajectory
    standardised_ycs = this_report$standardised_ycs
    ycs_values = this_report$ycs_values
    ycs_years = this_report$ycs_years
    Recruits = this_report$Recruits
    true_ycs = this_report$true_ycs

    full_df = data.frame(ycs_years = this_report$ycs_years,
                         standardised_ycs = this_report$standardised_ycs,
                         ycs_values = this_report$ycs_values,
                         recruits = this_report$Recruits,
                         true_ycs = this_report$true_ycs,
                         par_set = 1,
                         label = report_label)


    ## create a plot
    plt = ggplot(full_df, aes_string(x = "ycs_years", y = quantity)) +
      geom_line(size = 2)
    if(plot.it)
      return(plt)

  } else {
    ## Multiple parameter inputs
    n_runs = length(this_report)
    for(dash_i in 1:n_runs) {
      ## only a single trajectory
      temp_df = data.frame(
        standardised_ycs = this_report[[dash_i]]$standardised_ycs,
        ycs_values = this_report[[dash_i]]$ycs_values,
        ycs_years = this_report[[dash_i]]$ycs_years,
        Recruits = this_report[[dash_i]]$Recruits,
        true_ycs = this_report[[dash_i]]$true_ycs,
        par_set = dash_i,
        label = report_label)
      full_df = rbind(full_df, temp_df)
    }
    ## create a plot
    plt = ggplot(full_df, aes_string(x = "ycs_years", y = quantity)) +
      geom_line(size = 2)

    if(plot.it)
      return(plt)

  }

  if (plot.it == FALSE)
    return(full_df)
  invisible()
}

## method for class casal2TAB
#' @return \code{NULL}
#'
#' @rdname plot.recruitment
#' @method plot.recruitment casal2TAB
#' @export
"plot.recruitment.casal2TAB" = function(model, report_label = "", quantity = "ycs_values", plot.it = T, ...) {

  invisible()
}
