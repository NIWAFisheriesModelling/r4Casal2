#' @title plot.derived_quantities default
#'
#' @description
#' A plotting function for Casal2 derived_quantities
#'
#' @author Craig Marsh
#' @param model <casal2MPD, casal2TAB, list> object that are generated from one of the extract() functions. If list then we expect multiple mpd runs (should be a named list )
#' @param report_labels vector<string> of report labels to plot
#' @param plot.it Whether to generate a default plot or return the values as a matrix.
#' @param plot_type string
#' @return A ggplot object
#' @importFrom ggplot2 ggplot geom_line aes theme scale_fill_manual scale_alpha
#' @rdname plot.derived_quantities
#' @export plot.derived_quantities
#' @examples
#' \donttest{
#' library(Casal2)
#' # plotting Standard Output
#' data <- extract.mpd(file = system.file("extdata", "estimate.out", package="r4Casal2"))
#' names(data)
#' par(mfrow = c(1,2))
#' plot.derived_quantities(model = data, report_labels = "SSB")
#' }

"plot.derived_quantities" <- function(model, report_label = "", plot_type = "classic", plot.it = T) {
  UseMethod("plot.derived_quantities", model)
}


#'
#' @rdname plot.derived_quantities
#' @method plot.derived_quantities casal2MPD
#' @export
"plot.derived_quantities.casal2MPD" <- function(model, report_label = "", plot_type = "classic", plot.it = T) {
  full_DF = NULL
  multiple_iterations_in_a_report = FALSE
  this_report = get(report_label, model)
  if (any(names(this_report) == "type")) {
    if (this_report$type != "derived_quantity") {
      stop(paste0("The report label '", report_label, "' is not an derived_quantity Please check that the correct report label was specified."))
    }
  } else {
    print("multi iteration report found")
    multiple_iterations_in_a_report <- TRUE
    if (this_report$'1'$type != "derived_quantity") {
      stop(paste0("The report label '", report_label, "' is not an derived_quantity Please check that the correct report label was specified."))
    }
  }

  if (!multiple_iterations_in_a_report) {
    DQ_types = names(this_report)[!names(this_report) %in% "type"]
    dq_df = NULL
    for(dq_iter in 1:length(DQ_types)) {
      temp_df = data.frame(par_set = 1, years = names(this_report[[DQ_types[dq_iter]]]$values), values = this_report[[DQ_types[dq_iter]]]$values, label = DQ_types[dq_iter]);
      dq_df = rbind(dq_df, temp_df)
    }
    full_DF = rbind(full_DF, dq_df)
  } else {
    n_runs = length(this_report)
    for(dash_i in 1:n_runs) {
      ##
      DQ_types = names(this_report[[dash_i]])[!names(this_report[[dash_i]]) %in% "type"]
      dq_df = NULL
      for(dq_iter in 1:length(DQ_types)) {
        temp_df = data.frame(par_set = dash_i, years = names(this_report[[dash_i]][[DQ_types[dq_iter]]]$values), values = this_report[[dash_i]][[DQ_types[dq_iter]]]$values, label = DQ_types[dq_iter]);
        dq_df = rbind(dq_df, temp_df)
      }
      full_DF = rbind(full_DF, dq_df)
    }
    full_DF$par_set = factor(full_DF$par_set, ordered = T)
  }
  full_DF$years = as.numeric(full_DF$years)
  plt = NULL
  if(plot_type == "classic") {
    plt = ggplot(full_DF, aes(x = years, y = values, col = label)) +
      geom_line(size = 2) +
      theme(axis.text.x = element_text(angle = 90))
  } else {
    stop("unknown 'plot_type'")
  }
  if(plot.it)
    return(plt)

  if (!plot.it)
    return(full_DF)
}



#'
#' @rdname plot.derived_quantities
#' @method plot.derived_quantities casal2TAB
#' @export
"plot.derived_quantities.casal2TAB" <- function(model, report_label = "", plot_type = "classic", plot.it = T) {
  stop("function not coded yet")
}

#'
#' @rdname plot.derived_quantities
#' @method plot.derived_quantities list
#' @export
"plot.derived_quantities.list" <- function(model, report_label = "", plot_type = "classic", plot.it = T) {
  run_labs = names(model)
  full_DF = NULL

  for(i in 1:length(model)) {
    multiple_iterations_in_a_report = FALSE
    this_report = model[[i]][[report_label]]
    if (any(names(this_report) == "type")) {
      if (this_report$type != "derived_quantity") {
        stop(paste0("The report label '", report_label, "' is not an derived_quantity Please check that the correct report label was specified."))
      }
    } else {
      print("multi iteration report found")
      multiple_iterations_in_a_report <- TRUE
      if (this_report$'1'$type != "derived_quantity") {
        stop(paste0("The report label '", report_label, "' is not an derived_quantity Please check that the correct report label was specified."))
      }
    }

    if (!multiple_iterations_in_a_report) {
      DQ_types = names(this_report)[!names(this_report) %in% "type"]
      dq_df = NULL
      for(dq_iter in 1:length(DQ_types)) {
        temp_df = data.frame(model = run_labs[i], par_set = 1, years = names(this_report[[DQ_types[dq_iter]]]$values), values = this_report[[DQ_types[dq_iter]]]$values, label = DQ_types[dq_iter]);
        dq_df = rbind(dq_df, temp_df)
      }
      full_DF = rbind(full_DF, dq_df)
    } else {
      n_runs = length(this_report)
      for(dash_i in 1:n_runs) {
        ##
        DQ_types = names(this_report[[dash_i]])[!names(this_report[[dash_i]]) %in% "type"]
        dq_df = NULL
        for(dq_iter in 1:length(DQ_types)) {
          temp_df = data.frame(model = run_labs[i], par_set = dash_i, years = names(this_report[[dash_i]][[DQ_types[dq_iter]]]$values), values = this_report[[dash_i]][[DQ_types[dq_iter]]]$values, label = DQ_types[dq_iter]);
          dq_df = rbind(dq_df, temp_df)
        }
        full_DF = rbind(full_DF, dq_df)
      }
      full_DF$par_set = factor(full_DF$par_set, ordered = T)
    }
  }
  full_DF$years = as.numeric(full_DF$years)
  plt = NULL
  if(plot_type == "classic") {
    plt = ggplot(full_DF, aes(x = years, y = values, col = model, group = model)) +
      geom_line(size = 2) +
      theme(axis.text.x = element_text(angle = 90))
  } else {
    stop("unknown 'plot_type'")
  }
  if(plot.it)
    return(plt)

  if (!plot.it)
    return(full_DF)

  invisible()
}
