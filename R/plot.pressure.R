#' @title plot.pressure plot fishing pressure if there has been an exploitation process reported.
#'
#' @description
#' A plotting function to plot fishing presuure (U's ) for 'casal2TAB' and 'casal2MPD' objects.
#'
#' @author Craig Marsh
#' @param model <casal2MPD, casal2TAB> object that are generated from one of the extract.mpd() and extract.tabular() functions using the Casal2 base library
#' @param report_label <string>
#' @param fisheryLabels if you only want to plot a subset of the fisheries supply a vecor of characters which corresponds to the fisheries you want to plot.
#' @param plot.it Whether to generate a default plot or return the values as a dataframe for personal plots
#' @param ... remaining plotting options
#' @return generate a plot over time if plot.it = T, if plot.it = F it will return a matrix of values.
#' @rdname plot.pressure
#' @export plot.pressure
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot geom_line aes theme facet_wrap
#' @details
#' If you have multiple time-steps and fisheries happening at different time-steps it may be useful to use the fisheryLabels command to split out the plots.

"plot.pressure" <-
function(model, report_label = "", fisheryLabels = NULL, plot.it = T, ...) {
  UseMethod("plot.pressure", model)
}

#' @return \code{NULL}
#'
#' @rdname plot.pressure
#' @method plot.pressure casal2MPD
#' @export
"plot.pressure.casal2MPD" = function(model, report_label = "", fisheryLabels = NULL, plot.it = T, ...) {
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
    if (!(this_report$sub_type %in% c("mortality_instantaneous", "mortality_instantaneous_retained")))
      stop(paste0("The report label '", report_label, "' is a process that should be type 'mortality_instantaneous' or 'mortality_instantaneous_retained'."))

  } else {
    print("multi iteration report found")
    muliple_iterations_in_a_report = TRUE
    N_runs = length(this_report)
    if (this_report$'1'$type != "process")
      stop(paste0("The report label '", report_label, "' is not a process. Please check that the correct report_label was specified."))
    if (!(this_report$'1'$sub_type %in% c("mortality_instantaneous", "mortality_instantaneous_retained")))
      stop(paste0("The report label '", report_label, "' is a process that should be type 'mortality_instantaneous' or 'mortality_instantaneous_retained'."))
  }
  full_df = NULL;
  if (!muliple_iterations_in_a_report) {
    ## only a single trajectory
    f_ndx = grepl(pattern = "fishing_pressure", names(this_report))
    catch_ndx = grepl(pattern = "catch", substring(names(this_report), first = 1, last = 5))
    actual_catch_ndx = grepl(pattern = "actual_catch", names(this_report))

    start_index = as.numeric(regexpr(pattern = "\\[", text = names(this_report)[f_ndx])) + 1
    stop_index = as.numeric(regexpr(pattern = "\\]", text = names(this_report)[f_ndx])) - 1
    fisheries = substring(names(this_report)[f_ndx], start_index, last = stop_index)
    years = this_report$year

    for (i in 1:length(fisheries)) {
      temp_df = data.frame(year =this_report$year, exploitation = this_report[[which(f_ndx)[i]]],
                           catch = this_report[[which(catch_ndx)[i]]],
                           actual_catch = this_report[[which(actual_catch_ndx)[i]]],
                           fishery = fisheries[i],
                           par_set = 1)
      full_df = rbind(full_df, temp_df)
    }
    if(!is.null(fisheryLabels)) {
      if(!all(fisheryLabels %in% fisheries)) {
        stop(paste0("you supplied labesl ", paste(fisheryLabels, collapse = ", "), " but the following fisheries are available ",  paste(fisheries, collapse = ", ")))
      }
      full_df = full_df %>% filter(fishery %in% fisheryLabels)
    }

    ## create a plot
    plt = ggplot(full_df, aes(x = year, y = exploitation, group = fishery, col = fishery)) +
      geom_line(size = 2)
    if(plot.it)
      return(plt)

  } else {
    ## Multiple parameter inputs
    n_runs = length(this_report)
    for(dash_i in 1:n_runs) {
      ## only a single trajectory
      f_ndx = grepl(pattern = "fishing_pressure", names(this_report[[dash_i]]))
      start_index = as.numeric(regexpr(pattern = "\\[", text = names(this_report[[dash_i]])[f_ndx])) + 1
      stop_index = as.numeric(regexpr(pattern = "\\]", text = names(this_report[[dash_i]])[f_ndx])) - 1
      fisheries = substring(names(this_report[[dash_i]])[f_ndx], start_index, last = stop_index)
      years = this_report[[dash_i]]$year
      for (i in 1:length(fisheries)) {
        temp_df = data.frame(year =years, exploitation = this_report[[dash_i]][[which(f_ndx)[i]]], fishery = fisheries[i], par_set = dash_i)
        full_df = rbind(full_df, temp_df)
      }
    }
    if(!is.null(fisheryLabels)) {
      if(!all(fisheryLabels %in% fisheries)) {
        stop(paste0("you supplied labesl ", paste(fisheryLabels, collapse = ", "), " but the following fisheries are available ",  paste(fisheries, collapse = ", ")))
      }
      full_df = full_df %>% filter(fishery %in% fisheryLabels)
    }
    full_df$par_set = factor(full_df$par_set, ordered = T)
    plt = ggplot(full_df, aes(x = year, y = exploitation, group = par_set, col = par_set)) +
      geom_line(size = 2) +
      facet_wrap(~ fishery)
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
#' @rdname plot.pressure
#' @method plot.pressure casal2TAB
#' @export
"plot.pressure.casal2TAB" = function(model, report_label = "", fisheryLabels = NULL, plot.it = T, ...) {
  ## check report label exists
  if (!report_label %in% names(model))
    stop(paste0("The report label '", report_label, "' was not found. The report labels available are: ", paste(names(model), collapse = ", ")))
  ## get the report out
  this_report = get(report_label, model)
  ## check that the report label is of type derived_quantity
  if (this_report$type != "process") {
    stop(paste0("The report label '", report_label, "' is not a derived quantity. Please check that the correct report_label was specified."))
  }
  if (!(this_report$process_type %in% c("mortality_instantaneous", "mortality_instantaneous_retained")) || is.null(this_report$process_type)) {
    stop(paste0("The process type in report '", report_label, "' is not 'mortality_instantaneous' or 'mortality_instantaneous_retained'. Please check that the correct report_label was specified."))
  }

  if (plot.it) {
    Labs = colnames(this_report$values)
    start_index = as.numeric(regexpr(pattern = "\\[", text = Labs)) + 1
    stop_index = as.numeric(regexpr(pattern = "\\]", text = Labs)) - 1
    Fisheries = unique(substring(Labs, start_index, last = stop_index))

    par(mfrow = c(1, length(Fisheries)))
    for (i in 1:length(Fisheries)) {
      ## pull out label and years
      ndx = grepl(pattern = paste0("fishing_pressure\\[", Fisheries[i]), x = Labs)
      this_ssb = this_report$values[, ndx]
      start_nd = as.numeric(regexpr(pattern = "\\]", text = colnames(this_ssb))) + 2
      years = as.numeric(substring(colnames(this_ssb), first = start_nd, last = nchar(colnames(this_ssb)) - 1))
      vals = apply(this_ssb, 2, quantile, c(0.025, 0.5, 0.975))
      plot(years, vals["50%",], ylim = c(0, max(vals)), ylab = "Fishing Pressure (exploitation rate)", xlab = "years", type = "l", main = Fisheries[i])
      polygon(x = c(years, rev(years)), y = c(vals["2.5%",], rev(vals["97.5%",])), col = "gray60")
      lines(years, vals["50%",], col = "red", lwd = 2)
    }
  } else {
    return(this_report$values)
  }
  invisible()
}
