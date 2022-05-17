#' @title plot.relative_index default
#'
#' @description
#' A plotting function for Casal2 fits to abundance or biomass observations
#'
#' @author Craig Marsh
#' @param model <casal2MPD, casal2TAB> object that are generated from one of the extract() functions.
#' @param report_labels vector<string> of report labels to plot
#' @param plot.it Whether to generate a default plot or return the values as a matrix.
#' @param plot_type string
#' @return A ggplot object
#' @importFrom ggplot2 ggplot geom_line aes theme facet_wrap facet_grid geom_hline geom_ribbon  geom_point scale_colour_manual scale_fill_manual scale_alpha geom_errorbar
#' @rdname plot.relative_index
#' @export plot.relative_index
#' @examples
#' \dontrun{
#' library(casal2)
#' # plotting Standard Output
#' data <- extract.mpd(file = system.file("extdata", "estimate.out", package="casal2"))
#' names(data)
#' par(mfrow = c(1,2))
#' plot.fits(model = data, report_labels = "westF_at_age")
#' plot.fits(model = data, report_labels = "eastF_at_age")
#' # to create a user-specified plot, use plot.it = FALSE.
#' Tangaroa_fits <- plot.fits(model = data, report_label = "eastF_at_age", plot.it = FALSE)
#' }

"plot.relative_index" <- function(model, report_labels = "", plot_type = "classic", plot.it = T) {
  UseMethod("plot.relative_index", model)
}

#' @return \code{NULL}
#'
#' @rdname plot.relative_index
#' @method plot.relative_index casal2MPD
#' @export
"plot.relative_index.casal2MPD" <- function(model, report_labels = "", plot_type = "classic", plot.it = T) {
  multiple_iterations_in_a_report <- FALSE
  N_runs <- 1
  likelihoods_allowed <- c("lognormal", "normal")
  observations_allowed <- c("biomass", "abundance")
  likelihood = ""
  full_DF = NULL
  for(i in 1:length(report_labels)) {
    ## check report exists
    check_report = check_report_label(report_label = report_labels[i], model = model)
    if(!check_report$check)
      stop(check_report$msg)
    ## get the report out
    this_report = get(report_labels[i], model)
    if (any(names(this_report) == "type")) {
      if (this_report$type != "observation") {
        stop(paste0("The report label '", report_labels[i], "' is not an observation. Please check that the correct report label was specified."))
      }
      if (!this_report$observation_type %in% observations_allowed) {
        stop(paste0("This function can be used with observation types ", paste(observations_allowed, collapse = ", "), " only."))
      }
    } else {
      print("multi iteration report found")
      multiple_iterations_in_a_report <- TRUE
      N_runs <- length(this_report)

      if (this_report$'1'$type != "observation") {
        stop(paste0("The report label '", report_labels[i], "' is not an observation. Please check that the correct report label was specified."))
      }
      if (!this_report$'1'$observation_type %in% observations_allowed) {
        stop(paste0("This function can be used with observation types ", paste(observations_allowed, collapse = ", "), " only."))
      }
    }

    if (!multiple_iterations_in_a_report) {
      likelihood = this_report$likelihood
      temp_df = this_report$Values
      temp_df$label = report_labels[i]
      full_DF = rbind(full_DF, temp_df)
    } else {
      n_runs = length(this_report)
      likelihood = this_report[[1]]$likelihood
      for(dash_i in 1:n_runs) {
        temp_df = this_report[[dash_i]]$Values;
        temp_df$par_set = dash_i
        temp_df$label = report_labels[i]
        full_DF = rbind(full_DF, temp_df)
      }
      full_DF$par_set = factor(full_DF$par_set, ordered = T)
    }
  }
  ## calculate 95% CIs
  if(likelihood == "normal") {
    total_sigma <- full_DF$observed * full_DF$adjusted_error
    full_DF$U_CI <- full_DF$observed + 1.96 * total_sigma
    full_DF$L_CI <- full_DF$observed - 1.96 * total_sigma
  } else if(likelihood == "lognormal") {
    total_sigma <- sqrt(log(1 + full_DF$adjusted_error ^ 2))
    Mean <- log(full_DF$observed) - 0.5 * (total_sigma ^ 2)
    full_DF$U_CI <- exp(Mean + 1.96 * total_sigma)
    full_DF$L_CI <- exp(Mean - 1.96 * total_sigma)
  }
  ## create plot
  plt = NULL
  col_pallete =  c("observed"="#f04546","expected"="#3591d1","95% CI"="#62c76b")
  if(plot_type == "classic") {
    plt = ggplot(full_DF, aes(x = year, group = label, col = label)) +
      geom_errorbar(aes(ymin=L_CI, ymax=U_CI, col = "95% CI")) +
      geom_point(aes(y = observed, col = "observed"), size = 2) +
      geom_point(aes(y = expected, col = "expected"), size = 2) +
      scale_colour_manual(name="Key",values=col_pallete)
    if(multiple_iterations_in_a_report) {
      plt = plt + facet_grid(par_set~label)
    } else {
      plt = plt + facet_grid(~label)
    }
  } else if(plot_type == "classic_ribbon") {
    plt = ggplot(full_DF, aes(x = year, group = label, col = label)) +
      geom_ribbon(aes(ymin=L_CI, ymax=U_CI, alpha = 0.2, col = "95% CI", fill = "95% CI")) +
      geom_point(aes(y = observed, col = "observed"), size = 2) +
      geom_point(aes(y = expected, col = "expected"), size = 2) +
      scale_colour_manual(name="Key",values=col_pallete) +
      scale_fill_manual(name="Key",values=col_pallete) +
      scale_alpha(guide = 'none') +
      if(multiple_iterations_in_a_report) {
        plt = plt + facet_grid(par_set~label)
      } else {
        plt = plt + facet_grid(~label)
      }

  } else if(plot_type == "residual") {
    plt = ggplot(full_DF, aes(x = year, y = residual, group = label, col = label)) +
      geom_point(size = 2) +
      geom_hline(yintercept = 0, linetype = "dashed", size =1.2)
    if(multiple_iterations_in_a_report) {
      plt = plt + facet_grid(par_set~label)
    } else {
      plt = plt + facet_grid(~label)
    }
  }

  if(plot.it)
    return(plt)

  if (!plot.it)
    return(full_DF)
  invisible()
}


#' @return \code{NULL}
#'
#' @rdname plot.relative_index
#' @method plot.relative_index casal2TAB
#' @export
"plot.relative_index.casal2TAB" <- function(model, report_labels = "", plot_type = "classic", plot.it = T) {
  stop("function not coded yet")
}

