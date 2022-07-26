#' @title get_profile
#'
#' @description
#' An accessor function that returns a data frame of a profile class that can be easily plotted
#'
#' @author Craig Marsh
#' @param model <casal2MPD> object that are generated from one of the extract.mpd() and extract.tabular() functions.
#' @return A data frame with profile_values and likelihood components
#' @rdname get_profile
#' @export get_profile

"get_profile" <-
  function(model) {
    UseMethod("get_profile", model)
  }

#'
#' @rdname get_profile
#' @method get_profile casal2MPD
#' @export
get_profile.casal2MPD <- function(model) {
  reports_labels = reformat_default_labels(names(model))
  parameter_df = NULL
  objective_function_df = NULL
  for(i in 1:length(model)) {
    if (reports_labels[i] == "header") {
      this_report = model[[i]]
      if(!grepl(this_report$call, pattern = "-p")) {
        stop(paste0("not a projection casal output. Cannot find '-p' in the '", this_report$call,"'."))
      }
      next
    }
    this_report = model[[i]]
    if(exists(x = "type", where = this_report[[1]])) {
      if(this_report[[1]]$type == "profile") {
        ## we are in a profile class
        parameter_df = data.frame(label =this_report[[1]]$profile, parameter = this_report[[1]]$parameter,  parameter_values = this_report[[1]]$values)
      }
      if(this_report[[1]]$type == "objective_function") {
        ## found an objective function class
        objective_function_df = aggregate_objective_report(this_report)
      }
    }
  }
  ## merge and massage the data into a nicer format
  colnames(objective_function_df) = c("component", parameter_df$parameter_values)
  ##
  molten_profile = melt(objective_function_df, id.vars = "component")
  colnames(molten_profile) = c("component", "parameter_values", "negative_loglikelihood")
  # convert factor to numeric
  molten_profile$parameter_values = as.numeric(as.character(molten_profile$parameter_values))
  molten_profile$parameter = unique(parameter_df$parameter)
  return(molten_profile)
}
