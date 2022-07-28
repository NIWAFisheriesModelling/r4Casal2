#' @title get_transformed_parameters
#'
#' @description
#' An accessor function that returns a data frame of a parameter_transformations class that can be easily plotted
#'
#' @author Craig Marsh
#' @param model <casal2MPD> object that are generated from one of the extract.mpd() and extract.tabular() functions.
#' @return A data frame with profile_values and likelihood components
#' @rdname get_transformed_parameters
#' @export get_transformed_parameters

"get_transformed_parameters" <-
  function(model) {
    UseMethod("get_transformed_parameters", model)
  }

#'
#' @rdname get_transformed_parameters
#' @method get_transformed_parameters casal2MPD
#' @export
get_transformed_parameters.casal2MPD <- function(model) {
  reports_labels = reformat_default_labels(names(model))
  parameter_df = NULL
  for(i in 1:length(model)) {
    if (reports_labels[i] == "header") {
      next
    }
    this_report = model[[i]]
    if(exists(x = "type", where = this_report)) {
      if(this_report$type != "parameter_transformations") {
        next;
      }
      ## these reports are bespoke which sucks
      if(this_report$transformation_type == "log") {
        temp_df = data.frame(label = reports_labels[i], type = this_report$transformation_type, parameter = this_report$parameters, untransformed = this_report$parameter_values, transformed_value = this_report$log_parameter)
        parameter_df = rbind(parameter_df, temp_df)
      } else if (this_report$transformation_type == "logistic") {
        temp_df = data.frame(label = reports_labels[i], type = this_report$transformation_type, parameter = this_report$parameters, untransformed = this_report$parameter_values, transformed_value = this_report$logistic_parameter)
        parameter_df = rbind(parameter_df, temp_df)
      }
    } else {
      # multi -i report

    }
  }
  return(parameter_df)
}
