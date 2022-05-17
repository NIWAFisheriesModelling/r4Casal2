#' @title aggregate_objective_report
#' @details
#' take a Casal2 objective_function report and aggregrate components so easier to handle with
#' visualising likelihood components
#' @param objective_report casal2_objective_function which is a report from an extract.mpd() object
#' @return data frame of aggregrated objective likelihood components
#' @rdname aggregate_objective_report
#' @export aggregate_objective_report
#'

aggregate_objective_report <- function(objective_report) {
  if(is.null(objective_report$type))
    stop("objective_report not an expecated Casal2 objective function report")
  if(objective_report$type != "objective_function")
    stop("objective_report must be of type  'objective_function'")
  obj_labels = names(objective_report$values)
  type = Reduce(c, lapply(strsplit(obj_labels, split = "->", fixed = T), FUN = function(x){x[1]}))
  label = Reduce(c, lapply(strsplit(obj_labels, split = "-", fixed = T), FUN = function(x){x[2]}))
  label = substring(label, first = 2)
  unique_vals = paste0(type, "-",label)
  aggregate_labs = unique(unique_vals)
  ll_values = vector()
  ## observations
  for(i in 1:length(aggregate_labs)) {
    ndx = unique_vals  %in%  aggregate_labs[i]
    ll_values[i] = sum(objective_report$values[ndx])
  }
  obj_df = data.frame(component = aggregate_labs, negative_loglik = ll_values)
  return(obj_df)
}
