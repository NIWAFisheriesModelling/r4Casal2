#'
#' calculate_composition_stage_two_weights
#' @description this function will iterate over all (or a subset of) compositional observations to calculate the method TA 1.8 weights from \insertCite{francis2011data}{r4Casal2}
#' @param model casal2 mpd model
#' @param observation_labels vector<string> of observation labels to calculate the stage two weights for. This function assumes your observation label (@observation) is the same as the report label (@report)
#' @return data.frame of second stage weights
#' @importFrom Rdpack reprompt
#' @importFrom Casal2 Method.TA1.8
#' @references
#' \insertAllCited{}
calculate_composition_stage_two_weights <- function(model, observation_labels = NULL) {
  #model = Casal2::extract.mpd("C:\\Users\\marshc\\OneDrive - NIWA\\22_23\\SNA1\\csl\\P30 Base HGBP\\Casal2\\estimate.log")
  comp_obs = get_composition_observations(model)
  comp_labels = unique(comp_obs$observation_label)
  if(!is.null(observation_labels)) {
    if(any(!(observation_labels %in% comp_labels))) {
      stop("Could not find some of the observation_labels supplied in model. Please check this or this function.")
    }
    comp_labels = subset(comp_labels, subset = comp_labels %in% observation_labels)
  }
  weights = vector()
  for(i in 1:length(comp_labels)) {
    weights[i] = Method.TA1.8(model = model, observation_labels = comp_labels[i], plot.it = F)
  }
  return(data.frame(observation = comp_labels, weight = weights))
}
