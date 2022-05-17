#' plot_composition_predictive_dist
#' Plots a violin plot of the predictive distribution by age or length for an observation
#'
#' @param sim_data matrix of simulated data rows = year, col = simulations. This function assumes rownames(sim_data) = year
#' @param obs a data frame with columns obs and year
#' @param lab a label for the plot which clearly identifies the observation
#' @param age <bool> if true plot ages if FALSE it is length observations
#' @param plot_type = "violin" currently either violin or boxplot.
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot geom_violin aes geom_point geom_boxplot
#' @return a ggplot
#' @rdname plot_composition_predictive_dist
#' @export plot_composition_predictive_dist
plot_composition_predictive_dist <- function(sim_data, obs, lab, plot_type = "violin") {
  legend = c("Posterior Prediction" = "#56B4E9", "Observation" = "#D55E00")
  sim_data_long = melt(sim_data)
  colnames(sim_data_long) = c("year", "sim", "CPUE")
  yr_lvls = range(sim_data_long$year)
  sim_data_long$year = factor(sim_data_long$year, ordered = T, levels = yr_lvls[1]:yr_lvls[2])
  base_plot = ggplot(sim_data_long, aes(x = year, y = CPUE)) +
    ggtitle(lab) +
    scale_color_manual(values = legend) +
    guides(fill = "none") +
    labs(colour = "Legend", x = "Year", y = "Posterior Predictive Distribution") +
    scale_x_discrete(drop=FALSE)
  if(plot_type == "violin") {
    base_plot = base_plot + geom_violin(aes(color = "Posterior Prediction", fill = "Posterior Prediction"))
  } else {
    base_plot = base_plot + geom_boxplot(aes(color = "Posterior Prediction", fill = "Posterior Prediction"))
  }

  base_plot = base_plot + geom_point(data = obs, aes(x = factor(year), y = obs, color = "Observation", fill = "Observation"), size = 3.4, inherit.aes = F ) +
    theme(axis.text.x = element_text(angle = 90))
  return(print(base_plot))
}
