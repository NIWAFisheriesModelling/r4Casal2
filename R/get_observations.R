#' @title get_abundance_observations
#'
#' @description
#' An accessor function that returns a data frame of all relative abundance data sets in a model
#'
#' @author Craig Marsh
#' @param model <casal2MPD, casal2TAB, list> object that are generated from one of the extract() functions. If list then we expect multiple mpd runs (should be a named list )
#' @return dataframe with all observations of type == 'observation' and observation_type %in% c('biomass', 'abundance')
#' @rdname get_abundance_observations
#' @export get_abundance_observations

"get_abundance_observations" <- function(model) {
  UseMethod("get_abundance_observations", model)
}

#' @rdname get_abundance_observations
#' @method get_abundance_observations casal2MPD
#' @export

"get_abundance_observations.casal2MPD" <- function(model) {
  observation_type_allowed = c("biomass", "abundance")
  # can be -r or -r -i
  multiple_iterations_in_a_report = FALSE
  complete_df = NULL
  reports_labels = names(model)
  for(i in 1:length(model)) {
    if (reports_labels[i] == "header")
      next;
    this_report = model[[i]]
    if(any(names(this_report) == "type")) {
      if(this_report$type != "observation") {
        next;
      }
      if(this_report$observation_type %in% observation_type_allowed) {
        ## add it to full df
        this_ob = this_report$Values
        this_ob$observation_label = reports_labels[i]
        this_ob$observation_type = this_report$observation_type
        this_ob$likelihood = this_report$likelihood
        this_ob$par_set = 1 ## so compatible with -i runs
        ## check col compatibility some reports will print residuals and some wont
        if(!is.null(complete_df)) {
          if(any(!colnames(complete_df) %in% colnames(this_ob))) {
            drop_cols = which(!colnames(complete_df) %in% colnames(this_ob))
            complete_df = complete_df[, -drop_cols]
          }
          if(any(!colnames(this_ob) %in% colnames(complete_df))) {
            drop_cols = which(!colnames(this_ob) %in% colnames(complete_df))
            this_ob = this_ob[, -drop_cols]
          }
        }
        complete_df = rbind(complete_df, this_ob)
        next;
      }

    } else {
      #print("multi iteration report found")
      multiple_iterations_in_a_report <- TRUE
      if (this_report[[1]]$type != "observation") {
        next;
      }
      if(this_report[[1]]$observation_type %in% observation_type_allowed) {
        n_runs = length(this_report)
        for(dash_i in 1:n_runs) {
          ## add it to full df
          this_ob = this_report[[dash_i]]$Values
          this_ob$observation_label = reports_labels[i]
          this_ob$observation_type = this_report[[dash_i]]$observation_type
          this_ob$likelihood = this_report[[dash_i]]$likelihood
          this_ob$par_set = dash_i
          ## check col compatibility some reports will print residuals and some wont
          if(!is.null(complete_df)) {
            if(any(!colnames(complete_df) %in% colnames(this_ob))) {
              drop_cols = which(!colnames(complete_df) %in% colnames(this_ob))
              complete_df = complete_df[, -drop_cols]
            }
            if(any(!colnames(this_ob) %in% colnames(complete_df))) {
              drop_cols = which(!colnames(this_ob) %in% colnames(complete_df))
              this_ob = this_ob[, -drop_cols]
            }
          }
          complete_df = rbind(complete_df, this_ob)
        }
      }
    }
  }
  ## calculate U_CI only implemented for normal and lognormal
  complete_df$U_CI = NA
  complete_df$L_CI = NA
  ## deal with normal
  normal_ndx = complete_df$likelihood == "normal"
  total_sigma <- complete_df$observed * complete_df$adjusted_error
  complete_df$U_CI[normal_ndx] <- complete_df$observed[normal_ndx] + 1.96 * total_sigma[normal_ndx]
  complete_df$L_CI[normal_ndx] <- complete_df$observed[normal_ndx] - 1.96 * total_sigma[normal_ndx]
  ## deal with lognormal
  lognormal_ndx = complete_df$likelihood == "lognormal"
  total_sigma <- sqrt(log(1 + complete_df$adjusted_error[lognormal_ndx] ^ 2))
  Mean <- log(complete_df$observed[lognormal_ndx]) - 0.5 * (total_sigma ^ 2)
  complete_df$U_CI[lognormal_ndx] <- exp(Mean + 1.96 * total_sigma)
  complete_df$L_CI[lognormal_ndx] <- exp(Mean - 1.96 * total_sigma)
  return(complete_df);
}


#' @rdname get_abundance_observations
#' @method get_abundance_observations list
#' @export

"get_abundance_observations.list" <- function(model) {
  run_labs = names(model)
  full_DF = NULL
  ## iterate over the models
  for(i in 1:length(model)) {
    if(class(model[[i]]) != "casal2MPD") {
      stop(paste0("This function only works on a named list with elements of class = 'casal2MPD'"))
    }
    this_abundance = get_abundance_observations(model[[i]])
    if(!is.null(this_abundance)) {
      this_abundance$model_label = run_labs[i]
      full_DF = rbind(full_DF, this_abundance);
    }
  }
  return(full_DF)
  invisible()

}

#' @title get_composition_observations
#'
#' @description
#' An accessor function that returns a data frame of all composition data sets in a model
#'
#' @author Craig Marsh
#' @param model <casal2MPD, casal2TAB, list> object that are generated from one of the extract() functions. If list then we expect multiple mpd runs (should be a named list )
#' @return dataframe with all observations of type == 'observation' and observation_type %in% c('biomass', 'abundance')
#' @rdname get_composition_observations
#' @export get_composition_observations
#'

"get_composition_observations" <- function(model) {
  UseMethod("get_composition_observations", model)
}

#' @rdname get_composition_observations
#' @method get_composition_observations casal2MPD
#' @export
"get_composition_observations.casal2MPD" <- function(model) {
  observation_type_allowed = c("proportions_at_age", "proportions_at_length","process_removals_by_age", "process_removals_by_length")
  # can be -r or -r -i
  multiple_iterations_in_a_report = FALSE
  complete_df = NULL
  reports_labels = names(model)
  for(i in 1:length(model)) {
    if (reports_labels[i] == "header")
      next;
    this_report = model[[i]]
    if(any(names(this_report) == "type")) {
      if(this_report$type != "observation") {
        next;
      }
      if(this_report$observation_type %in% observation_type_allowed) {
        ## add it to full df
        this_ob = this_report$Values
        this_ob$observation_label = reports_labels[i]
        this_ob$observation_type = this_report$observation_type
        this_ob$likelihood = this_report$likelihood
        this_ob$par_set = 1 ## so compatible with -i runs
        ## check col compatibility some reports will print residuals and some wont
        if(!is.null(complete_df)) {
          if(any(!colnames(complete_df) %in% colnames(this_ob))) {
            drop_cols = which(!colnames(complete_df) %in% colnames(this_ob))
            complete_df = complete_df[, -drop_cols]
          }
          if(any(!colnames(this_ob) %in% colnames(complete_df))) {
            drop_cols = which(!colnames(this_ob) %in% colnames(complete_df))
            this_ob = this_ob[, -drop_cols]
          }
        }
        complete_df = rbind(complete_df, this_ob)
        next;
      }

    } else {
      multiple_iterations_in_a_report <- TRUE
      if (this_report$'1'$type != "observation") {
        next;
      }
      if(this_report$'1'$observation_type %in% observation_type_allowed) {
        n_runs = length(this_report)
        for(dash_i in 1:n_runs) {
          ## add it to full df
          this_ob = this_report[[dash_i]]$Values
          this_ob$likelihood = this_report[[dash_i]]$likelihood
          this_ob$observation_label = reports_labels[i]
          this_ob$observation_type = this_report[[dash_i]]$observation_type
          this_ob$par_set = dash_i
          ## check col compatibility some reports will print residuals and some wont
          if(!is.null(complete_df)) {
            if(any(!colnames(complete_df) %in% colnames(this_ob))) {
              drop_cols = which(!colnames(complete_df) %in% colnames(this_ob))
              complete_df = complete_df[, -drop_cols]
            }
            if(any(!colnames(this_ob) %in% colnames(complete_df))) {
              drop_cols = which(!colnames(this_ob) %in% colnames(complete_df))
              this_ob = this_ob[, -drop_cols]
            }
          }
          complete_df = rbind(complete_df, this_ob)
        }
      }
    }
  }
  return(complete_df);
}



#' @rdname get_composition_observations
#' @method get_composition_observations list
#' @export

"get_composition_observations.list" <- function(model) {
  run_labs = names(model)
  full_DF = NULL
  ## iterate over the models
  for(i in 1:length(model)) {
    if(class(model[[i]]) != "casal2MPD") {
      stop(paste0("This function only works on a named list with elements of class = 'casal2MPD'"))
    }
    this_abundance = get_composition_observations(model[[i]])
    if(!is.null(this_abundance)) {
      this_abundance$model_label = run_labs[i]
      full_DF = rbind(full_DF, this_abundance);
    }
  }
  return(full_DF)
  invisible()

}
#' @title get_tag_recapture_observations
#'
#' @description
#' An accessor function that returns a data frame of all tag-recapture observations
#'
#' @author Craig Marsh
#' @param model <casal2MPD, casal2TAB, list> object that are generated from one of the extract() functions. If list then we expect multiple mpd runs (should be a named list )
#' @return dataframe with all observations of type == 'observation' and observation_type %in% c('biomass', 'abundance')
#' @rdname get_tag_recapture_observations
#' @export get_tag_recapture_observations

"get_tag_recapture_observations" <- function(model) {
  UseMethod("get_tag_recapture_observations", model)
}

#' @rdname get_tag_recapture_observations
#' @method get_tag_recapture_observations casal2MPD
#' @export

"get_tag_recapture_observations.casal2MPD" <- function(model) {
  observation_type_allowed = c("tag_recapture_by_length_for_growth", "tag_recapture_by_length", "tag_recapture_by_age")
  # can be -r or -r -i
  multiple_iterations_in_a_report = FALSE
  complete_df = NULL
  reports_labels = names(model)
  for(i in 1:length(model)) {
    if (reports_labels[i] == "header")
      next;
    this_report = model[[i]]
    if(any(names(this_report) == "type")) {
      if(this_report$type != "observation") {
        next;
      }
      if(this_report$observation_type %in% observation_type_allowed) {
        ## add it to full df
        this_ob = this_report$Values
        this_ob$observation_label = reports_labels[i]
        this_ob$observation_type = this_report$observation_type
        this_ob$likelihood = this_report$likelihood
        this_ob$par_set = 1 ## so compatible with -i runs
        ## check col compatibility some reports will print residuals and some wont
        if(!is.null(complete_df)) {
          if(any(!colnames(complete_df) %in% colnames(this_ob))) {
            drop_cols = which(!colnames(complete_df) %in% colnames(this_ob))
            complete_df = complete_df[, -drop_cols]
          }
          if(any(!colnames(this_ob) %in% colnames(complete_df))) {
            drop_cols = which(!colnames(this_ob) %in% colnames(complete_df))
            this_ob = this_ob[, -drop_cols]
          }
        }
        complete_df = rbind(complete_df, this_ob)
        next;
      }
    } else {
      #print("multi iteration report found")
      multiple_iterations_in_a_report <- TRUE
      if (this_report$'1'$type != "observation") {
        next;
      }
      if(this_report$'1'$observation_type %in% observation_type_allowed) {
        n_runs = length(this_report)
        for(dash_i in 1:n_runs) {
          ## add it to full df
          this_ob = this_report[[dash_i]]$Values
          this_ob$observation_label = reports_labels[i]
          this_ob$observation_type = this_report[[dash_i]]$observation_type
          this_ob$likelihood = this_report[[dash_i]]$likelihood
          this_ob$par_set = dash_i
          ## check col compatibility some reports will print residuals and some wont
          if(!is.null(complete_df)) {
            if(any(!colnames(complete_df) %in% colnames(this_ob))) {
              drop_cols = which(!colnames(complete_df) %in% colnames(this_ob))
              complete_df = complete_df[, -drop_cols]
            }
            if(any(!colnames(this_ob) %in% colnames(complete_df))) {
              drop_cols = which(!colnames(this_ob) %in% colnames(complete_df))
              this_ob = this_ob[, -drop_cols]
            }
          }
          complete_df = rbind(complete_df, this_ob)
        }
      }
    }
  }
  return(complete_df);
}


#' @rdname get_tag_recapture_observations
#' @method get_tag_recapture_observations list
#' @export

"get_tag_recapture_observations.list" <- function(model) {
  run_labs = names(model)
  full_DF = NULL
  ## iterate over the models
  for(i in 1:length(model)) {
    if(class(model[[i]]) != "casal2MPD") {
      stop(paste0("This function only works on a named list with elements of class = 'casal2MPD'"))
    }
    this_abundance = get_tag_recapture_observations(model[[i]])
    if(!is.null(this_abundance)) {
      this_abundance$model_label = run_labs[i]
      full_DF = rbind(full_DF, this_abundance);
    }
  }
  return(full_DF)
  invisible()

}

#' @title get_composition_mean_bin
#'
#' @description
#' An accessor function that returns a data frame of mean age or length calculations
#'
#' @author Craig Marsh
#' @param model <casal2MPD, casal2TAB, list> object that are generated from one of the extract() functions. If list then we expect multiple mpd runs (should be a named list )
#' @param ignore_plus_group <bool> we will assume the max age or length is a plus group and remove it.
#' @return dataframe with all mean age or length and standard errors
#' @rdname get_composition_mean_bin
#' @export get_composition_mean_bin
#' @details the dataframe returned has Oy: mean age/length observed, Ey: mean age/length predicted, SEy: Standard error, Nassumed: mean effective sample size.
#' Why use ignore_plus_group? not sure we need to chat to Jeremy and C Francis taken from the SNA code. My guess is that if there is a large plus group length or age, then this can skew the mean
#' statistic. At first thoughts I don't think it should be employed that frequently
"get_composition_mean_bin" <- function(model, ignore_plus_group = FALSE) {
  UseMethod("get_composition_mean_bin", model)
}

#' @rdname get_composition_mean_bin
#' @method get_composition_mean_bin casal2MPD
#' @export
get_composition_mean_bin.casal2MPD <- function(model, ignore_plus_group = FALSE) {
  comp_obs = get_composition_observations(model)
  obs = unique(comp_obs$observation_label)
  mean_bin_df = NULL
  for(i in 1:length(obs)) {
    # age or length?
    this_obs = comp_obs %>% filter(observation_label == obs[i])
    is_age = grepl(unique(this_obs$observation_type), pattern = "_age")
    if(ignore_plus_group) {
      ## grop plus group
      if(is_age) {
        this_obs = this_obs %>% filter(age != max(age))
      } else {
        this_obs = this_obs %>% filter(length != max(length))
      }
    }
    ## force proportions to sum = 1
    this_obs = this_obs %>% group_by(year, observation_label) %>% mutate(expected = expected / sum(expected), observed = observed / sum(observed))

    if(is_age) {
      mean_age = this_obs %>% group_by(year, observation_label) %>% summarise(Ey = sum(age * expected), Oy = sum(age * observed), E_squared_y = sum(age^2 * expected), Nassumed = mean(adjusted_error))
      mean_age$Ry = mean_age$Oy - mean_age$Ey
      mean_age$SEy = sqrt((mean_age$E_squared_y - mean_age$Ey^2) / mean_age$Nassumed)
      mean_bin_df = rbind(mean_bin_df, mean_age)
    } else {
      mean_length = this_obs %>% group_by(year, observation_label) %>% summarise(Ey = sum(length * expected), Oy = sum(length * observed), E_squared_y = sum(length^2 * expected), Nassumed = mean(adjusted_error))
      mean_length$Ry = mean_length$Oy - mean_length$Ey
      mean_length$SEy = sqrt((mean_length$E_squared_y - mean_length$Ey^2) / mean_length$Nassumed)
      mean_bin_df = rbind(mean_bin_df, mean_length)
    }
    mean_bin_df$'Std.res' <- (mean_bin_df$Oy - mean_bin_df$Ey)/mean_bin_df$SEy
    ## I think this is the final Francis weighting value TODO: to check
    Nmult <- 1 / var(mean_bin_df$'Std.res',na.rm=TRUE)

    # Find the adjusted confidence intervals
    mean_bin_df$ObsloAdj <- mean_bin_df$Oy - 2 * mean_bin_df$SEy / sqrt(Nmult)
    mean_bin_df$ObshiAdj <- mean_bin_df$Oy + 2 * mean_bin_df$SEy / sqrt(Nmult)
  }
  return(mean_bin_df)
}


#' @rdname get_composition_mean_bin
#' @method get_composition_mean_bin list
#' @export

"get_composition_mean_bin.list" <- function(model) {
  run_labs = names(model)
  full_DF = NULL
  ## iterate over the models
  for(i in 1:length(model)) {
    if(class(model[[i]]) != "casal2MPD") {
      stop(paste0("This function only works on a named list with elements of class = 'casal2MPD'"))
    }
    this_abundance = get_composition_mean_bin(model[[i]])
    if(!is.null(this_abundance)) {
      this_abundance$model_label = run_labs[i]
      full_DF = rbind(full_DF, this_abundance);
    }
  }
  return(full_DF)
  invisible()

}
#' @title get_aggregated_composition_values
#'
#' @description
#' An accessor function that returns a data frame of aggregrated composition
#' @author Craig Marsh
#' @param model <casal2MPD, casal2TAB, list> object that are generated from one of the extract() functions. If list then we expect multiple mpd runs (should be a named list )
#' @return data frame with aggregated observed and expected over all years for each observation
#' @rdname get_aggregated_composition_values
#' @export get_aggregated_composition_values

"get_aggregated_composition_values" <- function(model) {
  UseMethod("get_aggregated_composition_values", model)
}



#' @rdname get_aggregated_composition_values
#' @method get_aggregated_composition_values casal2MPD
#' @export
get_aggregated_composition_values.casal2MPD <- function(model) {
  comp_df = get_composition_observations(model)
  comp_df$number_observed = comp_df$observed * comp_df$adjusted_error
  comp_df$number_expected = comp_df$expected * comp_df$adjusted_error

  is_age = grepl(comp_df$observation_type, pattern = "_age")
  comp_df$is_age = is_age
  obs_label = unique(comp_df$observation_label)
  full_summarised_df = NULL
  for(obs_ndx in 1:length(obs_label)) {
    this_obs = comp_df %>% filter(observation_label == obs_label[obs_ndx])

    if(all(this_obs$is_age)) {
      # age observation
      summarise_obs = this_obs %>% group_by(age, category) %>% summarise(aggregated_observed = sum(number_observed), aggregrated_expected = sum(number_expected))
    } else {
      # length observation
      summarise_obs = this_obs %>% group_by(length, category) %>% summarise(aggregated_observed = sum(number_observed), aggregrated_expected = sum(number_expected))
    }
    summarise_obs$observation_label = obs_label[obs_ndx]
    full_summarised_df = rbind(full_summarised_df, summarise_obs)
  }
  return(full_summarised_df)
}

#' @rdname get_aggregated_composition_values
#' @method get_aggregated_composition_values list
#' @export
get_aggregated_composition_values.list <- function(model) {
  run_labs = names(model)
  full_DF = NULL
  ## iterate over the models
  for(i in 1:length(model)) {
    if(class(model[[i]]) != "casal2MPD") {
      stop(paste0("This function only works on a named list with elements of class = 'casal2MPD'"))
    }
    this_aggregated_LF = get_aggregated_composition_values(model[[i]])
    if(!is.null(this_aggregated_LF)) {
      this_aggregated_LF$model_label = run_labs[i]
      full_DF = rbind(full_DF, this_aggregated_LF);
    }
  }
  return(full_DF)
}

#' @title get_aggregated_composition_values_by_blocks
#'
#' @description
#' An accessor function that returns a data frame of aggregrated composition
#' @author Craig Marsh
#' @param model <casal2MPD, casal2TAB, list> object that are generated from one of the extract() functions. If list then we expect multiple mpd runs (should be a named list )
#' @param n_year_blocks integer specifying the number of years in a time-block to aggregate over
#' @return data frame with aggregated observed and expected over all years for each observation
#' @rdname get_aggregated_composition_values_by_blocks
#' @export get_aggregated_composition_values_by_blocks

"get_aggregated_composition_values_by_blocks" <- function(model, n_year_blocks = 5) {
  UseMethod("get_aggregated_composition_values_by_blocks", model, n_year_blocks)
}

#' @rdname get_aggregated_composition_values_by_blocks
#' @method get_aggregated_composition_values_by_blocks casal2MPD
#' @export
get_aggregated_composition_values_by_blocks.casal2MPD <- function(model, n_year_blocks = 5) {
  comp_df = get_composition_observations(model)
  comp_df$number_observed = comp_df$observed * comp_df$adjusted_error
  comp_df$number_expected = comp_df$expected * comp_df$adjusted_error
  year_range = range(comp_df$year)
  year_blocks = seq(from = year_range[1] - (year_range[1] %% n_year_blocks), to = year_range[2] + (n_year_blocks - (year_range[2] %% n_year_blocks)), by = n_year_blocks)
  year_block_label = paste0(year_blocks[1:(length(year_blocks) - 1)], "-", year_blocks[2:(length(year_blocks))])
  comp_df$year_blocks = cut(comp_df$year, breaks = year_blocks, labels = year_block_label)
  is_age = grepl(comp_df$observation_type, pattern = "_age")
  comp_df$is_age = is_age
  obs_label = unique(comp_df$observation_label)
  full_summarised_df = NULL
  for(obs_ndx in 1:length(obs_label)) {
    this_obs = comp_df %>% filter(observation_label == obs_label[obs_ndx])

    if(all(this_obs$is_age)) {
      # age observation
      summarise_obs = this_obs %>% group_by(age, category, year_blocks) %>% summarise(aggregated_observed = sum(number_observed), aggregrated_expected = sum(number_expected))
    } else {
      # length observation
      summarise_obs = this_obs %>% group_by(length, category, year_blocks) %>% summarise(aggregated_observed = sum(number_observed), aggregrated_expected = sum(number_expected))
    }
    summarise_obs$observation_label = obs_label[obs_ndx]
    full_summarised_df = rbind(full_summarised_df, summarise_obs)
  }
  return(full_summarised_df)
}
#' @rdname get_aggregated_composition_values_by_blocks
#' @method get_aggregated_composition_values_by_blocks list
#' @export
get_aggregated_composition_values_by_blocks.list <- function(model, n_year_blocks = 5) {
  run_labs = names(model)
  full_DF = NULL
  ## iterate over the models
  for(i in 1:length(model)) {
    if(class(model[[i]]) != "casal2MPD") {
      stop(paste0("This function only works on a named list with elements of class = 'casal2MPD'"))
    }
    this_aggregated_LF = get_aggregated_composition_values_by_blocks(model[[i]], n_year_blocks)
    if(!is.null(this_aggregated_LF)) {
      this_aggregated_LF$model_label = run_labs[i]
      full_DF = rbind(full_DF, this_aggregated_LF);
    }
  }
  return(full_DF)
}
