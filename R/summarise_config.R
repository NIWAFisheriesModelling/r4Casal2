#' @title summarise_config
#' @description
#' utility function that reads in config.csl2 file and summarises a model in table formats and plots
#'
#' @author Craig Marsh
#' @param config_dir path directory
#' @param config_file the starting config that starts a Casal2 model default config.csl2, but can be overridden with casal2 -c my_config. in the later case it should be the my_config name
#' @importFrom Casal2 extract.csl2.file
#' @return
#' @examples
#' \dontrun{
#' library(r4Casal2)
#' library(ggplot2)
#' config_dir = system.file("extdata", "TestModelComplex", package = "r4Casal2", mustWork = TRUE)
#' summary = summarise_config(config_dir, config_file = "config.csl2", quiet = T)
#' names(summary)
#' # visualise observations in model
#' ggplot(summary$obs_year_df, aes(x = year, y = observation, col = observation, size = active)) +
#' geom_point() +
#' guides(colour = "none", size = "none")
#' }
#' @rdname summarise_config
#' @export summarise_config
summarise_config <- function(config_dir = "", config_file = "config.csl2", quiet = T, fileEncoding = "") {
  file = scan(file = file.path(config_dir, config_file), what = "", sep = "\n", quiet = T)
  ## deal with comments
  file <- file[substring(file, 1, 1) != "#"]
  index1 <- ifelse(substring(file, 1, 2) == "/*", 1:length(file), 0)
  index2 <- ifelse(substring(file, 1, 2) == "*/", 1:length(file), 0)
  index1 <- index1[index1 != 0]
  index2 <- index2[index2 != 0]
  if (length(index1) != length(index2))
    stop(paste("Error in the file ", filename, ". Cannot find a matching '/*' or '*/'", sep = ""))
  if (length(index1) > 0 || length(index2) > 0) {
    index <- unlist(apply(cbind(index1, index2), 1, function(x) seq(x[1], x[2])))
    file <- file[!1:length(file) %in% index]
  }
  file <- ifelse(regexpr("#", file) > 0, substring(file, 1, regexpr("#", file) - 1), file)
  file <- file[file != ""]
  ## get includes assumes file names have \" \"
  file = substring(file, first = 10) # '!include ' is 10 characters
  ## check for '"'
  ndx = regexpr("\"", file) > 0
  if(any(ndx)) {
    for(i in 1:length(file)) {
      if(ndx[i])
        file[i] = substring(file[i], first = 2, last = nchar(file[i]) - 1)
    }
  }
  if(!quiet)
    cat("found the following files to read in ", file, "\n")
  ## now read in these config files
  model_block = list()
  observation_blocks = list()
  process_blocks = list()
  estimate_blocks = list()
  time_steps_list = list()
  categories_list = list()
  age_length_list = list()
  length_weight_list = list();
  category_labels = NULL
  observation_labels = NULL
  category_age_lengths = NULL;
  category_format = NULL
  model_years = NULL
  model_length_bins = NULL
  ages = NULL
  time_steps = NULL
  for(i in 1:length(file)) {
    if(!file.exists(file.path(config_dir, file[i])))
      cat("couldn't find file = ", file.path(config_dir, file[i]))
    this_file = tryCatch(extract.csl2.file(path = config_dir, file = file[i], quiet = quiet), error=function(e) {e}, warning=function(w) {w})
    if(inherits(this_file, "error") | inherits(this_file, "warning")) {
      cat("failed to readin the following file ", file[i], " so skipping it.\n\nthe error\n",this_file$message,"\n")
      next
    }
    blocks = (sapply(strsplit(names(this_file), split = "\\["), "[", 1))
    labels = (sapply(strsplit(names(this_file), split = "\\["), "[", 2))
    labels = (sapply(strsplit(labels, split = "\\]"), "[", 1))
    for(j in 1:length(this_file)) {
      if(tolower(blocks[j]) == "model") {
        model_block[[labels[j]]] = this_file[[j]]
        model_years = as.numeric(this_file[[j]]$start_year$value):as.numeric(this_file[[j]]$final_year$value)
        ages = as.numeric(this_file[[j]]$min_age$value):as.numeric(this_file[[j]]$max_age$value)
        time_steps = this_file[[j]]$time_steps$value
        if(!is.null(this_file[[j]]$length_bins)) {
          for(k in 1:length(this_file[[j]]$length_bins$value))
            model_length_bins = c(model_length_bins, expand_shorthand_syntax(syntax = this_file[[j]]$length_bins$value[k]))
        }

      } else if(tolower(blocks[j]) == "time_step") {
        time_steps_list[[labels[j]]] = this_file[[j]]$processes$value
      } else if(tolower(blocks[j]) == "categories") {
        categories_list[[labels[j]]] = this_file[[j]]
        for(k in 1:length(this_file[[j]]$names$value)) {
          category_labels = c(category_labels, expand_category_block(categories = this_file[[j]]$names$value[k]))
        }
        for(k in 1:length(this_file[[j]]$age_lengths$value)) {
          category_age_lengths = c(category_age_lengths, expand_shorthand_syntax(syntax = this_file[[j]]$age_lengths$value[k]))
        }
        if(!is.null(this_file[[j]]$format))
          category_format = this_file[[j]]$format$value
      } else if(tolower(blocks[j]) == "age_length") {
        age_length_list[[labels[j]]] = this_file[[j]]
      } else if(tolower(blocks[j]) == "process") {
        process_blocks[[labels[j]]] = this_file[[j]]
      } else if(tolower(blocks[j]) == "length_weight") {
        length_weight_list[[labels[j]]] = this_file[[j]]
      } else if(tolower(blocks[j]) == "estimate") {
        estimate_blocks[[labels[j]]] = this_file[[j]]
      } else if(tolower(blocks[j]) == "observation") {
        observation_blocks[[labels[j]]] = this_file[[j]]
        observation_labels = c(observation_labels, labels[j])
      }
    }
  }
  ## now summarise...
  ## categories
  category_df = full_category_df = NULL
  age_length_time_step_growth = NULL
  for(i in 1:length(category_labels)) {
    ## get process type
    this_category = categories_list[[category_labels[i]]]
    ## get age-length label type
    this_age_length = age_length_list[[category_age_lengths[i]]]
    ## get length-weight label type
    this_length_weight = length_weight_list[[this_age_length$length_weight$value]]
    distribution = "normal" # default
    if(!is.null(this_age_length$distribution))
      distribution = this_age_length$distribution
    this_cat_df = data.frame("Category" = category_labels[i], "AgeLength" = category_age_lengths[i], "LengthWeight" = this_age_length$length_weight$value, "Distribution" = distribution)

    this_cat_full_df = data.frame("Category" = category_labels[i], "AgeLength" = paste0(category_age_lengths[i], " (",this_age_length$type$value, ")"),
                                  "LengthWeight" = paste0(this_age_length$length_weight$value, " (",this_length_weight$type$value, ")"), "Distribution" = distribution)

    category_df = rbind(category_df, this_cat_df)
    full_category_df = rbind(full_category_df, this_cat_full_df)
    if(is.null(age_length_time_step_growth))
      age_length_time_step_growth = rbind(age_length_time_step_growth, data.frame(AgeLength = category_age_lengths[i], time_step_proportions = this_age_length$time_step_proportions$value))
    if(!category_age_lengths[i] %in% age_length_time_step_growth$AgeLength)
      age_length_time_step_growth = rbind(age_length_time_step_growth, data.frame(AgeLength = category_age_lengths[i], time_step_proportions = this_age_length$time_step_proportions$value))
  }
  ## Observations
  obs_year_df = NULL
  if(!is.null(observation_labels)) {
    for(i in 1:length(observation_labels)) {
      this_obs = observation_blocks[[observation_labels[i]]]
      years = NULL
      for(y in 1:length(this_obs$years$value)) {
        years = c(years, expand_shorthand_syntax(this_obs$years$value[y]))
      }
      active_ndx = model_years %in% years
      obs_year_df = rbind(obs_year_df, data.frame(year = model_years, observation = observation_labels[i], type= this_obs$type$value, active = ifelse(active_ndx, 1, NA)))
    }
  }

  ## these are converted to dfs for easy conversion to tables.
  ## Do this last so we can bring growth props and M props
  time_step_df = NULL
  time_step_df_just_lab = NULL
  for(i in 1:length(time_steps)) {
    ## get process type
    proceses = time_steps_list[[time_steps[i]]]
    process_type = vector();
    df_entry = vector()
    for(j in 1:length(proceses)) {
      process_type[j] = process_blocks[[proceses[j]]]$type$value
      df_entry[j] = paste0(proceses[j], " (", process_type[j], ")")
    }
    this_step = data.frame(time_step = time_steps[i], processes = paste(df_entry, collapse = ", "))
    time_step_df = rbind(time_step_df, this_step)
    time_step_df_just_lab = rbind(time_step_df_just_lab, data.frame(time_step = time_steps[i], processes = paste(proceses, collapse = ", ")))
  }
  age_length_labs = unique(age_length_time_step_growth$AgeLength)
  for(i in 1:length(age_length_labs)) {
    this_growth = age_length_time_step_growth[which(age_length_time_step_growth$AgeLength == age_length_labs[i]),]
    time_step_df = cbind(time_step_df, this_growth$time_step_proportions)
    time_step_df_just_lab = cbind(time_step_df_just_lab, this_growth$time_step_proportions)
  }
  age_length_labs = paste0(age_length_labs, " (assumed growth)")
  colnames(time_step_df_just_lab) = c("Time-step", "Processes", age_length_labs)
  colnames(time_step_df) = c("Time-step", "Processes (type)", age_length_labs)


  ## Catch and M
  M_by_category = NULL
  M_time_steps = NULL
  catch_df = NULL
  method_df = NULL
  for(i in 1:length(process_blocks)) {
    this_process = process_blocks[[i]]
    if(this_process$type$value == "mortality_instantaneous") {
      m = expand_shorthand_syntax(this_process$m$value)
      categories = NULL
      for(j in 1:length(this_process$categories$value))
        categories = c(categories, expand_category_shorthand(shorthand_categories = this_process$categories$value[j], reference_categories=category_labels, category_format = category_format))
      selectivty = NULL
      for(j in 1:length(this_process$relative_m_by_age$value))
        selectivty = c(selectivty, expand_shorthand_syntax(this_process$relative_m_by_age$value[j]))
      M_by_category = rbind(data.frame(process = names(process_blocks)[i], category = categories, M = m, relative_M = selectivty))

      time_prop = NULL
      if(is.null(this_process$time_step_proportions$value)) {
        time_prop = rep(1, nrow(time_step_df))
      } else {
        time_prop = this_process$time_step_proportions$value
      }


      M_time_steps = rbind(M_time_steps, data.frame(process = names(process_blocks)[i], time_step_proportions = time_prop))
      this_catch = Reduce(cbind, this_process$Table$catches)
      class(this_catch) = "numeric"
      colnames(this_catch) = names(this_process$Table$catches)
      this_catch = as.data.frame(this_catch)
      this_catch$process = names(process_blocks)[i]
      catch_df = rbind(catch_df, this_catch)

      this_method = Reduce(cbind, this_process$Table$method)
      colnames(this_method) = names(this_process$Table$method)
      this_method = as.data.frame(this_method, stringsAsFactors = F)
      for(i in 1:nrow(this_method))
        this_method$category[i] = paste(expand_category_shorthand(this_method$category[i] , category_labels, category_format = category_format), collapse = ",")

      this_method$process = names(process_blocks)[i]
      method_df = rbind(method_df, this_method)
    } else if(this_process$type$value == "mortality_instantaneous_retained") {

    }
  }

  ## Summarise @estimate blocks
  ## priors type, bounds, starting values
  estimate_df = NULL
  if(length(estimate_blocks) > 0) {
    for(i in 1:length(estimate_blocks)) {
      this_estimate = estimate_blocks[[i]]
      label = names(estimate_blocks)[i]
      parameter = this_estimate$parameter$value
      type = this_estimate$type$value
      lower_bound = this_estimate$lower_bound$value
      upper_bound = this_estimate$upper_bound$value
      same = this_estimate$save$value
      if(!is.null(same)) {
        if(length(same) > 1) {
          same = paste(same, collapse = ", ");
        }
      } else {
        same = "-";
      }
      if(length(lower_bound) > 1) {
        lower_bound = paste(lower_bound, collapse = " ");
      }
      if(length(upper_bound) > 1) {
        upper_bound = paste(upper_bound, collapse = " ");
      }

      this_df = data.frame(label = label, same = same, prior = type, lower_bound = lower_bound, upper_bound = upper_bound)
      estimate_df = rbind(estimate_df, this_df)
    }
  }

  return(list(category_df = category_df, estimate_df = estimate_df, full_category_df = full_category_df, method_df = method_df, catch_df = catch_df, time_step_df = time_step_df, time_step_df_just_lab = time_step_df_just_lab, obs_year_df = obs_year_df, model_years = model_years, model_ages = ages, model_length_bins = model_length_bins, M_by_category = M_by_category))
}
