get_selectivities <-  function(model) {
  # can be -r or -r -i
  multiple_iterations_in_a_report = FALSE
  complete_df = NULL
  reports_labels = reformat_default_labels(names(model))
  for(i in 1:length(model)) {
    ## skip the header
    if (reports_labels[i] == "header")
      next;
    this_report = model[[i]]
    if(any(names(this_report) == "type")) {
      if(this_report$type != "selectivity") {
        next;
      }
      ## add it to full df
      this_selectivity = data.frame(selectivity = as.numeric(this_report$Values), bin = names(this_report$Values))
      this_selectivity$selectivity_label = reports_labels[i]
      this_selectivity$par_set = 1 ## so compatible with -i runs
      this_selectivity$sub_type = this_report$sub_type # used to identify logistic_producing for example
      ## check col compatibility some reports will print residuals and some wont
      if(!is.null(complete_df)) {
        if(any(!colnames(complete_df) %in% colnames(this_selectivity))) {
          drop_cols = which(!colnames(complete_df) %in% colnames(this_selectivity))
          complete_df = complete_df[, -drop_cols]
        }
        if(any(!colnames(this_selectivity) %in% colnames(complete_df))) {
          drop_cols = which(!colnames(this_selectivity) %in% colnames(complete_df))
          this_selectivity = this_selectivity[, -drop_cols]
        }
      }
      complete_df = rbind(complete_df, this_selectivity)
      next;
    } else {
      multiple_iterations_in_a_report <- TRUE
      if (this_report[[1]]$type != "selectivity") {
        next;
      }
      n_runs = length(this_report)
      iter_labs = names(this_report)
      for(dash_i in 1:n_runs) {
        ## add it to full df
        this_selectivity = data.frame(selectivity = as.numeric(this_report[[dash_i]]$Values), bin = names(this_report[[dash_i]]$Values))
        this_selectivity$selectivity_label = reports_labels[i]
        this_selectivity$par_set = iter_labs[dash_i]
        this_selectivity$sub_type = this_report[[dash_i]]$sub_type
        ## check col compatibility some reports will print residuals and some wont
        if(!is.null(complete_df)) {
          if(any(!colnames(complete_df) %in% colnames(this_selectivity))) {
            drop_cols = which(!colnames(complete_df) %in% colnames(this_selectivity))
            complete_df = complete_df[, -drop_cols]
          }
          if(any(!colnames(this_selectivity) %in% colnames(complete_df))) {
            drop_cols = which(!colnames(this_selectivity) %in% colnames(complete_df))
            this_selectivity = this_selectivity[, -drop_cols]
          }
        }
        complete_df = rbind(complete_df, this_selectivity)
      }
    }
  }
  complete_df$bin = as.numeric(complete_df$bin)
  return(complete_df)
  # invisible()
}