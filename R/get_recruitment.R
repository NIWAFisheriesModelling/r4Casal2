#' @title get_BH_recruitment
#'
#' @description
#' An accessor function that returns a data frame from a Casal2 model output of process type recruitment
#'
#' @author Craig Marsh
#' @param model <casal2MPD, casal2TAB, list> object that are generated from one of the extract.mpd() and extract.tabular() functions.
#' @return A data frame from Casal2 model output
#' @rdname get_BH_recruitment
#' @export get_BH_recruitment
#' @importFrom reshape2 melt


"get_BH_recruitment" <-
  function(model) {
    UseMethod("get_BH_recruitment", model)
  }

#'
#' @rdname get_BH_recruitment
#' @method get_BH_recruitment casal2MPD
#' @export
"get_BH_recruitment.casal2MPD" = function(model) {
  # can be -r or -r -i
  multiple_iterations_in_a_report = FALSE
  complete_df = NULL
  reports_labels = reformat_default_labels(names(model))
  for(i in 1:length(model)) {
    this_report = model[[i]]
    if(any(names(this_report) == "type")) {
      if(this_report$type != "process")
        next;
      if(this_report$type == "process" & this_report$sub_type != "recruitment_beverton_holt")
        next;
      ## only a single trajectory
      standardised_ycs = this_report$standardised_ycs
      ycs_values = this_report$ycs_values
      ycs_years = this_report$ycs_years
      Recruits = this_report$Recruits
      true_ycs = this_report$true_ycs

      full_df = data.frame(ycs_years = this_report$ycs_years,
                           standardised_ycs = this_report$standardised_ycs,
                           ycs_values = this_report$ycs_values,
                           recruits = this_report$Recruits,
                           true_ycs = this_report$true_ycs,
                           r0 = this_report$r0,
                           b0 = this_report$b0,
                           par_set = 1,
                           label = reports_labels[i])
      complete_df = rbind(complete_df, full_df)
    } else {
      if(this_report$'1'$type != "process")
        next;
      if(this_report$'1'$type == "process" & this_report$'1'$sub_type != "recruitment_beverton_holt")
        next;
      ## Multiple parameter inputs
      n_runs = length(this_report)
      for(dash_i in 1:n_runs) {
        ## only a single trajectory
        temp_df = data.frame(
          standardised_ycs = this_report[[dash_i]]$standardised_ycs,
          ycs_values = this_report[[dash_i]]$ycs_values,
          ycs_years = this_report[[dash_i]]$ycs_years,
          Recruits = this_report[[dash_i]]$Recruits,
          true_ycs = this_report[[dash_i]]$true_ycs,
          r0 = this_report[[dash_i]]$r0,
          b0 = this_report[[dash_i]]$b0,
          par_set = dash_i,
          label = reports_labels[i])
        complete_df = rbind(complete_df, temp_df)
      }
    }
  }
  return(complete_df)
  invisible()
}

#'
#' @rdname get_BH_recruitment
#' @method get_BH_recruitment list
#' @export
"get_BH_recruitment.list" = function(model) {
  run_labs = names(model)
  full_DF = NULL
  ## iterate over the models
  for(i in 1:length(model)) {

    if(class(model[[i]]) != "casal2MPD") {
      stop(paste0("This function only works on a named list with elements of class = 'casal2MPD'"))
    }
    this_dq = get_BH_recruitment(model[[i]])
    this_dq$model_label = run_labs[i]
    full_DF = rbind(full_DF, this_dq);
  }
  return(full_DF)
  invisible()
}

#'
#' @rdname get_BH_recruitment
#' @method get_BH_recruitment casal2TAB
#' @return A list frame from Casal2 model output. There is a non_multi_column_df which contains non year varying components such as B0, steepness etc and a multi_column_df which has ssb, recrutis, ycs etc

#' @export
"get_BH_recruitment.casal2TAB" = function(model) {
  cat("this can take a few minutes for large models and big mcmc chains. Please be patient :~) \n")

  reports_labels = reformat_default_labels(names(model))
  complete_df = NULL
  recruit_multi_col_df = recruit_df = NULL
  for(i in 1:length(model)) {
    this_report = model[[i]]
    if(this_report$type != "process") {
      next;
    }
    if(this_report$sub_type != "recruitment_beverton_holt") {
      next;
    }
    collabs = colnames(this_report$values)
    first_component = Reduce(c, lapply(strsplit(collabs, split = "[", fixed = T), FUN = function(x){x[1]}))
    components = unique(first_component)
    multi_column_df = non_multi_column_df = NULL
    non_multi_col_labs = multi_col_labs = "iteration"
    non_multi_column_df = cbind(non_multi_column_df)
    multi_column_df = cbind(multi_column_df)
    for(j in 1:length(components)) {
      col_ndx = grepl(collabs, pattern = components[j])
      if(sum(col_ndx, na.rm = T) == 1) {
        if(is.null(non_multi_column_df)) {
          non_multi_column_df = cbind(as.numeric(rownames(this_report$values)))
        }
        non_multi_column_df = cbind(non_multi_column_df, as.numeric(this_report$values[,col_ndx]))
        non_multi_col_labs = c(non_multi_col_labs, components[j])
      } else {
        long_format = suppressMessages({melt(as.matrix(this_report$values[,col_ndx]), variable.name = "colname", value.name = "values", factorsAsStrings = T)})
        colnames(long_format) =  c("iteration", "colname", "values")
        long_format$colname = as.character(long_format$colname)
        second_component = unlist(lapply(strsplit(long_format$colname, split = "[", fixed = T), FUN = function(x){x[2]}))
        second_component = substring(second_component, first = 1, last = nchar(second_component) - 1)
        long_format$years = second_component
        ## drop colname
        if(is.null(multi_column_df)) {
          long_format = long_format[, -which(colnames(long_format) %in% c("colname"))]
        } else {
          long_format = long_format[, -which(colnames(long_format) %in% c("colname", "iteration"))]
        }
        multi_col_labs = c(multi_col_labs, components[j], paste0(components[j],"_year"))
        temp_mat = as.matrix(long_format)
        class(temp_mat) = "as.numeric"
        multi_column_df = cbind(multi_column_df, temp_mat)

      }
    }
    colnames(multi_column_df) = multi_col_labs
    colnames(non_multi_column_df) = non_multi_col_labs
    multi_column_df = as.data.frame(multi_column_df)
    non_multi_column_df = as.data.frame(non_multi_column_df)
    non_multi_column_df$label = reports_labels[i]
    multi_column_df$label = reports_labels[i]

    recruit_multi_col_df = rbind(recruit_multi_col_df, multi_column_df)
    recruit_df = rbind(recruit_df, non_multi_column_df)
  }
  return(list(multi_column_df = recruit_multi_col_df, non_multi_column_df = recruit_df))
  invisible()
}

