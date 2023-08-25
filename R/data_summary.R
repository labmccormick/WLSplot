#'Get the mean std and se from a set of data with different variables
#'
#' @param in_data dataframe to be passed in (long format)
#' @param varname the variable to be tested
#' @param group_names groupnames of the data
#' @return Function returns matrix.
#' @export data_summary

data_summary <- function(in_data, varname, groupnames){
  summary_func <- function(x, column_var){
    c(mean = mean(x[[column_var]], na.rm=TRUE),
      sd = sd(x[[column_var]], na.rm=TRUE),
      se = sd(x[[column_var]], na.rm=TRUE) / sqrt(length(x[[column_var]]))
    )
  }
  data_sum<-plyr::ddply(in_data, groupnames, .fun=summary_func,
                        varname)
  data_sum <- plyr::rename(data_sum, c("mean" = varname))
  return(data_sum)
}
