#' Variance and Standard Deviation
#'
#' @param raw_data a data set of credibility.
#' @param categorical_columns categorical column of data set.
#' @param weights_column weights column of data set.
#' @param debt_column credit dept column of data set.
#'
#' @return variance and sd.
#' @export
#'
#' @examples
#' raw_data <- debt
#'
#' categorical_columns <- c(1,2)
#'
#' weights_column <- 3
#'
#' debt_column <- 4
#'
#' calculate_variance_and_std(raw_data, categorical_columns, weights_column, debt_column)

calculate_variance_and_std = function(raw_data, categorical_columns, weights_column, debt_column){
  prepared_data = set_data(raw_data, categorical_columns, weights_column, debt_column)
  weights_of_obs_matrix=calculate_weights_of_obs_matrix(raw_data, categorical_columns, weights_column, debt_column)
  return(c(variance = sum(prepared_data$variance_column)/(sum(weights_of_obs_matrix) - 1),
           std = sqrt(sum(prepared_data$variance_column)/(sum(weights_of_obs_matrix) - 1))))
}
