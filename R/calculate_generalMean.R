#' General Mean
#'
#' @param raw_data a data set of credibility.
#' @param categorical_columns categorical column of data set.
#' @param weights_column weights column of data set.
#' @param debt_column credit dept column of data set.
#'
#' @return general mean
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
#' calculate_generalMean(raw_data, categorical_columns, weights_column, debt_column)

calculate_generalMean = function(raw_data, categorical_columns, weights_column, debt_column){
  weights_of_obs_matrix = calculate_weights_of_obs_matrix(raw_data, categorical_columns, weights_column, debt_column)

  group_averages_matrix = calculate_group_averages_matrix(raw_data, categorical_columns, weights_column, debt_column)

  return(sum(group_averages_matrix * weights_of_obs_matrix) / sum(weights_of_obs_matrix))
}
