#' Weights of observation matrix
#'
#' @param raw_data a data set of credibility.
#' @param categorical_columns categorical column of data set.
#' @param weights_column weights column of data set.
#' @param debt_column credit dept column of data set.
#'
#' @return Weights of observation matrix.
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
#' calculate_weights_of_obs_matrix(raw_data, categorical_columns, weights_column, debt_column)

calculate_weights_of_obs_matrix = function(raw_data, categorical_columns, weights_column, debt_column){
  name_list <- save_names(raw_data, categorical_columns)
  prepared_data = set_data(raw_data, categorical_columns, weights_column, debt_column)
  repetition_of_obs_and_group_weights = calculate_obs_and_group_weights(raw_data, categorical_columns, weights_column, debt_column)
  obs_matrix = matrix(repetition_of_obs_and_group_weights$group_weights,
                                 nrow = length(name_list$names1),
                                 ncol = length(name_list$names2),
                                 byrow = T)

  rownames(obs_matrix) = name_list$names1
  colnames(obs_matrix) = name_list$names2
  return(obs_matrix)
}
