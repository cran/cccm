#' Group Averages Matrix
#'
#' @param raw_data a data set of credibility.
#' @param categorical_columns categorical column of data set.
#' @param weights_column weights column of data set.
#' @param debt_column credit dept column of data set.
#'
#' @return group averages matrix
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
#' calculate_group_averages_matrix(raw_data, categorical_columns, weights_column, debt_column)

calculate_group_averages_matrix = function(raw_data, categorical_columns, weights_column, debt_column){
  prepared_data = set_data(raw_data, categorical_columns, weights_column, debt_column)
  group_averages_matrix = matrix(unique(prepared_data$group_average_weights),
                                 nrow = length(save_names(raw_data, categorical_columns)$names1),
                                 ncol = length(save_names(raw_data, categorical_columns)$names2),
                                 byrow = T)

  rownames(group_averages_matrix) = save_names(raw_data, categorical_columns)$names1
  colnames(group_averages_matrix) = save_names(raw_data, categorical_columns)$names2
  return(group_averages_matrix)
}
