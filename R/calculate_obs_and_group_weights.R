#' Repeats of observations
#'
#' @param raw_data a data set of credibility.
#' @param categorical_columns categorical column of data set.
#' @param weights_column weights column of data set.
#' @param debt_column credit dept column of data set.
#'
#' @return This function returns categorical group sizes.
#' @export
#'
#' @import dplyr
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
#' calculate_obs_and_group_weights(raw_data, categorical_columns, weights_column, debt_column)

calculate_obs_and_group_weights = function(raw_data, categorical_columns, weights_column, debt_column){
  prepared_data = set_data(raw_data, categorical_columns, weights_column, debt_column)
  column_names = save_names(raw_data, categorical_columns)$column_names
  repetition_of_obs_and_group_weights = prepared_data %>%
    group_by(prepared_data[categorical_columns]) %>%
    summarise(n = n(),
              group_weights = sum(.data[[column_names[weights_column]]]), .groups = 'drop')
  return(repetition_of_obs_and_group_weights)
}
