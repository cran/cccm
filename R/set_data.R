#' Data prep
#'
#' @param raw_data a data set of credibility.
#' @param categorical_columns categorical column of data set.
#' @param weights_column weights column of data set.
#' @param debt_column credit debt column of data set.
#'
#' @return This function returns a tibble as prepared_data by using raw_data. Adds new columns to raw data as weighted_obs, group_average_weights, variance_column.
#' @importFrom rlang .data
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
#' prepared_data <- set_data(raw_data, categorical_columns, weights_column, debt_column)

set_data = function(raw_data, categorical_columns, weights_column, debt_column){
  control_data(raw_data)
  column_names = save_names(raw_data, categorical_columns)$column_names
  weighted_obs = raw_data[[column_names[weights_column]]] * raw_data[[column_names[debt_column]]]
  raw_data$weighted_obs = weighted_obs

  x = raw_data %>%
    group_by(raw_data[categorical_columns]) %>%
    mutate(y = sum(weighted_obs) / sum(.data[[column_names[weights_column]]]))


  group_average_weights = x$y
  raw_data$group_average_weights = group_average_weights

  raw_data = raw_data %>%
    group_by(raw_data[categorical_columns]) %>%
    mutate(variance_column = (.data[[column_names[debt_column]]] - group_average_weights)^2 * .data[[column_names[weights_column]]],
           .groups = 'drop')

  return(raw_data)
}
