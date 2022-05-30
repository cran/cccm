#' The Credibility Premium Estimates
#'
#' @param raw_data a data set of credibility.
#' @param categorical_columns categorical column of data set.
#' @param weights_column weights column of data set.
#' @param debt_column credit dept column of data set.
#'
#' @return returns premium estimation of credibility.
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
#' estimate_credibility(raw_data, categorical_columns, weights_column, debt_column)

estimate_credibility = function(raw_data, categorical_columns, weights_column, debt_column){
  name_list = save_names(raw_data, categorical_columns)

  prepared_data = set_data(raw_data, categorical_columns, weights_column, debt_column)

  repetition_of_obs_and_group_weights = calculate_obs_and_group_weights(raw_data, categorical_columns, weights_column, debt_column)

  weights_of_obs_matrix = calculate_weights_of_obs_matrix(raw_data, categorical_columns, weights_column, debt_column)

  group_averages_matrix = calculate_group_averages_matrix(raw_data, categorical_columns, weights_column, debt_column)

  variance_and_std = calculate_variance_and_std(raw_data, categorical_columns, weights_column, debt_column)

  gen_mean = sum(group_averages_matrix * weights_of_obs_matrix) / sum(weights_of_obs_matrix)


  xwjw_column_sum = colSums(weights_of_obs_matrix * group_averages_matrix) / colSums(weights_of_obs_matrix)
  xiww_row_sum = rowSums(weights_of_obs_matrix * group_averages_matrix) / rowSums(weights_of_obs_matrix)

  wi_row_sum = rowSums(weights_of_obs_matrix)
  wj_column_sum = colSums(weights_of_obs_matrix)

  # Eq 1
  Eq1_matrix = div_matrix_cols_with_vector(((col_diff_matrix_with_vector(group_averages_matrix, xiww_row_sum)^2)*weights_of_obs_matrix),wi_row_sum)
  rownames(Eq1_matrix) = name_list$names1
  colnames(Eq1_matrix) = name_list$names2
  Eq1_row_sum = rowSums(Eq1_matrix)
  Eq1_row_operations = Eq1_row_sum - variance_and_std[1] * (length(name_list$names2) - 1) / wi_row_sum
  Eq1_right_hand_side = sum(Eq1_row_operations) / length(name_list$names1)
  Eq1_weighted_matrix = (weights_of_obs_matrix / wi_row_sum)^2
  Eq1_constants = 1-sum(Eq1_weighted_matrix) / length(name_list$names1)

  # Eq 2
  Eq2_matrix = div_matrix_rows_with_vector(((row_diff_matrix_with_vector(group_averages_matrix, xwjw_column_sum)^2)*weights_of_obs_matrix),wj_column_sum)
  rownames(Eq2_matrix) = name_list$names1
  colnames(Eq2_matrix) = name_list$names2
  Eq2_column_sum = colSums(Eq2_matrix)
  Eq2_row_operations = Eq2_column_sum - variance_and_std[1] * (length(name_list$names1) - 1) / wj_column_sum
  Eq2_right_hand_side = sum(Eq2_row_operations) / length(name_list$names2)
  Eq2_weighted_matrix = div_matrix_rows_with_vector(weights_of_obs_matrix, wj_column_sum)^2
  rownames(Eq2_weighted_matrix) = name_list$names1
  colnames(Eq2_weighted_matrix) = name_list$names2
  Eq2_constants = 1-sum(Eq2_weighted_matrix) / length(name_list$names2)

  # Eq 3
  Eq3_matrix = (((group_averages_matrix - gen_mean)^2)*weights_of_obs_matrix) / sum(weights_of_obs_matrix)
  Eq3_column_sum = colSums(Eq3_matrix)
  Eq3_right_hand_side = sum(Eq3_column_sum) - variance_and_std[1] * (length(name_list$names1)*length(name_list$names2) - 1) / sum(weights_of_obs_matrix)
  Eq3_first_param_value = 1 - sum((wi_row_sum / sum(weights_of_obs_matrix))^2)
  Eq3_second_param_value = 1 - sum((wj_column_sum / sum(weights_of_obs_matrix))^2)
  Eq3_third_param_value = 1 - sum((weights_of_obs_matrix / sum(weights_of_obs_matrix))^2)

  # Solving Linear Eq.
  all_Eq_matrix = matrix(0, 3,3)
  all_Eq_matrix[1,2] = all_Eq_matrix[1,3] = Eq1_constants
  all_Eq_matrix[2,1] = all_Eq_matrix[2,3] = Eq2_constants
  all_Eq_matrix[3,] = c(Eq3_first_param_value,
                        Eq3_second_param_value,
                        Eq3_third_param_value)

  right_hand_side_constants = matrix(c(Eq1_right_hand_side,
                                       Eq2_right_hand_side,
                                       Eq3_right_hand_side),
                                     byrow = T)

  variance_components = solve(all_Eq_matrix) %*% right_hand_side_constants

  if (any(variance_components < 0)) {
    warning("Risk Factors you are about to use not suitable for cross classification credibility model. You may change your data or risk factors.")
  }

  # Credibility Factor Calculations
  variance_components_12 = variance_components[3]
  credibility_factor_values_for_cells = variance_components_12 / (variance_components_12 + (variance_and_std[1]/weights_of_obs_matrix))
  rowsums_of_credibility_factor_values_for_cells = rowSums(credibility_factor_values_for_cells)
  colsums_of_credibility_factor_values_for_cells = colSums(credibility_factor_values_for_cells)
  credibility_factor_values_for_rows = variance_components[1] / (variance_components[1] + (variance_components_12/rowsums_of_credibility_factor_values_for_cells))
  credibility_factor_values_for_cols = variance_components[2] / (variance_components[2] + (variance_components_12/colsums_of_credibility_factor_values_for_cells))
  cell_c1 = credibility_factor_values_for_rows * (xiww_row_sum - gen_mean)
  cell_c2 = credibility_factor_values_for_cols * (xwjw_column_sum - gen_mean)
  cell_d1 = credibility_factor_values_for_rows * credibility_factor_values_for_cells/rowsums_of_credibility_factor_values_for_cells
  cell_d2 = div_matrix_rows_with_vector(mult_matrix_cols_with_vector(credibility_factor_values_for_cells, credibility_factor_values_for_cols) ,colsums_of_credibility_factor_values_for_cells)
  transpose_of_cell_d2 = t(cell_d2)
  cell_e1 = solve(diag(nrow(cell_d1 %*% transpose_of_cell_d2)) - cell_d1 %*% transpose_of_cell_d2) %*% (cell_c1 - cell_d1 %*% cell_c2)
  cell_e2 = solve(diag(nrow(transpose_of_cell_d2 %*% cell_d1)) - transpose_of_cell_d2 %*% cell_d1) %*% (cell_c2 - transpose_of_cell_d2 %*% cell_c1)
  diff_mean_from_group_averages = group_averages_matrix-gen_mean
  E1_vector_diffs = col_diff_matrix_with_vector(diff_mean_from_group_averages, cell_e1)
  E2_vector_diffs = row_diff_matrix_with_vector(E1_vector_diffs, cell_e2)

  # Credibility_factor_values_for_cells * E2_vector_diffs
  eij_values_for_cells = E2_vector_diffs * credibility_factor_values_for_cells
  yizw_values = row_diff_matrix_with_vector(group_averages_matrix, cell_e2) * credibility_factor_values_for_cells
  sum_yizw = rowSums(yizw_values) / rowsums_of_credibility_factor_values_for_cells
  yzjw_values = col_diff_matrix_with_vector(group_averages_matrix, cell_e1) * credibility_factor_values_for_cells
  sum_yzjw = colSums(yzjw_values) / colsums_of_credibility_factor_values_for_cells
  credibility_predictions = gen_mean +
    credibility_factor_values_for_cells*(group_averages_matrix - gen_mean) +
    (1-credibility_factor_values_for_cells)*(credibility_factor_values_for_rows*(sum_yizw - gen_mean)) +
    mult_matrix_cols_with_vector((1-credibility_factor_values_for_cells), (credibility_factor_values_for_cols * (sum_yzjw - gen_mean)))

  results = list()
  results$general_mean = gen_mean
  results$variance_and_std = variance_and_std

  results$weights_of_obs_matrix = weights_of_obs_matrix
  results$group_averages_matrix = group_averages_matrix

  results$variance_components = variance_components

  results$credibility_factor_values = credibility_factor_values_for_cells

  results$first_risk_factor_Zi = credibility_factor_values_for_rows
  results$second_risk_factor_Zj = credibility_factor_values_for_cols

  results$Eij_values_for_cells = eij_values_for_cells

  results$first_risk_factor_Ei = cell_e1
  results$second_risk_factor_Ej = cell_e2

  results$adj_weighted_avg_for_first_risk_factor = yizw_values
  results$adj_weighted_avg_for_second_risk_factor = yzjw_values

  results$credibility_predictions = credibility_predictions

  return(results)
}
