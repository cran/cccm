#' Crossed Classification Credibility Model.
#'
#' Estimation of premium credibility for Crossed Classification Credibility Model.
#' In this model an insurance portfolio is subdivided by two qualitative risk factors, modeled in symmetrical way.
#' Especially this model presents an alternative way when data is not classifiable in a hierarchical manner and to determine main effects of both risk factors.
#' Also this model more useful to calculate co-effect both risk factors. Dannenburg et al., (1995, ISBN:90-802117-3-7)
#'
#' @author Muhlis Ozdemir \email{muhlisozdemir@gazi.edu.tr} Seda Tugce Altan \email{stugce.altan@gazi.edu.tr} Meral Ebegil \email{mdemirel@gazi.edu.tr}
#' @docType package
#' @name cccm
#'
#' @examples
#' raw_data <- debt
#'
#' categorical_columns = c(1,2)
#'
#' weights_column = 3
#'
#' debt_column = 4
#'
#' calculate_generalMean(raw_data, categorical_columns, weights_column, debt_column)
#'
#' calculate_variance_and_std(raw_data, categorical_columns, weights_column, debt_column)
#'
#' calculate_group_averages_matrix(raw_data, categorical_columns, weights_column, debt_column)
#'
#' calculate_weights_of_obs_matrix(raw_data, categorical_columns, weights_column, debt_column)
#'
#' calculate_varianceComponents(raw_data, categorical_columns, weights_column, debt_column)
#'
#' estimate_credibility(raw_data, categorical_columns, weights_column, debt_column)
"_PACKAGE"
