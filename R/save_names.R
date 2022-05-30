#' Get names
#'
#' @param raw_data a data set of credibility.
#' @param categorical_columns categorical column of data set.
#'
#' @return returns categorical variables' unique values and column names of data set.
#' @export
#'
#' @examples
#' raw_data <- debt
#'
#' categorical_columns <- c(1,2)
#'
#' save_names(raw_data, categorical_columns)

save_names = function(raw_data, categorical_columns){
  cat_names_list = list(levels(raw_data[,categorical_columns[1]]), levels(raw_data[,categorical_columns[2]]), names(raw_data))
  names(cat_names_list) = c("names1", "names2", "column_names")
  return(cat_names_list)
}
