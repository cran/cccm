#' Data checker
#'
#' Throws an error message if at least 2 features is not in categorical format.
#'
#' @param x a dataset.
#'
#' @return This function checks whether \code{dataset} has at least 2 features in categorical format.

control_data = function(x){
  flag = rep(0, ncol(x))
  for (i in seq_along(x)) {
    if (is.factor(x[,i]) == T) {
      flag[i] = 1
    }
  }
  if (sum(flag) < 1) {
    stop("2 features  should be in categorical format")
  } else if (sum(flag) == 1) {
    stop("at least 2 features should be in categorical format")
  } else if (sum(flag) > 2) {
    stop("data contains 3 or more categorical features. to continue 2 features must be in categorical format")
  }
}
