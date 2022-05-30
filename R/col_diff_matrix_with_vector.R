#' Column Wise Matrix Diff
#'
#' This function returns of the column wise difference between the m matrix and the vector v
#'
#' @param m is a matrix
#' @param vec is a vector
#'
#' @return This function returns a \code{num} matrix.
#'

col_diff_matrix_with_vector = function(m, vec){
  if (nrow(m) == length(vec)) {
    result = c()
    for (i in 1:ncol(m)) {
      result=c(result, m[,i] - vec)
    }
    return(matrix(result, nrow = nrow(m), ncol = ncol(m)))

  } else {
    message("dim attribute does not match")
  }
}
