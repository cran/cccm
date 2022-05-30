#' Column Wise Matrix Multiplication
#'
#' This function returns of the column wise multiplication of the m matrix and the vector v.
#'
#' @param m is a matrix
#' @param vec is a vector
#'
#' @return This function returns a \code{num} matrix.

mult_matrix_cols_with_vector = function(m,vec){
  result = c()
  for (i in 1:ncol(m)) {
    result=c(result, m[,i] * vec[i])
  }
  return(matrix(result, nrow = nrow(m), ncol = ncol(m)))
}
