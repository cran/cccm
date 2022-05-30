#' Row Wise Matrix Diff
#'
#' This function returns of the row wise difference between the m matrix and the vector v
#'
#' @param m is a matrix
#' @param vec is a vector
#'
#' @return This function returns a \code{num} matrix.
#'

row_diff_matrix_with_vector = function(m, vec){
  if (ncol(m) == length(vec)) {
    result = c()
    for (i in 1:nrow(m)) {
      result=c(result, m[i,] - vec)
    }
    return(matrix(result, nrow = nrow(m), ncol = ncol(m), byrow = T))
  }else {
    message("dim attribute does not match")
  }
}
