#' Row Wise Matrix Division
#'
#' This function returns of the row wise division of the m matrix and the vector v.
#'
#' @param m is a matrix
#' @param vec is a vector
#'
#' @return This function returns a \code{num} matrix.
#'

div_matrix_rows_with_vector = function(m,vec){
  if (ncol(m) == length(vec) & is.vector(vec)) {
    result = c()
    for (i in 1:nrow(m)) {
      result=c(result, m[i,] / vec)
    }
    return(matrix(result, nrow = nrow(m), ncol = ncol(m), byrow = T))
  } else {
    message("dim attribute does not match | check vec class")
  }
}
