#' Weave elements of two matrices of vectors
#' Weaves two matrices or vectors whereby rows (elements) of first matrix (vector) are interspersed with rows (elements) of the second matrix, one by one.
#' @param a A vector of matrix. Rows (elements) to be weaved.
#' @param b A vector or matrix. Rows (elements) to weave into \code{a}.
#' @param inpar_b Logical. Whether to enclose elements in \code{b} with parenthesis.
#' @param rnames A character vector. Names of elements in each row of \code{a} (assumed the same in \code{b}).
#' @param excl_0 Logical. Whether to exclude values of `0` in \code{b}.
#' @param within_col Logical. If \code{a} and \code{b} are vectors, `TRUE` weaves elements into one column and `FALSE` returns \code{a} in the first row, and \code{b} on the second row.
#' @export
#' @examples
#' weave(letters[1:5], 1:5, inpar_b = FALSE)
#' means <- round(runif(3), 3)
#' sds <- round(runif(10), 3)
#' weave(means, sds, rnames = c("var1", "var2", "var3"))
weave <- function(a, b, inpar_b = TRUE, rnames = NULL, excl_0 = TRUE, within_col = TRUE){
  if(!identical(dim(a), dim(b))) stop("Input matrices should be the same length")
  if(is.vector(a)){
    if(within_col){
      a <- matrix(a, ncol = 1)
      b <- matrix(b, ncol = 1)
    }else{
      a <- matrix(a, nrow = 1)
      b <- matrix(b, nrow = 1)
    }
  }
  if(!is.matrix(a)) a <- as.matrix(a)
  if(!is.matrix(b)) b <- as.matrix(b)

  matout <- matrix(NA, nrow(a)*2, ncol(a))
  for(i in 1:nrow(a)){
    matout[(2*i-1),] <- a[i,]
    if(inpar_b)
      matout[(2*i),] <- wrap_str(b[i,])
    else
      matout[(2*i),] <- b[i,]
  }

  if(!is.null(rnames)){
    rnames <- rep(rnames, each = 2)
    rmn <- 1:length(rnames)%%2 == 0
    rnames[rmn] <- ""
  }

  if(!is.null(colnames(a))) colnames(matout) <- colnames(a)
  matout <- cbind(rnames, matout)
  matout <- gsub("NaN", "", matout, fixed = TRUE)
  if(excl_0) matout <- gsub("(0)", "", matout, fixed = TRUE)

  return(matout)
}
