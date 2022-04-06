#' @useDynLib AnalysisHelper, .registration=TRUE
#' @importFrom Rcpp sourceCpp
#' @param mat mat is matrix.
#' @param th th is threshold.
#' @export
# -- Binarization
binary <- function(mat=NULL, th=0){

	Binary(mat, th)
	
}