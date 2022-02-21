#' Remake the matrix to other taxonomy level.
#' 
#' @description This function is useful for OTU/ASV data table. Similar to aggregate function.
#' 
#' @param data data is abundance table which colnames is ASV / OTU.
#' @param taxa taxa is taxnomy information table.
#' @param taxaLabel taxaLabel is a taxnomy level which you want to remake.
#' @param func func is function. default is sum()
#'
#' 
#' @examples
#' # If you want OTU table to Family level matrix.
#' library(microbiome) 
#' data(atlas1006)
#' 
#' data <- as.data.frame(t(otu_table(atlas1006)))
#' taxa <- tax_table(atlas1006)
#' taxaLabel <- 'Family'
#' a <- Taxa.mat(x,y, taxaLabel)
#'
#' @export


Taxa.mat <- function(data, taxa, taxaLabel, func=function(x){sum(x)}){
    
    # -------------------------------------------------#	
    # data is abundance table which colnames is ASV / OTU 
    # taxa is taxnomy information table
    # taxaLabel is a taxnomy level
    # func is function. default is mean()
    #
    # Exnample : 
    # library(microbiome) ; data(atlas1006)
    # x <- as.data.frame(t(otu_table(atlas1006)))
    # y <- tax_table(atlas1006)
    # taxaLabel <- 'Family'
    # 
    # a <- Taxa.mat(x,y, taxaLabel)
    # -------------------------------------------------#
    
    colnames(data) <- y[colnames(data), taxaLabel]
    
    summary <- do.call(cbind,
                       lapply(unique(colnames(data)), 
                              function(a){ num <- which(colnames(data)==a)
                              apply(as.matrix(data[,num]), 1, func)}) )
    
    colnames(summary) <- unique(colnames(data))
    summary
}