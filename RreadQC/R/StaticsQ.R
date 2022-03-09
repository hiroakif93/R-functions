#' @useDynLib RreadQC, .registration=TRUE
#' @importFrom Rcpp sourceCpp
#' @name staticsQ
#' @title  Calculate read quality indices
#'
#'@param dir : dir is a directory contained fastq files.
#'@return column 1 : Maximum quality in a read. column 2 : Minimum Quality in a read. column 3 : Maximum expected error
#'@export
staticsQ <-function(dir=x){ #dir=afterdir
    
    ## =================================== ##
    ## -- Load fastq file
    fastq <- readFastq(dir)
    
    ## -- Take structure
    Q <- quality(fastq)
    L <- width(fastq)
    ID <- id(fastq)
    
    totalread <- length(ID)
    ## =================================== ##
    ## -- Calculate quality score indices
    s <- Sys.time()
    covertChar <- as(ID, "vector")
    samples <- takeSamplenames2(x=covertChar, sep='__')
    Qmat <- as(Q, "matrix")
    stat1 <- Qstat( Qmat)
    stat2 <- rowMeans(Qmat, na.rm=TRUE)
    e <- Sys.time()
    print(e-s)
    
    colnames(stat1) <- c("MaxQ", "MinQ", "maxEE")
    Qres <- cbind(sample=samples, data.frame(stat1, Mean=stat2, length=L) ) 
    return(Qres)
}