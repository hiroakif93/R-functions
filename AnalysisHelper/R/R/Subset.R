#' Extracting data
#' @description This function is useful for a experiment data containing some treatment.
#' Similar to subset function.
#' 
#' @param data data is matrix or data frame.
#' @param info info is unique infomation of specific column in data.
#' @param row row is single integer. 
#' 
#'
#' 
#' @examples
#' # If you want infomation of one iris species.
#' data(iris) 
#' unique <- unique(iris[,4:5])
#' a <- Subset(data=iris, info= unique, row=1)
#' 
#' @examples
#' # If you want to split infomation by each species.
#' a <- lapply(1:nrow(unique), function(n){Subset(data= iris, info= unique, row=n)})
#' 
#' @export

Subset <- function(data,info,row){ 
    
    # -------------------------------------------------#	
    # data is matrix 
    # info is a matrix of unique sample information
    # row is the number of unique sample information rown which you want to subset set
    #
    # Exnample 1 : If you want extract sample containing Petal.Width=0.2 and Species=setosa
    # data(iris) 
    # unique <- unique(iris[,4:5])
    # a <- Subset(data=iris, info= unique, row=1)
    # 
    # Exnample 2 :
    # a <- lapply(1:nrow(unique), function(n){Subset(data= iris, info= unique, row=n)})
    # -------------------------------------------------#
    
    k <- colnames(info) # get colnames to subset each treatment
    for(i in 1:length(k)) { data <- data[which(data[,k[i]] %in% info[row,k[i]]),] }
    data
    
}
