#' Change the name shorter
#' @param x : x is a character vector of sample names.
#' @param sep : sep is a character to split the name.
#' #' 
#' @export

takeSamplenames1 <- function(x, sep=''){
    
    split <- do.call(rbind, strsplit(x, sep) )
    check.unique <- apply(split, 2, function(x) { length(unique(x)) })
    uniquename <- which(check.unique>1)
    if(length(uniquename)==1){
        
        if( all( !is.na( as.numeric(split[,uniquename]) )) ){
            samplename <- paste("S", split[,uniquename], sep="_")
        }else{
            samplename <- split[,uniquename]
        }
    }else{
        samplename <- sprintf("%s_%s", split[,uniquename[1] ], split[,uniquename[2]]) 
    }
    
    return(samplename)
}

#' Change the name shorter
#' @param x : x is a character vector of sample names.
#' @param sep : sep is a character to split the name.
#' #' 
#' @export

takeSamplenames2 <- function(x, sep=''){
    
    split <- do.call(rbind, strsplit(x, sep) )
    check.unique <- apply(split, 2, function(x) { length(unique(x)) })
    uniquename <- which(check.unique>1 & length(x)>check.unique)
    if(length(uniquename)==1){
        
        if( all( !is.na( as.numeric(split[,uniquename]) )) ){
            samplename <- paste("S", split[,uniquename], sep="_")
        }else{
            samplename <- split[,uniquename]
        }
    }else{
        samplename <- sprintf("%s_%s", split[,uniquename[1] ], split[,uniquename[2]]) 
    }
    
    return(samplename)
}