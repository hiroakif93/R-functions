#' Saving object in multiple file format
#' 
#' @param type : You can save object as RDS, CSV and tab delimited format. default outputs are RDS and CSV files
#' 
#' @examples
#' # If you want infomation of one iris species.
#' data(iris) 
#'
#' ## Save rds and csv format
#' saveFile(obj=iris, filename='iris')
#'
#' ## Save all format
#' saveFile(obj=iris, filename='iris', type=1:3)
#' 
#' @export
saveFiles <- function(obj=NULL, filename='file', 
                      row.names=NULL, names=NULL,
                      type=c(1:2, 'rds', 'csv')){
    
    if(class(obj)=='list'){
    	saveRDS(obj, file=sprintf('%s.rds', filename))
    }else{
    
    if(!is.null(row.names)){
    	
    	rownames(obj) <- obj[,row.names]
    	
        obj2 <- obj[,-row.names]
        
    }else{
    	
    	if(!is.null(row.names)){
    		obj2 <- obj
    		
    		obj <- cbind(rownames(obj), obj)
    		colnames(obj)[1] <- names
    	}else{
    		
    		obj2 <- obj
    		
    	}
        
    }
    
    if(any(type%in%c('rds', 1))) saveRDS(obj2, file=sprintf('%s.rds', filename))
    if(any(type%in%c('csv', 2))) write.csv(obj, file=sprintf('%s.csv', filename), row.names=FALSE)
    if(any(type%in%c('tab', 3))) write.table(obj, file=sprintf('%s.txt', filename), 
                                    quote=FALSE, sep='\t', row.names=FALSE)
    }
}
