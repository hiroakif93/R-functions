#' Setting saveFiles parameters.
#' 
#' @param dir (character): output path name.
#' @param type (character vector): file formats.
#' @param row.names (bool or character): If a character is provided, the column name is used as row names.
#' @export

savefile_set <- function(dir = getwd(),
                         type = c('rds', 'csv', 'txt'),
                         row.names = FALSE, as_name=NULL
                         ) {
  sf_params <- new.env(parent = emptyenv())
  
  sf_params$dir <- dir
  sf_params$type <- type
  sf_params$row.names <- row.names

  assign("sf_params", sf_params, envir = .GlobalEnv)
}

#' Saving an object in various file formats
#' 
#' @param type : You can save object as RDS, CSV and tab delimited format. default outputs are RDS and CSV files
#' 
#' @import dplyr
#' 
#' @examples
#' data(iris) 
#'
#' ## Save rds and csv format
#' saveFile(obj=iris, filename='iris')
#'
#' ## Save all format
#' saveFile(obj=iris, filename='iris', type=1:3)
#' 
#' @export

saveFiles <- function(obj, filename, 
                      dir = getwd(),
                      row.names=FALSE, 
                      type=c(1:3, 'rds', 'csv', 'tab')){
  
  filename <- as.character(substitute(filename))
  
  ## Utilize the sf_params environment if it exists.
  if(exists("sf_params", envir = .GlobalEnv)){
    
    dir = sf_params$dir
    type = sf_params$type
    row.names = sf_params$row.names
    as_name = sf_params$as_name
    
  }
  
  if(!dir.exists(dir)) dir.create(dir)
  
  ## Save as RDS if objects class is list type.
  if(class(obj)=='list'){
  	saveRDS(obj, file=sprintf('%s.rds', filename))
    print("Save as RDS format.")
    return(invisible())
  }
  
  ## Make a column from row names.
  if(row.names){
  	
    obj =
      obj %in%
      rownames_to_column(var=as_name)
    
  }
  
  ## Save results.
  if(any(type%in%c('rds', 1))) saveRDS(obj, file=sprintf('%s.rds', filename))
  if(any(type%in%c('csv', 2))) write.csv(obj, file=sprintf('%s.csv', filename), row.names=FALSE)
  if(any(type%in%c('tab', 3))) write.table(obj, file=sprintf('%s.txt', filename), 
                                  quote=FALSE, sep='\t', row.names=FALSE)
}
