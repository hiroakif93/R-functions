#' Loading multiple library
#' 
#' @param libs is a character vector of package names.
#' 
#' @examples
#' load.lib( "vegan" )
#' load.lib( c("vegan", "ggplot2") )
#' 
#' @export

load.lib <- function (libs) 
{	

    invisible(lapply(libs, function(x) invisible(library(package = x, 
                                                         character.only = TRUE))))
    invisible(sapply(libs, function(x) cat(sprintf("%s %s\n", 
                                                   x, packageVersion(x)))))
    
}
