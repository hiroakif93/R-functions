#' Loading multiple library
#' 
#' @param ...: package names. Quotations are unnecessary.
#' 
#' @examples
#' load.lib( "vegan" )
#' load.lib( c("vegan", "ggplot2") )
#' 
#' @export

load.lib <- function (...) 
{	

	# -------------------------------------------------#	
    # Loading multiple library function.
    # libs is a character vector of package names.
    #
    # Exnample 
    # load.lib( vegan )
    # -------------------------------------------------#
    
   libs <- as.character(substitute(list(...)))[-1]
  
  invisible(lapply(libs, function(x) invisible(library(package = x, character.only = TRUE))))
  invisible(sapply(libs, function(x) cat(sprintf("%s %s\n", x, packageVersion(x)))))
    
}
