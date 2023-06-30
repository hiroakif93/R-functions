#' Take color platte
#' @description You can generate maximum 70 colors from RColorBrewer palattes
#' 
#' @param x x is single integer. Default 70
#' @param pal you can select 'RColorBrewer' or 'rainbow'. Default RColorBrewer.
#' 
#' @importFrom RColorBrewer brewer.pal.info
#' @importFrom RColorBrewer brewer.pal
#' 
#' @examples
#' palettes(10)
#' palettes(20)
#' palettes(20, pal='gradient')
#' 
#' @references https://stackoverflow.com/questions/15282580/how-to-generate-a-number-of-most-distinctive-colors-in-r
#' @export

palettes <- function(x=70, pal='RColorBrewer')
{
    
    if(pal=='RColorBrewer'){
        qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
        col.vector <- unique(unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals))))
    }else if(pal=='rainbow'){
        
        if(x>35) {
            x <- 35
            warning("Sorry, 'rainbow' palette has only 35 colors.")
        }
        
        col1 <- c( 'firebrick3', 'firebrick2', 'indianred1', 'lightpink', 'firebrick4')
        col2 <- c( 'royalblue3', 'dodgerblue2', 'steelblue2', 'skyblue2', 'dodgerblue4') 
        col3 <- c( 'springgreen4','darkolivegreen4', 'olivedrab3',  'darkolivegreen1', 'darkseagreen4') 
        col4 <- c( 'mediumorchid4', 'mediumpurple', 'mediumorchid', 'orchid1', 'plum2')
        col5 <- c( 'tomato', 'darkorange2', 'orange2', 'tan1', 'lightgoldenrod1')
        col6 <- rev(c('cornsilk1','cornsilk2', 'bisque2', 'bisque3', 'bisque4'))
        
        colmat <- cbind(col1, col2, col3, col4, col5, col6)
        col.vector <- c( as.vector(colmat), brewer.pal(10, 'Set3')[-c(4,6:9)]) 
    }else if( length(pal) > 1 ) {
      
      col.vector <- pal
      x = length(pal)
    }
    
    
    return(col.vector[1:x])
}
