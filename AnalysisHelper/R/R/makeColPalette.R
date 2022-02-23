#' Make color coresspondece table
#' @description You can make color coresspondece table for ggplot2.
#' 
#' @param data matrix or dataframe containing sample x abundance
#' @param color a vector contained color information
#' @param others Not dominant color
#' @param specificName You can specify the color to specific species (like 'Unidentified')
#' @param specificColor For the specific species color
#' 
#' 
#' @examples
#' data(test)
#' col <- palettes()
#' colpal <- makeColPalette(data=test[,-1], 
#'                             color=col, 
#'                             others='grey30', 
#'                             specificName = 'unidentified', 
#'                             specificColor = 'grey90',
#'                             sort=sum)
#' 
#' @export


makeColPalette <- function(data=NULL, color=NULL, 
                           others=NULL, 
                           specificName=NULL, specificColor=NULL,
                           sort=sum ){
  
    ## ++++++++++++++++++++++++++++++++++++++ ##
    ## -- Sorting
    total.abundance <- apply(data, 2, sort) 
    
    df <- data.frame(taxa=colnames(data), total.abundance=total.abundance, 
                     stringsAsFactors = FALSE)
    df <- df[order(df$total.abundance, decreasing=TRUE),]
    
    ## ++++++++++++++++++++++++++++++++++++++ ##
    ## -- Separate dominat, others, spedific species
    
    specific <- which(df$taxa==specificName)
    
    dfsub <- df[-specific, ]
    
    dominat <- dfsub[c(1:length(color)),]
    other <- dfsub[-c(1:length(color)),]
    specific <-df[specific, ] 
    
    ## ++++++++++++++++++++++++++++++++++++++ ##
    ## -- Color
    dominat$color <- color
    other$color <- others
    
    names(specificColor) <- specificName
    specific$color <- NA
    for(i in length(specificColor)){
        specific[specific$taxa==names(specificColor)[i]]$color <- specificColor[i]
    }
    
    dfbind <- rbind(dominat, other, specific)    
    
    col <- dfbind$color
    names(col) <- dfbind$taxa
    
    return(col)
}
