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
#'                             othersCol='grey30', 
#'                             specificName = 'unidentified', 
#'                             specificColor = 'grey90',
#'                             sort=sum)
#' 
#' @export


makeColPalette <- function(data, color=NULL, othersCol='grey30',
               specificName =NULL, specificColor = 'grey90', sortFun = sum,
               na.rm=TRUE){

    if(na.rm){
        total.abundance <- apply(data, 2, sortFun, na.rm=TRUE)
    }else{
        total.abundance <- apply(data, 2, sortFun)
    }
    
    ## ||||||||||||||||||||||||||||||||||| ##
    
    df <- data.frame(taxa = colnames(data), 
                     total.abundance = total.abundance, 
                     stringsAsFactors = FALSE)
    df <- df[order(df$total.abundance, decreasing = TRUE), ]
    
    ## ||||||||||||||||||||||||||||||||||| ##
    
    df$color <- NA
    
    if(!is.null(specificName)) {
        
        if(is.null(specificColor)){ specificColor <- 'grey90' }
        
        if(any(df$taxa%in%specificName)){
            
            specific <- which(df$taxa %in% specificName)
            df <- rbind(df[-specific,], df[specific,])
            specific <- -(length(specific)-1):0 + nrow(df)
            
            df[c(1:length(color)), 'color'] <- color
            df[specific, 'color'] <- specificColor
            
            if(nrow(df[-specific,])>length(color)){
                other <- (df[-c(1:length(color)), ])
                df$color[is.na(df$color)] <- othersCol
            }
            
            col <- df$color
            names(col) <- df$taxa
        
        }else{
            
            warning('Ignored specificName species because missing the species')
            df[c(1:length(color)), 'color'] <- color
            
            if(nrow(df)>length(color)){
                df[-c(1:length(color)), 'color'] <- othersCol
            }
            
            col <- df$color
            names(col) <- df$taxa
        }
        
    
    }else{
        
        df[c(1:length(color)), 'color'] <- color
        
        if(nrow(df)>length(color)){
            df[-c(1:length(color)), 'color'] <- othersCol
        }
        
        col <- df$color
        names(col) <- df$taxa
    }
    
    return(col)
}
