#' ggplot2 theme
#' 
#' @description ggplot2 theme
#' 
#' @param bsize : mimum size of text.
#' @param lwd : linewidth in element_rect.
#' @param unitsize : legend.key.size.
#' @param family : font family.
#'
#' @examples
#'
#' @export


theme_text <- function(bsize=6, lwd=0.5, unitsize=1, family=NULL, gridcol="grey92"){
    
    themeown <- theme(plot.background = element_blank(),
                       panel.border=element_rect(fill=NA, linewidth=lwd),
                       panel.background = element_rect(fill = NA, 
                                                       colour = NA),
                       axis.text.x=element_text(size=bsize, family=family),
                       axis.text.y=element_text(size=bsize, family=family),
                       axis.title=element_text(size=bsize+1, family=family),
                       strip.background = element_blank(),
                       strip.text=element_text(size=bsize+1, family=family),
                       legend.background = element_blank(),
                       legend.text = element_text(size=bsize, family=family),
                       legend.title = element_text(size=bsize+1, family=family),
                       legend.key.size=unit(unitsize, 'line'),
                       title = element_text(size=bsize+1, family=family),
                       plot.subtitle = element_text(size=bsize+1, family=family),
                       plot.caption = element_text(size=bsize+1, family=family)
                       )
    if(!is.null(gridcol) ){
        themeown <- themeown+theme(panel.grid = element_line(colour = gridcol))
        
    }
    return(list(themeown))
}

#' ggplot2 theme
#' 
#' @description theme for mardown style.
#' 
#' @param bsize : mimum size of text.
#' @param lwd : linewidth in element_rect.
#' @param unitsize : legend.key.size.
#' @param family : font family.
#'
#' @importFrom ggtext element_markdown
#' @examples
#'
#' @export

theme_md <- function(bsize=6, lwd=0.5, unitsize=1, family=NULL, gridcol="grey92"){
    
    themeown <- theme(plot.background = element_blank(),
                       panel.border=element_rect(fill=NA, size=lwd),
                       panel.background = element_rect(fill = NA, 
                                                       colour = NA),
                       axis.text= element_markdown(size=bsize, family=family),
                       axis.title.x= element_markdown(size=bsize+1, family=family),
                       axis.title.y= element_markdown(size=bsize+1, family=family),
                       strip.background = element_blank(),
                       strip.text= element_markdown(size=bsize+1, family=family),
                       legend.background = element_blank(),
                       legend.text = element_markdown(size=bsize, family=family),
                       legend.title = element_markdown(size=bsize+1, family=family),
                       legend.key.size=unit(unitsize, 'line'),
                       title = element_markdown(size=bsize+1, family=family),
                       plot.subtitle = element_markdown(size=bsize+1, family=family),
                       plot.caption = element_markdown(size=bsize+1, family=family)
                       )
    if(!is.null(gridcol) ){
        themeown <- themeown+theme(panel.grid = element_line(colour = gridcol))
        
    }
    return(list(themeown))
}