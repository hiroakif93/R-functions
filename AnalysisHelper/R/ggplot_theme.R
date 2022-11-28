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


theme_text <- function(bsize=6, plssize=0, lwd=0.5, unitsize=NULL, family=NULL, gridcol="grey92"){
    
    themeown <- theme(plot.background = element_blank(),
                       panel.border=element_rect(fill=NA, linewidth=lwd),
                       panel.background = element_rect(fill = NA, 
                                                       colour = NA),
                       axis.text.x=element_text(size=bsize, family=family),
                       axis.text.y=element_text(size=bsize, family=family),
                       axis.title=element_text(size=bsize+plssize, family=family),
                       strip.background = element_blank(),
                       strip.text=element_text(size=bsize+plssize, family=family),
                       legend.background = element_blank(),
                       legend.text = element_text(size=bsize, family=family),
                       legend.title = element_text(size=bsize+plssize, family=family),
                       title = element_text(size=bsize+plssize, family=family),
                       plot.subtitle = element_text(size=bsize+plssize, family=family),
                       plot.caption = element_text(size=bsize+plssize, family=family),
                       legend.box.background = element_blank()
                       )
    if(!is.null(gridcol) ){
        themeown <- themeown+theme(panel.grid = element_line(colour = gridcol))
    }
    
    if(!is.null(unitsize) ){
        themeown <- themeown+theme(legend.key.size=unit(unitsize, 'line'),
                                   legend.key = element_rect(fill=NA))
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

theme_md <- function(bsize=6, plssize=0, lwd=0.5, unitsize=NULL, family=NULL, gridcol="grey92"){
    
    themeown <- theme(plot.background = element_blank(),
                       panel.border=element_rect(fill=NA, size=lwd),
                       panel.background = element_rect(fill = NA, 
                                                       colour = NA),
                       axis.text= element_markdown(size=bsize, family=family),
                       axis.title.x= element_markdown(size=bsize+plssize, family=family),
                       axis.title.y= element_markdown(size=bsize+plssize, family=family),
                       strip.background = element_blank(),
                       strip.text= element_markdown(size=bsize+plssize, family=family),
                       legend.background = element_blank(),
                       legend.text = element_markdown(size=bsize, family=family),
                       legend.title = element_markdown(size=bsize+plssize, family=family),
                       title = element_markdown(size=bsize+plssize, family=family),
                       plot.subtitle = element_markdown(size=bsize+plssize, family=family),
                       plot.caption = element_markdown(size=bsize+plssize, family=family),
                       legend.box.background = element_blank()
                       )
    if(!is.null(gridcol) ){
        themeown <- themeown+theme(panel.grid = element_line(colour = gridcol))
    }
    
    if(!is.null(unitsize) ){
        themeown <- themeown+theme(legend.key.size=unit(unitsize, 'line'),
                                   legend.key = element_rect(fill=NA))
    }
    return(list(themeown))
}
