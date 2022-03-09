#'Visualizing total read.
#'
#'@importFrom ggplot2 ggplot
#'@importFrom ggplot2 geom_bar
#'@importFrom ggplot2 aes
#'@importFrom ggplot2 geom_hline
#'@importFrom ggplot2 theme_bw
#'@importFrom ggplot2 scale_x_discrete
#'@importFrom ggplot2 guide_axis
#'@importFrom ggplot2 scale_y_continuous
#'@importFrom ggplot2 theme
#'@importFrom ggplot2 element_text
#'@importFrom ggplot2 element_blank
#'@importFrom ggplot2 labs
#'
#'@export
ggreadFun <- function(data){
    
    gtmp <- ggplot(data)+
        geom_bar(aes(x=sample, y=read.in), stat="identity", 
                 color="black", fill="skyblue4", size=0.1, width=1)+
        geom_bar(aes(x=sample, y=read.out),  stat="identity", 
                 color="black", fill="skyblue1", size=0.1, width=0.9)+
        geom_hline(yintercept = 2000, color="red", alpha=0.8, linetype=4)+
        theme_bw()+
        scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+
        scale_y_continuous(expand=c(0,0,0.05,0))+
        theme(axis.text.x=element_text(angle=60, hjust=1, size=5),
              panel.grid=element_blank())+
        labs(y='Total sequence read', x='')
    return(gtmp)
}

#'Visualizing read quality indices per sample
#'@importFrom ggplot2 ggplot
#'@importFrom ggplot2 aes_string
#'@importFrom ggplot2 geom_boxplot
#'@importFrom ggplot2 stat_summary
#'@importFrom ggplot2 geom_hline
#'@importFrom ggplot2 labs
#'@importFrom ggplot2 theme_bw
#'@importFrom ggplot2 scale_x_discrete
#'@importFrom ggplot2 guide_axis
#'@importFrom ggplot2 scale_y_continuous
#'@importFrom ggplot2 theme
#'@importFrom ggplot2 element_text
#'@importFrom ggplot2 element_blank
#'
#'@export
ggQFun <- function(data, xaxis, yaxis){
    
    medianQ <- median(data[,yaxis])
    gtmp <- ggplot(data, aes_string(x=xaxis, y=yaxis) )+
        geom_boxplot(color="royalblue4",
                     width=0.8, outlier.size = 0.1)+
        stat_summary(geom="point", fun=mean, color='red', size=1)+
        geom_hline(yintercept=medianQ, color='grey30', linetype=1, size=1, alpha=0.7)+
        geom_hline(yintercept=medianQ, color='gold', linetype=2, size=0.5, alpha=0.7)+
        labs(y=yaxis, x='sample')+
        theme_bw()+
        scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+
        scale_y_continuous(expand=c(0,0,0.05,0))+
        theme(axis.text.x=element_text(angle=60, hjust=1, size=5),
              panel.grid=element_blank())
    return(gtmp)       
}

#'Visualizing read quality indices distribution.
#'@importFrom ggplot2 ggplot
#'@importFrom ggplot2 geom_histogram
#'@importFrom ggplot2 aes
#'@importFrom ggplot2 aes_string
#'@importFrom ggplot2 theme_bw
#'@importFrom ggplot2 scale_fill_manual
#'@importFrom ggplot2 scale_color_manual
#'@importFrom ggplot2 geom_vline
#'@importFrom ggplot2 scale_y_sqrt
#'@importFrom scales comma
#'@importFrom ggplot2 labs
#'@export
ggQdistFun <- function(data, xaxis){
    
    th <- summary(data[,xaxis])[5]
    
    gtmp <- ggplot(data, aes_string(x= xaxis))+
        geom_histogram(aes(color=filt, fill=filt),
                       size=1.0, alpha=0.7, show.legend = FALSE)+
        theme_bw()+
        scale_fill_manual(values=c("royalblue4", "indianred4"))+
        scale_color_manual(values=c("lightblue", "lightpink"))+
        geom_vline(xintercept=th, 
                   size=1,color="red", linetype=4)+
        scale_y_sqrt(expand=c(0,0,0.01,0),labels = scales::comma)+
        labs(y="Sequence read count", x=xaxis,
             subtitle=paste("75 persent Sequence read threshold =", round(th, 1)))
    return(gtmp)       
}

