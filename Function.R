############################################################################
####
#### Functions
#### 2019.11.28 Fujita created
#### 2020.4.31 Fujita modified
#### R 3.6.0
#### 
############################################################################

##########################################################
######### ---       Loading Libraries       --- ##########

load.lib <- function (libs, install = FALSE) 
{
    if (install) {
        lib.all <- library()
        lib.name <- lib.all$results
        if (sum(libs %in% lib.name) != length(libs)) {
            installed <- which(libs %in% lib.name)
            uninstalled <- libs[-installed]
            invisible(lapply(uninstalled, function(x) invisible(install.packages(package = x))))
        }
    }
    invisible(lapply(libs, function(x) invisible(library(package = x, 
        character.only = TRUE))))
    invisible(sapply(libs, function(x) cat(sprintf("%s %s\n", 
        x, packageVersion(x)))))
}


##########################################################
######### ---     Subset each treatment     --- ##########

# --- Extract unique sanple set

Subset <- function(data,info,row){ 
	
	# -------------------------------------------------#	
 	# data is matrix 
  	# info is a matrix of unique sample information
  	# row is the number of unique sample information rown which you want to subset set
  	#
  	# Exnample 1 : If you want extract sample containing Petal.Width=0.2 and Species=setosa
  	# data(iris) 
  	# unique <- unique(iris[,4:5])
  	# a <- Subset(data=iris, info= unique, row=1)
  	# 
  	# Exnample 2 :
  	# a <- lapply(1:nrow(unique), function(n){Subset(data= iris, info= unique, row=n)})
    # -------------------------------------------------#
        
	k <- colnames(info) # get colnames to subset each treatment
	for(i in 1:length(k)) { data <- data[which(data[,k[i]] %in% info[row,k[i]]),] }
	data
	
}

##########################################################
######### ---      Make taxnomy matrix      --- ##########

# -- Data compile to other taxonomy level from OTUs/ASVs level matrix

Taxa.mat <- function(x, y, taxaLabel, func=function(x){mean(x)}){
		
	# -------------------------------------------------#	
 	# x is abundance table which colnames is ASV / OTU 
  	# y is taxnomy information table
  	# taxaLabel is a taxnomy level
  	# func is function. default is mean()
  	#
  	# Exnample : 
  	# library(microbiome) ; data(atlas1006)
  	# x <- as.data.frame(t(otu_table(atlas1006)))
	# y <- tax_table(atlas1006)
  	# taxaLabel <- 'Family'
  	# 
  	# a <- Taxa.mat(x,y, taxaLabel)
    # -------------------------------------------------#
 
  colnames(x) <- y[colnames(x), taxaLabel]
  
  summary <- do.call(cbind,
                  lapply(unique(colnames(x)), 
                         function(a){ num <- which(colnames(x)==a)
                         apply(as.matrix(x[,num]), 1, func)}) )
  
  colnames(summary) <- unique(colnames(x))
  summary
}

################################################################
######### ---     Extract Legend from ggplot      --- ##########

g_legend<-function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)}
  
################################################################
######### ---     Mean of element within list     --- ##########

# -- Caluculate mean value of among list

listMeans <- function(x){
	
	# -------------------------------------------------#	
 	# x is a list 
  	# Exnample : 
  	# list <- lapply(1:10, function(x){matrix(rnorm(9), 3, 3)})
  	# listMeans(list)
    # -------------------------------------------------#

	sum.mat <- count.mat <- matrix(0, 
								   ncol=ncol(as.data.frame(x[[1]])), 
								   nrow=nrow(as.data.frame(x[[1]])) )
	for(i in 1:length(x)) { sum.mat <- sum.mat + x[[i]] }
	mean.mat <- sum.mat/length(x)
	mean.mat
}

################################################################
######### ---           Color palette             --- ##########

lib <- 'RColorBrewer';library(package = lib, character.only=TRUE);packageVersion(lib) 
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col.vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

palettes <- function(x){ col.vector[1:length(unique(x))]}


##########################################################
######### ---     Barplot by ggplot     --- ##############
ggplotBar <- function(data=a, 
                      x=b, y=c, 
                      color.by=d,
                      split=NULL,
                      palette=TRUE,
                      style=c('absolute','relative'),width=0.8){
  # -------------------------------------------------#
  # data is long format data frame
  # x is character, variable of x axis 
  # y is character, variable of y axis 
  # color.by is character, color of barplot
  # split is character, decide to split or not
  # style is 100 % stack or not
  # width is a range of bar
  # -------------------------------------------------#
  if(length(style)>1){ 
    style='fill'
  }else{
      if(style=='relative'){
        style='fill'
      }else{
        style='stack'
      }
  }
  
  df <- as.data.frame(cbind(x=data[,x], y=data[,y], fill=data[,color.by], facet=data[,split] ))
  if(class(data[,y])=='numeric')df[,'y'] <- as.numeric(as.vector(df[,'y']))
  if(class(data[,x])=='numeric')df[,'x'] <- as.numeric(as.vector(df[,'x']))
  gg.tmp <- ggplot()+
            geom_bar(data=df, 
                     aes(x=x, y=y, fill=fill),
                     stat='identity',position=style)+
            theme_minimal()
  if(palette){
    my.pal <- palettes(df[,'fill'])
    gg.tmp <- gg.tmp+
              scale_fill_manual(values=my.pal)
  }
  
  if(length(split)>0){
    gg.tmp <- gg.tmp+
              facet_wrap(~facet, scales='free')
  }
  gg.tmp
}


##########################################################
######### ---     Areaplot by ggplot     --- #############
ggplotArea <- function(data=a, 
                      x=b, y=c, 
                      color.by=d,
                      split=NULL,
                      palette=TRUE,
                      style=c('absolute','relative'),width=0.8){
  
  # ---------------------------------------------------------------------------------------#
  # data is data frame
  # x is character, variable of x axis 
  # y is character, variable of y axis 
  # color.by is character, color of barplot
  # split is character, decide to split or not
  # style is 100 % stack or not
  # width is a range of bar
  # ---------------------------------------------------------------------------------------#
  
  if(length(style)>1){ 
    style='fill'
  }else{
      if(style=='relative'){
        style='fill'
      }else{
        style='stack'
      }
  }
  
  df <- as.data.frame(cbind(x=data[,x], y=data[,y], fill=data[,color.by], facet=data[,split] ))
  if(class(data[,y])=='numeric')df[,'y'] <- as.numeric(as.vector(df[,'y']))
  if(class(data[,x])=='numeric')df[,'x'] <- as.numeric(as.vector(df[,'x']))
  gg.tmp <- ggplot()+
            geom_area(data=df, 
                     aes(x=x, y=y, fill=fill),
                     stat='identity',position=style)+
            theme_minimal()
  if(palette){
    my.pal <- palettes(df[,'fill'])
    gg.tmp <- gg.tmp+
              scale_fill_manual(values=my.pal)
  }
  
  if(length(split)>0){
    gg.tmp <- gg.tmp+
              facet_wrap(~facet, scales='free')
  }
  gg.tmp
}

##########################################################
######### ---    Line plot by ggplot    --- ##############
ggplotLine <- function(data=a, 
                      x=b, y=c, 
                      color.by=NULL,
                      group=NULL,
                      split=NULL,
                      palette=TRUE,
                      style=1,size=0.8){
  # ---------------------------------------------------------------------------------------#
  # data is data frame
  # x is character, variable of x axis 
  # y is character, variable of y axis 
  # color.by is character, color of barplot
  # split is character, decide to split or not
  # style is 100 % stack or not
  # width is a range of bar
  # ---------------------------------------------------------------------------------------#
  df <- as.data.frame(cbind(x=data[,x], y=data[,y],group=data[,group],
                            col=data[,color.by], facet=data[,split] ))
  if(class(data[,y])=='numeric')df[,'y'] <- as.numeric(as.vector(df[,'y']))
  if(class(data[,x])=='numeric')df[,'x'] <- as.numeric(as.vector(df[,'x']))
  
  
  ## -- Main part
  if(length(color.by)>1){
      gg.tmp <- ggplot()+
                geom_line(data=df, 
                          aes(x=x, y=y, color=col, group=col),
                          linetype=style,size=size)+
                theme_minimal()+
                theme(panel.border = element_rect(fill=NA, color='grey20'))
  }else{
      gg.tmp <- ggplot()+
                geom_line(data=df, 
                          aes(x=x, y=y, color=col),
                          linetype=style,size=size)+
                theme_minimal()+
                theme(panel.border = element_rect(fill=NA, color='grey20')) 
  }
  
  if(palette){
    my.pal <- palettes(df[,'col'])
    gg.tmp <- gg.tmp+
              scale_color_manual(values=my.pal)
  }
  
  if(length(split)>0){
    gg.tmp <- gg.tmp+
      facet_wrap(~facet, scales='free')
  }
  gg.tmp
}


##########################################################
######### ---    PCA plot by ggplot    --- ###############

ggPCA <- function(pca=pca, sample=sample, 
                  dimension=c('PC1','PC2','PC3'),
                  color.by=NULL,
                  shape=NULL,
                  arrow=TRUE, 
                  scales='free',
                  brewer.pal=brewer.pal(11,'Spectral')){
  require(ggplot2) ; require(RColorBrewer); require(tidyr)
  
  # ---------------------------------------------------------------------------------------#
  
  # data is prcomp() result
  # sample is a dataframe of sample inforamation, which sample's rownames is sample ID/name
  # dimension is dimention of pca, you can choose more than 2 dimention each axis.
  # color.by is a character of variable which colored by.
  # split is character, decide to split or not
  # arrow is logical, if TRUE, it will plot variable loadings
  # scales is character, parameter of facet_wrap(). It could be 'free' or 'fixed'
  # 
  # Example : 
  # data(iris)
  # pca <- prcomp(iris[,1:4])
  # sample <- as.data.frame(iris[,5])
  #	ggPCA(pca, sample)
	
  # ---------------------------------------------------------------------------------------#
  
  ## -- Making base dataframe
  pca.score <- as.data.frame(apply(pca$x,2,scale) ) ; rownames(pca.score) <- rownames(pca$x)
  pca.score <- pca.score[,dimension]
  

  pca.mat <- cbind(sample=rownames(sample), 
                   pca.score[rownames(sample),],
                   color.by=sample[,color.by],
                   shape=sample[,shape])
  
  if(length(color.by)==0)pca.mat$color.by <- 'black'
  if(length(shape)==0)pca.mat$shape <- '1'
  
  ## -- Making varibale loadings matrix
  pca.arrow <- data.frame(pca$rotation)[,dimension]
  
  if(nrow(pca.arrow)>6){
  	range=c(1:3,(nrow(pca.arrow)-2):nrow(pca.arrow))
  }else{
  	range=c(1:nrow(pca.arrow))
  }
  pca.arrow.top <- do.call('rbind', 
                           apply(pca.arrow, 2, # x=pca.arrow[,1]
                                 function(x){ pca.arrow[order(x, decreasing=TRUE)[range],]}))
  arrow.scale <- pca.arrow.top *10
  for(i in 1:ncol(pca.arrow.top)) arrow.scale[,i] <- pca.arrow.top[,i]* min(abs(max(as.numeric(pca.score[,i]))/pca.arrow.top[,i]))
  arrow.scale$species <- gsub('PC\\d.', '', rownames(arrow.scale))
  arrow.scale <- unique(arrow.scale)
  
  ## -- Data compile
  combination = t(combn(dimension,2))
  pca.lf <- do.call(rbind, 
                lapply(1:nrow(combination), 
                       function(n){ 
                         x=combination[n,]
                         ex <- dimension[-which(dimension%in%x)]
                         pca.mat.sub <- pca.mat[, -which(colnames(pca.mat)==ex)]
                         lf.tmp <- gather(pca.mat.sub, key=key, value=axis2, 
                                          x[1])
                         colnames(lf.tmp)[grep('PC',colnames(lf.tmp))] <- 'axis1'
                         lf.tmp2 <- cbind(lf.tmp,cobn=paste(x[1],x[2]))
                         lf.tmp2
                }) )
  arrow.lf <- do.call(rbind, 
                lapply(1:nrow(combination), 
                       function(n){ 
                         x=combination[n,]
                         ex <- dimension[-which(dimension%in%x)]
                         pca.mat.sub <- arrow.scale[, -which(colnames(arrow.scale)==ex)]
                         lf.tmp <- gather(pca.mat.sub, key=key, value=axis2, 
                                          x[1])
                         colnames(lf.tmp)[grep('PC',colnames(lf.tmp))] <- 'axis1'
                         lf.tmp2 <- cbind(lf.tmp,cobn=paste(x[1],x[2]))
                         lf.tmp2
                       }) )

  ## -- Make ggplot object
  gg.pca <- ggplot()+
               geom_hline(yintercept=0, linetype='dashed',color='grey40')+
               geom_vline(xintercept=0, linetype='dashed',color='grey40')+
               geom_point(data= pca.lf,
                          aes(x=axis1, y=axis2, color=color.by,shape=shape), alpha=0.7, size=3)+

               geom_segment(data= arrow.lf , 
                            aes(x=0,y=0,xend=axis1,yend=axis2), 
                            arrow=arrow(length = unit(0.01, "npc")),
                            color='white', size=1)+
               geom_segment(data= arrow.lf , 
                            aes(x=0,y=0,xend=axis1,yend=axis2), 
                            arrow=arrow(length = unit(0.01, "npc")),
                            color='indianred3', size=0.8)+
               geom_text(data= arrow.lf ,aes(x=axis1,y= axis2, label= species))	+	
               facet_wrap(~cobn,ncol=length(dimension)-1, nrow=length(dimension)-1, scales=scales)+
               theme_minimal()+
               theme(panel.border=element_rect(fill=NA,color='grey40'),
                     strip.text =element_text(size=20))
  
  if(length(color.by)==0){
    gg.pca2 <- gg.pca+scale_color_manual(values='grey30') +
                      theme(legend.position='')
  }else{
    if(class(sample[,color.by])=='numeric'){
      gg.pca2 <- gg.pca+scale_color_gradientn(colors=brewer.pal)
    }else{
      gg.pca2 <- gg.pca+scale_color_manual(values=palettes(sample[,color.by]))
    }
  }
  if(length(shape)==0){
    gg.pca3 <- gg.pca2+scale_shape_manual(values=19)
  }else{
    gg.pca3 <- gg.pca2+scale_shape_manual(values=sample[,shape])
  }
  gg.pca3
  
}
