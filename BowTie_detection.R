############################################################################
####
#### For the network analysis
#### Detection which part in bow-tiestructure the nodes belong to.
#### 2019.11.28 Fujita modified and development 'Tekemoto lab. Turorials on R+igraph'
#### R 3.6.0
#### 
#### Reference 
#### 1. Yang et al., 2011, 14th International Conference on Information Fusion,
#### 	'Bow-tie decomposition in directed graphs'
#### 2. Tekemoto lab. Turorials on R+igraph
#### 	URL:https://sites.google.com/view/takemotolab/r-igraph
############################################################################

# -- bow-tie structure

bow.tie <- function(adj=x){ 
	
	#################################################################
	# 
	# x is a adjacency matrix
	#
	# This function will return the In, Out, Core, Intendrils,
	# Outtendrils and Tube. If the node is not belong to any part, 
	# then return 'Others'
	#
	# This functions also return 'all' list. 
	# '1' represents In, '2' represents Out, '3' represents Core,
	# '4' represents Tube, '5' represents Intendrils, '6' represents 
	# Outtendrils and '7' represents Others
	#################################################################
	
	graph <- graph.adjacency(adj, mode='directed', weighted=TRUE)
	
	# -- Core nodes (stongly connected components)
	comp <- components(graph, mode =  "strong")
	SCC = names(which( comp$membership == order(comp$csize,decreasing=TRUE)[1] ))
	
	no.SCC <- setdiff(V(graph)$name,SCC)
	
	# -- Find reachable nodes from periphery 'In' to SCC
	min.path <- shortest.paths(graph,m="out") ; min.path[min.path==Inf] <- 0
	min.path[min.path==Inf] <- 0
	
	# nodes reach to SCC
	if( is.null(nrow(min.path[no.SCC, SCC])) ){
		to.scc <- names(which( min.path[no.SCC, SCC]>0 ))
	}else{
		to.scc <- names(which(rowSums(min.path[no.SCC, SCC])>0))
	}
	
	if( is.null(nrow(min.path[SCC, no.SCC])) ){
		from.scc <- names(which( min.path[SCC, no.SCC]>0 ))
	}else{
		from.scc <- names(which(colSums(min.path[SCC, no.SCC])>0))
	}
	if(length(from.scc)==0) from.scc<- to.scc	
	In <- setdiff(to.scc,from.scc)
	
	# -- Find reachable nodes from SCC to periphery 'Out'
	if( is.null(nrow(min.path[from.scc, SCC])) ){
		Out <- from.scc
	}else{
		Out<- names(which(rowSums(min.path[from.scc, SCC])==0))
	}
	
	# -- Others
	if(is.null(nrow(min.path[In, setdiff(no.SCC, c(In,Out))]))){
		Tube <- NA
	} else{ 
		if(is.null(nrow(min.path[In, setdiff(no.SCC, c(In,Out))])) | sum(min.path[In, setdiff(no.SCC, c(In,Out))])==0){
			Tube <- NA
		}else{
			in.tube <- names(which(colSums(min.path[In, setdiff(no.SCC, c(In,Out))])>0))		
			
			if( is.matrix(min.path[in.tube, Out]) )	{
				Tube <- names(which(rowSums(min.path[in.tube, Out])>0))
			}else{
				Tube <- NA			
			}
			
		}
	}
	
	if(is.null(nrow(min.path[In, setdiff(no.SCC, c(In))]))){
		indril.tmp <- names(which(min.path[In, setdiff(no.SCC, c(In))]>0))
		INTENDRILS <- indril.tmp[-which(indril.tmp%in%c(Out, Tube))]
	} else{ 
		indril.tmp <- names(which(colSums(min.path[In, setdiff(no.SCC, c(In))])>0))
		INTENDRILS <- indril.tmp[-which(indril.tmp%in%c(Out, Tube))]
	}
	
	if(is.null(nrow(min.path[setdiff(no.SCC, c(Out)),Out]))){
		indril.tmp <- names(which(min.path[setdiff(no.SCC, c(Out)),Out]>0))
		OUTTENDRILS <- indril.tmp[-which(indril.tmp%in%c(In, Tube))]
	} else{ 				   
		indril.tmp <- names(which(rowSums(min.path[setdiff(no.SCC, c(Out)),Out])>0))
		OUTTENDRILS <- indril.tmp[-which(indril.tmp%in%c(In, Tube))]
	}
	
	OTHERS <- setdiff(V(graph)$name, c(SCC, In, Out, Tube, INTENDRILS, OUTTENDRILS))
	
	# Result
	a <- list(In=ifelse(V(graph)$name%in%In, 1, 0), 
		 	  Out=ifelse(V(graph)$name%in%Out, 2, 0), 
		 	  Core = ifelse(V(graph)$name%in%SCC, 3, 0),
		 	  Tube = ifelse(V(graph)$name%in%Tube, 4, 0),
		 	  INTENDRILS = ifelse(V(graph)$name%in%INTENDRILS, 5, 0),
		 	  OUTTENDRILS = ifelse(V(graph)$name%in%OUTTENDRILS, 6, 0),
		 	  OTHERS = ifelse(V(graph)$name%in% OTHERS, 7, 0)
		 	  ) 
		
	b <- lapply(a, function(x){
				    names(x)<-V(graph)$name
					x} )	
	b[['all']] <- colSums(do.call(rbind,b))
	b
}