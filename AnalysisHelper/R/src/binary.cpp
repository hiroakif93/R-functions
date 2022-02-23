#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix Binary(NumericMatrix mat, int th) {
	
	int threshold=th;
	
	int col=mat.nrow();
	int row=mat.nrow();
	
	for(int c = 0; c < col; c++){
		for(int r =0; r < row; r++){
			
			if( mat(c,r)>threshold ) {
				mat(c,r)=1;
			}else{
				mat(c,r)=0;
			}
		}
	}
	
	return(mat);
}	
