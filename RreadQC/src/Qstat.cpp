
// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <RcppArmadilloExtensions/sample.h>

using namespace arma;
using namespace Rcpp;

colvec NAomit(arma::vec x){
    
    x=x.replace( datum::nan, 0 );
    return x.elem(find(x>0) );
    
}

// [[Rcpp::export]]
double maxEE(arma::vec x) {
    
    int len=x.n_cols;
    vec ee=zeros(1,1);
    
    x=(x*(-1))*0.1 ;
    
    for( int i=0; i<len; i++){
        ee+= pow(10, x(i,0));
    }
    
    return conv_to<double>::from(ee) ;
}

// [[Rcpp::export]]
arma::mat Qstat(arma::mat x) {
    
    int itr = x.n_rows;
    mat res=zeros(itr, 3);
    
    for( int i=0; i<itr; i++){
        
        res(i, 0)+=x.row(i).max();
        res(i, 1)+=x.row(i).min();
        
        res(i, 2)+=maxEE(NAomit( conv_to<vec>::from(x.row(i)) )) ;
    }
    
    return res ;
}
