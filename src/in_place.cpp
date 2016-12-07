//[[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>

//[[Rcpp::export]]
void inplace(SEXP target, SEXP src)
{
    for(int i = 0; i < LENGTH(src); i++)
    {
	SET_VECTOR_ELT(target, i, VECTOR_ELT(src, i));
    }
} 
