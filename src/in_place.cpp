//[[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>

using namespace Rcpp;


//In place assignation for lists
//[[Rcpp::export]]
SEXP inplace(List ls, List newls)
{
    for(int i = 0; i < newls.size(); i++)
    {
	ls[i] = newls[i];
    }
    return(ls);
} 
