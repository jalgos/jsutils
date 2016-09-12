//[[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>

using namespace Rcpp;


int hash_string(const StringVector& sv, int index)
{
    int hush = 0;
    for(int i = 0; i < sv[index].size(); i++)
    {
	hush += (int) sv[index][i];
    }
    return(hush);
}

//In place assignation for lists
//[[Rcpp::export]]
IntegerVector hash_string_vector(const StringVector& sv)
{
    IntegerVector res(sv.size());
    for(int i = 0; i < sv.size(); i++)
    {
	res[i] = hash_string(sv, i);
    }
    return(res);
} 
