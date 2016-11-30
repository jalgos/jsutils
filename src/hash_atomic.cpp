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

//In place assignation for lists
//[[Rcpp::export]]
IntegerVector hash_string_vector_std(const StringVector& sv)
{
    IntegerVector res(sv.size());
    for(int i = 0; i < sv.size(); i++)
    {
	res[i] = std::hash<std::string>{}(std::string(sv[i]));
    }
    return(res);
} 

//In place assignation for lists
//[[Rcpp::export]]
IntegerVector hash_numeric_vector_std(const NumericVector& sv)
{
    IntegerVector res(sv.size());
    for(int i = 0; i < sv.size(); i++)
    {
	res[i] = std::hash<double>{}(sv[i]);
    }
    return(res);
} 

//In place assignation for lists
//[[Rcpp::export]]
IntegerVector hash_integer_vector_std(const IntegerVector& sv)
{
    IntegerVector res(sv.size());
    for(int i = 0; i < sv.size(); i++)
    {
	if(IntegerVector::is_na(sv[i]))
	    res[i] = 0;
	else
	    res[i] = std::hash<int>{}(sv[i]);
    }
    return(res);
} 

//In place assignation for lists
//[[Rcpp::export]]
IntegerVector hash_logical_vector_std(const LogicalVector& sv)
{
    IntegerVector res(sv.size());
    for(int i = 0; i < sv.size(); i++)
    {
	if(LogicalVector::is_na(sv[i]))
	    res[i] = 2;
	else
	    res[i] = std::hash<bool>{}(sv[i]);
    }
    return(res);
} 
