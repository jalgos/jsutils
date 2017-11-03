//[[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
#include <map>
#include <unordered_map>
#include "jsutils.h"

using namespace Rcpp;
// rc_sparse_rep for row or column sparse representation
typedef std::map <int, double>  vec_rep;
typedef std::map <int, vec_rep>  rc_sparse_rep;
typedef std::unordered_map <int, double>  un_vec_rep;
typedef std::unordered_map <int, un_vec_rep> un_rc_sparse_rep;
typedef std::unordered_set <int> vec_proj;
typedef std::unordered_map <int, vec_proj> proj_rep;
// First index is the row
template <typename T> T create_rc_sparse_rep(const IntegerVector& i, 
					     const IntegerVector& j, 
					     const NumericVector& x)
{
    T lmap;
    int nels = i.size();
    for(int ct = 0; ct < nels; ct++)
    {
	lmap[i[ct]][j[ct]] += x[ct];
    }
    return lmap;
}

template <typename T> T create_rc_sparse_rep(const List& tr,
					     const std::string& ivar = "i",
					     const std::string& jvar = "j",
					     const std::string& xvar = "x")
{
    return create_rc_sparse_rep<T>(as<IntegerVector>(tr[ivar]),
				   as<IntegerVector>(tr[jvar]),			  
				   as<NumericVector>(tr[xvar]));
}

template <typename T, typename Y> List rc_sparse_rep_to_list(const T& rcs)
{
    int tot_size(0);
    for(typename T::const_iterator it_rcs = rcs.begin(); it_rcs != rcs.end(); it_rcs++)
    {
	tot_size += it_rcs->second.size();
    }
  
    IntegerVector ires(tot_size);
    IntegerVector jres(tot_size);
    NumericVector xres(tot_size);
  
    int trip_count(0);
    for(typename T::const_iterator it_rcs = rcs.begin(); it_rcs != rcs.end(); it_rcs++)
    {
	for(typename Y::const_iterator it_row = it_rcs->second.begin(); it_row != it_rcs->second.end(); it_row++)
	{
	    ires[trip_count] = it_rcs->first;
	    jres[trip_count] = it_row->first;
	    xres[trip_count] = it_row->second;
	    trip_count ++;
	}
    }
    return List::create(Rcpp::Named("i") = ires,
			Rcpp::Named("j") = jres,
			Rcpp::Named("x") = xres);
}

template <typename T> typename std::map<int, T>::iterator find_or_insert(std::map<int, T>& data,
									 typename std::map<int, T>::iterator it_from, // C++ antics
									 int key_insert)
{
    while(it_from == data.end() || it_from->first != key_insert)
    {
	if(it_from == data.end() || it_from->first > key_insert)
	{
	    it_from = data.insert(it_from, std::pair<int, T>(key_insert, T()));
	}
	else 
	    it_from ++;       
    }
    return it_from;
}

// Implementation that represents both matrices as ordered map with different side major (column major for LHS and row major for RHS). The algorithm iterates over the side. The creation of each representation is costly.
//[[Rcpp::export]]
List triplet_prod(const IntegerVector& i1,
		  const IntegerVector& j1,
		  const NumericVector& x1,
		  const IntegerVector& i2,
		  const IntegerVector& j2,
		  const NumericVector& x2)
{
    rc_sparse_rep ltriplet = create_rc_sparse_rep<rc_sparse_rep>(j1, i1, x1);
    rc_sparse_rep rtriplet = create_rc_sparse_rep<rc_sparse_rep>(i2, j2, x2);
    rc_sparse_rep prod;
  
    std::vector<int> ires;
    std::vector<int> jres;
    std::vector<double> xres;
  
    rc_sparse_rep::iterator lit = ltriplet.begin();
    rc_sparse_rep::iterator rit = rtriplet.begin();
    while(lit != ltriplet.end() && rit != rtriplet.end())
    {
	Rcpp::checkUserInterrupt();
	if(lit->first < rit->first)
	{
	    lit++;
	    continue;
	}
	else if(lit->first > rit->first)
	{
	    rit++;
	    continue;
	}
	for(vec_rep::iterator clit = lit->second.begin(); clit != lit->second.end(); clit++)
	{
	    vec_rep& prod_row = prod[clit->first];
	    for(vec_rep::iterator rrit = rit->second.begin(); rrit != rit->second.end(); rrit++)
	    {
		prod_row[rrit->first] += rrit->second * clit->second;
	    }	  
	}
	lit++;
	rit++;
    }
    return rc_sparse_rep_to_list<rc_sparse_rep, vec_rep>(prod);
}

// The following uses an unordered map as a container (a regular hash map). Only RHS is converted. Value of LHS is looked up in RHS then.
//[[Rcpp::export]]
List triplet_prod_un(const IntegerVector& i1,
		     const IntegerVector& j1,
		     const NumericVector& x1,
		     const IntegerVector& i2,
		     const IntegerVector& j2,
		     const NumericVector& x2)
{
    un_rc_sparse_rep rtriplet = create_rc_sparse_rep<un_rc_sparse_rep>(i2, j2, x2);
    un_rc_sparse_rep prod;
    int rn = i1.size();
    for(int ct = 0; ct < rn; ct ++)
    {
	Rcpp::checkUserInterrupt();
	un_rc_sparse_rep::iterator rrow = rtriplet.find(j1[ct]);
	if(rrow == rtriplet.end())
	    continue;
	un_vec_rep& prod_row = prod[i1[ct]];
	for(un_vec_rep::iterator it = rrow->second.begin(); it != rrow->second.end(); it++)
	{
	    prod_row[it->first] += it->second * x1[ct];
	}
    }
    return rc_sparse_rep_to_list<un_rc_sparse_rep, un_vec_rep>(prod);
}

proj_rep create_proj_rep(const IntegerVector& proj,
			 int nside2)
{
    proj_rep mproj;
    int nels = proj.size();
  
    for(int ct = 0; ct < nels; ct++)
    {
	// R Indexing	
	int lindex = (proj[ct] - 1) / nside2 + 1;
	int rindex = (proj[ct] - 1) % nside2 + 1;
	mproj[lindex].insert(rindex);
    }
    return mproj;
  
}

//[[Rcpp::export]]
List partial_kronecker(const List& trl,
		       const List& trr,
		       const SEXP& projl,
		       const SEXP& projr,
		       const IntegerVector& dim2)
{
  
    proj_rep mprojl;
    proj_rep mprojr;
    if(projl != R_NilValue) mprojl = create_proj_rep(projl, dim2[0]);
    if(projr != R_NilValue) mprojr = create_proj_rep(projr, dim2[1]);
    auto mtrl = create_rc_sparse_rep<un_rc_sparse_rep>(trl);
    auto mtrr = create_rc_sparse_rep<un_rc_sparse_rep>(trr);
    un_rc_sparse_rep prod;
    for(auto it_rowl = mtrl.begin(); it_rowl != mtrl.end(); it_rowl++)
    { 
	Rcpp::checkUserInterrupt();
	proj_rep::iterator it_cprojl;
	if(projl != R_NilValue)
	{
	    it_cprojl = mprojl.find(it_rowl->first);
	    if(it_cprojl == mprojl.end())
		continue;
	}

	for(auto it_rowr = mtrr.begin(); it_rowr != mtrr.end(); it_rowr++)
	{
	    if(projl != R_NilValue && it_cprojl->second.find(it_rowr->first) == it_cprojl->second.end())
		continue;
	    // We've established that row i1 of LHS and i2 of LHS contribute to the final result
	    for(auto it_ell = it_rowl->second.begin(); it_ell != it_rowl->second.end(); it_ell++)
	    {
		proj_rep::iterator it_rprojr;
		if(projr != R_NilValue)
		{
		    it_rprojr = mprojr.find(it_ell->first);
		    if(it_rprojr == mprojr.end())
			continue;
		}
		for(auto it_elr = it_rowr->second.begin(); it_elr != it_rowr->second.end(); it_elr++)
		{
		  
		    if(projr != R_NilValue && it_rprojr->second.find(it_elr->first) ==  it_rprojr->second.end())
			continue;
		  
		    int ikron = (it_rowl->first - 1) * dim2[0] + it_rowr->first;
		    int jkron = (it_ell->first - 1) * dim2[1] + it_elr->first;
		    prod[ikron][jkron] = it_elr->second * it_ell->second;
		}
	    }
	}
    }
  
    return rc_sparse_rep_to_list<un_rc_sparse_rep, un_vec_rep>(prod);
}

// We assume that matrices are ordered by the colum/ row if LHS and row/ colum if RHS
//[[Rcpp::export]]
SEXP triplet_prod_preordered(const SEXP& i1,
			     const SEXP& j1,
			     const SEXP& x1,
			     const SEXP& i2,
			     const SEXP& j2,
			     const SEXP& x2)
{
    int cur1 = 0;
    int cur2 = 0;
    int n1 = LENGTH(i1) - 1;
    int n2 = LENGTH(i2) - 1;
    int ci1 = INTEGER(i1)[cur1];
    int ci2 = INTEGER(i2)[cur2];
    int cj1 = INTEGER(j1)[cur1];
    int cj2 = INTEGER(j2)[cur2];
    rc_sparse_rep prod;
    while(true)
    {
	
	while(cj1 != ci2)
	{
	    if(cj1 > ci2)
	    {
		cur2++;
		if(cur2 > n2)
		    break;				
	    }
	    else
	    {
		cur1++;
		if(cur1 > n1)
		    break;	    
	    }	    
	    ci2 = INTEGER(i2)[cur2];
	    cj1 = INTEGER(j1)[cur1];		
	   
	}

	int down2 = cur2;
	while(true)
	{
	    cur2 = down2;	
	    ci1 = INTEGER(i1)[cur1];
	    rc_sparse_rep::iterator it_prod_row = prod.insert(prod.begin(), std::make_pair(ci1, vec_rep()));
	    vec_rep::iterator it_prod_val = it_prod_row->second.begin();	    
	    ci2 = INTEGER(i2)[cur2];
	    while(cj1 == ci2)
	    {
		cj2 = INTEGER(j2)[cur2];			
		it_prod_val = it_prod_row->second.insert(it_prod_val, std::make_pair(cj2, 0));
		     
		it_prod_val->second += REAL(x1)[cur1] * REAL(x2)[cur2];
		cur2++;
		if(cur2 > n2)
		    break;;
		ci2 = INTEGER(i2)[cur2];
	    }
	    cur1++;
	    if(cur1 > n1)
		break;
	    int prevcj1 = cj1;
	    cj1 = INTEGER(j1)[cur1];
	    if(cj1 != prevcj1)
		break;
	}
	
	if(cur1 > n1 || cur2 > n2)
	    break;
    }
    
    return rc_sparse_rep_to_list<rc_sparse_rep, vec_rep>(prod);
}
