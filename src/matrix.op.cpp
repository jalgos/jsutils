// Must set environment var: Sys.setenv("PKG_CXXFLAGS"="-std=c++11")
//[[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
#include <map>
#include <unordered_map>

using namespace Rcpp;
// rc_sparse_rep for row or column sparse representation
typedef std::map <int, std::map<int, double> >  rc_sparse_rep;
typedef std::map <int, double>  vec_rep;
typedef std::unordered_map <int, std::unordered_map<int, double> > un_rc_sparse_rep;
typedef std::unordered_map <int, double>  un_vec_rep;
typedef std::unordered_map <int, std::unordered_map<int, un_vec_rep> > proj_rep;
// First index is the row
template<typename T> T create_rc_sparse_rep(const IntegerVector& i, 
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

template<typename T> T create_rc_sparse_rep(const List& tr,
					    const std::string& ivar = "i",
					    const std::string& jvar = "j",
					    const std::string& xvar = "x")
{
  return create_rc_sparse_rep<T>(as<IntegerVector>(tr[ivar]),
				 as<IntegerVector>(tr[jvar]),			  
				 as<NumericVector>(tr[xvar]));
}

template<typename T, typename Y> List rc_sparse_rep_to_list(const T& rcs)
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

template<typename T> typename std::map<int, T>::iterator find_or_insert(std::map<int, T>& data,
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

proj_rep create_proj_rep(const List& proj,
			 int nside2,
			 const std::string& ivar = "i",
			 const std::string& jvar = "j",
			 const std::string& xvar = "x")
{
  proj_rep mproj;
  auto mj = as<IntegerVector>(proj[jvar]);
  auto mi = as<IntegerVector>(proj[ivar]);
  auto mx = as<NumericVector>(proj[xvar]);
  int nels = mx.size();
  
  for(int ct = 0; ct < nels; ct++)
    {
      // R Indexing
      int lindex = (mj[ct] - 1) / nside2 + 1;
      int rindex = (mj[ct] - 1) % nside2 + 1;
      mproj[lindex][rindex][mi[ct]] = mx[ct];
    }
  return mproj;
}

//[[Rcpp::export]]
List partial_kronecker(const List& trl,
		       const List& trr,
		       const List& projl,
		       const List& projr,
		       const IntegerVector& dim2)
{
  
  auto mprojl = create_proj_rep(projl, dim2[0]);
  auto mprojr = create_proj_rep(projl, dim2[1], "j", "i");
  auto mtrl = create_rc_sparse_rep<un_rc_sparse_rep>(trl);
  auto mtrr = create_rc_sparse_rep<un_rc_sparse_rep>(trl);
  un_rc_sparse_rep prod;
  for(auto it_rowl = mtrl.begin(); it_rowl != mtrl.end(); it_rowl++)
    {
      auto it_cprojl = mprojl.find(it_rowl->first);
      if(it_cprojl == mprojl.end())
	continue;

      for(auto it_rowr = mtrr.begin(); it_rowr != mtrr.end(); it_rowr++)
	{
	  auto it_ccprojl = it_cprojl->second.find(it_rowr->first);
	  if(it_ccprojl == it_cprojl->second.end())
	    continue;
	  // We've established that row i1 of LHS and i2 of LHS contribute to the final result
	  for(auto it_ell = it_rowl->second.begin(); it_ell != it_rowl->second.end(); it_ell++)
	    {
	      auto it_rprojr = mprojr.find(it_ell->first);
	      if(it_rprojr == mprojr.end())
		continue;
	      for(auto it_elr = it_rowr->second.begin(); it_elr != it_rowr->second.end(); it_elr++)
		{
		  auto it_rrprojr = it_rprojr->second.find(it_elr->first);
		  if(it_rrprojr == it_rprojr->second.end())
		    continue;
		  int ikron = (it_rowl->first - 1) * dim2[0] + it_rowr->first;
		  int jkron = (it_ell->first - 1) * dim2[1] + it_elr->first;
		  //Rcout << "Multiplication: " << it_rowl->first << " " << it_rowr->first << " " << it_ell->first << " " << it_elr->first <<  " kron indices: " << ikron << " " << jkron << " Values: " << it_elr->second << " * " << it_ell->second << std::endl;
		  prod[ikron][jkron] = it_elr->second * it_ell->second;
		}
	    }
	}
    }
  
  return rc_sparse_rep_to_list<un_rc_sparse_rep, un_vec_rep>(prod);
}
