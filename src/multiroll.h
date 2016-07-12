#ifndef __MULTI_ROLL_H_INCLUDED__
#define __MULTI_ROLL_H_INCLUDED__

//[[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
#include <map>
#include <vector>

using namespace Rcpp;


template <typename T>
class Matcher
{
    typedef typename std::vector<std::pair< T, Matcher > > MatchMap;
    typedef typename MatchMap::const_iterator MatchIter;
    MatchMap m_matcher;
    MatchIter m_iter;
    int m_speed_incr;
    int m_nmatch;
public:
    int match(const DataFrame& data_match,
	      int row_index,
	      int col_index);

    virtual MatchIter approximatch(MatchIter itinf,
				   MatchIter itsup);
    
    MatchIter
    find_ordered(const T1& val,
		 MatchIter itinf,
		 MatchIter itsup)
    {
	if(itinf->first > val)
	    Rf_Error("value fed is lower than lower bound");
	
	if(itsup->first < val)
	Rf_Error("value fed is higher than higher bound");

	int speed_incr = m_speed_incr;
	std::advance(itguess, speed_incr);
	while(true)
	{
	    if(itguess->first == val)
	    {
		m_speed_incr = (double)(m_nmatch * m_speed_incr + (itguess - itinf)) / (m_nmatch + 1);
		m_nmatch ++;
		return(itguess->second);
	    }
	    MatchIter itnext = itguess;
	    std::advance(itnext, 1);
	    if(itguess->first < val & itnext->first > val)
	    {
		return(approximatch(itguess, itnext));
	    }
	    
	    if(itguess->first < val)
	    {
		speed_incr *= 2;
		itguess += speed_incr;
		std::advance(itguess, speed_incr)
	    }
	    else
	    {
		speed_incr \= 2;
		if(speed_incr == 0)
		    speed_incr = 1;
		std::advance(itguess, -speed_incr);
	    }
	};
    };

};

template <>
class MatchedIndex : public Matcher <>
{
    int m_index;
public:
    inline int match(const DataFrame& data_match,
		     int row_index,
		     int col_index)
    {
	return m_index;
    }
};

template <typename T>
class ExactMatcher : public Matcher<T>
{
    
public:
    int match(const DataFrame& data_match,
	      int row_index,
	      int col_index)
    {
	m_matcher 
    }
};





#endif
