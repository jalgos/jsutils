#ifndef __JSUTILS_H_INCLUDED__
#define __JSUTILS_H_INCLUDED__
#include <boost/date_time/gregorian/greg_month.hpp>
#include <boost/date_time/gregorian/gregorian.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/math/common_factor.hpp>


#define __TIME_MS__ boost::posix_time::to_iso_extended_string(boost::posix_time::microsec_clock::universal_time())
#define JLOG(x) Rcpp::Rcout << __TIME_MS__ << ": " <<  x << std::endl;

#endif
