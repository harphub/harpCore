#include <harpCore.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix cpp_cumsum2d(
  NumericMatrix indat,
  NumericVector threshold,
  String comparator = "ge",
  bool includeLow = true,
  bool includeHigh = true
) {
  return harpCore::cpp_cumsum2d(indat, threshold, comparator, includeLow, includeHigh);
}

// [[Rcpp::export]]
NumericMatrix cpp_nbhd_smooth_cumsum(NumericMatrix indat, int rad, String boundaryCondition = "zero_pad") {
  return harpCore::cpp_nbhd_smooth_cumsum(indat, rad, boundaryCondition);
}

// [[Rcpp::export]]
NumericMatrix cpp_nbhd_smooth(
  NumericMatrix indat,
  int rad,
  NumericVector threshold,
  String comparator = "ge",
  bool includeLow = true,
  bool includeHigh = true,
  String boundaryCondition = "zero_pad"
) {
  return harpCore::cpp_nbhd_smooth(indat, rad, threshold, comparator, includeLow, includeHigh, boundaryCondition);
}




