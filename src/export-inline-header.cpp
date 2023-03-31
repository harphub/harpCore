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

// [[Rcpp::export]]
NumericMatrix cpp_geolist_mean(List geolist, bool na_rm = false) {
  return harpCore::cpp_geolist_mean(geolist, na_rm);
}

// [[Rcpp::export]]
NumericMatrix cpp_geolist_sum(List geolist, bool na_rm = false) {
  return harpCore::cpp_geolist_sum(geolist, na_rm);
}

// [[Rcpp::export]]
NumericMatrix cpp_geolist_prod(List geolist, bool na_rm = false) {
  return harpCore::cpp_geolist_prod(geolist, na_rm);
}

// [[Rcpp::export]]
NumericMatrix cpp_geolist_min(List geolist, bool na_rm = false) {
  return harpCore::cpp_geolist_min(geolist, na_rm);
}

// [[Rcpp::export]]
NumericMatrix cpp_geolist_max(List geolist, bool na_rm = false) {
  return harpCore::cpp_geolist_max(geolist, na_rm);
}

// [[Rcpp::export]]
NumericMatrix cpp_geolist_var_sd(List geolist, bool na_rm = false, bool var = true) {
  return harpCore::cpp_geolist_var_sd(geolist, na_rm, var);
}


