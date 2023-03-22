#ifndef harpCore_nbhdMean_H
#define harpCore_nbhdMean_H


#include <Rcpp.h>
#include "cumsumComparators.h"
using namespace Rcpp;

namespace harpCore{

inline NumericMatrix cpp_cumsum2d(
    NumericMatrix indat,
    NumericVector threshold,
    String comparator = "ge",
    bool includeLow = true,
    bool includeHigh = true
) {
  int i, j, ni = indat.nrow(), nj = indat.ncol();
  NumericMatrix result(ni, nj);

  if (NumericVector::is_na(threshold[0])) {

    for (j = 0; j < nj; j++) {
      result(0, j) = indat(0, j);
      for (i = 1; i < ni; i++) {
        result(i, j) = indat(i, j) + result(i - 1, j);
      }
    }

    for (j = 1; j < nj; j++) {
      for (i = 0; i < ni; i++) {
        result(i, j) += result(i, j - 1);
      }
    }

  } else {

    if (comparator == "ge") {
      result = cumsumGreaterThanEqual(indat, threshold, ni, nj);
    }
    if (comparator == "gt") {
      result = cumsumGreaterThan(indat, threshold, ni, nj);
    }
    if (comparator == "le") {
      result = cumsumLessThanEqual(indat, threshold, ni, nj);
    }
    if (comparator == "lt") {
      result = cumsumLessThan(indat, threshold, ni, nj);
    }
    if (comparator == "between") {
      result = cumsumBetween(indat, threshold, ni, nj, includeLow, includeHigh);
    }
    if (comparator == "outside") {
      result = cumsumBetween(indat, threshold, ni, nj, includeLow, includeHigh);
    }

  }

  return result;
}

inline NumericMatrix cpp_nbhd_smooth_cumsum(NumericMatrix indat, int rad, String boundaryCondition = "zero_pad") {
  int i, j, ni = indat.nrow(), nj = indat.ncol();
  int istart, jstart, iend, jend, imax, jmax;
  NumericMatrix result(ni, nj);

  if (boundaryCondition == "missing") {
    std::fill(result.begin(), result.end(), NumericVector::get_na());
  }

  // Version that does "zero padding":
  if (boundaryCondition == "zero_pad") {
    istart = 0;
    jstart = 0;
    iend = ni;
    jend = nj;
  } else if (boundaryCondition == "missing") {
    istart = rad;
    jstart = rad;
    iend = ni - rad;
    jend = nj - rad;
  }

  for (j = jstart; j < jend; j++) {
    jmax = std::min(j + rad, nj - 1) ;
    for(i = istart; i < iend; i++) {

      imax = std::min(i + rad, ni - 1) ;
      result(i, j) = indat(imax, jmax) ;

      if (j > rad) {
        result(i, j) -= indat(imax, j - rad - 1);
        if (i > rad) {
          result(i, j) += indat(i - rad - 1, j - rad - 1) - indat(i - rad - 1, jmax);
        }
      } else if (i > rad) {
        result(i, j) -= indat(i - rad - 1, jmax);
      }

      result(i, j) /= ((rad * 2 + 1) * (rad * 2 + 1));
    }
  }

  return result;

}

inline NumericMatrix cpp_nbhd_smooth(
    NumericMatrix x,
    int rad,
    NumericVector threshold,
    String comparator = "ge",
    bool includeLow = true,
    bool includeHigh = true,
    String boundaryCondition = "zero_pad"
) {
  return cpp_nbhd_smooth_cumsum(cpp_cumsum2d(x, threshold, comparator, includeLow, includeHigh), rad, boundaryCondition);
}
}

#endif
