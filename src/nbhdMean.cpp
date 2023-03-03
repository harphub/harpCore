#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix cpp_cumsum2d(
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

    for (i = 0; i < ni; i++) {
      for (j = 1; j < ni; j++) {
        result(i, j) += result(i, j - 1);
      }
    }

  } else {

    for (j = 0; j < nj; j++) {

      if (comparator == "ge") {
        result(0, j) = (indat(0, j) >= threshold[0]);
      } else if (comparator == "gt") {
        result(0, j) = (indat(0, j) > threshold[0]);
      } else if (comparator == "le") {
        result(0, j) = (indat(0, j) <= threshold[0]);
      } else if (comparator == "lt") {
        result(0, j) = (indat(0, j) < threshold[0]);
      } else if (comparator == "between") {
        if (includeLow && includeHigh) {
          result(0, j) = (indat(0, j) >= threshold[0] && indat(0, j) <= threshold[1]);
        }
        if (includeLow && !includeHigh) {
          result(0, j) = (indat(0, j) >= threshold[0] && indat(0, j) < threshold[1]);
        }
        if (!includeLow && includeHigh) {
          result(0, j) = (indat(0, j) > threshold[0] && indat(0, j) <= threshold[1]);
        }
        if (!includeLow && !includeHigh) {
          result(0, j) = (indat(0, j) > threshold[0] && indat(0, j) < threshold[1]);
        }
      } else if (comparator == "outside") {
        if (includeLow && includeHigh) {
          result(0, j) = (indat(0, j) <= threshold[0] || indat(0, j) >= threshold[1]);
        }
        if (includeLow && !includeHigh) {
          result(0, j) = (indat(0, j) <= threshold[0] || indat(0, j) > threshold[1]);
        }
        if (!includeLow && includeHigh) {
          result(0, j) = (indat(0, j) < threshold[0] || indat(0, j) >= threshold[1]);
        }
        if (!includeLow && !includeHigh) {
          result(0, j) = (indat(0, j) < threshold[0] || indat(0, j) > threshold[1]);
        }
      }

      for (i = 1; i < ni; i++) {
        if (comparator == "ge") {
          result(i, j) = (indat(i, j) >= threshold[0]) + result(i - 1, j);
        }
        if (comparator == "gt") {
          result(i, j) = (indat(i, j) > threshold[0]) + result(i - 1, j);
        }
        if (comparator == "le") {
          result(i, j) = (indat(i, j) <= threshold[0]) + result(i - 1, j);
        }
        if (comparator == "lt") {
          result(i, j) = (indat(i, j) < threshold[0]) + result(i - 1, j);
        } else if (comparator == "between") {
          if (includeLow && includeHigh) {
            result(i, j) = (indat(i, j) >= threshold[0] && indat(i, j) <= threshold[1]) + result(i - 1, j);
          }
          if (includeLow && !includeHigh) {
            result(i, j) = (indat(i, j) >= threshold[0] && indat(i, j) < threshold[1]) + result(i - 1, j);
          }
          if (!includeLow && includeHigh) {
            result(i, j) = (indat(i, j) > threshold[0] && indat(i, j) <= threshold[1]) + result(i - 1, j);
          }
          if (!includeLow && !includeHigh) {
            result(i, j) = (indat(i, j) > threshold[0] && indat(i, j) < threshold[1]) + result(i - 1, j);
          }
        } else if (comparator == "outside") {
          if (includeLow && includeHigh) {
            result(i, j) = (indat(i, j) <= threshold[0] || indat(i, j) >= threshold[1]) + result(i - 1, j);
          }
          if (includeLow && !includeHigh) {
            result(i, j) = (indat(i, j) <= threshold[0] || indat(i, j) > threshold[1]) + result(i - 1, j);
          }
          if (!includeLow && includeHigh) {
            result(i, j) = (indat(i, j) < threshold[0] || indat(i, j) >= threshold[1]) + result(i - 1, j);
          }
          if (!includeLow && !includeHigh) {
            result(i, j) = (indat(i, j) < threshold[0] || indat(i, j) > threshold[1]) + result(i - 1, j);
          }
        }
      }

    }

    for (i = 0; i < ni; i++) {
      for (j = 1; j < nj; j++) {
        result(i, j) += result(i, j - 1);
      }
    }

  }

  return result;
}

// [[Rcpp::export]]
NumericMatrix cpp_nbhd_smooth_cumsum(NumericMatrix indat, int rad, String boundaryCondition = "zero_pad") {
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

  for (i = istart; i < iend; i++) {
    imax = std::min(i + rad, ni - 1) ;
    for(j = jstart; j < jend; j++) {

      jmax = std::min(j + rad, nj - 1) ;
      result(i, j) = indat(imax, jmax) ;

      if (i > rad) {
        result(i, j) -= indat(i - rad - 1, jmax);
        if (j > rad) {
          result(i, j) += indat(i - rad - 1, j - rad - 1) - indat(imax, j - rad - 1);
        }
      } else if (j > rad) {
        result(i, j) -= indat(imax, j - rad - 1);
      }

      result(i, j) /= ((rad * 2 + 1) * (rad * 2 + 1));
    }
  }

  return result;
}

// [[Rcpp::export]]
NumericMatrix cpp_nbhd_smooth(
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
