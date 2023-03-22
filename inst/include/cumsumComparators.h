#ifndef harpCore_cumsumComparators_H
#define harpCore_cumsumComparators_H

#include <Rcpp.h>
using namespace Rcpp;

inline NumericMatrix cumsumGreaterThanEqual(NumericMatrix indat, NumericVector threshold, int ni, int nj) {
  NumericMatrix result(ni, nj);
  int i, j;
  for (j = 0; j < nj; j++) {
    result(0, j) = (indat(0, j) >= threshold[0]);
    for (i = 1; i < ni; i++) {
      result(i, j) = (indat(i, j) >= threshold[0]) + result(i - 1, j);
    }
  }

  for (j = 1; j < nj; j++) {
    for (i = 0; i < ni; i++) {
      result(i, j) += result(i, j - 1);
    }
  }
  return result;
}

inline NumericMatrix cumsumGreaterThan(NumericMatrix indat, NumericVector threshold, int ni, int nj) {
  NumericMatrix result(ni, nj);
  int i, j;
  for (j = 0; j < nj; j++) {
    result(0, j) = (indat(0, j) > threshold[0]);
    for (i = 1; i < ni; i++) {
      result(i, j) = (indat(i, j) > threshold[0]) + result(i - 1, j);
    }
  }

  for (j = 1; j < nj; j++) {
    for (i = 0; i < ni; i++) {
      result(i, j) += result(i, j - 1);
    }
  }
  return result;
}

inline NumericMatrix cumsumLessThanEqual(NumericMatrix indat, NumericVector threshold, int ni, int nj) {
  NumericMatrix result(ni, nj);
  int i, j;
  for (j = 0; j < nj; j++) {
    result(0, j) = (indat(0, j) <= threshold[0]);
    for (i = 1; i < ni; i++) {
      result(i, j) = (indat(i, j) <= threshold[0]) + result(i - 1, j);
    }
  }

  for (j = 1; j < nj; j++) {
    for (i = 0; i < ni; i++) {
      result(i, j) += result(i, j - 1);
    }
  }
  return result;
}

inline NumericMatrix cumsumLessThan(NumericMatrix indat, NumericVector threshold, int ni, int nj) {
  NumericMatrix result(ni, nj);
  int i, j;
  for (j = 0; j < nj; j++) {
    result(0, j) = (indat(0, j) < threshold[0]);
    for (i = 1; i < ni; i++) {
      result(i, j) = (indat(i, j) < threshold[0]) + result(i - 1, j);
    }
  }

  for (j = 1; j < nj; j++) {
    for (i = 0; i < ni; i++) {
      result(i, j) += result(i, j - 1);
    }
  }
  return result;
}

inline NumericMatrix cumsumBetween(NumericMatrix indat, NumericVector threshold, int ni, int nj, bool includeLow, bool includeHigh) {

  NumericMatrix result(ni, nj);
  int i, j;
  float threshLow = min(threshold);
  float threshHigh = max(threshold);

  if (includeLow && includeHigh) {
    for (j = 0; j < nj; j++) {
      result(0, j) = (indat(0, j) >= threshLow && indat(0, j) <= threshHigh);
      for (i = 1; i < ni; i++) {
        result(i, j) = (indat(i, j) >= threshLow && indat(0, j) <= threshHigh) + result(i - 1, j);
      }
    }
  }

  if (includeLow && !includeHigh) {
    for (j = 0; j < nj; j++) {
      result(0, j) = (indat(0, j) >= threshLow && indat(0, j) < threshHigh);
      for (i = 1; i < ni; i++) {
        result(i, j) = (indat(i, j) >= threshLow && indat(0, j) < threshHigh) + result(i - 1, j);
      }
    }
  }

  if (!includeLow && includeHigh) {
    for (j = 0; j < nj; j++) {
      result(0, j) = (indat(0, j) > threshLow && indat(0, j) <= threshHigh);
      for (i = 1; i < ni; i++) {
        result(i, j) = (indat(i, j) > threshLow && indat(0, j) <= threshHigh) + result(i - 1, j);
      }
    }
  }

  if (!includeLow && !includeHigh) {
    for (j = 0; j < nj; j++) {
      result(0, j) = (indat(0, j) > threshLow && indat(0, j) < threshHigh);
      for (i = 1; i < ni; i++) {
        result(i, j) = (indat(i, j) > threshLow && indat(0, j) < threshHigh) + result(i - 1, j);
      }
    }
  }


  for (j = 1; j < nj; j++) {
    for (i = 0; i < ni; i++) {
      result(i, j) += result(i, j - 1);
    }
  }

  return result;
}

inline NumericMatrix cumsumOutside(NumericMatrix indat, NumericVector threshold, int ni, int nj, bool includeLow, bool includeHigh) {

  NumericMatrix result(ni, nj);
  int i, j;
  float threshLow = min(threshold);
  float threshHigh = max(threshold);

  if (includeLow && includeHigh) {
    for (j = 0; j < nj; j++) {
      result(0, j) = (indat(0, j) <= threshLow || indat(0, j) >= threshHigh);
      for (i = 1; i < ni; i++) {
        result(i, j) = (indat(i, j) <= threshLow || indat(0, j) >= threshHigh) + result(i - 1, j);
      }
    }
  }

  if (includeLow && !includeHigh) {
    for (j = 0; j < nj; j++) {
      result(0, j) = (indat(0, j) <= threshLow || indat(0, j) > threshHigh);
      for (i = 1; i < ni; i++) {
        result(i, j) = (indat(i, j) <= threshLow || indat(0, j) > threshHigh) + result(i - 1, j);
      }
    }
  }

  if (!includeLow && includeHigh) {
    for (j = 0; j < nj; j++) {
      result(0, j) = (indat(0, j) < threshLow || indat(0, j) >= threshHigh);
      for (i = 1; i < ni; i++) {
        result(i, j) = (indat(i, j) < threshLow || indat(0, j) >= threshHigh) + result(i - 1, j);
      }
    }
  }

  if (!includeLow && !includeHigh) {
    for (j = 0; j < nj; j++) {
      result(0, j) = (indat(0, j) < threshLow || indat(0, j) > threshHigh);
      for (i = 1; i < ni; i++) {
        result(i, j) = (indat(i, j) < threshLow || indat(0, j) > threshHigh) + result(i - 1, j);
      }
    }
  }


  for (j = 1; j < nj; j++) {
    for (i = 0; i < ni; i++) {
      result(i, j) += result(i, j - 1);
    }
  }

  return result;
}


#endif
