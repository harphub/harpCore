#ifndef harpCore_geolistMath_H
#define harpCore_geolistMath_H

#include <cmath>
#include <Rcpp.h>
using namespace Rcpp;

namespace harpCore{

inline NumericMatrix cpp_geolist_mean(List geolist, bool na_rm = false){
  int i, j, k, ni, nj, nk = geolist.size();
  NumericMatrix tmp = geolist[0];
  ni = tmp.nrow();
  nj = tmp.ncol();
  NumericMatrix result(ni, nj);
  NumericMatrix count(ni, nj);

  if (na_rm) {
    for (k = 0; k < nk; k++) {
      NumericMatrix geofield = geolist[k];
      for (j = 0; j < nj; j++) {
        for (i = 0; i < ni; i++) {
          result(i, j) += NumericVector::is_na(geofield(i, j)) ? 0 : geofield(i, j);
          count(i, j)  += NumericVector::is_na(geofield(i, j)) ? 0 : 1;
        }
      }
    }
  } else {
    for (k = 0; k < nk; k++) {
      NumericMatrix geofield = geolist[k];
      for (j = 0; j < nj; j++) {
        for (i = 0; i < ni; i++) {
          result(i, j) += geofield(i, j);
        }
      }
    }
  }

  if (na_rm) {
    for (j = 0; j < nj; j++) {
      for (i = 0; i < ni; i++) {
        result(i, j) /= count(i, j);
      }
    }
  } else {
    for (j = 0; j < nj; j++) {
      for (i = 0; i < ni; i++) {
        result(i, j) /= nk;
      }
    }
  }
  return result;
}

inline NumericMatrix cpp_geolist_sum(List geolist, bool na_rm = false) {
  int i, j, k, ni, nj, nk = geolist.size();
  NumericMatrix tmp = geolist[0];
  ni = tmp.nrow();
  nj = tmp.ncol();
  NumericMatrix result(ni, nj);

  if (na_rm) {
    for (k = 0; k < nk; k++) {
      NumericMatrix geofield = geolist[k];
      for (j = 0; j < nj; j++) {
        for (i = 0; i < ni; i++) {
          result(i, j) += NumericVector::is_na(geofield(i, j)) ? 0 : geofield(i, j);
        }
      }
    }
  } else {
    for (k = 0; k < nk; k++) {
      NumericMatrix geofield = geolist[k];
      for (j = 0; j < nj; j++) {
        for (i = 0; i < ni; i++) {
          result(i, j) += geofield(i, j);
        }
      }
    }
  }

  return result;
}

inline NumericMatrix cpp_geolist_prod(List geolist, bool na_rm = false) {
  int i, j, k, ni, nj, nk = geolist.size();
  NumericMatrix start = geolist[0];
  ni = start.nrow();
  nj = start.ncol();
  NumericMatrix result(ni, nj);
  result = clone(start);

  if (na_rm) {
    for (k = 1; k < nk; k++) {
      NumericMatrix geofield = geolist[k];
      for (j = 0; j < nj; j++) {
        for (i = 0; i < ni; i++) {
          result(i, j) *= NumericVector::is_na(geofield(i, j)) ? 1 : geofield(i, j);
        }
      }
    }
  } else {
    for (k = 1; k < nk; k++) {
      NumericMatrix geofield = geolist[k];
      for (j = 0; j < nj; j++) {
        for (i = 0; i < ni; i++) {
          result(i, j) *= geofield(i, j);
        }
      }
    }
  }

  return result;
}


inline NumericMatrix cpp_geolist_min(List geolist, bool na_rm = false) {
  int i, j, k, ni, nj, nk = geolist.size();
  NumericMatrix start = geolist[0];
  ni = start.nrow();
  nj = start.ncol();
  NumericMatrix result(ni, nj);
  result = clone(start);

  if (na_rm) {
    for (k = 1; k < nk; k++) {
      NumericMatrix geofield = geolist[k];
      for (j = 0; j < nj; j++) {
        for (i = 0; i < ni; i++) {
          result(i, j) = NumericVector::is_na(result(i, j))
          ? geofield(i, j)
            : NumericVector::is_na(geofield(i, j))
          ? result(i, j)
            : std::min(result(i, j), geofield(i, j));
        }
      }
    }
  } else {
    for (k = 1; k < nk; k++) {
      NumericMatrix geofield = geolist[k];
      for (j = 0; j < nj; j++) {
        for (i = 0; i < ni; i++) {
          result(i, j) = NumericVector::is_na(result(i, j)) || NumericVector::is_na(geofield(i, j))
          ? NA_REAL
          : std::min(result(i, j), geofield(i, j));
        }
      }
    }
  }

  return result;
}

inline NumericMatrix cpp_geolist_max(List geolist, bool na_rm = false) {
  int i, j, k, ni, nj, nk = geolist.size();
  NumericMatrix start = geolist[0];
  ni = start.nrow();
  nj = start.ncol();
  NumericMatrix result(ni, nj);
  result = clone(start);

  if (na_rm) {
    for (k = 1; k < nk; k++) {
      NumericMatrix geofield = geolist[k];
      for (j = 0; j < nj; j++) {
        for (i = 0; i < ni; i++) {
          result(i, j) = NumericVector::is_na(result(i, j))
          ? geofield(i, j)
            : NumericVector::is_na(geofield(i, j))
          ? result(i, j)
            : std::max(result(i, j), geofield(i, j));
        }
      }
    }
  } else {
    for (k = 1; k < nk; k++) {
      NumericMatrix geofield = geolist[k];
      for (j = 0; j < nj; j++) {
        for (i = 0; i < ni; i++) {
          result(i, j) = NumericVector::is_na(result(i, j)) || NumericVector::is_na(geofield(i, j))
          ? NA_REAL
          : std::max(result(i, j), geofield(i, j));
        }
      }
    }
  }

  return result;
}

inline LogicalMatrix cpp_geolist_any(List geolist, bool na_rm = false) {
  int i, j, k, ni, nj, nk = geolist.size();
  LogicalMatrix result = geolist[0];
  ni = result.nrow();
  nj = result.ncol();

  if (na_rm) {
    for (k = 1; k < nk; k++) {
      LogicalMatrix geofield = geolist[k];
      for (j = 0; j < nj; j++) {
        for (i = 0; i < ni; i++) {
          result(i, j) = LogicalVector::is_na(result(i, j))
          ? geofield(i, j)
            : LogicalVector::is_na(geofield(i, j))
          ? result(i, j)
            : result(i, j) || geofield(i, j);
        }
      }
    }
  } else {
    for (k = 1; k < nk; k++) {
      LogicalMatrix geofield = geolist[k];
      for (j = 0; j < nj; j++) {
        for (i = 0; i < ni; i++) {
          result(i, j) = LogicalVector::is_na(result(i, j)) || LogicalVector::is_na(geofield(i, j))
          ? NA_LOGICAL
          : result(i, j) || geofield(i, j);
        }
      }
    }
  }

  return result;
}

inline LogicalMatrix cpp_geolist_all(List geolist, bool na_rm = false) {
  int i, j, k, ni, nj, nk = geolist.size();
  LogicalMatrix start = geolist[0];
  ni = start.nrow();
  nj = start.ncol();
  LogicalMatrix result(ni, nj);
  result = clone(start);

  if (na_rm) {
    for (k = 1; k < nk; k++) {
      LogicalMatrix geofield = geolist[k];
      for (j = 0; j < nj; j++) {
        for (i = 0; i < ni; i++) {
          result(i, j) = LogicalVector::is_na(result(i, j))
          ? geofield(i, j)
            : LogicalVector::is_na(geofield(i, j))
          ? result(i, j)
            : result(i, j) && geofield(i, j);
        }
      }
    }
  } else {
    for (k = 1; k < nk; k++) {
      LogicalMatrix geofield = geolist[k];
      for (j = 0; j < nj; j++) {
        for (i = 0; i < ni; i++) {
          result(i, j) = is_na(all(LogicalVector::create(result(i, j), geofield(i, j))))
          ? NA_LOGICAL
          : is_true(all(LogicalVector::create(result(i, j), geofield(i, j))));
        }
      }
    }
  }

  return result;
}

inline NumericMatrix cpp_geolist_var_sd(List geolist, bool na_rm = false, bool var = true) {
  int i, j, k, ni, nj, nk = geolist.size();
  NumericMatrix mean = cpp_geolist_mean(geolist, na_rm);
  ni = mean.nrow();
  nj = mean.ncol();
  NumericMatrix result(ni, nj);
  NumericMatrix count(ni, nj);

  if (na_rm) {
    for (k = 0; k < nk; k++) {
      NumericMatrix geofield = geolist[k];
      for (j = 0; j < nj; j++) {
        for (i = 0; i < ni; i++) {
          result(i, j) += NumericVector::is_na(geofield(i, j)) ? 0 : pow((geofield(i, j) - mean(i, j)), 2);
          count(i, j)  += NumericVector::is_na(geofield(i, j)) ? 0 : 1;
        }
      }
    }
  } else {
    for (k = 0; k < nk; k++) {
      NumericMatrix geofield = geolist[k];
      for (j = 0; j < nj; j++) {
        for (i = 0; i < ni; i++) {
          result(i, j) += pow((geofield(i, j) - mean(i, j)), 2);
        }
      }
    }
  }

  if (var) {
    if (na_rm) {
      for (j = 0; j < nj; j++) {
        for (i = 0; i < ni; i++) {
          result(i, j) = count(i, j) < 2 ? NA_REAL : result(i, j) / (count(i, j) - 1);
        }
      }
    } else {
      for (j = 0; j < nj; j++) {
        for (i = 0; i < ni; i++) {
          result(i, j) = nk < 2 ? NA_REAL : result(i, j) / (nk - 1);
        }
      }
    }
    return result;
  }
  if (na_rm) {
    for (j = 0; j < nj; j++) {
      for (i = 0; i < ni; i++) {
        result(i, j) = count(i, j) < 2 ? NA_REAL : sqrt(result(i, j) / (count(i, j) - 1));
      }
    }
  } else {
    for (j = 0; j < nj; j++) {
      for (i = 0; i < ni; i++) {
        result(i, j) = nk < 2 ? NA_REAL : sqrt(result(i, j) / (nk - 1));
      }
    }
  }
  return result;
}

}

#endif
