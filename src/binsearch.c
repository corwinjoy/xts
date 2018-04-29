#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>

/* Binary search range to find interval contributed by Corwin Joy */

/* Find the smallest index for which A[index] >= key */
int lower_bound(SEXP key, SEXP vec)
{
  int mid;
  int lo = 0;
  int hi = length(vec) - 1;

  switch (TYPEOF(vec)) {
    case REALSXP:
      {
        double *A = REAL(vec);
        double k = REAL(key)[0];
        while (lo < hi) {
          mid = lo + (hi - lo) / 2;
          if (A[mid] >= k) {
            hi = mid;
          }
          else {
            lo = mid + 1;
          }
        }
        /* start = TRUE, but entire array < key */
        if (A[lo] < k) {
          lo = NA_INTEGER;
        } else {
          lo++;
        }
      }
      break;
    case INTSXP:
      {
        int *A = INTEGER(vec);
        int k = INTEGER(key)[0];
        while (lo < hi) {
          mid = lo + (hi - lo) / 2;
          if (A[mid] >= k) {
            hi = mid;
          }
          else {
            lo = mid + 1;
          }
        }
        /* start = TRUE, but entire array < key */
        if (A[lo] < k) {
          lo = NA_INTEGER;
        } else {
          lo++;
        }
      }
      break;
    default:
      error("unsupported type");
  }

  return lo; // lo is the least x for which A[x] >= key is true
}

/* Find the smallest index for which A[index] > key */
int upper_bound(SEXP key, SEXP vec)
{
  int mid;
  int lo = 0;
  int hi = length(vec) - 1;

  switch (TYPEOF(vec)) {
    case REALSXP:
      {
        double *A = REAL(vec);
        double k = REAL(key)[0];
        while (lo < hi) {
          mid = lo + (hi - lo) / 2;
          if (A[mid] > k) {
            hi = mid;
          }
          else {
            lo = mid + 1;
          }
        }
        /* if handles edge cases. item may be at the lo/hi end of array */
        if (lo > 0 && A[lo] > k) lo--;

        /* start = FALSE, but entire array > key */
        if (A[lo] > k) {
          lo = NA_INTEGER;
        } else {
          lo++;
        }
      }
      break;
    case INTSXP:
      {
        int *A = INTEGER(vec);
        int k = INTEGER(key)[0];
        while (lo < hi) {
          mid = lo + (hi - lo) / 2;
          if (A[mid] > k) {
            hi = mid;
          }
          else {
            lo = mid + 1;
          }
        }
        /* handle edge case where item is at the lo/hi end of array */
        if (lo > 0 && A[lo] > k) lo--;

        /* start = FALSE, but entire array > key */
        if (A[lo] > k) {
          lo = NA_INTEGER;
        } else {
          lo++;
        }
      }
      break;
    default:
      error("unsupported type");
  }

  return lo; // lo is the least x for which A[x] > key is true
}

SEXP binsearch(SEXP key, SEXP vec, SEXP start)
{
  if (!isLogical(start)) {
    error("start must be specified as true or false");
  }

  if (length(vec) < 1) {
    return ScalarInteger(NA_INTEGER);
  }

  /* bound the key in the array of vals.
     if start == true, return lowest index s.t. vals[index] >= key
     if start == false, return highest index s.t. vals[index] <= key
  */
  int item = NA_INTEGER;
  if (LOGICAL(start)[0]) {
    item = lower_bound(key, vec);
  } else {
    item = upper_bound(key, vec);
  }

  return ScalarInteger(item);
}
