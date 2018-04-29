#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>

/* Binary search range to find interval contributed by Corwin Joy */

static struct keyvec {
  double *dvec;
  double dkey;
  int *ivec;
  int ikey;
};

typedef int (*bound_comparer)(const struct keyvec, const int);
static inline int
cmp_dbl_upper(const struct keyvec kv, const int i)
{
  const double cv = kv.dvec[i];
  const double ck = kv.dkey;
  return cv > ck;
}
static inline int
cmp_dbl_lower(const struct keyvec kv, const int i)
{
  const double cv = kv.dvec[i];
  const double ck = kv.dkey;
  return cv >= ck;
}
static inline int
cmp_int_upper(const struct keyvec kv, const int i)
{
  const int cv = kv.ivec[i];
  const int ck = kv.ikey;
  return cv > ck;
}
static inline int
cmp_int_lower(const struct keyvec kv, const int i)
{
  const int cv = kv.ivec[i];
  const int ck = kv.ikey;
  return cv >= ck;
}

SEXP binsearch(SEXP key, SEXP vec, SEXP start)
{
  if (!isLogical(start)) {
    error("start must be specified as true or false");
  }

  if (length(vec) < 1) {
    return ScalarInteger(NA_INTEGER);
  }

  int use_start = LOGICAL(start)[0];
  bound_comparer cmp_func = NULL;
  struct keyvec data;

  switch (TYPEOF(vec)) {
    case REALSXP:
      data.dkey = REAL(key)[0];
      data.dvec = REAL(vec);
      cmp_func = (use_start) ? cmp_dbl_lower : cmp_dbl_upper;
      break;
    case INTSXP:
      data.ikey = INTEGER(key)[0];
      data.ivec = INTEGER(vec);
      cmp_func = (use_start) ? cmp_int_lower : cmp_int_upper;
      break;
    default:
      error("unsupported type");
  }

  int mid;
  int lo = 0;
  int hi = length(vec) - 1;

  while (lo < hi) {
    mid = lo + (hi - lo) / 2;
    if (cmp_func(data, mid)) {
      hi = mid;
    }
    else {
      lo = mid + 1;
    }
  }

  /* handle edge cases where item may be at the lo/hi end of array */
  if (use_start) {
    /* cmp_func is: vec[lo] >= k */
    if (cmp_func(data, lo)) {
      lo++;
    } else {
      lo = NA_INTEGER;
    }
  } else {
    if (lo > 0 && cmp_func(data, lo)) lo--;
    /* cmp_func is: vec[lo] > k */
    if (cmp_func(data, lo)) {
      lo = NA_INTEGER;
    } else {
      lo++;
    }
  }

  return ScalarInteger(lo);
}
