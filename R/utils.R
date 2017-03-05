
# borrowed from the scales package https://github.com/hadley/scales/blob/master/R/bounds.r
rescale <- function(x, to = c(0, 1), from = range(x, na.rm = TRUE, finite = TRUE)) {
  if (zero_range(from) || zero_range(to)) {
    return(ifelse(is.na(x), NA, mean(to)))
  }

  (x - from[1]) / diff(from) * diff(to) + to[1]
}

rescale_none <- function(x, ...) {
  x
}

squish <- function(x, range = c(0, 1), only.finite = TRUE) {
  force(range)
  finite <- if (only.finite) is.finite(x) else TRUE
  x[finite & x < range[1]] <- range[1]
  x[finite & x > range[2]] <- range[2]
  x
}

zero_range <- function(x, tol = 1000 * .Machine$double.eps) {
  if (length(x) == 1) return(TRUE)
  if (length(x) != 2) stop("x must be length 1 or 2")
  if (any(is.na(x)))  return(NA)

  # Special case: if they are equal as determined by ==, then there
  # is zero range. Also handles (Inf, Inf) and (-Inf, -Inf)
  if (x[1] == x[2]) return(TRUE)

  # If we reach this, then x must be (-Inf, Inf) or (Inf, -Inf)
  if (all(is.infinite(x))) return(FALSE)

  # Take the smaller (in magnitude) value of x, and use it as the scaling
  # factor.
  m <- min(abs(x))

  # If we get here, then exactly one of the x's is 0. Return FALSE
  if (m == 0) return(FALSE)

  # If x[1] - x[2] (scaled to 1) is smaller than tol, then return
  # TRUE; otherwise return FALSE
  abs((x[1] - x[2])/m) < tol
}
