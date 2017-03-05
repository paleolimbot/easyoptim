
#' Combine two values of a parameter type using weighted probabilities
#'
#' @param param A 'param' object
#' @param value1 The first value
#' @param value2 The second value
#' @param prob A named \code{list} in the form \code{functionname=probability}.
#'   The function described by \code{functionname} should take the parameter as
#'   its first argument and (at least) two vector parameters of arbitrary length
#'   and return a vector of identical length. Examples include
#'   \link{breed.use1}, \link{breed.use2}, \link{breed.rinterpolate}, and
#'   \link{breed.random}.
#' @param seed A random number seed for replicability.
#'
#' @return A vector of new parameter values
#' @export
#'
seibert.breed <- function(param, value1, value2,
                          prob=list(breed.use1=0.41, breed.use2=0.41,
                                    breed.rinterpolate=0.16, breed.random=0.02),
                          seed=NULL) {
  # seed for replicability
  if(!is.null(seed)) set.seed(seed)

  # making a data frame ensures identical length/recycling for value1 and value2
  df <- data.frame(v1=value1, v2=value2, stringsAsFactors = FALSE)

  # param args can override pA, pB, pAB, pmut
  prob <- sapply(names(prob), function(n) {
    if(!is.null(param[[n]])) {
      param[[n]]
    } else {
      prob[[n]]
    }
  })

  df$func <- names(prob)[sample(length(prob), size=nrow(df), prob=unlist(prob), replace=TRUE)]
  df$result <- NA
  # probably a more efficient way to do this
  for(fun in names(prob)) {
    filter <- df$func == fun
    if(sum(filter) == 0) next
    df$result[filter] <- do.call(fun, list(df$value1[filter], df$value2[filter]))
  }

  df$result
}

breed.use1 <- function(param, value1, value2) value1

breed.use2 <- function(param, value1, value2) value2

breed.random <- function(param, value1, value2) random.value(param, n=length(value1))

breed.rinterpolate <- function(param, value1, value2) UseMethod("breed.rinterpolate")

breed.rinterpolate.param.real <- function(param, value1, value2) {
  runif(n=length(value1), 0, 1) * (value2 - value1) + value1
}

breed.rinterpolate.param.discrete <- function(param, value1, value2) {
  if(param$ordered) {
    int1 <- match(value1, param$choices)
    int2 <- match(value2, param$choices)
    param$choices[round(breed.rinterpolate.param.real(NULL, int1, int2))]
  } else {
    c(value1, value2)[sample(2, size = length(value1), replace = TRUE)]
  }
}

breed.rinterpolate.param.distributed <- function(param, value1, value2) {
  breed.rinterpolate.param.real(param, value1, value2)
}

breed.rinterpolate.param.distr.int <- function(param, value1, value2) {
  round(breed.rinterpolate.param.real(param, value1, value2))
}


