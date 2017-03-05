

#' Deifine a Discrete Parameter
#'
#' @param choices A vector of valid choices. Can be any type.
#' @param initial The initial value (must be one of \code{choices} or NULL). See \link{initial.value}.
#' @param weights Probability weights for each choice (must be of same length as \code{choices}).
#' @param ordered TRUE if order matters, FALSE otherwise.
#' @param ... Additional arguments to be used in optimisation methods
#'
#' @return An object of type 'param.discrete'
#' @export
#'
#' @examples
#' # regular discrete parameter
#' param.discrete(c("inside", "outside", "I don't care"))
#'
#' # ordered discrete parameter
#' param.discrete(c("disagree", "somewhat agree", "agreee"), ordered=TRUE)
#'
#' # create a parameter that is a loaded coin
#' param.discrete(c(TRUE, FALSE), weights=c(0.75, 0.25))
#'
param.discrete <- function(choices, initial=NULL, weights=NULL, ordered=FALSE, ...) {
  if(is.null(choices) || !is.atomic(choices)) stop("Choices must be an atomic vector")
  if(!is.null(initial) && !all(initial %in% choices)) {
    stop("Argument 'initial' must be one of ", paste(choices, collapse=", "))
  }
  ordered <- as.logical(ordered)
  if(is.na(ordered)) stop("Ordered could not be coerced to type 'logical'")

  structure(list(choices=choices, weights=weights, ordered=ordered, initial=initial, ...),
            class=c("param.discrete", "param"))
}

#' Define a Real Parameter
#'
#' @param bounds A vector of length 2 describing the upper and lower bounds for the parameter.
#'   Some optimisation algorithms can handle values of \code{c(-Inf, Inf)}, but most cannot.
#' @param initial The initial value (must be within \code{bounds}), or NULL. See \link{initial.value}
#' @param ... Additional arguments to be used in optimisation methods
#'
#' @return An object of type 'param.real'
#' @export
#'
#' @examples
#' # unbounded real parameter (may not work in all optimisation algorithms)
#' param.real(c(-Inf, Inf))
#'
#' # bounded real parameter
#' param.real(c(0, 100))
param.real <- function(bounds, initial=NULL, ...) {
  bounds <- range(bounds)
  if(any(is.na(bounds))) stop("Bounds cannot be NA")
  if(any(!is.finite(initial))) stop("Initial value must be finite")
  if(!is.null(initial)) {
    if(any((initial < bounds[1]) || (initial > bounds[2]))) {
      stop("Initial value must be NULL or between ", bounds[1], " and ", bounds[2])
    }
  }
  structure(list(bounds=range(bounds), initial=initial, ...),
            class=c("param.real", "param"))
}

#' Define a parameter with a known probability distribution
#'
#' @param type The type of probability distrubtion. This is used to call the functions
#'   'rTYPE' and 'dTYPE' (e.g, rnorm and dnorm), which are used by various optimisation
#'   algorithms.
#' @param args A named list of arguments to pass to rTYPE and dTYPE (e.g., mean, sd).
#' @param initial The initial value or NULL.
#' @param ... Additional arguments to be used in optimisation methods.
#'
#' @seealso \link{param.normal}, \link{param.real}
#' @return An object of type 'param.distributed'
#' @export
#'
#' @examples
#' param.distributed("norm", args=list(mean=12, sd=2))
#' # is the same as
#' param.normal(mean=12, sd=2)
#'
param.distributed <- function(type=c("norm", "beta", "binom", "cauchy", "chisq", "exp", "f", "gamma",
                                     "geom", "hyper", "lnorm", "logis", "multinom", "nbinom", "pois",
                                     "signrank", "t", "unif", "weibull", "wilcox", "birthday",
                                     "tukey"), args=list(), initial=NULL, ...) {
  type <- match.arg(type)
  if(!is.list(args)) stop("Argument 'args' must be of type 'list'")
  if(!is.null(initial) && !all(is.finite(initial))) stop("Argument 'initial' must be finite or NULL")

  structure(list(type=type, args=args, initial=initial, ...),
            class=c("param.distributed", "param"))
}

#' @rdname param.distributed
#' @export
param.distr.int <- function(type=c("norm", "beta", "binom", "cauchy", "chisq", "exp", "f", "gamma",
                                   "geom", "hyper", "lnorm", "logis", "multinom", "nbinom", "pois",
                                   "signrank", "t", "unif", "weibull", "wilcox", "birthday",
                                   "tukey"), args=list(), initial=NULL, ...) {
  type <- match.arg(type)
  if(!is.list(args)) stop("Argument 'args' must be of type 'list'")
  if(!is.null(initial)) {
    if(!is.numeric(initial)) stop("Argument 'initial' must contain only integers")
    intvals <- suppressWarnings(as.integer(initial))
    if(!all(is.finite(intvals))) stop("Argument 'initial' must be finite or NULL")
    if(any(intvals != initial)) stop("Argument 'initial' must contain only integers")
    initial <- intvals
  }

  structure(list(type=type, args=args, initial=initial, ...),
            class=c("param.distr.int", "param"))
}

#' Define a real parameter with a normal distribution
#'
#' @param mean The mean of the distribution
#' @param sd The standard deviation of the distribution
#' @param initial The initial value or NULL
#' @param ... Additional arguments to be used in optimisation methods.
#'
#' @return An object of type 'param.distributed'
#' @seealso \link{param.distributed}, \link{param.real}
#' @export
#'
#' @examples
#' param.distributed("norm", args=list(mean=12, sd=2))
#' # is the same as
#' param.normal(mean=12, sd=2)
#'
param.normal <- function(mean, sd, initial=NULL, ...) {
  param.distributed("norm", args=list(mean=mean, sd=sd), initial=initial, ...)
}

#' @rdname param.normal
#' @export
param.normal.int <- function(mean, sd, initial=NULL, ...) {
  param.distr.int("norm", args=list(mean=mean, sd=sd), initial=initial, ...)
}

#' Test if an object is a param
#'
#' @param x An object
#'
#' @return TRUE if the object is a 'param', FALSE otherwise
#' @export
#'
#' @examples
#' is.param(4) # false
#' is.param(param.discrete(c('heads', 'tails'))) # true
#'
is.param <- function(x) {
  return(inherits(x, "param"))
}

#' Generate a random parameter value
#'
#' @param param An object of type 'param'
#' @param n The number of values to generate
#' @param seed The random seed
#'
#' @return \code{n} valid random parameter values
#' @export
#'
#' @examples
#' p <- param.discrete(c('heads', 'tails'))
#' random.value(p)
#' random.value(p, n=10)
#'
#' p <- param.real(c(0, 100))
#' random.value(p, n=10)
#'
#' p <- param.normal(50, 10)
#' random.value(p, n=10)
#'
#' p <- param.normal.int(50, 10)
#' random.value(p, n=10)
#'
random.value <- function(param, n=1, seed=NULL) {
  # seed for replicability
  if(!is.null(seed)) set.seed(seed)

  UseMethod("random.value")
}

#' @rdname random.value
#' @export
random.value.param.discrete <- function(param, n=1, seed=NULL) {
  param$choices[sample(length(param$choices), prob=param$weights, size=n, replace=TRUE)]
}

#' @rdname random.value
#' @export
random.value.param.real <- function(param, n=1, seed=NULL) {
  runif(n=n, min=param$bounds[1], max=param$bounds[2])
}

#' @rdname random.value
#' @export
random.value.param.distributed <- function(param, n=1, seed=NULL) {
  do.call(paste0("r", param$type), c(list(n), param$pargs))
}

#' @rdname random.value
#' @export
random.value.param.distr.int <- function(param, n=1, seed=NULL) {
  round(do.call(paste0("r", param$type), c(list(n), param$pargs)))
}


#' Generate an initial parameter value
#'
#' @param param An object of type 'param'
#' @param n The number of initial values to generate
#' @param seed The random seed
#'
#' @return A vector of length \code{n} with valid parameter values
#' @export
#'
#' @examples
#' p <- param.discrete(c('heads', 'tails', 'edge of coin'),
#'                     weights=c(0.45, 0.45, 0.1))
#' initial.value(p, n=10)
#'
initial.value <- function(param, n=1, seed=NULL) {
  if(is.null(param$initial)) {
    random.value(param, n=n, seed=seed)
  } else {
    rep_len(param$initial, length.out=n)
  }
}

#' Validate parameter values
#'
#' @param param An object of type 'param'
#' @param value A vector of values to validate
#'
#' @return A logical vector indicating if \code{value} was valid.
#' @export
#'
#' @examples
#' p <- param.discrete(c('heads', 'tails'))
#' is.param.valid(p, 'heads') # true
#' is.param.valid(p, 'tails') # true
#' is.param.valid(p, 'neither heads nor tails') # false
#'
is.param.valid <- function(param, value) UseMethod("is.param.valid")

#' @rdname is.param.valid
#' @export
is.param.valid.default <- function(param, value) {
  rep_len(FALSE, length.out = length(value))
}

#' @rdname is.param.valid
#' @export
is.param.valid.param.discrete <- function(param, value) {
  value %in% param$choices
}

#' @rdname is.param.valid
#' @export
is.param.valid.param.real <- function(param, value) {
  is.finite(value) & (value >= param$bounds[1]) & (value <= param$bounds[2])
}

#' @rdname is.param.valid
#' @export
is.param.valid.param.distributed <- function(param, value) {
  is.finite(value)
}

#' @rdname is.param.valid
#' @export
is.param.valid.param.distr.int <- function(param, value) {
  ifelse(is.finite(value), ((value %% 1) == 0), FALSE)
}


