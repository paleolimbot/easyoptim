% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gap_seibert.R
\name{seibert.gapo}
\alias{seibert.gapo}
\title{Seibert GAP function optimisation}
\usage{
seibert.gapo(.fun, ..., .objective = function(x) x, .validate = TRUE,
  .seed = NULL, .rescaler = c("rescale_none", "rescale", "squish"),
  .generations = 100, .pop = 50, .keepbest = TRUE,
  .breedargs = list(breed.use1 = 0.41, breed.use2 = 0.41, breed.rinterpolate =
  0.16, breed.mutate = 0.02), .progress = c("none", "text", "tk"),
  .output = c("best", "last", "all"))
}
\arguments{
\item{.fun}{The function to be called}

\item{...}{Arguments to be passed to .fun expressed as param objects.}

\item{.objective}{An optional function to be called on the result of .fun to produce an
objective value. If this is not passed, the result of .fun is used to maximize.}

\item{.validate}{Pass FALSE to skip validation. May be faster.}

\item{.seed}{Seed for random operations.}

\item{.rescaler}{An optional rescaling method used to rescale the output of .objective.}

\item{.generations}{The number of generations to evolve the population.}

\item{.pop}{The population size to use.}

\item{.keepbest}{Pass TRUE to discard the results of a next generation if no individuals have
an improved objective value.}

\item{.breedargs}{Passed to \link{seibert.breed}: default values for parameter breeding. These
may be overridden by additional arguments to each parameter.}

\item{.progress}{A plyr progress bar to use, or 'none'.}

\item{.output}{Use to output the best parameters, the last generation, or all parameters.}
}
\value{
A data.frame with columns for each named parameter
}
\description{
Seibert GAP function optimisation
}

