
context("test genetic algorithm functions")

test_that("use1 and use2 return the first and second arguments", {
  expect_identical(breed.use1(NULL, c("first", "second", "third"), c("fourth", "fifth", "sixth")),
                   c("first", "second", "third"))
  expect_identical(breed.use2(NULL, c("first", "second", "third"), c("fourth", "fifth", "sixth")),
                   c("fourth", "fifth", "sixth"))
})

test_that("mutate returns random results", {
  p <- param.discrete(c('heads', 'tails'))
  set.seed(10)
  expect_identical(breed.mutate(p, rep(NA, 100), rep(NA, 100)), random.value(p, n=100, seed=10))
  p <- param.real(c(-10, 10))
  set.seed(10)
  expect_identical(breed.mutate(p, rep(NA, 100), rep(NA, 100)), random.value(p, n=100, seed=10))
  p <- param.distributed()
  set.seed(10)
  expect_identical(breed.mutate(p, rep(NA, 100), rep(NA, 100)), random.value(p, n=100, seed=10))
  p <- param.normal(0, 1)
  set.seed(10)
  expect_identical(breed.mutate(p, rep(NA, 100), rep(NA, 100)), random.value(p, n=100, seed=10))
  p <- param.distr.int()
  set.seed(10)
  expect_identical(breed.mutate(p, rep(NA, 100), rep(NA, 100)), random.value(p, n=100, seed=10))
  p <- param.normal.int(13, 3)
  set.seed(10)
  expect_identical(breed.mutate(p, rep(NA, 100), rep(NA, 100)), random.value(p, n=100, seed=10))
})

test_that("passing invalid arguments to siebert.breed result in errors", {
  p <- param.discrete(c('heads', 'tails'))
  expect_error(seibert.breed(p, 'heads', 'tails', cols='v1'), "Invalid columns passed as 'cols': v1")
})

test_that("seibert.breed results in the correct dimensionality", {
  p <- param.discrete(c('heads', 'tails'))
  expect_null(seibert.breed(p, NULL, NULL))
  expect_is(seibert.breed(p, NULL, NULL, cols=c('func', 'result')), 'data.frame')
  expect_equal(nrow(seibert.breed(p, NULL, NULL, cols=c('func', 'result'))), 0)
  expect_equal(ncol(seibert.breed(p, NULL, NULL, cols=c('func', 'result'))), 2)

  # args of length 1 should produce a data frame for multiple cols, vector for single col
  expect_is(seibert.breed(p, 'heads', 'tails', cols=c('func', 'result')), 'data.frame')
  expect_false(inherits(seibert.breed(p, 'heads', 'tails', cols='result'), 'data.frame'))

  # args of length n should produce a data frame of nrow n or vector of length n
  expect_is(seibert.breed(p, rep('heads', 10), rep('tails', 10), cols=c('func', 'result')),
            'data.frame')
  expect_length(seibert.breed(p, rep('heads', 10), rep('tails', 10), cols='result'), 10)
})

test_that("passing invalid arguments to siebert.breed result in errors", {

  expect_error(seibert.breed(NULL, NULL, NULL), "Argument 'param' must be of type 'param'")

  p <- param.discrete(c('heads', 'tails'))
  expect_error(seibert.breed(p, NULL, NULL, cols=c('v1', 'v8')),
               "Invalid columns passed as 'cols': v1, v8")
  expect_error(seibert.breed(p, NULL, NULL, cols=NULL),
               "Argument 'cols' must have length > 0")

  # probabilities
  expect_error(seibert.breed(p, NULL, NULL, prob=NULL),
               "Argument 'prob' must be of type 'list'")
  expect_error(seibert.breed(p, NULL, NULL, prob=list()),
               "Argument 'prob' must have length > 0")
  expect_error(seibert.breed(p, NULL, NULL, prob=list(1, 2, 3)),
               "Argument 'prob' must be a named list")
  expect_error(seibert.breed(p, NULL, NULL, prob=list(1, 2, 3, fish="thing")),
               "Argument 'prob' must be a named list")
  expect_error(seibert.breed(p, NULL, NULL, prob=list(breed.use1=0.5, notafunction=0.5)),
               "object 'notafunction' of mode 'function' was not found")

  # values
  expect_error(seibert.breed(p, 'heads', 'notheadsortails'),
               "Invalid values passed: 'notheadsortails'")
})

test_that("Passing validate=FALSE skips validation", {
  expect_error(seibert.breed(NULL, NULL, NULL),
               "Argument 'param' must be of type 'param'")
  expect_null(seibert.breed(NULL, NULL, NULL, validate=FALSE))
})
