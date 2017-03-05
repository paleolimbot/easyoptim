
context("param constructors")

test_that("parameter constructors produce the correct class", {
  expect_is(param.discrete(c('heads', 'tails')), "param.discrete")
  expect_is(param.real(c(-10, 10)), "param.real")
  expect_is(param.distributed(), "param.distributed")
  expect_is(param.normal(0, 1), "param.distributed")
  expect_is(param.distr.int(), "param.distr.int")
  expect_is(param.normal.int(50, 10), "param.distr.int")
})

test_that("initial values are respected when set", {
  p <- param.discrete(c('heads', 'tails'), initial='heads')
  expect_identical(unique(initial.value(p, n=100)), 'heads')
  p <- param.real(c(-10, 10), initial=8.6)
  expect_identical(unique(initial.value(p, n=100)), 8.6)
  p <- param.distributed(initial=5.8)
  expect_identical(unique(initial.value(p, n=100)), 5.8)
  p <- param.normal(0, 1, initial=5.8)
  expect_identical(unique(initial.value(p, n=100)), 5.8)
  p <- param.distr.int(initial=10)
  expect_identical(unique(initial.value(p, n=100)), 10L)
  p <- param.normal.int(13, 3, initial=10)
  expect_identical(unique(initial.value(p, n=100)), 10L)
})

test_that("invalid initial values are detected", {
  expect_error(param.discrete(c('heads', 'tails'), initial=NA),
               "Argument 'initial' must be one of heads, tails")
  expect_error(param.discrete(c('heads', 'tails'), initial='edge of coin'),
               "Argument 'initial' must be one of heads, tails")

  expect_error(param.real(c(-10, 10), initial=NA),
               "Initial value must be finite")
  expect_error(param.real(c(-10, 10), initial=Inf),
               "Initial value must be finite")
  expect_error(param.real(c(-10, 10), initial=NaN),
               "Initial value must be finite")
  expect_error(param.real(c(-10, 10), initial=11),
               "Initial value must be NULL or between -10 and 10")
  expect_error(param.real(c(-10, 10), initial=-15),
               "Initial value must be NULL or between -10 and 10")

  expect_error(param.distributed(initial=NA), "Argument 'initial' must be finite or NULL")
  expect_error(param.distributed(initial="12.3"), "Argument 'initial' must be finite or NULL")

  expect_error(param.distr.int(initial=NA),
               "Argument 'initial' must contain only integers")
  expect_error(param.distr.int(initial=NA_integer_),
               "Argument 'initial' must be finite or NULL")
  expect_error(param.distr.int(initial=Inf),
               "Argument 'initial' must be finite or NULL")
  expect_error(param.distr.int(initial=NaN),
               "Argument 'initial' must be finite or NULL")
  expect_error(param.distr.int(initial="fish"),
               "Argument 'initial' must contain only integers")
  expect_error(param.distr.int(initial="10"),
               "Argument 'initial' must contain only integers")
  expect_error(param.distr.int(initial=10.5),
               "Argument 'initial' must contain only integers")
})

test_that("random choices are the same with the same seed", {
  p <- param.discrete(c('heads', 'tails'))
  expect_identical(random.value(p, n=100, seed=10), random.value(p, n=100, seed=10))
  p <- param.real(c(-10, 10))
  expect_identical(random.value(p, n=100, seed=10), random.value(p, n=100, seed=10))
  p <- param.distributed()
  expect_identical(random.value(p, n=100, seed=10), random.value(p, n=100, seed=10))
  p <- param.normal(0, 1)
  expect_identical(random.value(p, n=100, seed=10), random.value(p, n=100, seed=10))
  p <- param.distr.int()
  expect_identical(random.value(p, n=100, seed=10), random.value(p, n=100, seed=10))
  p <- param.normal.int(13, 3)
  expect_identical(random.value(p, n=100, seed=10), random.value(p, n=100, seed=10))
})

test_that("initial values are the same with the same seed", {
  p <- param.discrete(c('heads', 'tails'))
  expect_identical(initial.value(p, n=100, seed=10), initial.value(p, n=100, seed=10))
  p <- param.real(c(-10, 10))
  expect_identical(initial.value(p, n=100, seed=10), initial.value(p, n=100, seed=10))
  p <- param.distributed()
  expect_identical(initial.value(p, n=100, seed=10), initial.value(p, n=100, seed=10))
  p <- param.normal(0, 1)
  expect_identical(initial.value(p, n=100, seed=10), initial.value(p, n=100, seed=10))
  p <- param.distr.int()
  expect_identical(initial.value(p, n=100, seed=10), initial.value(p, n=100, seed=10))
  p <- param.normal.int(13, 3)
  expect_identical(initial.value(p, n=100, seed=10), initial.value(p, n=100, seed=10))
})

test_that("parameter validation works properly", {
  # test for null values
  expect_identical(is.param.valid(NULL, NULL), logical(0))

  p <- param.discrete(c('heads', 'tails'))
  expect_true(is.param.valid(p, 'heads'))
  expect_true(is.param.valid(p, 'tails'))
  expect_false(is.param.valid(p, 'notheadsortails'))
  # check vectorization
  expect_length(is.param.valid(p, c('heads', 'tails', 'notheadsortails')), 3)

  p <- param.real(c(-10, 10))
  expect_true(is.param.valid(p, 0))
  expect_false(is.param.valid(p, "10"))
  expect_false(is.param.valid(p, 10.1))
  expect_false(is.param.valid(p, -10.1))
  expect_false(is.param.valid(p, NA))
  expect_false(is.param.valid(p, Inf))
  expect_false(is.param.valid(p, NaN))

  p <- param.distributed()
  expect_true(is.param.valid(p, 0))
  expect_false(is.param.valid(p, "10"))
  expect_false(is.param.valid(p, NA))
  expect_false(is.param.valid(p, Inf))
  expect_false(is.param.valid(p, NaN))

  p <- param.distr.int()
  expect_true(is.param.valid(p, 0L))
  expect_true(is.param.valid(p, 0))
  expect_false(is.param.valid(p, 10.1))
  expect_false(is.param.valid(p, "10"))
  expect_false(is.param.valid(p, NA))
  expect_false(is.param.valid(p, Inf))
  expect_false(is.param.valid(p, NaN))
})
