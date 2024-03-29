context("Check binomial arguments")

###check_trials <- function(trials)
test_that("check_trials with valid trials", {

  expect_true(check_trials(1))
  expect_true(check_trials(0))
  expect_true(check_trials(300))
})


test_that("check_trials with invalid trials", {

  expect_error(check_trials(-1))
  expect_error(check_trials(1.5))
  expect_error(check_trials("i"))
})

test_that("check_trials fails with invalid lengths", {

  expect_error(check_trials(1:5))
  expect_error(check_trials(0:10))
})

###check_prob <- function(prob)
test_that("check_prob with valid prob", {

  expect_true(check_prob(1))
  expect_true(check_prob(0))
  expect_true(check_prob(0.5))
})

test_that("check_prob with invalid prob", {

  expect_error(check_prob(-1))
  expect_error(check_prob(1.5))
  expect_error(check_prob(2))
  expect_error(check_prob("i"))
})

test_that("check_prob fails with invalid lengths", {

  expect_error(check_prob(1:5))
  expect_error(check_prob(0:10))
})


###check_success <- function(success, trials)
test_that("check_success with valid inputs", {

  expect_true(check_success(1, 2))
  expect_true(check_success(0, 2))
  expect_true(check_success(2, 2))
})

test_that("check_success with invalid inputs", {

  expect_error(check_success(-1, 2))
  expect_error(check_success(1.5, 2))
  expect_error(check_success("i", 2))
})

test_that("check_success can take in success with length larger than 1", {

  expect_true(check_success(1:5, 10))
  expect_true(check_success(0:10, 11))
})

###aux_mean(trials, prob)
test_that("aux_mean with valid inputs", {

  expect_is(aux_mean(1, 0.8), "numeric")
  expect_is(aux_mean(0, 0.4), "numeric")
  expect_is(aux_mean(2, 0.5), "numeric")
})

test_that("aux_mean with invalid inputs", {

  expect_error(aux_mean("i", 2))
})

test_that("aux_mean return with correct value", {

  expect_equal(aux_mean(1, 0.8), 0.8)
  expect_equal(aux_mean(0, 0.4), 0)
  expect_equal(aux_mean(2, 0.5), 1)
})

###aux_variance(trials, prob)
test_that("aux_variance with valid inputs", {

  expect_is(aux_variance(1, 0.8), "numeric")
  expect_is(aux_variance(0, 0.4), "numeric")
  expect_is(aux_variance(2, 0.5), "numeric")
})

test_that("aux_variance with invalid inputs", {

  expect_error(aux_variance("i", 2))
})

test_that("aux_variance return with correct value", {

  expect_equal(aux_variance(1, 0.8), 0.16)
  expect_equal(aux_variance(0, 0.4), 0)
  expect_equal(aux_variance(2, 0.5), 0.5)
})

###aux_mode(trials, prob)
test_that("aux_mode with valid inputs", {

  expect_is(aux_mode(1, 0.8), "integer")
  expect_is(aux_mode(0, 0.4), "integer")
  expect_is(aux_mode(2, 0.5), "integer")
})

test_that("aux_mode with invalid inputs", {

  expect_error(aux_mode("i", 2))
})

test_that("aux_mode return with correct value", {

  expect_equal(aux_mode(1, 0.8), 1)
  expect_equal(aux_mode(0, 0.4), 0)
  expect_equal(aux_mode(4, 0.5), 2)
})

###aux_skewness(trials, prob)
test_that("aux_skewness with valid inputs", {

  expect_is(aux_skewness(1, 0.8), "numeric")
  expect_is(aux_skewness(0, 0.4), "numeric")
  expect_is(aux_skewness(2, 0.5), "numeric")
})

test_that("aux_skewness with invalid inputs", {

  expect_error(aux_skewness("i", 2))
})

test_that("aux_skewness return with correct value", {

  expect_equal(aux_skewness(1, 0.8), -1.5)
})

###aux_kurtosis(trials, prob)
test_that("aux_kurtosis with valid inputs", {

  expect_is(aux_kurtosis(1, 0.8), "numeric")
  expect_is(aux_kurtosis(0, 0.4), "numeric")
  expect_is(aux_kurtosis(2, 0.5), "numeric")
})

test_that("aux_kurtosis with invalid inputs", {

  expect_error(aux_kurtosis("i", 2))
})

test_that("aux_kurtosis return with correct value", {

  expect_equal(aux_kurtosis(1, 0.8), 0.25)
  expect_equal(aux_kurtosis(4, 0.5), -0.5)
})

###bin_choose <- function(n, k)
test_that("bin_choose with valid inputs", {

  expect_is(bin_choose(5, 3), "numeric")
  expect_is(bin_choose(5, 0), "numeric")
  expect_is(bin_choose(20, 18), "numeric")
})

test_that("bin_choose with invalid inputs", {

  expect_error(bin_choose("i", 2))
  expect_error(bin_choose(1, 2))
})

test_that("bin_choose return with correct value", {

  expect_equal(bin_choose(5, 3), choose(5, 3))
  expect_equal(bin_choose(5, 0), choose(5, 0))
  expect_equal(bin_choose(20, 18), choose(20, 18))
})

###bin_probability <- function(success, trials, prob)
test_that("bin_probability with valid inputs", {

  expect_is(bin_probability(3, 5, 0.5), "numeric")
  expect_is(bin_probability(0, 5, 0.5), "numeric")
  expect_is(bin_probability(18, 20, 0.3), "numeric")
})

test_that("bin_probability with invalid inputs", {

  expect_error(bin_probability("i", 2))
})

test_that("bin_probability return with correct value", {

  expect_equal(bin_probability(3, 5, 0.5), 0.3125)
  expect_equal(bin_probability(0, 5, 0.5), 0.03125)
  expect_equal(bin_probability(18, 20, 0.3), 3.606885e-08)
})

###bin_distribution <- function(trials, prob)
test_that("bin_distribution with valid inputs", {

  expect_is(bin_distribution(3, 0.5)$probability, "numeric")
  expect_is(bin_distribution(2, 0.5)$probability, "numeric")
  expect_is(bin_distribution(18, 0.4)$probability, "numeric")
})

test_that("bin_distribution with invalid inputs", {

  expect_error(bin_distribution(3, -1))
})

test_that("bin_distribution return with correct value", {

  expect_equal(bin_distribution(3, 0.5)$success, 0:3)
  expect_equal(bin_distribution(3, 0.5)$probability, c(0.125, 0.375, 0.375, 0.125))
  expect_equal(bin_distribution(7, 0.4)$success, 0:7)
  expect_equal(bin_distribution(7, 0.4)$probability, c(0.0279936, 0.1306368, 0.2612736,
                                                       0.2903040, 0.1935360, 0.0774144,
                                                       0.0172032, 0.0016384))
})

###bin_cumulative <- function(trials, prob)
test_that("bin_cumulative with valid inputs", {

  expect_is(bin_cumulative(3, 0.5)$cumulative, "numeric")
  expect_is(bin_cumulative(2, 0.5)$cumulative, "numeric")
  expect_is(bin_cumulative(18, 0.4)$cumulative, "numeric")
})

test_that("bin_cumulative with invalid inputs", {

  expect_error(bin_cumulative(3, -1))
})

test_that("bin_distribution return with correct value", {

  expect_equal(bin_cumulative(3, 0.5)$success, 0:3)
  expect_equal(bin_cumulative(3, 0.5)$probability, c(0.125, 0.375, 0.375, 0.125))
  expect_equal(bin_cumulative(3, 0.5)$cumulative, c(0.125, 0.500, 0.875, 1.000))
  expect_equal(bin_cumulative(7, 0.4)$success, 0:7)
  expect_equal(bin_cumulative(7, 0.4)$probability, c(0.0279936, 0.1306368, 0.2612736,
                                                       0.2903040, 0.1935360, 0.0774144,
                                                       0.0172032, 0.0016384))
  expect_equal(bin_cumulative(7, 0.4)$cumulative, c(0.0279936, 0.1586304, 0.4199040,
                                                    0.7102080, 0.9037440, 0.9811584,
                                                    0.9983616, 1.0000000))
})
