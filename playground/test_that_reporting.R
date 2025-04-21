################################################################################
# Experiments with {testthat} and reporting
# How easy is it to use testthat and create a 'validation' report / log file?
# Jonathan Whiteley                2025-04-21                R v4.5.0
################################################################################
# Basic test file from: https://stackoverflow.com/questions/65126790/is-there-a-way-to-document-testthat-tests-and-get-a-report-output

# Contexts are deprecated in testthat 3e, but would be useful for this purpose
# - nothing returned - see Reporters (ListReporter) below
contxt <- context("Check unit-testing functionality")

# test_that() returns invisible(TRUE) when run interactively
# - unless the test fails, which throw an ERROR !
# - not the same behaviour if run from a function like test_file()
test_results <- c()
test_results <- c(
  test_results,
  test_that("Succeed", {succeed()})
)
test_results <- c(
  test_results,
  test_that("Fail", {fail()})
)
test_results <- c(
  test_results,
  test_that("True", {expect_true(TRUE)})
)
test_that("False", {expect_true(FALSE)})

context("Check unit-testing condition reporting")
test_that("Error", {stop("Force an error")})
test_that("Warn+succeed", {
  warning("Force a warning, but succeed")
  succeed()
})
test_that("Warn+fail", {
  warning("Force a warning, and fail")
  fail()
})

cat(test_results)

if (F) { # run manually
  test_file(this.path::this.path(), reporter = SummaryReporter$new())
  # ListReporter returns generic test results that could be parsed for a custom report
  # but it isn't accessible if the file is source()d directly, only if tests are run as below.
  results <- test_file(this.path::this.path(), reporter = ListReporter$new())
}

# It's not clear how to me how to combine {testthat} tests with an output report from the same file. :/
# It might be possible to create a custom function that runs tests on a file, 
#  then outputs a custom report to a log file ({logr} or {logger} style).
