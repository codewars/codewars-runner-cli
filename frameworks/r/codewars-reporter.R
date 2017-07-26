CodewarsReporter <- R6::R6Class("CodewarsReporter", inherit = Reporter,
  public = list(
    context_start_time = NULL,
    test_start_time = NULL,

    start_context = function(context) {
      self$context_start_time <- proc.time()
      self$cat_line("\n<DESCRIBE::>", context)
    },
    end_context = function(context) {
      t <- (proc.time() - self$context_start_time)[["elapsed"]]
      self$cat_line("\n<COMPLETEDIN::>", format(1000*t, nsmall = 4))
    },
    start_test = function(context, test) {
      self$test_start_time <- proc.time()
      self$cat_line("\n<IT::>", test)
    },
    end_test = function(context, test) {
      t <- (proc.time() - self$test_start_time)[["elapsed"]]
      self$cat_line("\n<COMPLETEDIN::>", format(1000*t, nsmall = 4))
    },

    add_result = function(context, test, result) {
      if (expectation_success(result)) {
        self$cat_line("\n<PASSED::>Test Passed")
      } else if (expectation_broken(result)) {
        msg <- gsub("(^|\n)", "\\1\t", format(result))
        msg <- gsub("\n", "<:LF:>", msg)
        if (expectation_failure(result)) {
          self$cat_line("\n<FAILED::>Test Failed", "<:LF:>", msg)
        } else {
          self$cat_line("\n<ERROR::>Test Failed", "<:LF:>", msg)
        }
      } else {
        msg <- gsub("(^|\n)", "\\1\t", format(result))
        msg <- gsub("\n", "<:LF:>", msg)
        self$cat_line("\n", toupper(expectation_type(result)), "<:LF:>", msg)
      }
    }
  )
)

# borrowing from testthat/R/expectation.R
expectation_type <- function(exp) {
  stopifnot(is.expectation(exp))
  gsub("^expectation_", "", class(exp)[[1]])
}

expectation_success <- function(exp) { expectation_type(exp) == "success" }
expectation_failure <- function(exp) { expectation_type(exp) == "failure" }
expectation_error   <- function(exp) { expectation_type(exp) == "error" }
expectation_skip    <- function(exp) { expectation_type(exp) == "skip" }
expectation_warning <- function(exp) { expectation_type(exp) == "warning" }
expectation_broken  <- function(exp) {
  expectation_failure(exp) || expectation_error(exp)
}
expectation_ok <- function(exp) {
  expectation_type(exp) %in% c("success", "warning")
}
