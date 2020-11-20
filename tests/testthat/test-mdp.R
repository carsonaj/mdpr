test_that("mdp works", {
  # test for get_expectation:
  states <- c(-1, 1)
  prob_fn <- function(s, a, sn) {
    if (sn == 1) {
      return(.1)
    } else {
      return(.9)
    }
  }
  time <- 1
  cost_fn <- function(time, s, a, sn) (2*s + a + sn)
  optim_vals <- matrix(0, 2, 2)
  s <- -1
  a <- 2

  expect_equal(get_expectation(states, prob_fn, time, cost_fn, optim_vals, s, a), -.8)
})
