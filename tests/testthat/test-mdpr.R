test_that("mdpr works", {

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
  cost_fn <- function(time, s, a, sn) return(2*s + a + sn)
  optim_vals <- matrix(0, 2, 2)
  s <- -1
  a <- 2

  expect_equal(get_expectation(states, s, a, prob_fn, time, cost_fn, optim_vals), -.8)
  #===================================================================================
  #===================================================================================

  # test for get_mins:
  states <- 0:1
  s <- 0
  actions <- 1:2
  prob_fn <- function(current_state, action_taken, next_state)
    ((current_state + action_taken + next_state)/(current_state + action_taken + next_state + 1))
  time <- 1
  cost_fn <- function(time, current_state, action_taken, next_state) return(1)
  optim_vals <- matrix(0, 2, 2)

  out <- get_mins(states, s, actions, prob_fn, time, cost_fn, optim_vals)
  check.min_arg <- 1
  check.min_val <- 7/6

  expect_equal(out$min_arg, check.min_arg)
  expect_equal(out$min_val, check.min_val)
  #===================================================================================
  #===================================================================================

  # test for mdp:


})
