test_that("mdpr works", {

  # test for check_compatible_time:
  horizon <- 3
  t1 <- 0
  t2 <- 4
  t3 <- 2

  expect_error(check_compatible_time(t1, horizon), "time must be a positive integer less than horizon", fixed = T)
  expect_error(check_compatible_time(t2, horizon), "time must be a positive integer less than horizon", fixed = T)
  expect_error(check_compatible_time(t3, horizon), NA)
  #===================================================================================
  #===================================================================================

  # test for check_compatible_state:
  states <- 1:3
  s1 <- 1
  s2 <- 4

  expect_error(check_compatible_state(s1, states), NA)
  expect_error(check_compatible_state(s2, states), "current state must be in states", fixed = T)
  #===================================================================================
  #===================================================================================

  # test for check_compatible_set:
  v1 <- 1:3
  v2 <- c(1, 2, 2, 5, 9, 9)

  expect_error(check_compatible_set(v1), NA)
  expect_error(check_compatible_set(v2), "sets must have unique elements. To see which paramters must be set-like,
         use '?mdp' in the console. Vectors with nonunique elements can be fixed
         using the 'unique()' function.", fixed = T)
  #===================================================================================
  #===================================================================================

  # test for check_compatible_horizon
  h1 <- 4
  h2 <- 1
  h3 <- 3.1

  expect_error(check_compatible_horizon(h1), NA)
  expect_error(check_compatible_horizon(h2), "horizon must be an integer greater than 1", fixed = T)
  expect_error(check_compatible_horizon(h3), "horizon must be an integer greater than 1", fixed = T)
  #===================================================================================
  #===================================================================================

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

  # test for get_min:
  states <- 0:1
  s <- 0
  actions <- 1:2
  prob_fn <- function(current_state, action_taken, next_state)
    ((current_state + action_taken + next_state)/(current_state + action_taken + next_state + 1))
  time <- 1
  cost_fn <- function(time, current_state, action_taken, next_state) return(1)
  optim_vals <- matrix(0, 2, 2)

  out <- get_min(states, s, actions, prob_fn, time, cost_fn, optim_vals)
  check.min_arg <- 1
  check.min_val <- 7/6

  expect_equal(out$min_arg, check.min_arg)
  expect_equal(out$min_val, check.min_val)
  #===================================================================================
  #===================================================================================

  # test for get_optim:
  states <- -2:2
  action_fn <- function(s) return(0:(2-s))

  probs <- array(0, dim = c(5, 5, 5))
  probs[1, 1, ] <- c(1, 0, 0, 0, 0)
  probs[2, 1, ] <- c(.9, .1, 0, 0, 0)
  probs[3, 1, ] <- c(.3, .6, .1, 0, 0)
  probs[4, 1, ] <- c(0, .3, .6, .1, 0)
  probs[5, 1, ] <- c(0, 0, .3, .6, .1)
  probs[1, 2, ] <- c(.9, .1, 0, 0, 0)
  probs[2, 2, ] <- c(.3, .6, .1, 0, 0)
  probs[3, 2, ] <- c(0, .3, .6, .1, 0)
  probs[4, 2, ] <- c(0, 0, .3, .6, .1)
  probs[1, 3, ] <- c(.3, .6, .1, 0, 0)
  probs[2, 3, ] <- c(0, .3, .6, .1, 0)
  probs[3, 3, ] <- c(0, 0, .3, .6, .1)
  probs[1, 4, ] <- c(0, .3, .6, .1, 0)
  probs[2, 4, ] <- c(0, 0, .3, .6, .1)
  probs[1, 5, ] <- c(0, 0, .3, .6, .1)

  prob_fn <- function(s1, a, s2) return(probs[s1 + 3, a + 1, s2 + 3])
  cost_fn <- function(t, s1, a, s2) return(a + 2 * max(0, s2) + 3 * max(0, -s2))
  final_cost_fn <- function(s) return(0)
  horizon <- 3

  optim <- get_optim(states, action_fn, prob_fn, cost_fn, final_cost_fn, horizon)
  out1 <- optim$optim_vals
  out2 <- optim$optim_args
  check1 <- matrix(c(6.4, 4.1, 0, 5.4, 3.1, 0, 4.4, 2.1, 0, 3.4, 1.1, 0, 3.05, 1.6, 0), 3, 5)
  check2 <- matrix(rep(c(3, 2, 1, 0, 0), 2), 2, 5, byrow = T)

  expect_equal(out1, check1)
  expect_equal(out2, check2)

  #===================================================================================
  #===================================================================================

  # test for mdp:

  states <- 1:3
  states1 <- c(1, 1, 2)
  action_fn <- function(s) return(1:s)
  prob_fn <- function(s1, a, s2) return(1)
  cost_fn <- function(t, s1, a, s2) return(0)
  final_cost_fn <- function(s) return(0)
  horizon <- 2
  horizon1 <- 1
  horizon2 <- 2.1

  expect_error(mdp(states1, action_fn, prob_fn, cost_fn, final_cost_fn, horizon),
               "sets must have unique elements. To see which paramters must be set-like,
         use '?mdp' in the console. Vectors with nonunique elements can be fixed
         using the 'unique()' function.", fixed = T)
  expect_error(mdp(states, action_fn, prob_fn, cost_fn, final_cost_fn, horizon1),
               "horizon must be an integer greater than 1", fixed = T)
  expect_error(mdp(states, action_fn, prob_fn, cost_fn, final_cost_fn, horizon2),
               "horizon must be an integer greater than 1", fixed = T)

  states <- -2:2
  action_fn <- function(s) return(0:(2-s))

  probs <- array(0, dim = c(5, 5, 5))
  probs[1, 1, ] <- c(1, 0, 0, 0, 0)
  probs[2, 1, ] <- c(.9, .1, 0, 0, 0)
  probs[3, 1, ] <- c(.3, .6, .1, 0, 0)
  probs[4, 1, ] <- c(0, .3, .6, .1, 0)
  probs[5, 1, ] <- c(0, 0, .3, .6, .1)
  probs[1, 2, ] <- c(.9, .1, 0, 0, 0)
  probs[2, 2, ] <- c(.3, .6, .1, 0, 0)
  probs[3, 2, ] <- c(0, .3, .6, .1, 0)
  probs[4, 2, ] <- c(0, 0, .3, .6, .1)
  probs[1, 3, ] <- c(.3, .6, .1, 0, 0)
  probs[2, 3, ] <- c(0, .3, .6, .1, 0)
  probs[3, 3, ] <- c(0, 0, .3, .6, .1)
  probs[1, 4, ] <- c(0, .3, .6, .1, 0)
  probs[2, 4, ] <- c(0, 0, .3, .6, .1)
  probs[1, 5, ] <- c(0, 0, .3, .6, .1)

  prob_fn <- function(s1, a, s2) return(probs[s1 + 3, a + 1, s2 + 3])
  cost_fn <- function(t, s1, a, s2) return(a + 2 * max(0, s2) + 3 * max(0, -s2))
  final_cost_fn <- function(s) return(0)
  horizon <- 3

  out_fn <- mdp(states, action_fn, prob_fn, cost_fn, final_cost_fn, horizon)
  check1 <- 3
  check2 <- 1

  expect_equal(out_fn(2, -2), check1)
  expect_equal(out_fn(1, 0), check2)
  expect_error(out_fn(0, 0), "time must be a positive integer less than horizon", fixed = T)
  expect_error(out_fn(2, -3), "current state must be in states", fixed = T)

})
