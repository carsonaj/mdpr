#' Policy minimizer for Markov decision processes via value iteration
#'
#' @usage mdp(states, action_fn, prob_fn, cost_fn, final_cost_fn, horizon)
#'
#' @param states vector of states (like a set, elements should not be repeated)
#' @param action_fn function with\cr
#'     \itemize{
#'     \item input: \cr
#'     current_state - state belonging to states\cr
#'     \item output: actions - vector of allowed actions for current_state (like a
#'     set, elements should not be repeated)
#'     }
#' @param prob_fn function with\cr
#'     \itemize{
#'     \item input: \cr
#'     current_state - state in states, \cr
#'     action_taken - action in actions,\cr
#'     next_state - state in states, \cr
#'     \item output: \cr
#'     probability - scalar in [0, 1]
#'     }
#' @param cost_fn function with\cr
#'     \itemize{
#'     \item input: \cr
#'     time - positive integer less than horizon,\cr
#'     current_state - state in states, \cr
#'     action_taken - action in actions, \cr
#'     next_state - state in states,
#'     \item output: \cr
#'     cost - scalar
#'     }
#' @param final_cost_fn function with \cr
#'     \itemize{
#'     \item input: \cr
#'     current_state - state \cr
#'     \item output: \cr
#'     final_cost - scalar
#'     }
#' @param horizon integer greater than 1
#'
#' @return policy_fn function with \cr
#'     \itemize{
#'     \item input: \cr
#'     time - positive integer less than horizon, \cr
#'     current_state - state in states \cr
#'     \item output: \cr
#'     action_taken - action in actions
#'     }
#' @export
#'
#' @examples
#' # the following example is covered in more detail in these notes:
#' # https://github.com/carsonaj/Math/blob/master/Optimization/Markov%20Decision%20Processes.pdf
#'
#' states <- -2:2
#' action_fn <- function(s) return(0:(2-s))
#'
#' # store probabilities to define prob_fn
#' probs <- array(0, dim = c(5, 5, 5))
#' probs[1, 1, ] <- c(1, 0, 0, 0, 0)
#' probs[2, 1, ] <- c(.9, .1, 0, 0, 0)
#' probs[3, 1, ] <- c(.3, .6, .1, 0, 0)
#' probs[4, 1, ] <- c(0, .3, .6, .1, 0)
#' probs[5, 1, ] <- c(0, 0, .3, .6, .1)
#' probs[1, 2, ] <- c(.9, .1, 0, 0, 0)
#' probs[2, 2, ] <- c(.3, .6, .1, 0, 0)
#' probs[3, 2, ] <- c(0, .3, .6, .1, 0)
#' probs[4, 2, ] <- c(0, 0, .3, .6, .1)
#' probs[1, 3, ] <- c(.3, .6, .1, 0, 0)
#' probs[2, 3, ] <- c(0, .3, .6, .1, 0)
#' probs[3, 3, ] <- c(0, 0, .3, .6, .1)
#' probs[1, 4, ] <- c(0, .3, .6, .1, 0)
#' probs[2, 4, ] <- c(0, 0, .3, .6, .1)
#' probs[1, 5, ] <- c(0, 0, .3, .6, .1)
#'
#' prob_fn <- function(s1, a, s2) return(probs[s1 + 3, a + 1, s2 + 3])
#' cost_fn <- function(t, s1, a, s2) return(a + 2 * max(0, s2) + 3 * max(0, -s2))
#' final_cost_fn <- function(s) return(0)
#' horizon <- 3
#'
#' policy_fn <- mdp(states, action_fn, prob_fn, cost_fn, final_cost_fn, horizon)
mdp <- function(states, action_fn, prob_fn, cost_fn, final_cost_fn, horizon) {
  # compatibility check for states
  check_compatible_set(states)

  # compatibility check for horizon
  check_compatible_horizon(horizon)

  # get optimal arguments
  optim <- get_optim(states, action_fn, prob_fn, cost_fn, final_cost_fn, horizon)

  # define policy function
  policy_fn <- function(t, s) {
    check_compatible_time(t, horizon)
    check_compatible_state(s, states)
    return(optim$optim_args[t, match(s, states)])
  }

  return(policy_fn)
}
#==========================================================================================
#==========================================================================================

get_optim <- function(states, action_fn, prob_fn, cost_fn, final_cost_fn, horizon) {
  optim_vals <- matrix(0, horizon, length(states))
  optim_args <- matrix(0, horizon - 1, length(states))
  for (j in 1:length(states)) {
    optim_vals[horizon, j] <- final_cost_fn(states[j])
  }

  # iterate over remaining time
  for (t in (horizon - 1):1) {
    # iterate over states
    for (i in 1:length(states)) {
      s <- states[i]
      # get available actions
      actions <- action_fn(s)
      min <- get_min(states, s, actions, prob_fn, t, cost_fn, optim_vals)
      optim_vals[t, i] <- min$min_val
      optim_args[t, i] <- min$min_arg
    }
  }

  return(list(optim_vals = optim_vals, optim_args = optim_args))
}
#==========================================================================================
#==========================================================================================

get_min <- function(states, s, actions, prob_fn, time, cost_fn, optim_vals){
  a1 <- actions[1]
  # set min to expectation of cost for state s, action a1
  min_val <- get_expectation(states, s, a1, prob_fn, time, cost_fn, optim_vals)
  min_arg <- a1
  if (length(actions) > 1) {
    #iterate over remaining actions
    for (j in 2:length(actions)) {
      a <- actions[j]
      val <- get_expectation(states, s, a, prob_fn, time, cost_fn, optim_vals)
      if (val < min_val) {
        min_val <- val
        min_arg <- a
      }
    }
  }

  return(list(min_val = min_val, min_arg = min_arg))
}
#==========================================================================================
#==========================================================================================

get_expectation <- function(states, s, a, prob_fn, time, cost_fn, optim_vals) {
  # initialize costs and transition probabilities
  costs <- rep(0, length(states))
  probs <- rep(0, length(states))

  # get costs and transition probabilities
  for (i in 1:length(states)) {
    costs[i] <- cost_fn(time, s, a, states[i]) + optim_vals[time + 1, i]
    probs[i] <- prob_fn(s, a, states[i])
  }
  # compute expected cost
  expected <- as.numeric(crossprod(costs, probs))

  return(expected)
}
#==========================================================================================
#==========================================================================================

check_compatible_set <-function(v) {
  if (length(v) != length(unique(v))) {
    stop("sets must have unique elements. To see which paramters must be set-like,
         use '?mdp' in the console. Vectors with nonunique elements can be fixed
         using the 'unique()' function.")
  }
}
#==========================================================================================
#==========================================================================================

check_compatible_horizon <- function(h) {
  if ((h <= 1) || (h%%1 != 0)) {
    stop("horizon must be an integer greater than 1")
  }
}
#==========================================================================================
#==========================================================================================

check_compatible_time <- function(t, horizon) {
  if((t <= 0) || t >= horizon) {
    stop("time must be a positive integer less than horizon")
  }
}
#==========================================================================================
#==========================================================================================

check_compatible_state <- function(s, states) {
  if (!(s %in% states)) {
    stop("current state must be in states")
  }
}


