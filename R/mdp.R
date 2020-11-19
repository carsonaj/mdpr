#' Policy minimizer for Markov decision processes via value iteration
#'
#' @param states - vector of states
#' @param action_fn - function with input: current_state - state and output: possible_actions - vector of actions
#' @param prob_fn - function with input: (current_state - state, action_taken - action, next_state - state) and output: probability - double in 0 to 1
#' @param cost_fn - function with input: (time - positive integer less than horizon, current_state - state, action_taken - action, next_state - state) and output: cost
#' @param final_cost_fn - function with input: current_state - state and output: final_cost - scalar
#' @param initial_state - state
#' @param horizon - positive integer
#'
#' @return policy_fn - function, input: (time - positive integer less than horizon, current_state - state) and output: action_taken - action
#' @export
#'
#' @examples
mdp <- function(states, action_fn, prob_fn, cost_fn, final_cost_fn, initial_state, horizon) {
  optim_vals <- matrix(0, horizon, length(states))
  for (j in 1:length(states)) {
    optim_vals[horizon, j] <- final_cost_fn(states[j])
  }

  optim_args <- matrix(0, horizon - 1, length(states))

  # iterate over remaining time
  for (t in (horizon - 1):2) {
    # iterate over states
    for (i in 1:len(states)) {
      s <- states[i]
      # get available actions
      actions <- action_fn(s)
      mins <- get_mins(states, actions, prob_fn, cost_fn, optim_vals, s)
      optim_vals[t, i] <- mins$min_val
      optim_args[t, i] <- mins$min_arg
    }
  }

  return(policy_fn)
}
#==========================================================================================
#==========================================================================================

get_mins <- function(states, actions, prob_fn, cost_fn, optim_vals, s){
  a1 <- actions[1]
  # set min to expectation of cost for state s, action a1
  min_val <- get_expectation(states, prob_fn, time, cost_fn, optim_vals, s, a1)
  min_arg <- a1
  #iterate over remaining actions
  for (j in 2:length(actions)) {
    a <- actions[j]
    val <- get_expectation(time, states, prob_fn, time, cost_fn, optim, s, a)
    if (val < min_val) {
      min_val <- val
      min_arg <- a
    }
  }

  return(list(min_val = min_val, min_arg = min_arg))
}
#==========================================================================================
#==========================================================================================

get_expectation <- function(states, prob_fn, time, cost_fn, optim_vals, s, a) {
  costs <- rep(0, length(states))
  probs <- rep(0, length(states))

  for (i in 1:length(states)) {
    costs[i] <- cost_fn(time, s, a, states[i]) + optim_vals[time + 1, i]
    probs[i] <- prob_fn(s, a, states[i])
  }
  expected <- as.numeric(crossprod(costs, probs))

  return(expected)
}


