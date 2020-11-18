# INPUTS
# states - vector of states
# actions - function, input: state,
#                     output: possible actions
# trans_probs - function, input: current state, action taken, next state,
#                         output: probability
# costs - function, input: time, current state, action taken, next state,
#                   output: cost
# OUTPUTS
# policy - function, input: time, current state,
#                    output: action taken
mdp <- function(states, actions, trans_probs, costs, horizon) {
  policy <- mdp_c(states, actions, trans_probs, costs, horizon)
  return(policy)
}
