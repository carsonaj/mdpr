
# mdpr

<!-- badges: start -->
<!-- badges: end -->

**Background:** 
A Markov decision process (MDP) consists of:

* a set of states, 
* a set of actions (decisions) such that in any given state, only a subset of 
possible (allowed) actions may be taken, 
* a cost function that takes as inputs the current the state, the allowed action taken in the current state and the subsequent state, which then outputs a cost amount
* a policy function that takes as input the current state and outputs an allowed action to take 
* the transition probabilities between states which depend on the current state, the allowed action taken in the current state and the subsequent state and satisfy a Markov property. 

**Goal:** 
Find an optimal policy for a Markov decision process that minimizes the cost.

**Functionality:** An optimal policy is found using a dynamic programming algorithm called “value iteration” that uses the Bellman equations by going backwards inductively. A more detailed explanation of the background and the algorithm with an example is given in my notes here: https://github.com/carsonaj/Math/blob/master/Optimization/Markov%20Decision%20Processes.pdf

## Installation

You can install the mdpr package from github using the following:

``` r
# install.packages("devtools")
library(devtools)
devtools::install_github("carsonaj/mdpr")
```

## Example

We will cover the inventory control example that is given in the notes mentioned above. 

``` r
library(mdpr)

# possible states
states <- -2:2
# get allowed actions for a given state
action_fn <- function(s) return(0:(2-s))

# store probabilities to define prob_fn
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

# define transition probabilites
prob_fn <- function(s1, a, s2) return(probs[s1 + 3, a + 1, s2 + 3])
# define cost function
cost_fn <- function(t, s1, a, s2) return(a + 2 * max(0, s2) + 3 * max(0, -s2))
# define cost function at horizon
final_cost_fn <- function(s) return(0)
# set horizon
horizon <- 3

# get policy
policy_fn <- mdp(states, action_fn, prob_fn, cost_fn, final_cost_fn, horizon)

# For example, if our initial state is 0 (that is, we are in state s = 0 at time t = 1), 
# then out of the allowed actions we may take: 0:2 = action_fn(0), the optimal action
# that we can take so as to minimize the total cost is policy_fn(1, 0) = 1.
```

