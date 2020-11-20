
# mdpr

<!-- badges: start -->
<!-- badges: end -->

Background: A Markov decision process (MDP) is: 
(1) a set of states, together with 
(2) a set of actions (decisions) that one may take based on the current state, 
(3) a cost function that takes as inputs the current the state, the decision taken in the current state and the subsequent state, which then outputs a cost amount
(4) a policy function that takes as input the current state and outputs an action to take 
(5) the transition probabilities between states which depend on the current state, the action taken in the current state and the subsequent state and satisfy a Markov property. 

Goal: Find an optimal policy for a Markov decision process that minimizes the cost.

Functionality: I will implement a dynamic programming algorithm called “value iteration” that uses the Bellman equation to find an optimal policy by going backwards inductively. As many loops are required, I will be implementing much of the code in C++ using the armadillo package.

A deeper explanation of the background and the algorithm is given here: https://github.com/carsonaj/Math/blob/master/Optimization/Markov%20Decision%20Processes.pdf



## Installation

You can install the released version of mdpr from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("mdpr")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(mdpr)
## basic example code
```

