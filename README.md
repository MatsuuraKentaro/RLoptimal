
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RLoptimal

<!-- badges: start -->
<!-- badges: end -->

The goal of RLoptimal is to …

## Installation

You can install the development version of RLoptimal from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("MatsuuraKentaro/RLoptimal")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(RLoptimal)

doses <- c(0, 2, 4, 6, 8)

dose_response_models <- DoseFinding::Mods(
  doses = doses, 
  linear = NULL, emax = 0.79, sigEmax = c(4, 5)
)

allocation_rule <- learn_allocation_rule(
  dose_response_models,
  N_total = 150, N_ini = 50, N_block = 10,
  Delta = 0.9, sd_normal = sqrt(4.5), rl_seed = 314,
  rl_config = rl_config(iter = 1, cores = 10)
)
#>  1: Min/Mean/Max reward:   0.0000/  0.4242/  0.9947 (time: 30 secs)
#> Checkpoint saved in directory 'checkpoints/20240706_102848_01'
#> Allocation rule saved in directory 'allocation_rules/20240706_102848'
```

``` r

allocation_rule
#> <AllocationRule>
#> dir: allocation_rules/20240706_102848
#> created at: 2024-07-06 10:30:36.28957
#> call:
#> learn_allocation_rule(models = dose_response_models, N_total = 150, 
#>     N_ini = 50, N_block = 10, Delta = 0.9, sd_normal = sqrt(4.5), 
#>     rl_seed = 314, rl_config = rl_config(iter = 1, cores = 10))
#> iterations: 1
```

``` r
true_response_matrix <- DoseFinding::getResp(dose_response_models, doses = doses)
true_response_list <- as.list(data.frame(true_response_matrix, check.names = FALSE))

true_model <- "linear"
resps_true <- true_response_list[[true_model]]

set.seed(314)
simulated_dose_index <- sample(length(doses), 50, replace = TRUE)
simulated_dose <- doses[simulated_dose_index]
simulated_response <- rnorm(50, mean = resps_true[simulated_dose_index], sd = sqrt(4.5))

allocation_rule$get_next_action_probs(simulated_dose, simulated_response)
#> [1] 0.2496047 0.1800668 0.1490722 0.1939762 0.2272802
```
