#' DNN Configuration for Reinforcement Learning
#'
#' DNN (deep neural network) configuration for reinforcement learning.
#' For detail, see Section 3.2.6 of the original paper.
#'
#' @param fcnet_hiddens A positive integer vector. Numbers of units of the
#'        intermediate layers.
#' @param fcnet_activation A character value specifying the activation function.
#'        Possible values are "tanh" (default), "ReLU", "Swish" (or "SiLU"), or
#'        "linear".
#' @param ... Other configurations. See source code of RLlib.
#'        https://github.com/ray-project/ray/blob/master/rllib/models/catalog.py
#'
#' @return A list of DNN configuration parameters
#'
#' @export
rl_dnn_config <- function(
    fcnet_hiddens = c(256L, 256L),
    fcnet_activation = c("tanh", "relu", "swish", "silu", "linear"), ...) {

  fcnet_hiddens <- as.integer(fcnet_hiddens)
  fcnet_activation <- tolower(fcnet_activation)
  fcnet_activation <- match.arg(fcnet_activation)

  config <- list(fcnet_hiddens = fcnet_hiddens, fcnet_activation = fcnet_activation)
  other_config <- list(...)

  append(config, other_config)
}
