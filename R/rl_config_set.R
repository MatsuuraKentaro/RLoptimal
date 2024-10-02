#' Configuration of Reinforcement Learning
#'
#' Mainly settings for the arguments of the training() function.
#' Not compatible with the new API stack introduced in Ray 2.10.0.
#'
#' @param iter A positive integer value. Number of iterations.
#' @param save_start_iter,save_every_iter An integer value. Save checkpoints every
#'        'save_every_iter' iterations starting from 'save_start_iter' or later.
#' @param cores A positive integer value. Number of CPU cores used for learning.
#' @param gamma A positive numeric value. Discount factor of the Markov decision
#'        process. Default is 1.0 (not discount).
#' @param lr A positive numeric value. Learning rate (default 5e-5). You can set
#'        a learning schedule instead of a learning rate.
#' @param train_batch_size A positive integer value. Training batch size.
#'        Deprecated on the new API stack.
#' @param model A list. Arguments passed into the policy model. See
#'        \link{rl_dnn_config} for details.
#' @param sgd_minibatch_size A positive integer value. Total SGD batch size
#'        across all devices for SGD. Deprecated on the new API stack.
#' @param num_sgd_iter A positive integer value. Number of SGD iterations in
#'        each outer loop.
#' @param ... Other settings for training(). See the arguments of the training()
#'        function in the source code of RLlib.
#'        https://github.com/ray-project/ray/blob/master/rllib/algorithms/algorithm_config.py
#'        https://github.com/ray-project/ray/blob/master/rllib/algorithms/ppo/ppo.py
#'
#' @return A list of reinforcement learning configuration parameters
#' 
#' @examples
#' \dontrun{
#' allocation_rule <- learn_allocation_rule(
#'   models, 
#'   N_total = 150, N_ini = rep(10, 5), N_block = 10, Delta = 1.3,
#'   outcome_type = "continuous", sd_normal = sqrt(4.5), 
#'   seed = 123, 
#'   # We change `iter` to 200 and `cores` for reinforcement learning to 2
#'   rl_config = rl_config_set(iter = 200, cores = 2), 
#'   alpha = 0.025
#' )} 
#'
#' @export
rl_config_set <- function(iter = 1000L, 
                          save_start_iter = NULL,
                          save_every_iter = NULL,
                          cores = 4L,
                          # Common settings
                          gamma = 1.0, lr = 5e-5,
                          train_batch_size = 10000L, model = rl_dnn_config(),
                          # PPO specific settings
                          sgd_minibatch_size = 200L, num_sgd_iter = 20L,
                          ...) {
  iter <- as.integer(iter)
  save_start_iter <- ifelse(is.null(save_start_iter), 
                            ceiling(iter / 2), as.integer(save_start_iter))
  save_every_iter <- ifelse(is.null(save_every_iter), 
                            ceiling(iter / 2 / 5), as.integer(save_every_iter))
  cores <- as.integer(cores)
  gamma <- as.double(gamma)
  lr <- as.double(lr)
  train_batch_size <- as.integer(train_batch_size)
  sgd_minibatch_size <- as.integer(sgd_minibatch_size)
  num_sgd_iter <- as.integer(num_sgd_iter)

  stopifnot(length(iter) == 1L, iter > 0)
  stopifnot(length(cores) == 1L, cores > 0)
  stopifnot(length(gamma) == 1L, gamma > 0)
  stopifnot(length(train_batch_size) == 1L, train_batch_size > 0)
  stopifnot(length(sgd_minibatch_size) == 1L, sgd_minibatch_size > 0)
  stopifnot(length(num_sgd_iter) == 1L, num_sgd_iter > 0)

  df_config <- data.frame(
    iter, save_start_iter, save_every_iter, cores, 
    gamma, train_batch_size, sgd_minibatch_size, num_sgd_iter,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  if (nrow(df_config) != 1L) {
    stop("The length of each argument of rl_config is restricted to 1.")
  }
  config <- as.list(df_config)
  config$lr <- lr
  config$model <- model
  other_config <- list(...)

  append(config, other_config)
}
