train_algo <- function(algo, n_start, N_update,
                       output_path, output_checkpoint_path,
                       save_start_iter, save_every_iter) {
  digits <- floor(log10(N_update)) + 1

  timer <- Timer$new()
  checkpoints <- c()
  episode_data <- data.frame()
  for (n in seq(n_start, N_update)) {
    timer$start()

    result <- algo$train()
    
    return_path <- "result$env_runners$episode_return_{x}"
    episode_len_mean <- result$env_runners$episode_len_mean
    return_func <- function(x) eval(parse(text = glue(return_path)))
    returns <- Map(return_func, c("min", "mean", "max"))
    episode <- data.frame(n = n,
                          episode_return_min  = returns$min,
                          episode_return_mean = returns$mean,
                          episode_return_max  = returns$max,
                          episode_len_mean    = episode_len_mean)
    episode_data <- rbind(episode_data, episode)

    returns <- Map(sprintf, returns, fmt = "%8.4f")
    # print(glue("{formatC(n, digits)}: Min/Mean/Max return: {returns$min}/{returns$mean}/{returns$max}"))

    # Save checkpoint
    if (n >= save_start_iter && (n - save_start_iter) %% save_every_iter == 0) {
      dir_path <- glue("{output_checkpoint_path}_{formatC(n, digits, flag = '0')}")
      save_result <- algo$save(dir_path)
      checkpoint_path <- save_result$checkpoint$path
      checkpoints <- c(checkpoints, checkpoint_path)
      message(glue("Checkpoint saved in directory '{checkpoint_path}'"))
    }

    timer$stop()
    elapsed_time <- timer$elapsed()
    estimated_remaining <- timer$estimate_remaining(N_update - n)

    print(glue("{formatC(n, digits)}: Min/Mean/Max return: {returns$min}/{returns$mean}/{returns$max} ({round(elapsed_time)} secs, remaining: {estimated_remaining})"))
  }

  dir_path <- glue("{output_checkpoint_path}_{formatC(n, digits, flag = '0')}")
  if (!dir.exists(dir_path)) {
    save_result <- algo$save(dir_path)
    checkpoint_path <- save_result$checkpoint$path
    checkpoints <- c(checkpoints, checkpoint_path)
    message(glue("Checkpoint saved in directory '{checkpoint_path}'"))
  }
  algo$stop()

  # Export allocation rule (policy)
  policy <- algo$get_policy(policy_id = "default_policy")
  policy$export_checkpoint(output_path)  # return NULL
  message(glue("Allocation rule saved in directory '{output_path}'"))

  list(episode_data = episode_data, checkpoints = checkpoints)
}
