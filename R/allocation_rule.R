#' Allocation Rule Class
#'
#' @description
#' This class represents an allocation rule that generates a next allocation.
#'
#' @field policy The RLlib policy that is a Python object.
#' @field dir Directory path of the allocation rule (policy).
#' @field dirpath Full path to the directory of the allocation rule.
#' @field created_at Created time of this object.
#' @field info Information when learning the allocation rule.
#' @field input Inputs for learning the allocation rule.
#' @field log The log of scores during the learning of the allocation rule.
#'
#' @export
AllocationRule <- R6Class(
  "AllocationRule",

  public = list(
    policy = NULL,
    dir = NULL,
    dirpath = NULL,
    created_at = NULL,

    info = NULL,
    input = NULL,
    log = NULL,
    checkpoints = NULL,
    checkpoints_paths = NULL,

    #' @description
    #' Create a new AllocationRule object.
    #'
    #' @param dir A character value. A directory name or path where an
    #'        allocation rule is outputted. By default, the latest allocation
    #'        rule is searched in 'base_dir'.
    #' @param base_dir A character value. A directory path that is used as the
    #'        parent directory if the 'dir' argument is a directory name and is
    #'        not used otherwise.
    initialize = function(dir = "latest", base_dir = "allocation_rules") {
      # Check arguments
      stopifnot(length(dir) == 1L)
      stopifnot(length(base_dir) == 1L)

      # Identify the specified directory path
      if (dir == "latest") {
        # Search the latest directory in 'base_dir'
        df <- file.info(dir(base_dir, full.names = TRUE))
        df <- df[df$isdir, , drop = FALSE]
        df <- df[df$mtime == max(df$mtime), , drop = FALSE]
        stopifnot("Cannot identify the latest allocation rule" = nrow(df) == 1L)
        dir <- rownames(df)
      } else if (!grepl("/", dir)) {
        # If 'dir' does not contain '/', it is a directory name.
        dir <- file.path(base_dir, dir)
      }

      # Restore policy object
      policy_lib <- reticulate::import("ray.rllib.policy.policy")
      policy <- policy_lib$Policy$from_checkpoint(dir)

      self$policy <- policy
      self$dir <- dir
      self$dirpath <- normalizePath(dir)
      self$created_at <- format(Sys.time(), format = "%Y-%m-%d %H:%M:%S")
    },

    #' @description
    #' Obtain the probabilities of the next action.
    #'
    #' @param doses A numeric vector. The doses actually administered to each
    #'        participant in your clinical trial. It must include all previous
    #'        doses.
    #' @param resps A numeric vector. The values of responses corresponding to
    #'        each participant for the 'doses' argument.
    #'
    #' @return A vector of the probabilities of the next action.
    #'
    #' @importFrom glue glue
    get_next_action_probs = function(doses, resps) {
      # Extract the clinical trial settings from the allocation rule (policy)
      policy <- self$policy
      env_config <- policy$config$env_config
      K <- policy$action_space$n
      N_total <- env_config$N_total
      N_ini <- sum(env_config$N_ini)

      # Check arguments
      stopifnot(length(doses) == length(resps))
      if (length(doses) < N_ini) {
        warning("The length of 'doses' and 'resps' is less than 'N_ini'. You must input all data obtained so far.")
      }

      # Cast arguments
      doses <- as.numeric(doses)
      responses <- as.numeric(resps)

      # Convert doses to actions
      action_list <- seq_len(K) - 1L
      dose_list <- env_config$doses
      names(action_list) <- as.character(dose_list)
      excluded_doses <- setdiff(unique(doses), dose_list)
      if (length(excluded_doses) > 0) {
        excluded_doses <- paste(excluded_doses, collapse = ", ")
        stop(glue("{excluded_doses} is not included in doses on the learning."))
      }
      actions <- action_list[as.character(doses)]

      # Obtain the probabilities of next actions
      reticulate::source_python(system.file("python/RProcess.py", package = "RLoptimal"))
      reticulate::source_python(system.file("python/MCPModEnv.py", package = "RLoptimal"))
      state <- MCPModEnv$compute_state(actions, responses, N_total)
      info <- policy$compute_single_action(state, full_fetch = TRUE)[[3]]
      action_probs <- info$action_dist_inputs  # array
      action_probs <- as.vector(action_probs)  # cast to numeric vector
      action_probs <- softmax(action_probs)
      action_probs
    },

    resume_learning = function(iter) {
      checkpoint_path <- tail(self$checkpoints_paths, 1L)
      algorithm <- reticulate::import("ray.rllib.algorithms.algorithm")
      algo <- algorithm$Algorithm$from_checkpoint(checkpoint_path)

      output_path <- self$dirpath
      output_checkpoint_path <- sub("^(.*)_\\d+$", "\\1", checkpoint_path)
      # TODO
      save_start_iter <- 300L
      save_every_iter <- 100L
      N_update <- self$info$iterations + iter
      digits <- floor(log10(N_update)) + 1

      timer <- Timer$new()
      checkpoints <- self$checkpoints_paths
      episode_data <- self$info$log
      for (n in seq(self$info$iterations + 1, N_update)) {
        timer$start()

        result <- algo$train()

        if (ray_version() < "2.23.0") {
          reward_path <- "result$episode_reward_{x}"
          episode_len_mean <- result$episode_len_mean
        } else {
          reward_path <- "result$env_runners$episode_reward_{x}"
          episode_len_mean <- result$env_runners$episode_len_mean
        }
        reward_func <- function(x) eval(parse(text = glue(reward_path)))
        rewards <- Map(reward_func, c("min", "mean", "max"))
        episode <- data.frame(n = n,
                              episode_reward_min  = rewards$min,
                              episode_reward_mean = rewards$mean,
                              episode_reward_max  = rewards$max,
                              episode_len_mean    = episode_len_mean)
        episode_data <- rbind(episode_data, episode)

        rewards <- Map(sprintf, rewards, fmt = "%8.4f")
        # print(glue("{formatC(n, digits)}: Min/Mean/Max reward: {rewards$min}/{rewards$mean}/{rewards$max}"))

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

        print(glue("{formatC(n, digits)}: Min/Mean/Max reward: {rewards$min}/{rewards$mean}/{rewards$max} ({round(elapsed_time)} secs, remaining: {estimated_remaining})"))
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

      self$info$iterations <- n
      self$log <- episode_data
      self$checkpoints_paths <- checkpoints
      self$checkpoints <- as.integer(sub(".*_(\\d+)$", "\\1", checkpoints))
    },

    #' @description
    #' Set information when learning the allocation rule.
    #'
    #' @param info Information when learning the allocation rule.
    #' @param input Inputs for learning the allocation rule.
    #' @param log The log of scores during the learning of the allocation rule.
    set_info = function(info, input, log, checkpoints) {
      self$info <- info
      self$input <- input
      self$log <- log
      self$checkpoints_paths <- checkpoints
      self$checkpoints <- as.integer(sub(".*_(\\d+)$", "\\1", checkpoints))
    },

    #' @description
    #' Print function for AllocationRule object
    #'
    #' @importFrom glue glue
    print = function() {
      print(glue("<AllocationRule>"))
      print(glue("dir: {self$dir}"))
      print(glue("created at: {self$created_at}"))
      if (!is.null(self$info)) {
        print(glue("call:"))
        print(glue("{deparse(self$info$call)}"))
        print(glue("iterations: {self$info$iterations}"))
        checkpoints <- paste0(self$checkpoints, collapse = ", ")
        print(glue("checkpoints: {checkpoints}"))
      }
    }
  )
)
