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
#' @field checkpoints The integer vector of iteration counts for checkpoints.
#' @field checkpoints_paths The paths to the directories where each checkpoint is stored.
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
      } else if (!grepl("/|\\\\", dir)) {
        # If 'dir' does not contain '/' or '\\', it is a directory name.
        dir <- file.path(base_dir, dir)
      }

      # If dir is a checkpoint directory, change it to its policy directory
      if ("policies" %in% list.dirs(dir, full.names = FALSE, recursive = FALSE)) {
        dir <- file.path(dir, "policies", "default_policy")
      }

      # Restore policy object
      policy_lib <- reticulate::import("ray.rllib.policy.policy")
      policy <- policy_lib$Policy$from_checkpoint(dir)

      # Compress the policy directory and keep it in binary format in this object
      compressed_policy_file <- tempfile(fileext = ".zip")
      zip(zipfile = compressed_policy_file, files = list.files(dir, full.names = TRUE))
      private$policy_binary <- readBin(compressed_policy_file, what = "raw",
                                       n = file.info(compressed_policy_file)$size)

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
      # If policy has been reset, reload it from the directory where it is stored
      if (is.null(self$policy$config)) {
        # If the directory where policy is stored cannot be found, restore it from the binary data
        if (!dir.exists(self$dir)) {
          compressed_policy_file <- tempfile(fileext = ".zip")
          writeBin(private$policy_binary, compressed_policy_file)
          unzip(compressed_policy_file)
          message(glue("Created allocation rule directory '{self$dir}'."))
        }
        policy_lib <- reticulate::import("ray.rllib.policy.policy")
        self$policy <- policy_lib$Policy$from_checkpoint(self$dir)
      }

      # Extract the clinical trial settings from the allocation rule (policy)
      policy <- self$policy
      env_config <- policy$config$env_config
      K <- policy$action_space$n
      N_total <- env_config$N_total
      N_ini <- sum(env_config$N_ini)

      # Check arguments
      stopifnot(length(doses) == length(resps))
      
      doses <- as.numeric(doses)
      responses <- as.numeric(resps)

      # Convert doses to actions
      action_list <- seq_len(K) - 1L
      dose_list <- env_config$doses
      names(action_list) <- as.character(dose_list)
      excluded_doses <- setdiff(unique(doses), dose_list)
      if (length(excluded_doses) > 0L) {
        excluded_doses <- paste(excluded_doses, collapse = ", ")
        stop(glue("{excluded_doses} is not included in doses on the learning."))
      }
      actions <- action_list[as.character(doses)]

      d <- data.frame(action = actions, resp = responses)
      count_per_action <- tapply(d$resp, d$action, length)
      
      # Check arguments
      stopifnot("the number of allocated subjects at each dose should be >= 2" = count_per_action >= 2L)
      
      # Obtain the probabilities of next actions
      mean_resps <- tapply(d$resp, d$action, mean)
      shifted_mean_resps <- mean_resps[-1] - mean_resps[1]
      sd_resps <- tapply(d$resp, d$action, function(x) sd(x)*sqrt((length(x) - 1)/length(x)))
      ratio_per_action <- count_per_action / N_total
      state <- as.array(unname(c(shifted_mean_resps, sd_resps, ratio_per_action)))
      info <- policy$compute_single_action(state, full_fetch = TRUE)[[3L]]
      action_probs <- info$action_dist_inputs  # array
      action_probs <- as.vector(action_probs)  # cast to numeric vector
      action_probs <- softmax(action_probs)
      action_probs
    },

    #' @description
    #' Resume learning the allocation rule. This function updates the original
    #' AllocationRule object.
    #'
    #' @param iter A number of additional iterations.
    #'
    #' @return An updated \link{AllocationRule} object.
    resume_learning = function(iter) {
      checkpoint_path <- tail(self$checkpoints_paths, 1L)
      algorithm <- reticulate::import("ray.rllib.algorithms.algorithm")
      algo <- algorithm$Algorithm$from_checkpoint(checkpoint_path)

      output_path <- self$dirpath
      output_checkpoint_path <- sub("^(.*)_\\d+$", "\\1", checkpoint_path)
      save_start_iter <- self$input$save_start_iter
      save_every_iter <- self$input$save_every_iter
      N_update <- self$info$iterations + iter

      n_start <- self$info$iterations + 1L

      result <- train_algo(algo, n_start, N_update,
                           output_path, output_checkpoint_path,
                           save_start_iter, save_every_iter)

      checkpoints <- c(self$checkpoints_paths, result$checkpoints)
      episode_data <- rbind(self$info$log, result$episode_data)

      self$info$iterations <- N_update
      self$log <- episode_data
      private$set_checkpoints(checkpoints)

      self
    },

    #' @description
    #' Set information when learning the allocation rule.
    #'
    #' @param info Information when learning the allocation rule.
    #' @param input Inputs for learning the allocation rule.
    #' @param log The log of scores during the learning of the allocation rule.
    #' @param checkpoints The paths to the directories where each checkpoint is stored.
    set_info = function(info, input, log, checkpoints) {
      self$info <- info
      self$input <- input
      self$log <- log
      private$set_checkpoints(checkpoints)
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
  ),

  private = list(
    policy_binary = NULL,

    set_checkpoints = function(checkpoints_paths) {
      self$checkpoints_paths = checkpoints_paths
      self$checkpoints <- as.integer(sub(".*_(\\d+)$", "\\1", checkpoints_paths))
    }
  )
)
