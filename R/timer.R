#' @importFrom glue glue
Timer <- R6Class(
  "Timer",

  public = list(
    initialize = function(max_history_length = 5L) {
      private$max_history_length = max_history_length
    },
    start = function() {
      private$start_time = Sys.time()
    },
    stop = function() {
      private$stop_time = Sys.time()
      elapsed_time = difftime(private$stop_time, private$start_time, units = "secs")
      private$register_elapsed_time_history(elapsed_time)
    },
    elapsed = function(units = "secs") {
      difftime(private$stop_time, private$start_time, units = units)
    },
    estimate_remaining = function(n_remain) {
      mean_elapsed_time <- mean(private$elapsed_time_history)
      estimated_remaining <- mean_elapsed_time * n_remain

      units(estimated_remaining) <- "hours"
      hours <- trunc(estimated_remaining)
      estimated_remaining <- estimated_remaining - hours

      units(estimated_remaining) <- "mins"
      mins <- trunc(estimated_remaining)
      estimated_remaining <- estimated_remaining - mins

      units(estimated_remaining) <- "secs"
      secs <- trunc(estimated_remaining)
      estimated_remaining <- estimated_remaining - secs

      if (hours > 0) {
        glue("{hours} hours {mins} mins")
      } else if (mins > 0) {
        glue("{mins} mins")
      } else {
        glue("{secs} secs")
      }
    },
    estimate_finish_time = function(n_remain, format = "%Y-%m-%d %H:%M") {
      mean_elapsed_time <- mean(private$elapsed_time_history)
      estimated_remaining <- mean_elapsed_time * n_remain
      format(Sys.time() + estimated_remaining, format = format)
    }
  ),
  private = list(
    start_time = NULL,
    stop_time = NULL,
    elapsed_time = NULL,
    max_history_length = NULL,
    elapsed_time_history = NULL,

    register_elapsed_time_history = function(elapsed_time) {
      if (length(private$elapsed_time_history) >= private$max_history_length) {
        private$elapsed_time_history <- private$elapsed_time_history[-1]
      }
      private$elapsed_time_history = append(private$elapsed_time_history, elapsed_time)
    }
  )
)
