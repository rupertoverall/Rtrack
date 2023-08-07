#' Analysis of spatial tracking data and strategy classification.
#'
#' A toolkit for the analysis of paths from spatial tracking experiments (such
#' as the Morris water maze, Barnes maze and open field among others) and
#' calculation of goal-finding strategies where appropriate. This package
#' includes methods for path classification using machine learning.
#'
#' @section Functions provided: \code{\link{calculate_metrics}}
#'
#'   \code{\link{call_strategy}}
#'
#'   \code{\link{check_experiment}}
#'
#'   \code{\link{export_data}}
#'
#'   \code{\link{export_results}}
#'
#'   \code{\link{identify_track_format}}
#'
#'   \code{\link{plot_density}}
#'
#'   \code{\link{plot_path}}
#'
#'   \code{\link{plot_strategies}}
#'
#'   \code{\link{plot_variable}}
#'
#'   \code{\link{read_arena}}
#'
#'   \code{\link{read_experiment}}
#'
#'   \code{\link{read_path}}
#'
#'   \code{\link{threshold_strategies}}
#'
#' @docType package
#' @name Rtrack
NULL

.onLoad <- function(...) {
	registerS3method("print", "rtrack_experiment", print_experiment)
	registerS3method("print", "rtrack_metrics", print_metrics)
	registerS3method("print", "rtrack_path", print_path)
}
