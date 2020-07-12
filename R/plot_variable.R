#' Plot path metrics.
#'
#' Plots the metrics that have been calculated from path coordinates.
#'
#' Many of the summary metrics (as returned in the \code{summary} component of
#' the \code{\link{calculate_metrics}} output are useful for analysis in their
#' own right. These can be plotted as mean values over each trial with standard
#' error bars. If a factor is provided, then one data series will be plotted for
#' each level of the factor. To view data for mutliple factors, they will need
#' to be collapsed into one composite factor for plotting using this function.
#' If probe trials were used, then 'latency to goal' and several other variables
#' do not make much sense, so the data for the probe trials can be suppressed.
#' For this to work, a column named 'Probe' must be present in the experiment
#' description spreadsheet and must contain the value 'TRUE' for each probe
#' trial.
#'
#' The \code{variable} parameter can either be specified as the name of one of
#' the summary metrics, the name of one of the columns from the experiment
#' description, or as a numeric vector. In the latter case, the numeric vector
#' must be the same length as the number of tracks in the experiment.
#'
#'
#' Boundaries are drawn (as broken vertical lines) between different arena types
#' (for example between acquisition and goal reversal phases of a Morris water
#' maze experiment). By default, these are added between each unique arena
#' definition. If this is not appropriate, then this can be overridden by
#' providing the \code{boundaries} parameter with a \code{\link{data.frame}}
#' with two columns 'day' and 'trial'. Multiple boundaries can be defined by
#' entering the day and trial index into rows of this table. Use
#' \code{boundaries = NULL} to suppress boundary lines altogether.

#' @param variable The variable/metric that should be plotted. See Details for the ways to
#'   specify this.
#' @param experiment The \code{rtrack_experiment} object as returned from
#'   \code{\link{read_experiment}}.
#' @param factor The factor by which the data should be grouped. Each factor level will be
#'   plotted as a separate series. If not specified, all values are plotted together in
#'   one series.
#' @param factor.colours A colour to be used for each factor level. If not specified,
#'   colours will be automatically generated. The vector of colours is returned to allow
#'   additional plot customisation.
#' @param exclude.probe Should data from probe trials be excluded (see Details).
#' @param boundaries Where should the boundaries between arena types be drawn (see
#'   Details).
#' @param legend Should a legend be added. Default is \code{TRUE}.
#' @param x.axis The scale of the x axis. Default, "Day", is to add a labelled axis with
#'   tick marks at each day. If this parameter is set to "Trial", then tick marks are
#'   added for each trial. If set to "none", then no x axis will be drawn.
#' @param titles Should titles be drawn. Default is to add a main title and titles for the
#'   x and y axes. These can be supressed and added afterwards (using
#'   \code{\link[graphics]{title}}). This might be helpful for localising to a different
#'   language for example.
#' @param margins The margins of the plot (see the option \code{mar} in
#'   \code{\link[graphics]{par}}). The defaults should usually be fine, but they can be
#'   overridden if, for example, factor names are very long.
#' @param ... Other parameters passed to \code{\link[graphics]{segments}} to control the
#'   plotted lines.
#'
#' @return A named vector of colours used for each factor level.
#'
#' @examples
#' # This function relies on data too large to include in the package.
#' # For a worked example, please see the vignette "Rtrack MWM analysis".
#'
#' @importFrom graphics par plot lines segments rect axis title
#' @importFrom stats aggregate
#'
#' @export
plot_variable = function(variable, experiment, factor = NA, factor.colours = "auto", exclude.probe = FALSE, boundaries = NA, legend = TRUE, x.axis = "Day", titles = TRUE, margins = c(5, 4, 4, 8), ...){
	plotting.data = experiment$factors
	# The variable can be in the summary metrics, in the experiment factors, or a separately supplied factor/character vector (checked in that order)
	if(is.character(variable) & length(variable) == 1 & variable[1] %in% experiment$summary.variables){
		plotting.data$y = sapply(lapply(experiment$metrics, "[[", "unscaled.summary"), "[[", variable)
	}else if(is.numeric(variable) & length(variable) == 1 & variable[1] %in% colnames(experiment$factors)){
		plotting.data$y = experiment$factors[, variable]
	}else if(is.numeric(variable) & length(variable) == nrow(experiment$factors)){
		plotting.data$y = variable
		variable = deparse(substitute(variable)) # This becomes the name shown on the plot
	}else{
			stop(paste0("The supplied variable cannot be found in the summary metrics or among the experiment factors and/or it is not a valid numeric vector."))
	}
	if(exclude.probe){
		if(!is.null(plotting.data$Probe) & all(plotting.data$Probe %in% c("TRUE", "FALSE"))){
			plotting.data$y[plotting.data$Probe == "TRUE"] = NA
		}else{
			warning("There is no 'Probe' factor in the experiment (or it contains values other that TRUE/FALSE). All data will be plotted instead.")
		}
	}
	plotting.data$`_Day` = as.numeric(plotting.data$`_Day`)
	plotting.data$`_Trial` = as.numeric(plotting.data$`_Trial`)
	plotting.data = plotting.data[order(plotting.data$`_Day`, plotting.data$`_Trial`), ]
	plotting.data$trial.id = paste(plotting.data$`_Day`, plotting.data$`_Trial`, sep = "_")
	plotting.data$x = sapply(plotting.data$trial.id, function(id) which(unique(plotting.data$trial.id) == id) )

	parameters = c(...)
	boundary.lwd = as.numeric(parameters[which(names(parameters) == "lwd")])
	if(length(boundary.lwd) == 0 ) boundary.lwd = 2

	if(is.na(factor)){
		plot.factor = as.character(rep(1, nrow(experiment$factors)))
		plot.levels = unique(plot.factor)
		plot.series = list("All data" = 1:nrow(experiment$factors))
	}else if(factor %in% names(plotting.data)){
		plot.factor = as.character(plotting.data[, factor])
		plot.levels = unique(plot.factor)
		plot.series = lapply(plot.levels, function(level) which(plot.factor == level) )
		names(plot.series) = plot.levels
	}else{
		warning(paste0("The factor '", factor, "' is not present in this experiment. Plotting all values in one series instead."))
		plot.factor = as.character(rep(1, nrow(experiment$factors)))
		plot.levels = unique(plot.factor)
		plot.series = list("All data" = 1:nrow(experiment$factors))
	}
	
	global.y.mean = by(plotting.data$y, list(plotting.data$`_Trial`, plotting.data$`_Day`, plot.factor), mean, na.rm = T)
	global.y.se = by(plotting.data$y, list(plotting.data$`_Trial`, plotting.data$`_Day`, plot.factor), sd, na.rm = T) / sqrt(by(plotting.data$y, list(plotting.data$`_Trial`, plotting.data$`_Day`, plot.factor), length))
	plot.min = min(global.y.mean - global.y.se, na.rm = T)
	plot.max = max(global.y.mean + global.y.se, na.rm = T)
	if(legend){
		.parprevious = graphics::par(mar = margins, xpd = TRUE)
		on.exit(par(.parprevious))
		plot(plotting.data$x, plotting.data$y, ylim = c(plot.min, plot.max), type = "n", xaxt = "n", bty = "n", las = 2, lwd = boundary.lwd, xlab = "", ylab = "")
	}else{
		plot(plotting.data$x, plotting.data$y, ylim = c(plot.min, plot.max), type = "n", xaxt = "n", bty = "n", las = 2, lwd = boundary.lwd, xlab = "", ylab = "")
	}
	if(titles){
		if(tolower(x.axis) == "day"){
			title(xlab = "Day", ylab = gsub("\\.", " ", paste0(toupper(substring(variable, 1, 1)), tolower(substring(variable, 2)))) )
		}else if(tolower(x.axis) == "trial"){
			title(xlab = "Trial", ylab = gsub("\\.", " ", paste0(toupper(substring(variable, 1, 1)), tolower(substring(variable, 2)))) )
		}else{
			title(xlab = "", ylab = gsub("\\.", " ", paste0(toupper(substring(variable, 1, 1)), tolower(substring(variable, 2)))) )
		}
	}
	pad = 0.1
	arena = as.character(aggregate(plotting.data$`_Arena`, list(plotting.data$`_Trial`, plotting.data$`_Day`), `[`, 1)$x)
	# If boundaries not user-set, then place at the interface of different arenas
	if(is.null(boundaries)){
		boundaries = NULL
	}else if(is.na(boundaries)){
		boundaries = which(arena != c(arena[1], arena[-length(arena)]))
	}else{
		colnames(boundaries) = tolower(colnames(boundaries))
		all.trials = unique(plotting.data[, c("_Day", "_Trial")])
		boundaries = apply(boundaries, 1, function(row) which(all.trials$`_Day` == as.numeric(row["day"]) & all.trials$`_Trial` == as.numeric(row["trial"])) )
	}
	if(!is.null(boundaries) & length(boundaries) > 0){
		segments(boundaries - 0.5, plot.min, boundaries - 0.5, plot.max, lty = 3, lwd = boundary.lwd, col = "#000000FF")
	}
	# Plotting.data is ordered, so put day marker at first trial of the day (might not be trial 1 if there were experimental dropouts)
	day = match(unique(plotting.data$`_Day`), unique(plotting.data[, c("_Trial", "_Day")])$`_Day`)
	if(tolower(x.axis) == "day"){
		axis(1, at = c(day, max(day) + max(as.numeric(plotting.data$`_Trial`))), labels = c(1:length(day), ""))
	}else if(tolower(x.axis) == "trial"){
		axis(1, at = 1:length(unique(plotting.data$trial.id)), labels = 1:length(unique(plotting.data$trial.id)))
	}

	if(all(factor.colours == "auto")){
		if(all(names(plot.series) %in% names(factor.colours)) & !is.null(names(factor.colours))){
			# Reorder the factor based on the order of colour names (but still auto-select colours)
			plot.series = plot.series[names(factor.colours)]
		}
		factor.colours = colorRampPalette(c("#D40000", "#FEFA00", "#339900", "#1040C0"))(length(names(plot.series)))
		names(factor.colours) = names(plot.series)
	}else if(all(names(plot.series) %in% names(factor.colours)) & !is.null(names(factor.colours))){
		# Reorder the factor based on the order of colour names
		plot.series = plot.series[names(factor.colours)]
	}else{
		warning(paste0("The parameter '", factor.colours, "' is not a vector of colours with the names. Automatically assigning colours instead."))
		factor.colours = colorRampPalette(c("#D40000", "#FEFA00", "#339900", "#1040C0"))(length(names(plot.series)))
		names(factor.colours) = names(plot.series)
	}
	
	plot.values = sapply(names(plot.series), function(level){
		group = plotting.data[plot.series[[level]], ]
		x = unique(group$x)
		y.mean = by(group$y, list(group$`_Trial`, group$`_Day`), mean, na.rm = T)
		y.se = by(group$y, list(group$`_Trial`, group$`_Day`), sd, na.rm = T) / sqrt(by(group$y, list(group$`_Trial`, group$`_Day`), length))
	
		plotting.segments = cbind(x[1:(length(x) - 1)], y.mean[1:(length(x) - 1)], x[2:length(x)], y.mean[2:length(x)])
		if(!is.null(boundaries) & length(boundaries) > 0){
			plotting.segments = plotting.segments[-(boundaries - 1), ]
		}
		list(plotting.segments = plotting.segments, x = x, y.mean = y.mean, y.se = y.se, col = factor.colours[level])
	}, simplify = FALSE, USE.NAMES = TRUE)

	. = lapply(plot.values, function(plot.val){
		segments(plot.val$plotting.segments[, 1], plot.val$plotting.segments[, 2], plot.val$plotting.segments[, 3], plot.val$plotting.segments[, 4], xaxt = "n", col = plot.val$col, ...)
		segments(plot.val$x, plot.val$y.mean + plot.val$y.se, plot.val$x, plot.val$y.mean - plot.val$y.se, col = plot.val$col, ...)
		segments(plot.val$x - pad, plot.val$y.mean + plot.val$y.se, plot.val$x + pad, plot.val$y.mean + plot.val$y.se, col = plot.val$col, ...)
		segments(plot.val$x - pad, plot.val$y.mean - plot.val$y.se, plot.val$x + pad, plot.val$y.mean - plot.val$y.se, col = plot.val$col, ...)
	})

	if(legend) legend(par("usr")[2], par("usr")[4], legend = names(factor.colours), fill = factor.colours, cex = .7, bty = "n", border = NA)

	invisible(factor.colours)
}
