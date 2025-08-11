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
#' If probe trials were used, then "latency to goal" and several other variables
#' do not make much sense, so the data for the probe trials can be suppressed.
#' For this to work, a column named "Probe" must be present in the experiment
#' description spreadsheet and must contain the value "TRUE" for each probe
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
#' providing the \code{boundaries} parameter with a \code{\link{matrix}} or
#' \code{\link{data.frame}} with two columns for the day and trial number
#' respectively. Multiple boundaries can be defined by entering the day and
#' trial index into rows of this table. Use \code{boundaries = NULL} to suppress
#' boundary lines altogether.
#'
#' @param variable The variable/metric that should be plotted. See Details for
#'   the ways to specify this.
#' @param experiment The \code{rtrack_experiment} object as returned from
#'   \code{\link{read_experiment}}.
#' @param factor The factor (from the table in \code{experiment$factors}) by
#'   which the data should be grouped. Each factor level will be plotted as a
#'   separate series. If not specified, all values are plotted together in one
#'   series.
#' @param factor.colours A colour to be used for each factor level. If not
#'   specified, colours will be automatically generated. The vector of colours
#'   is returned to allow additional plot customisation.
#' @param x.axis The scale of the x axis. "Day" will add a labelled axis with
#'   tick marks at each day. If this parameter is set to "Trial", then tick
#'   marks are added for each trial. If set to "none", then no x axis will be
#'   drawn. Default (\code{NA}) selects an option appropriate for the experiment
#'   type.
#' @param type The type of plot to draw. Either "p" for a side-by-side
#'   stripchart with individual points, "l" for a line graph connecting
#'   medians/means and whiskers/error bars for data spread (upper and lower
#'   quartiles or standard deviation, depending on the value of \code{dist}), or
#'   "b" for both of these overlaid. Additional options "L" and "B" are like
#'   their lower-case counterparts but a point is also added at the median/mean.
#'   This is valuable especially when there is only one trial between
#'   boundaries. The default is \code{NA} indicating that the function should
#'   choose the most appropriate type for the plot.
#' @param dist The type of distribution. Either "quartiles" or "non-parametric"
#'   (default) to show the median +/- upper and lower quartiles, or "sd" or
#'   "parametric" to show mean +/- standard deviation. This parameter can be
#'   abbreviated. The parametric option should only be used if the data come
#'   from a normal/Gaussian distribution. Although not a measure of the
#'   distribution, it is also possible to use this parameter to show the mean
#'   +/- the standard error of the mean (SEM) using the option "sem".
#' @param transparency A value from 0 (fully transparent) to 1 (fully opaque)
#'   governing the transparency of the points. If filled symbols are used, then
#'   transparency can help distinguish overlapping points.
#' @param exclude.probe Should data from probe trials be excluded (see Details).
#' @param boundaries Where should the boundaries between arena types be drawn
#'   (see Details).
#' @param boundary.lwd The thickness of the boundaries. Default is 1.
#' @param legend Should a legend be added. Default is \code{TRUE}.
#' @param axis.titles Should axis titles be drawn. Default is to add titles for
#'   the x and y axes. These can be suppressed and added afterwards (using
#'   \code{\link[graphics]{title}}). This might be helpful for localising to a
#'   different language for example.
#' @param margins The margins of the plot (see the option \code{mar} in
#'   \code{\link[graphics]{par}}). The defaults should usually be fine, but they
#'   can be overridden if, for example, factor names are very long.
#' @param ... Additional arguments to \code{plot}.
#'
#' @return A named vector of colours used for each factor level.
#'
#' @examples
#' # This function relies on data too large to include in the package.
#' # For a worked example, please see the vignette "Rtrack MWM analysis"
#' # in the online package documentation at
#' # https://rupertoverall.net/Rtrack/.
#'
#' @importFrom graphics par plot lines points segments axis title
#' @importFrom stats aggregate sd na.omit
#' @importFrom utils head tail
#' @importFrom grDevices boxplot.stats
#' @importFrom scales alpha
#'
#' @export
plot_variable = function(variable, experiment, factor = NA, factor.colours = "auto", x.axis = NA, type = NA, dist = "q", transparency = 0.25, exclude.probe = FALSE, boundaries = NA, boundary.lwd = 1, legend = TRUE, axis.titles = TRUE, margins = c(5, 4, 4, 8), ...){
	plotting.data = experiment$factors
	# The variable can be in the summary metrics, in the experiment factors, or a separately supplied factor/character vector (checked in that order)
	if(is.character(variable) & length(variable) == 1 & variable[1] %in% experiment$summary.variables){
		plotting.data$y = sapply(lapply(experiment$metrics, "[[", "summary"), "[[", variable)
	}else if(is.character(variable) & length(variable) == 1 & variable[1] %in% colnames(experiment$factors)){
		plotting.data$y = experiment$factors[, variable]
		if(!is.numeric(plotting.data$y)) stop(paste0("The supplied variable is not a valid numeric vector."))
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
	trial.names = setNames(seq_along(unique(plotting.data$trial.id)), unique(plotting.data$trial.id))
	plotting.data$trial.index = factor(as.numeric(trial.names[plotting.data$trial.id]))

	if(tolower(substring(dist, 1, 2)) == "se"){
		dist = "sem"
	}else if(substring(dist, 1, 1) %in% c("q", "n")){
		dist = "q"
	}else if(substring(dist, 1, 1) %in% c("s", "p")){
		dist = "s"
	}else{
		dist = "q"
		warning(paste0("The value of '", dist, "' for the 'dist' parameter is not valid. The default settings of median +/- quartiles will be used instead."))
	}
	
	parameters = list(...)
	lwd = as.numeric(parameters[which(names(parameters) == "lwd")])
	if(length(lwd) == 0 ) lwd = 2
	las = as.numeric(parameters[which(names(parameters) == "las")])
	if(length(las) == 0 ) las = 2
	cex = as.numeric(parameters[which(names(parameters) == "cex")])
	if(length(cex) == 0 ) cex = 1
	pch = as.numeric(parameters[which(names(parameters) == "pch")])
	if(length(pch) == 0 ) pch = 20
	
	count = function(x) sum(!is.na(x))
		
	if(is.na(factor)){
		plot.factor = as.character(rep("All data", nrow(experiment$factors)))
		plot.levels = unique(plot.factor)
		plot.series = list("All data" = 1:nrow(experiment$factors))
	}else if(factor %in% names(plotting.data)){
		plot.factor = as.character(plotting.data[, factor])
		plot.levels = unique(plot.factor)
		plot.series = lapply(plot.levels, function(level) which(plot.factor == level) )
		names(plot.series) = plot.levels
	}else{
		warning(paste0("The factor '", factor, "' is not present in this experiment. Plotting all values in one series instead."))
		plot.factor = as.character(rep("All data", nrow(experiment$factors)))
		plot.levels = unique(plot.factor)
		plot.series = list("All data" = 1:nrow(experiment$factors))
	}
	# Make sure levels are correct.
	plot.factor = factor(plot.factor, plot.levels)
	
	.parprevious = graphics::par(mar = margins, xpd = TRUE)
	on.exit(graphics::par(.parprevious))

	if(all(factor.colours == "auto")){
		if(all(names(plot.series) %in% names(factor.colours)) & !is.null(names(factor.colours))){
			# Reorder the factor based on the order of colour names (but still auto-select colours)
			plot.series = plot.series[names(factor.colours)]
		}
		factor.colours = colorRampPalette(c("#C42000", "#FEB000", "#638900", "#0840B0"))(length(names(plot.series)))
		names(factor.colours) = names(plot.series)
	}else if(all(names(plot.series) %in% names(factor.colours)) & !is.null(names(factor.colours))){
		# Reorder the factor based on the order of colour names
		plot.series = plot.series[names(factor.colours)]
	}else{
		warning(paste0("The parameter '", factor.colours, "' is not a vector of colours with the group level names. Automatically assigning colours instead."))
		factor.colours = colorRampPalette(c("#C42000", "#FEB000", "#638900", "#0840B0"))(length(names(plot.series)))
		names(factor.colours) = names(plot.series)
	}

	# Plot type and x axis default is different for the different test types.
	test.type = experiment$metrics[[1]]$arena$type
	type = type[1]
	# Override the plot if no days or trials (factors will be used).
	if(length(table(plotting.data$`_Trial`)) == 1 & length(table(plotting.data$`_Day`)) == 1){
		if(is.na(x.axis)) x.axis = ""
		if(is.na(type)) type = "p"
	}
	if(test.type == "mwm" | test.type == "barnes" | test.type == "apa"){
		if(is.na(x.axis)) x.axis = "Day"
		if(is.na(type)) type = "L"
	}else if(test.type == "oft" | test.type == "nor"){
		if(is.na(x.axis)) x.axis = "Trial"
		if(is.na(type)) type = "p"
	}
	
	pad = 0.25
	total.plot.width = length(levels(plotting.data$trial.index)) + ((length(levels(plotting.data$trial.index)) - 1) * pad)
	if(!all(is.na(plotting.data$y))){
		plot.range = range(plotting.data$y, na.rm = TRUE)
	}else{
		plot.range = c(0, 1)
	}

	arena = as.character(stats::aggregate(plotting.data$`_Arena`, list(plotting.data$`_Trial`, plotting.data$`_Day`), `[`, 1)$x)
	do.call(graphics::plot, c(list(c(0, max(total.plot.width)), plot.range, type = "n", xaxt = "n", bty = "n", las = las, xlab = "", ylab = ""), parameters))

	# If boundaries not user-set, then place at the interface of different arenas
	boundary.indices = NULL
	if(is.null(boundaries[1])){
		boundary.indices = NULL
	}else if(is.na(boundaries[1])){
		boundary.indices = which(arena != c(arena[1], arena[-length(arena)]))
	}else{
		all.trials = unique(plotting.data[, c("_Day", "_Trial")])
		boundary.indices = apply(boundaries, 1, function(row) which(all.trials$`_Day` == as.numeric(row[1]) & all.trials$`_Trial` == as.numeric(row[2])) )
	}
	# Plot boundaries.
	if(!is.null(boundary.indices) & length(boundary.indices) > 0){
		boundary.points = (boundary.indices - 1 - pad / 2) + ((boundary.indices - 1) * pad)
		graphics::segments(boundary.points, plot.range[1], boundary.points, plot.range[2], lty = 3, lwd = boundary.lwd, col = "#000000FF")
	}

	# For each time block ('trial'), plot the y values for each group side-by-side.
	ti = as.numeric(unique(plotting.data$trial.index))
	gi = names(plot.series)
	step = 1 / length(gi)

	grouped.values = NULL
	if(dist == "q"){
		grouped.values = setNames(lapply(gi, function(g){
			setNames(lapply(ti, function(t){
				list(x = numeric(), y = numeric(), col = "")
			}), paste0("trial.", ti))
		}), gi)
		for(t in seq_along(ti)){
			tstart = (t - 1 - step / 2) + ((t - 1) * pad)
			for(g in seq_along(gi)){
				y = plotting.data$y[which(plotting.data$trial.index == ti[t] & plot.factor == gi[g])]
				x = rep(tstart + (g * step), length(y))
				stats = grDevices::boxplot.stats(y)$stats
				if(type == "p" | type == "b" | type == "B") graphics::points(x, y, col = scales::alpha(factor.colours[g], transparency), pch = pch)
				grouped.values[[g]][[t]]$x = t + ((t - 1) * pad) - 0.5
				grouped.values[[g]][[t]]$y = y
				grouped.values[[g]][[t]]$lower = stats[2]
				grouped.values[[g]][[t]]$median = stats[3]
				grouped.values[[g]][[t]]$upper = stats[4]
				grouped.values[[g]][[t]]$col = factor.colours[g]
			}
		}
	}else if(dist == "s"){
		grouped.values = setNames(lapply(gi, function(g){
			setNames(lapply(ti, function(t){
				list(x = numeric(), y = numeric(), col = "")
			}), paste0("trial.", ti))
		}), gi)
		for(t in seq_along(ti)){
			tstart = (t - 1 - step / 2) + ((t - 1) * pad)
			for(g in seq_along(gi)){
				y = plotting.data$y[which(plotting.data$trial.index == ti[t] & plot.factor == gi[g])]
				x = rep(tstart + (g * step), length(y))
				mean = mean(y, na.rm = TRUE)
				sd = stats::sd(y, na.rm = TRUE)
				if(type == "p" | type == "b" | type == "B") graphics::points(x, y, col = scales::alpha(factor.colours[g], transparency), pch = pch)
				grouped.values[[g]][[t]]$x = t + ((t - 1) * pad) - 0.5
				grouped.values[[g]][[t]]$y = y
				grouped.values[[g]][[t]]$lower = mean - sd
				grouped.values[[g]][[t]]$mean = mean
				grouped.values[[g]][[t]]$upper = mean + sd
				grouped.values[[g]][[t]]$col = factor.colours[g]
			}
		}
	}else if(dist == "sem"){
		grouped.values = setNames(lapply(gi, function(g){
			setNames(lapply(ti, function(t){
				list(x = numeric(), y = numeric(), col = "")
			}), paste0("trial.", ti))
		}), gi)
		for(t in seq_along(ti)){
			tstart = (t - 1 - step / 2) + ((t - 1) * pad)
			for(g in seq_along(gi)){
				y = plotting.data$y[which(plotting.data$trial.index == ti[t] & plot.factor == gi[g])]
				x = rep(tstart + (g * step), length(y))
				mean = mean(y, na.rm = TRUE)
				se = stats::sd(y, na.rm = TRUE) / sqrt(length(stats::na.omit(y)))
				if(type == "p" | type == "b" | type == "B") graphics::points(x, y, col = scales::alpha(factor.colours[g], transparency), pch = pch)
				grouped.values[[g]][[t]]$x = t + ((t - 1) * pad) - 0.5
				grouped.values[[g]][[t]]$y = y
				grouped.values[[g]][[t]]$lower = mean - se
				grouped.values[[g]][[t]]$mean = mean
				grouped.values[[g]][[t]]$upper = mean + se
				grouped.values[[g]][[t]]$col = factor.colours[g]
			}
		}
	}
	
	if(type == "l" | type == "b" | type == "L" | type == "B"){
		for(g in gi){
			if(dist == "q"){
				all.x = sapply(grouped.values[[g]], "[[", "x")
				all.y = sapply(grouped.values[[g]], "[[", "median")
			}else if(dist == "s" | dist == "sem"){
				all.x = sapply(grouped.values[[g]], "[[", "x")
				all.y = sapply(grouped.values[[g]], "[[", "mean")
			}
			# Split line plot at boundaries.
			splits = cbind(c(1, boundary.indices), c(boundary.indices, length(all.x) + 1))
			for(i in seq_len(nrow(splits))){
				block = seq(splits[i, 1], splits[i, 2] - 1)
				x = all.x[block]
				y = all.y[block]
				n = length(x) - 1
				graphics::segments( utils::head(x, n), utils::head(y, n), utils::tail(x, n), utils::tail(y, n), col = grouped.values[[g]][[1]]$col, lwd = lwd) # Because 'lines' is giving strange errors.
				graphics::segments(
					sapply(grouped.values[[g]], "[[", "x")[block], 
					sapply(grouped.values[[g]], "[[", "lower")[block], 
					sapply(grouped.values[[g]], "[[", "x")[block], 
					sapply(grouped.values[[g]], "[[", "upper")[block], 
					col = grouped.values[[g]][[1]]$col
				)
			}
			if(type == "L" | type == "B"){
				graphics::points(all.x, all.y, col = grouped.values[[g]][[1]]$col, pch = pch)
			}
		}
	}

	# Plotting.data is ordered, so put day marker at first trial of the day (might not be trial 1 if there were experimental dropouts)
	if(tolower(x.axis) == "day"){
		day = match(unique(plotting.data$`_Day`), unique(plotting.data[, c("_Trial", "_Day")])$`_Day`)
		graphics::axis(1, at = ti + ((ti - 1) * pad) - 0.5, labels = NA)
		graphics::axis(1, at = day + ((day - 1) * pad) - 0.5, labels = seq_along(day), tick = FALSE)
	}else if(tolower(x.axis) == "trial"){
		graphics::axis(1, at = ti + ((ti - 1) * pad) - 0.5, labels = seq_along(unique(plotting.data$trial.id)))
	}
	
	if(axis.titles){
		if(tolower(x.axis) == "day"){
			title(xlab = "Day", ylab = gsub("\\.", " ", paste0(toupper(substring(variable, 1, 1)), tolower(substring(variable, 2)))) )
		}else if(tolower(x.axis) == "trial"){
			title(xlab = "Trial", ylab = gsub("\\.", " ", paste0(toupper(substring(variable, 1, 1)), tolower(substring(variable, 2)))) )
		}else{
			title(xlab = "", ylab = gsub("\\.", " ", paste0(toupper(substring(variable, 1, 1)), tolower(substring(variable, 2)))) )
		}
	}

	if(legend) graphics::legend(graphics::par("usr")[2], graphics::par("usr")[4], legend = names(factor.colours), fill = factor.colours, cex = .7, bty = "n", border = NA)

	invisible(list(factor.colours = factor.colours, values = grouped.values))
}
