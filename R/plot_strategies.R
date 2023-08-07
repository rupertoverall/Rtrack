#' Plot water maze strategies.
#'
#' Plots the strategy usage for all groups.
#'
#' The strategies returned by \code{\link{read_experiment}} can be shown in a
#' summary plot. In these plots, the fraction of subjects utilising a particular
#' strategy is shown for each day/trial. If a factor is provided, then one plot
#' will be made for each level of the factor. To view data for mutliple factors,
#' they will need to be collapsed into one composite factor for plotting using
#' this function. If probe trials were used, these can be ignored (not plotted)
#' as the strategy use in the absence of the goal will be somewhat different.
#' For this to work, a column named "Probe" must be present in the experiment
#' description spreadsheet and must contain the value "TRUE" for each probe
#' trial.
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
#' @param strategies The strategy calls as returned from
#'   \code{\link{call_strategy}}.
#' @param experiment The experiment object as returned from
#'   \code{\link{read_experiment}}.
#' @param factor The factor by which the data should be grouped. The default,
#'   \code{NA}, means that all data will be shown in one plot. Specifying a
#'   grouping factor will cause a separate plot to be generated for each group.
#' @param exclude.probe Should data from probe trials be excluded (see Details).
#' @param boundaries Where should the boundaries between arena types be drawn
#'   (see Details).
#' @param legend Should a legend be drawn. Default is to add a legend to the
#'   plot.
#' @param screen Should multiple plots be drawn to one page. Default is
#'   \code{FALSE}. This can be useful for advanced layout using
#'   \code{\link[graphics]{split.screen}}.
#' @param margins The margins of the plot (see the option \code{mar} in
#'   \code{\link[graphics]{par}}). The defaults should usually be fine, but they
#'   can be overridden if, for example, factor names are very long.
#' @param lwd The thickness of the plotted lines. Default is 2.
#'
#' @return A \code{\link[base]{list}} of strategy call information.
#'
#' @examples
#' # This function relies on data too large to include in the package.
#' # For a worked example, please see the vignette "Rtrack MWM analysis"
#' # in the online package documentation at
#' # https://rupertoverall.net/Rtrack/.
#'
#' @importFrom methods is
#' @importFrom graphics par plot lines segments axis box polygon rect title
#'   split.screen
#' @importFrom stats aggregate
#'
#' @export
plot_strategies = function(strategies, experiment, factor = NA, exclude.probe = FALSE, boundaries = NA, legend = TRUE, screen = FALSE, margins = c(5, 4, 4, 8), lwd = 2){
	if(methods::is(strategies, "rtrack_strategies") & methods::is(experiment, "rtrack_experiment")){
		boundary.lwd = lwd

		plotting.data = experiment$factors
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

		arena = as.character(aggregate(plotting.data$`_Arena`, list(plotting.data$`_Trial`, plotting.data$`_Day`), `[`, 1)$x)
		# If boundaries not user-set, then place at the interface of different arenas
		if(is.null(boundaries)){
			boundaries = NULL
		}else if(any(is.na(boundaries))){
			boundaries = which(arena != c(arena[1], arena[-length(arena)]))
		}else{
			all.trials = unique(plotting.data[, c("_Day", "_Trial")])
			boundaries = apply(boundaries, 1, function(row) which(all.trials$`_Day` == as.numeric(row[1]) & all.trials$`_Trial` == as.numeric(row[2])) )
		}
		# plotting.data is ordered, so put day marker at first trial of the day (might not be trial 1 if there were experimental dropouts)
		day = match(unique(plotting.data$`_Day`), unique(plotting.data[, c("_Trial", "_Day")])$`_Day`)

		if(screen){
			n = length(plot.levels)
			screen.layout = t(sapply(1:n, function(m){ c(0, 1, m / n - 1 / n, m / n) }))
			screens = graphics::split.screen(screen.layout)
		}
		plot.values = sapply(seq_along(names(plot.series)), function(i){
			level = names(plot.series)[i]
			group = plotting.data[plot.series[[level]], ]
			strategy.ids = as.numeric(strategies$calls[rownames(group), ]$strategy)
			x = unique(group$x)
	
			# Reversed, ordered, cumulative counts - ready for plotting
			y.cum = do.call("rbind", by(strategy.ids, list(group$`_Trial`, group$`_Day`), function(y){
				rev(cumsum(sapply(rev(strategies$plot.order), function(s) length(which(y == s)) )))
			}))
			plot.colours = strategies$strategy.colours[strategies$plot.order]
			
			if(screen) graphics::screen(screens[i])
			.parprevious = graphics::par(mar = margins, xpd = TRUE)
			on.exit(par(.parprevious))

			plot(x, seq(0, max(y.cum), length.out = length(x)), type = "n", las = 1, xaxt = "n", xaxs="i", yaxs="i", ylab = "Strategy usage", xlab = "Day")
			for(n in 1:ncol(y.cum)){
				polygon(c(x[1], x, x[length(x)]), c(0, y.cum[, n], 0), col = plot.colours[n], border = NA)
			}
			if(exclude.probe & !is.null(plotting.data$Probe)){
				probe = which(aggregate(plotting.data$Probe, list(plotting.data$`_Trial`, plotting.data$`_Day`), `[`, 1)$x == "TRUE")
				rect(probe, 0, probe - 1, max(y.cum), border = NA, col = "#FFFFFFFF")
			}
			if(!is.null(boundaries) & length(boundaries) > 0){
				rect(boundaries, 0, boundaries - 1, max(y.cum), border = NA, col = "#FFFFFFFF")
				segments(boundaries - 0.5, 0, boundaries - 0.5, max(y.cum), lty = 3, lwd = boundary.lwd, col = "#000000FF")
			}
			box(lwd = boundary.lwd)
			axis(1, at = day, labels = 1:length(day))
			if(length(plot.series) > 1){
				title(main = paste0("Strategy usage for ", tolower(factor), " '", level, "'"))
			}else{
				title(main = paste0("Strategy usage for all data"))
			}
			if(legend) graphics::legend(x = nrow(y.cum), y = max(y.cum), legend = rev(strategies$strategy.names), fill = rev(strategies$strategy.colours), cex = .7, bty = "n", border = NA)
		return(NULL)
		})
		graphics::close.screen(screens)
	}
	invisible()
}
