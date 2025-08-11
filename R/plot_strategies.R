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
#' @param x.axis The scale of the x axis. The default, "Day", will add a
#'   labelled axis with tick marks at each day. If this parameter is set to
#'   "Trial", then tick marks are added for each trial. If set to "none", then
#'   no x axis will be drawn.
#' @param exclude.probe Should data from probe trials be excluded (see Details).
#' @param boundaries Where should the boundaries between arena types be drawn
#'   (see Details).
#' @param boundary.lwd The thickness of the boundaries. Default is 1.
#' @param legend Should a legend be drawn. Default is to add a legend to the
#'   plot.
#' @param screen Should multiple plots be drawn to one page. Default is
#'   \code{FALSE}. This can be useful for advanced layout using
#'   \code{\link[graphics]{split.screen}}.
#' @param layout If \code{screen} is \code{TRUE}, this parameter can be given as
#'   a matrix of indices or level names. The multiple sub-plots will be drawn in
#'   a grid in the order and layout specified.
#' @param title Text for the plot title. The default includes the name of the
#'   factor and the level encoded as the wildcard "%%f". The remaining text can
#'   be replaced (for example for language localisation). Using the string "%%f"
#'   on its own will print the factor and level without additional text. The
#'   string "" will suppress the title entirely. Note that this overrides the
#'   usual plotting parameter 'main' (which would give the same title to all
#'   subplots).
#' @param axis.titles Should axis titles be drawn. Default is to add titles for
#'   the x and y axes. These can be suppressed and added afterwards (using
#'   \code{\link[graphics]{title}}). This might be helpful for localising to a
#'   different language for example.
#' @param margins The margins of the plot (see the option \code{mar} in
#'   \code{\link[graphics]{par}}). The defaults should usually be fine, but they
#'   can be overridden if, for example, factor names are very long.
#' @param ... Additional arguments to \code{plot}.
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
plot_strategies = function(strategies, experiment, factor = NA, x.axis = "day", exclude.probe = FALSE, boundaries = NA, boundary.lwd = 1, legend = TRUE, screen = FALSE, layout = NULL, title = "Strategy usage for %%f", axis.titles = TRUE, margins = c(5, 4, 4, 8), ...){
	if(methods::is(strategies, "rtrack_strategies") & methods::is(experiment, "rtrack_experiment")){
		plotting.data = experiment$factors
		if(exclude.probe){
			if(!is.null(plotting.data$Probe) & all(plotting.data$Probe %in% c("TRUE", "FALSE"))){
				plotting.data$y[plotting.data$Probe == "TRUE"] = NA
			}else{
				warning("There is no 'Probe' factor in the experiment (or it contains values other than TRUE/FALSE). All data will be plotted instead.")
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

		parameters = list(...)
		lwd = as.numeric(parameters[which(names(parameters) == "lwd")])
		if(length(lwd) == 0 ) lwd = 1
		bty = parameters[which(names(parameters) == "bty")]
		if(length(bty) == 0 ) bty = "o"
		las = as.numeric(parameters[which(names(parameters) == "las")])
		if(length(las) == 0 ) las = 2
		maintitle = as.numeric(parameters[which(names(parameters) == "main")])
		if(length(maintitle) > 0 ){
			message("Use 'title' instead of 'main' to allow titling of multiple plots.") 
			parameters$main = NULL
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
		# Plotting.data is ordered, so put day marker at first trial of the day (might not be trial 1 if there were experimental dropouts)
		day = match(unique(plotting.data$`_Day`), unique(plotting.data[, c("_Trial", "_Day")])$`_Day`)
		trial = seq_along(unique(plotting.data[, c("_Trial", "_Day")])$`_Trial`)
		
		indices = seq_along(plot.levels)
		screens = NULL
		if(screen){
			n = length(plot.levels)
			# Check if there is a user-defined layout - if not, use default.
			if(is.null(layout)){
				screens = graphics::split.screen(figs = c(length(plot.levels), 1))
			}else{
				if(length(layout) == 1) layout = as.matrix(layout)
				# Convert to byRow format.
				layout = matrix(as.vector(t(layout)), byrow = FALSE, ncol = ncol(layout))
				# Sanity check.
				indices = suppressWarnings(as.numeric(layout))
				if(length(na.omit(indices)) == 0){
					indices = match(as.character(layout), plot.levels) # Match to level names.
				}
				sane = all(is.matrix(layout), length(indices) > 0, all(na.omit(indices) %in% seq_len(n)))
				if(!sane) stop("The 'layout' parameter must be a matrix with indices/elements corresponding to the factor levels.")
				screens = graphics::split.screen(figs = c(nrow(layout), ncol(layout)))
			}
		}
		plot.values = sapply(seq_along(indices), function(i){
			if(screen) graphics::screen(screens[i])
			.parprevious = graphics::par(mar = margins, xpd = TRUE)
			on.exit(par(.parprevious))
			
			if(!is.na(indices[i])){
				level = names(plot.series)[indices[i]]
				group = plotting.data[plot.series[[level]], ]
				strategy.ids = as.numeric(strategies$calls[rownames(group), ]$strategy)
				x = unique(group$x)
		
				# Reversed, ordered, cumulative counts - ready for plotting
				y.cum = do.call("rbind", by(strategy.ids, list(group$`_Trial`, group$`_Day`), function(y){
					rev(cumsum(sapply(rev(strategies$plot.order), function(s) length(which(y == s)) )))
				}))
				plot.colours = strategies$strategy.colours[strategies$plot.order]
				
				do.call(plot, c(list(x, seq(0, max(y.cum), length.out = length(x)), type = "n", las = 1, xaxt = "n", xaxs = "i", yaxs = "i", ylab = "", xlab = "", main = ""), parameters))
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
				if(bty != "n") graphics::box(lwd = lwd)
				
				if(length(plot.series) > 1){
					factor.text = paste0(tolower(factor), " '", level, "'")
					title(main = sub("\\%\\%f", factor.text, title))
				}else{
					factor.text = "all data"
					title(main = sub("\\%\\%f", factor.text, title))
				}
				
				# Plotting.data is ordered, so put day marker at first trial of the day (might not be trial 1 if there were experimental dropouts)
				if(tolower(x.axis) == "day"){
					axis(1, at = day, labels = 1:length(day))
				}else if(tolower(x.axis) == "trial"){
					axis(1, at = trial, labels = 1:length(trial))
				}
				
				if(axis.titles){
					if(tolower(x.axis) == "day"){
						title(xlab = "Day", ylab = "Strategy usage")
					}else if(tolower(x.axis) == "trial"){
						title(xlab = "Trial", ylab = "Strategy usage")
					}
				}
			
				if(legend) graphics::legend(x = nrow(y.cum), y = max(y.cum), legend = rev(strategies$strategy.names), fill = rev(strategies$strategy.colours), cex = .7, bty = "n", border = NA)
			}
			return(NULL)
		})
		graphics::close.screen(screens)
	}
	invisible()
}
