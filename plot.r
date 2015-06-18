
# Tools to simplify plotting tasks

# LAYOUT
# A simple function for creating prettier axes.
nice.axis <- function(side, values, ...) {
	rng <- pretty(range(values))
	axis(
		side = side,
		at = rng,
		labels = format(rng, format="d", big.mark=',', scientific = F, trim = T),
		...
	)
}

# COLORS
# Get a decent palette using RColorBrewer
getColors <- function(x, pal.name, n = 5, int.style = "quantile") {
	library(RColorBrewer)
	library(classInt)
	pal <- brewer.pal(n, pal.name) # For a list: display.brewer.all(n=10, exact.n=FALSE)
	findColours(classIntervals(x, style = int.style), pal)
}

# Get a palette based on a range of colors
getColorRamp <- function(x, n, colors = c("blue", "red"), int.style = "quantile") {
	library(classInt)
	pal <- colorRampPalette(colors)(n)
	findColours(classIntervals(x, style = int.style), pal)	
}