# vi: se ft=r

# Interactive session
if(interactive()) {

	# Set default mirror
	options(repos=structure(c(CRAN="https://cloud.r-project.org/")))

	# Try to load package colorout for colorizing R output
	if ( ! require(colorout, quietly=TRUE))
		message("Install package colorout with `devtools::install_github('jalvesaq/colorout')`.")

	# Load history
	if (file.exists('~/.Rhistory'))
		utils::loadhistory('~/.Rhistory')

	# Load data
	if (file.exists('/.RData'))
		load('/.RData')

#	# Define termination function
#	.Last <- function() {
#		utils::savehistory('~/.Rhistory')
#		save('~/.RData')
#	}
}
