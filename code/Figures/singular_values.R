
# library(ggplot2)
library(RColorBrewer)
# library(plot.matrix)

# read in GOF by dimensionality measures
godoyC <- "../../results/Spain/spain.Control.pseudo-rsquared.csv"
godoyT <- "../../results/Spain/spain.Treatment.pseudo-rsquared.csv"

wainwrightOpen <- "../../results/Australia/australia.Open.pseudo-rsquared.csv"
wainwrightShade <- "../../results/Australia/australia.Shade.pseudo-rsquared.csv"

# # these datasets were fit from Levine matrix only
# levine.dir <- "../../results/Levine/"
# levine.files <- list.files(levine.dir, "rsquared")
# levine.files <- paste0(levine.dir, levine.files)

# these datasets were fit from Kinlock matrix only
kinlock.dir <- "../../results/Kinlock/"
kinlock.files <- list.files(kinlock.dir, "rsquared")
kinlock.files <- paste0(kinlock.dir, kinlock.files)

# remove Engel due to errors in the data and we have it from Levine
# remove Goldberg & Landa because we have it from Levine
# kinlock.files <- grep("Engel|Landa",kinlock.files,invert=TRUE,value=TRUE)

# put all files together
all.files <- c(
	godoyC,
	godoyT,
	wainwrightOpen,
	wainwrightShade,
	kinlock.files
)

# read in the data sets
rsquareds <- sapply(all.files, function(x) read.table(x), simplify=FALSE)

rsquareds <- rsquareds[order(sapply(rsquareds, nrow), decreasing=TRUE)]

# the first column of each output corresponds to the singular values
sing_vals <- lapply(
	rsquareds,
	function(x){
		x[,1]
	}
)
sing_vals <- as.data.frame(do.call(rbind,sapply(
	1:length(sing_vals),
	function(i,sing_vals){
		return(cbind(didx=i,d=1:length(sing_vals[[i]]),sval=sing_vals[[i]]))
	},
	sing_vals = sing_vals
)))

sing_vals$dataset <- NA
for(i in 1:nrow(sing_vals)){
	sing_vals$dataset[i] <- switch(
		as.character(sing_vals$didx[i]),
		"1" = "1-Wet",
		"2" = "1-Dry",
		"3" = "2-Sun",
		"4" = "2-Shade",
		sing_vals$didx[i] - 2
	)
}

library(tidyverse)
kk <-
	sing_vals %>%
	mutate(sval = signif(sval, 4)) %>%
	pivot_wider(
		id_cols = dataset,
		names_from = d,
		values_from = sval
		# values_fill = '-'
	)
write.table(
	kk,
	'../../manuscript/Supplementary/Tables/singular_values.csv',
	quote=FALSE,
	sep = ' & ',
	row.names=FALSE,
	col.names=FALSE
)
