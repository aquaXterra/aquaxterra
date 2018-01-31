# Load all csvs from huc and debug.

fp <- '/mnt/research/aquaxterra/CODE/R/nhd/csvs'

d <- dir(fp)
isbad <- rep(FALSE, length(d))

for (i in 1:length(d)) {
	x <- try(read.csv(file.path(fp, d[i]), stringsAsFactors = FALSE), TRUE)
	if (inherits(x, 'try-error') || !all(is.numeric(x$size))) isbad[i] <- TRUE
}

for (fn in d[isbad]) system2('rm', args = file.path(fp, fn))