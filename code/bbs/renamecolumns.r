# Rename HUC CSVs.

hucs <- c('HUC4','HUC8','HUC12')
stats <- c('mean','median')
n <- expand.grid(hucs=hucs,stats=stats)

for (i in 1:nrow(n)) {
	file_name <- paste0('bbs_div_', tolower(n$hucs[i]), '_', n$stats[i], '.csv')
	x <- read.csv(file_name, stringsAsFactors = FALSE)
	names(x)[-1] <- paste(n$hucs[i], n$stats[i], names(x)[-1], sep = '_')
	write.csv(x, file_name, row.names = FALSE)
}
