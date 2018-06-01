# Rename HUC CSVs.

setwd('/mnt/research/aquaxterra/DATA/Processing/final')

hucs <- c('HUC4','HUC8','HUC12')
stats <- c('mean','median')
n <- expand.grid(hucs=hucs,stats=stats)

for (i in 1:nrow(n)) {
	file_name <- paste0('bbs_div_', tolower(n$hucs[i]), '_', n$stats[i], '.csv')
	x <- read.csv(file_name, stringsAsFactors = FALSE)
	names(x)[-1] <- paste(n$hucs[i], n$stats[i], names(x)[-1], sep = '_')
	write.csv(x, file_name, row.names = FALSE)
}

# Combine the mean and median ones into a single data frame.
hucs <- c('HUC4','HUC8','HUC12')

for (i in hucs) {
	file_name <- paste0('bbs_div_', tolower(i), '_', c('mean','median'), '.csv')
	x1 <- read.csv(file_name[1], stringsAsFactors = FALSE)
	x2 <- read.csv(file_name[2], stringsAsFactors = FALSE)
	x <- cbind(x1, x2[,-1])
	write.csv(x, paste0('bbs_div_', tolower(i), '.csv'))
	system2('rm', args = file_name)
}