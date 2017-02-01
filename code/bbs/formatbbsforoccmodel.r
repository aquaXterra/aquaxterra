# Put BBS into format needed for detectability estimation.


fp <- '/mnt/research/aquaxterra/DATA/raw_data'
load(file.path(fp, 'BBS/bbsmatconsolidated2015.r'))

# We now have a stop by species matrix. We want a segment by species matrix. This can be split by route to make a route x segment x species array.

# Clean it by removing zero species and removing nocturnal species.
birdtrait <- read.csv(file.path(fp, 'bird_traits/birdtraitmerged.csv'), stringsAsFactors = FALSE)

birdtrait[birdtrait == -999] <- NA
nocturnalbirdAOUs <- birdtrait$AOU[birdtrait$Nocturnal == 1]
birdtrait_diurnal <- subset(birdtrait, Nocturnal != 1 | is.na(Nocturnal))

# Select traits to use
# Ones that seem important and/or have a lot of records.
# Includes diet, foraging strategy, body size, and life history.
traitnames <- names(birdtrait)[c(15:24, 29:36, 46:50, 53, 55:59)]

ns <- colSums(fixedbbsmat)
fixedbbsmat_nonzero <- fixedbbsmat[, ns > 0 & !(sppids %in% nocturnalbirdAOUs)]
sppids_nonzero <- sppids[ns > 0 & !(sppids %in% nocturnalbirdAOUs)]

# Clean site x species matrix.
dimnames(fixedbbsmat_nonzero)[[2]] <- sppids_nonzero # Already sorted by AOU

# Add segment ID. 10 stops per segment.
stopno <- as.numeric(substr(as.character(bbsgrps$Stop), 5, nchar(as.character(bbsgrps$Stop))))
bbsgrps$segment <- ceiling(stopno/10)

# Reshape the matrix into an array: Route by species by segment.
# Use 2001 as an example.
bbs2001 <- fixedbbsmat_nonzero[bbsgrps$year == 2001,]
bbsgrps2001 <- bbsgrps[bbsgrps$year == 2001,]
# There are 3008 routes.

# Sum up by segment.
bbsseg2001 <- cbind(bbsgrps2001[,c('rteNo','segment')], bbs2001) %>% group_by(rteNo, segment) %>% summarize_all(.funs = 'sum')

# Reshape into an array.
library(reshape2)
bbsmelt2001 <- melt(bbsseg2001, id.vars = c('rteNo','segment'))
bbsarray2001 <- acast(bbsmelt2001, rteNo ~ segment ~ variable)

# Instead of using segments, use each stop as a replicate.
bbsstop2001 <- cbind(bbsgrps2001[,c('rteNo','Stop')], bbs2001)
bbsstopmelt2001 <- melt(bbsstop2001, id.vars = c('rteNo','Stop'))
bbsstoparray2001 <- acast(bbsstopmelt2001, rteNo ~ Stop ~ variable)

# Save these arrays for future use in the model.
save(bbsstoparray2001, bbsarray2001, file = file.path(fp, 'BBS/bbs2001arrays.r'))