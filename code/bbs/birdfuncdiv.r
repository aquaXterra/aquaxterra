# Bird functional diversity calculations
# QDR/Aquaxterra/created 17Nov2016/modified 17Nov2016


fp <- '/mnt/research/aquaxterra/DATA/raw_data/bird_traits'
birdtrait <- read.csv(file.path(fp, 'birdtraitmerged.csv'), stringsAsFactors = FALSE)

birdtrait[birdtrait == -999] <- NA

# Run principal components analysis on the bird traits.

birddietpca <- prcomp(~., data=birdtrait[,15:24], scale = TRUE, center = TRUE, na.action = na.omit)

# Select traits to use
# Ones that seem important and/or have a lot of records.
# Includes diet, foraging strategy, body size, and life history.
traitnames <- names(birdtrait)[c(15:24, 29:36, 46:50, 53, 55:59)]

# Match bird traits with the site by species matrix.
traitmatch <- sppids %in% birdtrait$AOU # 65 don't match :-(
