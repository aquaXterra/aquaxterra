# Map to visualize the holes in bird trait data.

btm <- read.csv('C:/Users/Q/Dropbox/projects/aquaxterra/birdtraitmerged.csv',stringsAsFactors = FALSE)
btm[btm == -999] <- NA

# show holes in life history traits since no interpolation was used on them

lcols <- 46:74

visdat <- btm[,c(5,lcols)]
novals <- apply(visdat, 2, function(x) sum(!is.na(x)))
visdat <- visdat[, novals > 0]

# standardize and transform with reshape for ggplot
library(cowplot)
library(reshape2)
library(dplyr)

vismelt <- melt(visdat) %>% 
  group_by(variable) %>%
  mutate(rescale = scale(value))

# cut down to manageable thing.
somenames <- sample(visdat$Latin_Name, size = 25)
somevars <- sample(names(visdat)[-1], size = 10)
smallvisdat <- vismelt %>% filter(Latin_Name %in% somenames, variable %in% somevars)

# visualize
p <- ggplot(smallvisdat, aes(variable, Latin_Name)) + 
  geom_tile(aes(fill = rescale), colour = "white") + 
  scale_fill_gradientn(colours=RColorBrewer::brewer.pal(9, 'OrRd')[-1]) + 
  panel_border(colour='black') +
  theme(legend.position = 'none',
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8))

ggsave('C:/Users/Q/Dropbox/presentations/imgs for presentations/missingdata.png', p, height = 7, width = 6, dpi = 400)
