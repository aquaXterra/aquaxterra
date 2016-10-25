# Make tables and plots of freshwater LDG studies for review
# Author: QDR
# Project: Aquaxterra
# Created: 20 Oct 2016
# Last modified: 25 Oct 2016

# Modified 25 Oct: added classification by biome and scale of study, as well as some visualizations

library(XLConnect)
ldg <- readWorksheetFromFile('C:/Users/Q/Google Drive/aquaxterra_watercube_all/literature/general_biodiversity/Latitudinal_diversity_freshwater/freshwater_ldg_data.xlsx', sheet = 'Primary research')

ldg <- subset(ldg, Gradient_type == 'latitude') # Ignore elevation studies for now

with(ldg, table(LDG))
with(ldg, table(BroadTaxon, LDG))
with(ldg, table(BroadWaterbody, LDG))
with(ldg, table(c(Continent,Continent2)))
with(ldg, table(Scale, LDG))
with(ldg, table(Biome, LDG))

#library(kirkegaard)  
library(ggplot2)
library(dplyr)
library(extrafont)

myggtable <- function (data, var1, var2, margin = NULL) 
{
  library(ggplot2)
  library(magrittr)
  data = data[c(var1, var2)]
  data[] = lapply(data, as.factor)
  t_table = table(data[[var1]], data[[var2]]) %>% prop.table(margin = margin)
  rawtable = table(data[[var1]], data[[var2]]) %>% as.data.frame()
  d_table = t_table %>% as.data.frame()
  ggplot(d_table, aes(Var2, Var1)) + geom_tile(aes(fill = Freq), color = 'black') + 
    geom_text(aes(label = round(Freq, 2)), data = rawtable) + scale_fill_continuous(name = "Proportion") + 
    ylab(substitute(var1)) + xlab(substitute(var2))
}

ldg <- ldg %>% 
  filter(BroadTaxon %in% c('vertebrate','invertebrate','plant','microbe')) %>% 
  mutate(LDG = factor(LDG, levels = c('standard','opposite','unimodal','trough','none')),
         BroadTaxon = factor(BroadTaxon, levels = c('microbe','invertebrate','vertebrate','plant')),
         Scale = factor(Scale, levels = c('regional', 'continental', 'global')),
         Biome = factor(Biome, levels = c('temperate','tropical','boreal','many')))

theme_table <- theme_bw() + theme(panel.grid = element_blank(),
                                  text = element_text(family='Helvetica'),
                                  axis.ticks = element_blank(), 
                                  legend.position = 'none',
                                  axis.text.y = element_text(angle = 90, hjust=0.5),
                                  axis.title = element_text(size = 16))

oscale <- scale_fill_distiller(palette = 'Oranges', name = 'Proportion', direction = 1)

taxtable <- myggtable(ldg, 'BroadTaxon', 'LDG', margin = 1) + 
  oscale + 
  theme_table +
  labs(x = 'Latitudinal Diversity Gradient', y = 'Taxon')
bodytable <- myggtable(ldg, 'BroadWaterbody', 'LDG', margin = 1) + 
  oscale +
  theme_table +
  labs(x = 'Latitudinal Diversity Gradient', y = 'Waterbody Class')
scaletable <- myggtable(ldg, 'Scale', 'LDG', margin = 1) + 
  oscale +
  theme_table +
  labs(x = 'Latitudinal Diversity Gradient', y = 'Spatial Extent')
biometable <- myggtable(ldg, 'Biome', 'LDG', margin = 1) + 
  oscale +
  theme_table +
  labs(x = 'Latitudinal Diversity Gradient', y = 'Biome') 


ggsave('figs/ldg_table_by_taxon.png', taxtable, height=5, width=6, dpi=400)
ggsave('figs/ldg_table_by_waterbody.png', bodytable, height=5, width=6, dpi=400)
ggsave('figs/ldg_table_by_spatialextent.png', scaletable, height=5, width=6, dpi=400)
ggsave('figs/ldg_table_by_biome.png', biometable, height=5, width=6, dpi=400)

# Curve plots to visualize the number of studies with each type of trend, instead of the contingency tables.

scalecounts <- ldg %>% group_by(Scale) %>% do(as.data.frame(table(.$LDG)))

emptydata <- data.frame(Scale = factor(rep(c('regional','continental','global'),each=2), levels=c('regional','continental','global')),
                        x = c(0, 1, 0, 1, 0, 1),
                        y = c(0, 1, 0, 1, 0, 1))

ggplot(emptydata) + 
  facet_wrap(~ Scale) 



curveplot <- function(title, counts, scalefactor=0.75, lt1=1, lt2=3) {
  cs <- RColorBrewer::brewer.pal(n=5, name='Set1')
  
  theme_curve <- theme_bw() + theme(panel.grid = element_blank(),
                                    text = element_text(family='Helvetica'),
                                    axis.ticks = element_blank(),
                                    axis.text = element_blank(),
                                    axis.title = element_blank(),
                                    legend.position = 'none')
  
  th <- counts * scalefactor
  lt <- rep(1,length(counts))
  lt[th==0] <- 3
  
  ggplot(data.frame(x=c(0,1)), aes(x)) + 
    theme_curve +
    ggtitle(title) +
    stat_function(geom='line', color=cs[1], size=th[1], linetype=lt[1], fun = function(x) -x+1) +
    stat_function(geom='line', color=cs[2], size=th[2], linetype=lt[2], fun = function(x) x) +
    stat_function(geom='line', color=cs[3], size=th[3], linetype=lt[3], fun = function(x) 1 - (5*(x-0.5)^2)) +
    stat_function(geom='line', color=cs[4], size=th[4], linetype=lt[4], fun = function(x) 5*(x-0.5)^2 + 0.1) +
    stat_function(geom='line', color=cs[5], size=th[5], linetype=lt[5], fun = function(x) 0.5)
  
}

pgl <- curveplot(title = 'Global-extent studies', counts = subset(scalecounts,Scale=='global')$Freq)
pco <- curveplot(title = 'Continental-extent studies', counts = subset(scalecounts,Scale=='continental')$Freq)
pre <- curveplot(title = 'Regional-extent studies', counts = subset(scalecounts,Scale=='regional')$Freq)

library(gridExtra)
pallscale <- arrangeGrob(pre, pco, pgl, nrow=1, ncol=3)

ggsave('figs/curveplotsbyscale.png', pallscale, height=4, width=9, dpi=400)
