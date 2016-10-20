# Make tables and plots of freshwater LDG studies for review
# Author: QDR
# Project: Aquaxterra
# Created: 20 Oct 2016

library(XLConnect)
ldg <- readWorksheetFromFile('C:/Users/Q/Google Drive/aquaxterra_watercube_all/literature/general_biodiversity/Latitudinal_diversity_freshwater/freshwater_ldg_data.xlsx', sheet = 'Primary research')

ldg <- subset(ldg, Gradient_type == 'latitude') # Ignore elevation studies for now

with(ldg, table(LDG))
with(ldg, table(BroadTaxon, LDG))
with(ldg, table(BroadWaterbody, LDG))
with(ldg, table(c(Continent,Continent2)))

library(kirkegaard)     
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
         BroadTaxon = factor(BroadTaxon, levels = c('microbe','invertebrate','vertebrate','plant')))

theme_table <- theme_bw() + theme(panel.grid = element_blank(),
                                  text = element_text(family='Helvetica'),
                                  axis.ticks = element_blank(), 
                                  legend.position = 'none',
                                  axis.text.y = element_text(angle = 90, hjust=0.5),
                                  axis.title = element_text(size = 16))

taxtable <- myggtable(ldg, 'BroadTaxon', 'LDG', margin = 1) + 
  scale_fill_distiller(palette = 'RdBu', name = 'Proportion') + 
  theme_table +
  labs(x = 'Latitudinal Diversity Gradient', y = 'Taxon')
bodytable <- myggtable(ldg, 'BroadWaterbody', 'LDG', margin = 1) + 
  scale_fill_distiller(palette = 'RdBu', name = 'Proportion') +
  theme_table +
  labs(x = 'Latitudinal Diversity Gradient', y = 'Waterbody Class')

ggsave('figs/ldg_table_by_taxon.png', taxtable, height=5, width=6, dpi=400)
ggsave('figs/ldg_table_by_waterbody.png', bodytable, height=5, width=6, dpi=400)
