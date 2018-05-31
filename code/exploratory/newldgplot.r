# Additional ldg plot that is not too blocky.

theme_table <- theme_bw() + theme(panel.grid = element_blank(),
                                  text = element_text(family='Helvetica'),
                                  axis.ticks = element_blank(), 
                                  legend.position = 'none',
                                  axis.text.y = element_text(angle = 90, hjust=0.5),
                                  axis.title = element_text(size = 16))

oscale <- scale_fill_distiller(palette = 'Oranges', name = 'Proportion', direction = 1)


ggplot(ldg, aes(x = LDG, y = Scale)) + 
  stat_summary(geom = 'text', fun.data = function(x) data.frame(y = x, label = length(x))) +
  oscale +
  theme_table +
  labs(x = 'Latitudinal Diversity Gradient', y = 'Spatial Extent') +
  theme(plot.background = element_rect(fill = 'transparent', color = 'blue', size = 2))


scalecounts <- ldg %>% group_by(Scale) %>% do(as.data.frame(table(.$LDG)))

indivcurveplots <- list()
funlist <- list(standard = function(x) -x+1, 
                opposite = function(x) x,
                unimodal = function(x) 1 - (5*(x-0.5)^2),
                trough = function(x) 5*(x-0.5)^2 + 0.1,
                none = function(x) 0.5)

for (i in 1:nrow(scalecounts)) {
  ctype <- scalecounts$Var1[i]
  stype <- scalecounts$Scale[i]
  nstudies <- scalecounts$Freq[i]
  
  fx <- funlist[ctype][[1]]
  theme_i <- theme_void() + theme(text = element_text(family='Helvetica'))
  
  lt <- 1
  th <- nstudies
  if (ctype == 'standard') theme_i <- theme_i + theme(axis.title.y = element_text(size = 12, angle = 90))
  if (nstudies == 0) {th <- 1; lt <- 3}
  
  indivcurveplots[[i]] <- ggplot(data.frame(x = c(0,1)), aes(x)) + theme_i + ylab(stype) + ylim(-0.1,1) +
    stat_function(geom='line', color='black', size=th * 0.75, linetype=lt, fun = fx) +
    geom_text(data = data.frame(x = 0.5, y = 0, lab = paste('n =', nstudies)), aes(label=lab, y=y)) +
    theme(plot.background = element_rect(fill = 'transparent', color = 'black'))
  
}

library(gridExtra)
grid.arrange(grobs = indivcurveplots, nrow=3)


# Bar plot

pb1 <- ggplot(ldg, aes(x = LDG, fill = LDG)) + 
  facet_wrap(~ Scale) + 
  geom_bar(position='identity') +
  theme_table + theme(strip.background = element_blank()) +
  scale_y_continuous(name = 'number of studies', expand = c(0,0), limits = c(0,17)) +
  scale_fill_brewer(type = 'qual', palette = 'Set1') +
  xlab('latitudinal diversity gradient')

# Hillebrand bar plot

hilldat <- hill_a %>% filter(Realm != 'Aquatic') %>% mutate(LDG = factor(LDG, levels = c('standard','opposite','none')))

pb2 <- ggplot(hilldat, aes(x = LDG, fill = LDG)) + 
  facet_wrap(~ Realm) + 
  geom_bar(position='identity') +
  theme_table + theme(strip.background = element_blank()) +
  scale_y_continuous(name = 'number of studies', expand = c(0,0), limits = c(0,225)) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(5,'Set1')[c(1,2,5)]) +
  xlab('latitudinal diversity gradient')

# Schematic function plots. Can be added as annotation to the main plot.
curveschematics <- list()
for (i in 1:5) {
  
  fx <- funlist[i][[1]]
  theme_i <- theme_classic() + theme(axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank())
  
  curveschematics[[i]] <- ggplot(data.frame(x = c(0,1)), aes(x)) + theme_i + ylim(0,1) +
    stat_function(geom='line', color='black', fun = fx, size = 1.5)
}

grid.arrange(grobs = curveschematics, nrow = 1)
schems <- arrangeGrob(grobs = curveschematics, nrow = 1)
schems2 <- arrangeGrob(grobs = curveschematics[c(1,2,5)], nrow = 1)

ggsave('figs/ldg_barplot.png', pb1 + annotation_custom(grob = schems, xmin = 0.5, xmax = 5.5, ymin = 14, ymax = 16.5), height = 4, width = 9, dpi = 400)
ggsave('figs/ldg_barplot_hillebrand.png', pb2 + annotation_custom(grob = schems2, xmin = 0.5, xmax = 3.5, ymin = 180, ymax = 220), height = 4, width = 9, dpi = 400)
