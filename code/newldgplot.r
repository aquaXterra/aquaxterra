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
