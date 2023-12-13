# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: alarm_dialect_paper  
# Author: Simeon Q. Smeele
# Description: Plots the figures for the main text.
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('callsync', 'scales')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths 
path_functions = 'analysis/code/functions'
path_pco = 'analysis/results/pco_out.RData'
path_pco_1 = 'analysis/results/model_output/pco1_results.RData'
path_pco_2 = 'analysis/results/model_output/pco2_results.RData'
path_composite_figure = 'analysis/results/figures/composite_figure.pdf'
path_combined_selection_tables = 'analysis/results/selection_tables.RData'

# Load functions
.functions = sapply(list.files(path_functions, 
                               pattern = '*R', full.names = TRUE), source)

# Colours
colours = c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", 
            "#44AA99", "#882255", "#661100", "#888888")
colours = palette.colors(n = 9, palette = "Classic Tableau")

# Plot composite figure
load(path_pco)
load(path_combined_selection_tables)
pdf(path_composite_figure, 10, 10)
par(cex.axis = 1.25, cex.lab = 1.25)
par(mfrow = c(2, 2), mar = c(5.1, 4.1, 2.1, 2.1))
plot.park.and.city.means(path_pco_2, flip = T, lab = 'PC 2')
# text(35, 1.4, 'a)', font = 2, cex = 1.5)
load(path_pco_1)
s = sample(1:length(selection_tables$city))
plot(apply(pco_out$vectors[s,1:2], 2, scale), 
     col = alpha(colours[trans_cities[selection_tables$city][s]], 0.8), 
     pch = 16, xlab = 'PC 1', ylab = 'PC 2')
# text(-2.35, 2.33, 'b)', font = 2, cex = 1.5)
par(mar = c(0, 0, 0, 0))
plot(NULL, xlim = c(-0.1, 1.1), ylim = c(0, 20), xaxt = 'n', yaxt = 'n',
     xlab = '', ylab = '', bty ='n')
labels = c()
cc = c()
city_per_park = sapply(1:max(dat$park), function(x) 
  unique(dat$city[dat$park == x]))
for(i in trans_cities){
  cc = c(cc, i)
  labels = c(labels, sprintf('%s. %s', i, names(trans_cities)[i]))
  for(j in which(city_per_park == i)){
    cc = c(cc, i)
    labels = c(labels, sprintf('  %s', names(trans_parks)[j]))
  }
}
labels = c(labels, rep('', 3))
text(rep(c(0, 0.6), each = 19), rep(seq(19, 1, -1), 2), 
     labels, 
     col = alpha(colours[trans_cities[cc]], 0.9),
     adj = 0, cex = 0.9)
par(mar = c(5.1, 4.1, 2.1, 2.1))
plot.park.and.city.means(path_pco_1, flip = F, lab = 'PC 1')
text(-1.4, 2.5, 'c)', font = 2, cex = 1.5)
dev.off()
