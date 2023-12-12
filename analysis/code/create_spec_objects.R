# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: alarm_dialect_paper  
# Author: Simeon Q. Smeele
# Description: Creates spec_objects from the waves. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('tuneR', 'callsync', 'stringr', 'dplyr', 'seewave')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths 
path_functions = 'analysis/code/functions'
path_waves = 'analysis/results/waves.RData'
path_spec_objects = 'analysis/results/spec_objects.RData'

# Load functions
.functions = sapply(list.files(path_functions, 
                               pattern = '*R', full.names = TRUE), source)

# Load data
load(path_waves)

# Report
message('Starting... ')

# Generate spec_ojects
spec_objects = lapply(waves, function(wave){
  wave = ffilter(wave, from = 500, output = 'Wave')
  spec_object = create.spec.object(wave, freq_range = c(1000, 6000), 
                                   plot_it = F, 
                                   thr_low = 1.3, thr_high = 1.8,
                                   wl = 512, ovl = 450, 
                                   method = 'sd',
                                   sum_one = T)
  return(spec_object)
})
names(spec_objects) = names(waves)

# Test example
image(t(spec_objects[[4]]), col = hcl.colors(12, 'Blue-Yellow', rev = T)) 

# Save spec_objects and message
save(spec_objects, file = path_spec_objects)
message(sprintf('Created %s spec_objects.', length(spec_objects)))