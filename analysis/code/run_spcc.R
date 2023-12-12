# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: alarm_dialect_paper  
# Author: Simeon Q. Smeele
# Description: Runs spectrographic cross correlation on the spec_objects.  
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('tuneR', 'callsync', 'stringr', 'dplyr', 'seewave', 'parallel')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths 
path_functions = 'analysis/code/functions'
path_spec_objects = 'analysis/results/spec_objects.RData'
path_spcc_out = 'analysis/results/spcc_out.RData'

# Load functions
.functions = sapply(list.files(path_functions, 
                               pattern = '*R', full.names = TRUE), source)

# Load data
load(path_spec_objects)

# Settings
n_cores = 20

# Get combinations and run function
message(sprintf('Starting with %s spec_objects... ', length(spec_objects)))
c = combn(1:length(spec_objects), 2)
o = mclapply(1:ncol(c), function(i) 
  sliding.pixel.comparison(spec_objects[[c[1,i]]], spec_objects[[c[2,i]]]),
             mc.cores = n_cores) %>% unlist
o = o/max(o)
spcc_out = list(o = o, names = names(spec_objects))
save(spcc_out, file = path_spcc_out)
message('Done.')