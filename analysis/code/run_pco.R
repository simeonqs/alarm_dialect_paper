# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: alarm_dialect_paper  
# Author: Simeon Q. Smeele
# Description: Loading SPCC results. Running pco and storing results.  
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('ape', 'stringr', 'callsync')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths 
path_functions = 'analysis/code/functions'
path_spcc_out = 'analysis/results/spcc_out.RData'
path_combined_selection_tables = 'analysis/results/selection_tables.RData'
path_pco = 'analysis/results/pco_out.RData'

# Load previous result
load(path_spcc_out)
load(path_combined_selection_tables)

# Report
message('Starting conversion to matrix... ')

# Make matrix
n = spcc_out$names
files = n |> str_split('-') |> sapply(`[`, 1)
m = o.to.m(spcc_out$o, n)

# Report
message('Done.\nStarting PCO... ')

# PCO
pco_out = pcoa(m)
save(pco_out, file = path_pco)

# Report
message('Done.')
message(sprintf('Saved data for %s recordings.', nrow(selection_tables)))