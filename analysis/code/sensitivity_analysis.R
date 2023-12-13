# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: alarm_dialect_paper  
# Author: Simeon Q. Smeele
# Description: Running sensitivity analysis.
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('cmdstanr')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths 
path_functions = 'analysis/code/functions'
path_out = 'analysis/results/sensitivity_analysis'
path_model = 'analysis/code/m_4.stan'
path_combined_selection_tables = 'analysis/results/selection_tables.RData'

# Load functions
.functions = sapply(list.files(path_functions, 
                               pattern = '*R', full.names = T), source)

# Load selection tables
load(path_combined_selection_tables)

# Run simulations
set.seed(1)
run.clean.simulation(selection_tables = selection_tables,
                     path_model = path_model,
                     path_out = path_out,
                     sd_city = 0.001,
                     sd_park = 0.001,
                     sd_ind = 1,
                     sd_obs = 1)
for(lambda_rerec in seq(1, 2, 0.25))
  for(p_next_chunk in seq(0, 0.7, 0.1))
    run.noisy.simulation(selection_tables = selection_tables,
                         path_model = path_model,
                         path_out = path_out,
                         sd_city = 0.001,
                         sd_park = 0.001,
                         sd_ind = 1,
                         sd_obs = 1,
                         lambda_ind_per_rec = 1.5,
                         lambda_rerec = lambda_rerec,
                         p_next_chunk = p_next_chunk)

# Report
message('Finished all simulations!')



