# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: alarm_dialect_paper  
# Author: Simeon Q. Smeele
# Description: Running Bayesian model.   
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
path_combined_selection_tables = 'analysis/results/selection_tables.RData'
path_pco = 'analysis/results/pco_out.RData'
path_model = 'analysis/code/m_4.stan'
path_out = 'analysis/results/model_output'

# Load previous result
load(path_pco)
load(path_combined_selection_tables)

# Find parks
parks = selection_tables$park
unique_parks = sort(unique(parks))
trans_parks = 1:length(unique_parks)
names(trans_parks) = unique_parks

# Find cities
cities = selection_tables$city

# Translate cities
unique_cities = sort(unique(cities))
trans_cities = 1:length(unique_cities)
names(trans_cities) = unique_cities

# Run for both dimensions
for(i in 1:2){
  
  # Pick data
  PC_dat = list(pco_out$vectors[,1], pco_out$vectors[,2])[[i]]
  name = c('pco1', 'pco2')[i]
  
  # Combine data
  dat = data.frame(city = trans_cities[cities],
                   park = trans_parks[parks],
                   ind = as.integer(as.factor(selection_tables$id)),
                   PC1 = scale(PC_dat))
  dat = dat[order(dat$city, dat$park),]
  dat = as.list(dat)
  dat$N_city = max(dat$city)
  dat$N_park = max(dat$park)
  dat$N_ind = max(dat$ind)
  dat$N_obs = length(dat$PC1)
  
  # Print
  message(paste('Starting', name, 'model with', dat$N_obs, 'observations. \n'))
  
  # Run model
  model = cmdstan_model(path_model)
  fit = model$sample(data = dat, 
                     seed = 1, 
                     chains = 4, 
                     parallel_chains = 4,
                     refresh = 500,
                     adapt_delta = 0.95)
  diag = fit$cmdstan_diagnose()  
  post = fit$draws(format = 'df') |> as.data.frame()
  
  # Save
  save(list = c('fit', 'post', 'dat', 'trans_cities', 'trans_parks', 'diag'), 
       file = sprintf('%s/%s_results.RData', path_out, name))
}

# Message
message('All done.')