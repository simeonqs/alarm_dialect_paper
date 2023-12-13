# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: alarm_dialect_paper
# Author: Simeon Q. Smeele
# Description: Slightly modified version of the function used in 
# Smeele et al. 2023
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

require(cmdstanr)

run.clean.simulation = function(
  selection_tables,
  path_model,
  sd_city = 1,
  sd_park = 0.5,
  sd_ind = 0.3,
  sd_obs = 0.1,
  path_out = NULL
){
  
  # Simulate
  dat = data.frame()
  for(city in unique(selection_tables$city)){
    mean_city = rnorm(1, 0, sd_city)
    parks = unique(selection_tables$park[selection_tables$city == city])
    for(park in parks){
      mean_park = rnorm(1, mean_city, sd_park)
      inds = unique(selection_tables$id[selection_tables$park == park])
      for(ind in inds){
        mean_ind = rnorm(1, mean_park, sd_ind)
        N_obs = length(which(selection_tables$id == ind))
        dat = rbind(dat,
                    data.frame(
                      city = city,
                      park = park,
                      ind = ind,
                      PC1 = rnorm(N_obs, mean_ind, sd_obs)
                    ))
      }
    }
  }
  
  # Rescale PC1
  dat$PC1 = as.numeric(scale(dat$PC1))
  
  # Translate park such that dmat fits
  trans_park = seq_along(unique(dat$park))
  names(trans_park) = unique(dat$park)
  
  # Add other info
  dat$city = as.integer(as.factor(dat$city))
  dat$park = trans_park[dat$park]
  dat$ind = as.integer(as.factor(dat$ind))
  dat = as.list(dat)
  dat$N_city = max(dat$city)
  dat$N_park = max(dat$park)
  dat$N_ind = max(dat$ind)
  dat$N_obs = length(dat$PC1)
  
  # Run some checks
  if(length(unique(dat$ind)) != length(unique(selection_tables$id))) 
    stop('Number inds do not match!')
  if(all(sort(table(dat$park)) != sort(table(selection_tables$park)))) 
    stop('Parks do not match!')
  
  # Print size dataset and parameters
  parameters = c(sd_city = sd_city,
                 sd_park = sd_park,
                 sd_ind = sd_ind,
                 sd_obs = sd_obs)
  message('\n==============================================================\n')
  message('Starting model with ', dat$N_obs, ' observations.')
  message('Running with these parameters:')
  print(parameters)
  
  # Run model
  model = cmdstan_model(path_model)
  fit = model$sample(data = dat, 
                     seed = 1, 
                     chains = 4, 
                     parallel_chains = 4,
                     refresh = 500,
                     adapt_delta = 0.95)
  post = fit$draws(format = "df") %>% as.data.frame
  
  # Save
  file = sprintf('%s/results_clean_%s_%s_%s_%s.RData', 
                 path_out, sd_city, sd_park, sd_ind, sd_obs)
  save(list = c('parameters', 'post', 'dat'), file = file)
  message('Done. Saved parameters, post and dat.')

} # end function