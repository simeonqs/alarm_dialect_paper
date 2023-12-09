# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: alarm_dialect_paper
# Author: Simeon Q. Smeele
# Description: Slightly modified version of the function used in 
# Smeele et al. 2023
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

require(stringr)
require(cmdstanr)

run.noisy.simulation = function(
  selection_tables,
  path_model,
  sd_city = 1,
  sd_park = 0.5,
  sd_ind = 0.3,
  sd_obs = 0.1,
  lambda_ind_per_rec = 1.5, # average number of inds per chunk
  lambda_rerec = 1.5, # average number of times ind is recorded across recs
  p_next_chunk = 0.5, # probability of recording same in in next chunk
  path_out = NULL
){
  
  # Get lambda's
  ## Function to get zero-truncated lambda with same mean
  find.l = function(lambda){
    ii = 0
    cont = T
    while(cont){
      ii = ii + 1
      f = function(x) x/(1 - exp(-x))
      x = seq(0.01, 10, length.out = 1e4)
      y = f(x)
      d = abs(y-lambda)
      l = x[d == min(d)]
        s1 = rpois(1e6, l)
      s1 = s1[s1 != 0]
      if(round(mean(s1), 2) == lambda) cont = F
      if(ii > 10) stop('Lambda cannot be found.')
    }
    return(l)
  }
  l1 = find.l(lambda_ind_per_rec)
  l2 = find.l(lambda_rerec)
  # Function to draw single zero truncated sample
  zpois = function(lambda){
    cont = T
    while(cont){
      r = rpois(1, lambda)
      if(r != 0) cont = F
    }
    return(r)
  } 
  
  # Get simulation ids
  selection_tables$underlying_id = selection_tables$id
  ## Also recorded in next chunk
  o1 = length(unique(selection_tables$underlying_id))
  ui = unique(selection_tables$underlying_id)
  # don't do it for annotations as these are the same ind 
  ui = ui[!str_detect(ui, 'sqs|SQS|sat|SAT|NA|audio')] 
  files = selection_tables$underlying_id %>% str_split('_') %>% 
    lapply(`[`, 1:4) %>% sapply(paste, collapse = '_')
  chunks = selection_tables$underlying_id %>% str_split('_') %>% sapply(`[`, 5)
  for(rec in sort(ui)){
    file = rec %>% str_split('_') %>% sapply(`[`, 1:4) %>% 
      paste(collapse = '_')
    chunk = rec %>% str_split('_') %>% sapply(`[`, 5) %>% as.numeric
    # skip if not in next chunk
    if(sample(c(T, F), 1, prob = c(1-p_next_chunk, p_next_chunk))) next 
    next_rec = which(files == file & chunks == chunk + 1)
    if(length(next_rec) != 0) selection_tables$underlying_id[next_rec] = rec
  }
  o2 = length(unique(selection_tables$underlying_id))
  if(o1 < o2) stop('Problem with first step.')
  ## Get multiple inds per rec
  ui = unique(selection_tables$underlying_id)
  # don't do it for annotations as these are the same ind
  ui = ui[!str_detect(ui, 'sqs|SQS|sat|SAT')]
  for(x in ui){
    o = selection_tables$underlying_id[selection_tables$id == x] # old id
    n = zpois(l1) # how many inds to split between
    e = sample(1:n, length(o), replace = T) # find which bird vocalises
    selection_tables$underlying_id[selection_tables$id == x] = 
      paste(o, e, sep = '_') # create new labels
  }
  o3 = length(unique(selection_tables$underlying_id))
  if(o2 > o3) stop('Problem with second step.')
  ## Also recorded other recording
  for(id in unique(selection_tables$underlying_id)){
    n = zpois(l2) # how many times recorded this ind across recordings
    park = selection_tables$park[selection_tables$underlying_id == id][1]
    n = min(n, length(unique(selection_tables$underlying_id[
      selection_tables$park == park])))
    if(n == 1) next
    for(ni in 1:n){
      r = sample(selection_tables$underlying_id[
        selection_tables$park == park & 
          selection_tables$underlying_id != id], 1)
      selection_tables$underlying_id[selection_tables$underlying_id == r] = id
      if(length(unique(selection_tables$underlying_id[
        selection_tables$park == park])) == 1) break
    }
  }
  o4 = length(unique(selection_tables$underlying_id))
  if(o3 < o4) stop('Problem with third step.')

  # Simulate
  dat = data.frame()
  for(city in unique(selection_tables$city)){
    mean_city = rnorm(1, 0, sd_city)
    parks = unique(selection_tables$park[selection_tables$city == city])
    for(park in parks){
      mean_park = rnorm(1, mean_city, sd_park)
      inds = unique(selection_tables$underlying_id[
        selection_tables$park == park])
      for(ind in inds){
        mean_ind = rnorm(1, mean_park, sd_ind)
        N_obs = length(which(selection_tables$underlying_id == ind))
        dat = rbind(dat,
                    data.frame(
                      city = city,
                      park = park,
                      ind = selection_tables$id[
                        selection_tables$underlying_id == ind],
                      PC1 = rnorm(N_obs, mean_ind, sd_obs)
                    ))
      }
    }
  }
  
  # Rescale PC1
  dat$PC1 = as.numeric(scale(dat$PC1))
  
  # Add other info
  dat$city = as.integer(as.factor(dat$city))
  dat$park = as.integer(as.factor(dat$park))
  dat$ind = as.integer(as.factor(dat$ind))
  dat = as.list(dat)
  dat$N_city = max(dat$city)
  dat$N_park = max(dat$park)
  dat$N_ind = max(dat$ind)
  dat$N_obs = length(dat$PC1)
  
  # Run some checks
  if(length(unique(dat$ind)) != length(unique(selection_tables$id))) 
    stop('Number inds do not match!')
  if(all(sort(as.numeric(table(dat$park))) != 
         sort(as.numeric(table(selection_tables$park))))) 
    stop('Parks do not match!')
  
  # Print size dataset and parameters
  parameters = c(sd_city = sd_city,
                 sd_park = sd_park,
                 sd_ind = sd_ind,
                 sd_obs = sd_obs,
                 lambda_ind_per_rec = lambda_ind_per_rec,
                 lambda_rerec = lambda_rerec,
                 p_next_chunk = p_next_chunk)
  message('\n================================================\n')
  message('Starting model with ', dat$N_obs, ' observations.')
  message('Running with these paramters:')
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
  file = sprintf('%s/results_noisy_%s_%s_%s_%s_%s_%s_%s.RData', 
                 path_out, sd_city, sd_park, sd_ind, sd_obs, 
                 lambda_ind_per_rec, lambda_rerec, p_next_chunk)
  save(list = c('parameters', 'post', 'dat'), file = file)
  message('Done. Saved parameters, post and dat.')
  
} # end function