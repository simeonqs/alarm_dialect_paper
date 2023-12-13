# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: alarm_dialect_paper  
# Author: Simeon Q. Smeele
# Description: Creates a table with the sample effort.
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('dplyr')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths 
path_functions = 'analysis/code/functions'
path_combined_selection_tables = 'analysis/results/selection_tables.RData'
path_table_effort = 'analysis/results/table_effort.csv'

# Load functions
.functions = sapply(list.files(path_functions, 
                               pattern = '*R', full.names = TRUE), source)

# Load data
load(path_combined_selection_tables)

# Get effort
out = data.frame(park = unique(selection_tables$park))
out$city = sapply(out$park, function(p) 
  selection_tables$city[selection_tables$park == p][1])
out = out[,c(2,1)]
out$n_days = sapply(out$park, function(p){
  sub = selection_tables[selection_tables$park == p,]
  dates = sub$file %>% str_sub(1, 10)
  return(length(unique(dates)))
})
out$n_calls = sapply(out$park, function(p){
  sub = selection_tables[selection_tables$park == p,]
  return(nrow(sub))
})
out = out[order(out$city),]
names(out) = c('city', 'park', 'number of days', 'number of calls')

write.csv(out, path_table_effort, quote = FALSE, row.names = FALSE)
