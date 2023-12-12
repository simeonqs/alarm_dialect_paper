# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: alarm_dialect_paper  
# Author: Simeon Q. Smeele
# Description: Loads all selection tables, filters for alarm calls, loads
# waves and stores clips in RData object. Also retrieves other relevant data
# and stores is as extra columns in the selection table. Selection table is 
# stored in seperate RData object. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('tuneR', 'callsync', 'stringr', 'dplyr')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths 
path_functions = 'analysis/code/functions'
path_selection_tables = 'analysis/data/selection_tables'
path_sorted_spectrograms = 'analysis/data/sorted_spectrograms_sqs'
path_overview_recordings = 'analysis/data/overview_recordings'
path_overview_parks = 'analysis/data/overview_parks'
path_audio = '/home/au472091/Documents/large_data/audio_alarm_dialect_paper'
path_combined_selection_tables = 'analysis/results/selection_tables.RData'
path_waves = 'analysis/results/waves.RData'

# Load functions
.functions = sapply(list.files(path_functions, 
                               pattern = '*R', full.names = TRUE), source)

# Load selection tables
selection_tables = load.selection.tables(path_selection_tables,
                                         split_anno = TRUE)

# Load sorted spectrograms
sorted = list.files(path_sorted_spectrograms, recursive = TRUE, 
                    full.names = TRUE)

# Load overview recordings and parks
files_overview_recordings = list.files(path_overview_recordings, '*csv', 
                                       full.names = TRUE)
files_overview_parks = list.files(path_overview_parks, '*csv', 
                                  full.names = TRUE)
overview_recordings = lapply(files_overview_recordings, read.csv2, 
                             na.strings = c('', ' ', 'NA'), 
                             stringsAsFactors = FALSE) |> bind_rows()
overview_parks = lapply(files_overview_parks, read.csv2, 
                        na.strings = c('', ' ', 'NA'), 
                        stringsAsFactors = FALSE) |> bind_rows()

# Subset for alarm calls AKA 'growl - u'
sorted = sorted[str_detect(sorted, 'growl - u')]
sorted_fs = sorted |> basename() |> str_remove('.wav') |> str_remove('.pdf')
rownames(selection_tables) = selection_tables$fs
selection_tables = selection_tables[sorted_fs,]

# Load waves
waves = lapply(seq_len(nrow(selection_tables)), function(i)  
  readWave(sprintf('%s/%s.wav', path_audio, selection_tables$file[i]),
           from = selection_tables$Begin.Time..s.[i],
           to = selection_tables$End.Time..s.[i],
           units = 'seconds')
  )
names(waves) = selection_tables$fs

# Find other info
files = selection_tables$fs |> strsplit('-') |> sapply(`[`, 1)
sels = selection_tables$fs |> strsplit('-') |> sapply(`[`, 2)
selection_tables$park = vapply(selection_tables$fs, function(x){
  recording = strsplit(x, '-')[[1]][1]
  return(overview_recordings$park[overview_recordings$file == recording])
}, character(1)) |> as.character()
selection_tables$city = vapply(selection_tables$park, function(x) 
  overview_parks$city[which(overview_parks$park == x)], character(1)) |> 
  as.character()

# Get ID or 5 min control
selection_tables$id = sapply(selection_tables$fs, function(fs){
  file = fs |> strsplit('-') |> sapply(`[`, 1)
  id = selection_tables$individual[selection_tables$fs == fs]
  if(is.na(id)) id = ceiling(selection_tables$Begin.Time..s.[
    selection_tables$fs == fs]/300)
  if(id == 'NA') id = ceiling(selection_tables$Begin.Time..s.[
    selection_tables$fs == fs]/300)
  return(paste(file, id, sep = '_'))
}) |> as.character()

# Save 
save(waves, file = path_waves)
save(selection_tables, file = path_combined_selection_tables)
