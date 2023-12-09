# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: monk parakeets
# Date started: 10-12-2020
# Date last modified: 30-08-2021
# Author: Simeon Q. Smeele
# Description: Loads selection tables made in Raven and outputs them binded into a dataframe. 
# This version fixes a problem where there were more than eight columns. 
# This version includes the option to merge the annotations onto the selection tables. 
# This version adds a fs column with the file selections. 
# This version adds the context if supplied. 
# This version has the option to split annotations. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

require(readxl)

load.selection.tables = function(path_selection_tables,
                                 path_annotations = NULL, # if included annotations are merged on
                                 path_context = NULL, # if included adds the context
                                 split_anno = F
                                 ){
  
  selection_tables = path_selection_tables %>% 
    list.files('*txt', full.names = T) %>% 
    lapply(function(x){
      temp = read.csv(x, sep = '\t')
      if(ncol(temp) != 8){ # if there are decibel columns, make sure it still works
        temp = read.csv(x, sep = '\t', colClasses = c('numeric', 'character', 'numeric', 'numeric', 'numeric', 
                                                      'numeric', 'numeric', 'character', 'character', 'character', 
                                                      'character'))
        temp = temp[,colnames(temp) %in% c('Selection', 'View', 'Channel', 'Begin.Time..s.', 'End.Time..s.', 
                                           'Low.Freq..Hz.', 'High.Freq..Hz.', 'Delta.Time..s.', 'Annotation')]
      } else {
        temp = read.csv(x, sep = '\t', colClasses = c('numeric', 'character', 'numeric', 'numeric', 'numeric', 
                                                      'numeric', 'numeric', 'character'))
      }
      return(temp)
    })
  names(selection_tables) = path_selection_tables %>% 
    list.files('*txt') %>% str_remove('.Table.1.selections.txt')
  dat = selection_tables %>%
    bind_rows(.id = 'file')
  dat = dat[dat$View == 'Waveform 1',]
  
  # Add fs column
  dat$fs = paste(dat$file, dat$Selection, sep = '-')
  
  # Splitting the annotations
  if(split_anno){
    dat[c('individual', 'behaviour', 'group_size')] = NA
    for(i in 1:nrow(dat)){
      row_dat = dat$Annotation[i] %>% as.character
      if(!(is.na(row_dat) | row_dat == '')){
        split = strsplit(row_dat, '_')[[1]]
        if(length(split) != 3){ # skip problematic annotations
          print('Problem with the annotation in:')
          print(dat[i,]$fs %>% as.character)
          next
        }
        dat$individual[i] = split[1]
        dat$behaviour[i] = split[2]
        if(split[3] %in% c('NA', 'na')){ # if group size is NA, fill in and skip the last few lines
          dat$group_size[i] = NA
          next
        }
        if(split[3] %>% str_remove('u.') %>% str_remove('U.') %>% as.numeric() %>% is.na){
          print('Problem with the annotation in:')
          print(dat[i,]$fs %>% as.character)
          next
        }
        dat$group_size[i] = split[3] %>% str_remove('u.') %>% str_remove('U.') %>% as.numeric()
      }
    }
  }
  
  # Merge annotations
  if(!is.null(path_annotations)){
    annotations = read.csv2(path_annotations)
    dat = merge(dat, annotations, by.x = 'Annotation', by.y = 'annotation_ref',
                all.x = T, all.y = F)
  }
  
  # Merge annotations
  if(!is.null(path_context)){
    context = load.call.type.context(path_context)
    dat = merge(dat, context, by = 'fs',
                all.x = T, all.y = F)
  }
  
  return(dat)
  
}
