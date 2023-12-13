# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: alarm_dialect_paper  
# Author: Simeon Q. Smeele
# Description: Plotting results sensitivity analysis and real data.
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('dplyr', 'scales', 'rethinking')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths 
path_functions = 'analysis/code/functions'
path_results_sensitivity_analysis = 'analysis/results/sensitivity_analysis'
path_pco_1 = 'analysis/results/model_output/pco1_results.RData'
path_pco_2 = 'analysis/results/model_output/pco2_results.RData'
path_pdf_city = 'analysis/results/sensitivity_analysis/city.pdf'
path_pdf_park = 'analysis/results/sensitivity_analysis/park.pdf'
path_pdf_real_data = 'analysis/results/sensitivity_analysis/real_data.pdf'

# Load functions
.functions = sapply(list.files(path_functions, 
                               pattern = '*R', full.names = T), source)

# Plot city results
pdf(path_pdf_city, 12, 5)
files = list.files(path_results_sensitivity_analysis, '*RData', full.names = T)
noisy_no_signal_files = files[str_detect(files, 'noisy_0.001_0.001')]
settings = noisy_no_signal_files %>% basename() %>% str_split('_')
lambda_ind_per_rec = settings %>% sapply(`[`, 7) %>% as.numeric
lambda_rerec = settings %>% sapply(`[`, 8) %>% as.numeric
p_next_chunk = settings %>% sapply(`[`, 9) %>% 
  str_remove('.RData') %>% as.numeric
ls = unique(lambda_rerec)
ps = unique(p_next_chunk)
## Sigma city
plot(NULL, xlim = c(0, 0.9), ylim = c(0, 1), xaxt = 'n', 
     xlab = 'p next chunk', ylab = 'sigma city')
axis(1, ps + 1.5/40, ps)
load(files[str_detect(files, 'clean')])
lr = 0
points(0 + lr/80, 
       mean(post$sigma_city), 
       pch = 16, col = alpha(lr+1, 0.8))
lines(rep(0 + lr/80, 2), 
      PI(post$sigma_city), 
      lwd = 3, col = alpha(lr+1, 0.8))
for(i in seq_along(noisy_no_signal_files)){
  load(noisy_no_signal_files[i])
  lr = as.integer(as.factor(lambda_rerec))[i]
  points(p_next_chunk[i] + lr/80, 
         mean(post$sigma_city), 
         pch = 16, col = alpha(lr+1, 0.8))
  lines(rep(p_next_chunk[i] + lr/80, 2), 
        PI(post$sigma_city), 
        lwd = 3, col = alpha(lr+1, 0.8))
}
legend('topright', c('clean', paste0('lambda rerec = ', ls)), 
       text.col = 1:(length(ls)+1))
dev.off()

# Plot park results
pdf(path_pdf_park, 12, 5)
plot(NULL, xlim = c(0, 0.9), ylim = c(0, 1), xaxt = 'n', 
     xlab = 'p next chunk', ylab = 'sigma park')
axis(1, ps + 1.5/40, ps)
load(files[str_detect(files, 'clean')])
lr = 0
points(0 + lr/80, 
       mean(post$sigma_park), 
       pch = 16, col = alpha(lr+1, 0.8))
lines(rep(0 + lr/80, 2), 
      PI(post$sigma_park), 
      lwd = 3, col = alpha(lr+1, 0.8))
for(i in seq_along(noisy_no_signal_files)){
  load(noisy_no_signal_files[i])
  lr = as.integer(as.factor(lambda_rerec))[i]
  points(p_next_chunk[i] + lr/80, 
         mean(post$sigma_park), 
         pch = 16, col = alpha(lr+1, 0.8))
  lines(rep(p_next_chunk[i] + lr/80, 2), 
        PI(post$sigma_park), 
        lwd = 3, col = alpha(lr+1, 0.8))
}
ll = levels(as.factor(lambda_rerec))
legend('topright', c('clean', paste0('lambda rerec = ', ls)), 
       text.col = 1:(length(ls)+1))
dev.off()

# Plot actual model results
pdf(path_pdf_real_data, 5, 5)
plot(NULL, xlim = c(-0.5, 3), ylim = c(0, 1), 
     xaxt = 'n', xlab = '', ylab = 'sigma')
axis(1, c(0.25, 2.25), c('PC 1', 'PC 2'))
ii = 0
for(path in c(path_pco_1, path_pco_2)){
  load(path)
  points(ii + 0, 
         mean(post$sigma_park), 
         pch = 16, col = alpha(2, 0.8))
  lines(rep(ii + 0, 2), 
        PI(post$sigma_park), 
        lwd = 3, col = alpha(2, 0.8))
  points(ii + 00.5, 
         mean(post$sigma_city), 
         pch = 16, col = alpha(3, 0.8))
  lines(rep(ii + 00.5, 2), 
        PI(post$sigma_city), 
        lwd = 3, col = alpha(3, 0.8))
  ii = 2
  message(sprintf('Mean sigma cities %s: %s, PI: %s-%s. 
                  Mean sigma parks: %s, PI: %s-%s.',
                  path,
                  mean(post$sigma_city), PI(post$sigma_city)[1], 
                  PI(post$sigma_city)[2],
                  mean(post$sigma_par), PI(post$sigma_park)[1], 
                  PI(post$sigma_park)[2]))
}
legend('topleft', legend = c('park', 'city'), pch = 16, lty = 1, 
       col = alpha(2:3, 0.8))

dev.off()

# Report
message('Succesfully plotted the results!')