library(fslr)
library(tidyverse)
library(car)
#note that the first part of this script must be run in ubuntu
#create 3d images of r2star and extract the value
home_dir <- getwd()
setwd("../Image/")
system('fslmerge -t 4d_normalized_r2s w*r2*')
system('fslmerge -t 4d_normalized_t2s w*T2*')
iron_data <- system('fslstats -t 4d_normalized_r2s.nii.gz -M', intern = TRUE)
setwd(home_dir)
save(file = "iron_data.RData", iron_data)
#from here the script can be run in windows