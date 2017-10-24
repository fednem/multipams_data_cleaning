library(fslr)
#note that the first part of this script must be run in ubuntu
#create 3d images of r2star and extract the value
setwd("/mnt/d/MultiPAMS/iron/Image/")
system('fslmerge -t 4d_normalized_r2s w*r2*')
iron_data <- system('fslstats -t 4d_normalized_r2s.nii.gz -k /mnt/d/MultiPAMS/rs-fmri/ROIs/sn_bilateral_Pat.nii -M', intern = TRUE)
save(file = "iron_data.RData", iron_data)
#from here the script can be run in windows
system("cp iron_data.RData /mnt/d/MultiPAMS/clinical_data/script/data_cleaning_github")