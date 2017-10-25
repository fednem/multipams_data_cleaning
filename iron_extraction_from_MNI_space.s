library(fslr)
#note that this script must be run in ubuntu
#create 3d images of r2star and extract the value
setwd("/mnt/d/MultiPAMS/iron/Image/")
system('fslmerge -t 4d_normalized_r2s w*r2*')
iron_data <- system('fslstats -t 4d_normalized_r2s.nii.gz -k /mnt/d/MultiPAMS/rs-fmri/ROIs/sn_bilateral_Pat.nii -M', intern = TRUE)
save(file = "iron_data.RData", iron_data)
#from here the script can be run in windows
system("cp iron_data.RData /mnt/d/MultiPAMS/clinical_data/script/data_cleaning_github")

#extract from eroded version of the roi
iron_data_eroded_roi <- system('fslstats -t 4d_normalized_r2s.nii.gz -k /mnt/d/MultiPAMS/rs-fmri/ROIs/sn_bilateral_Pat_eroded.nii -M', intern = TRUE)
save(file = "iron_data_eroded_roi.RData", iron_data_eroded_roi)

system("cp iron_data_eroded_roi.RData /mnt/d/MultiPAMS/clinical_data/script/data_cleaning_github")

#extract from left and right separately and avg, as germaine did
iron_data_left_roi <- system('fslstats -t 4d_normalized_r2s.nii.gz -k /mnt/d/MultiPAMS/rs-fmri/ROIs/sn_l_mask_Pat.nii -M', intern = TRUE)
iron_data_right_roi <- system('fslstats -t 4d_normalized_r2s.nii.gz -k /mnt/d/MultiPAMS/rs-fmri/ROIs/sn_r_mask_Pat.nii -M', intern = TRUE)
iron_data_avg_left_right_roi <- (as.numeric(iron_data_right_roi) + as.numeric(iron_data_left_roi))/2
save(file = "iron_data_avg_left_right_roi.RData", iron_data_avg_left_right_roi)
system("cp iron_data_avg_left_right_roi.RData /mnt/d/MultiPAMS/clinical_data/script/data_cleaning_github")

#extract from mni sn roi and from Wu et al roi
iron_data_mni_2mm_roi <- system('fslstats -t 4d_normalized_r2s.nii.gz -k /mnt/d/MultiPAMS/rs-fmri/ROIs/substantia_nigra_bilateral_2mm.nii -M', intern = TRUE)
save(file = "iron_data_mni_2mm_roi.RData", iron_data_mni_2mm_roi)
system("cp iron_data_mni_2mm_roi.RData /mnt/d/MultiPAMS/clinical_data/script/data_cleaning_github")


iron_data_Wu_roi <- system('fslstats -t 4d_normalized_r2s.nii.gz -k /mnt/d/MultiPAMS/rs-fmri/ROIs/bilateral_SN_Wu_2012.nii -M', intern = TRUE)
save(file = "iron_data_Wu_roi.RData", iron_data_Wu_roi)
system("cp iron_data_Wu_roi.RData /mnt/d/MultiPAMS/clinical_data/script/data_cleaning_github")
