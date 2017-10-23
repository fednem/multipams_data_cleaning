library(fslr)
library(tidyverse)

#NB: this block lust be run in ubuntu!
r2star_files <- dir(pattern = "^P.*r2star.nii")
roi_files <- dir(pattern = "subject_space.*.nii")

iron_values <- vector(length = length(roi_files), mode = "numeric")

for (subject_index in 1:length(roi_files)) {
  iron_values[subject_index] <- fslstats(r2star_files[subject_index], paste("-k ", roi_files[subject_index], "-M", sep = " "))


code <- sapply(r2star_files, substr, 6, 10)

}

iron_data <- data_frame(code = code,  iron = as.numeric(iron_values))

save(file = "iron_data_from_single_subject_space.RData", iron_data)
#NB: this block lust be run in ubuntu!