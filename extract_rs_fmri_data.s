library(tidyverse)
#read functional data

source("D:/r_script/ExtractROIsValues.r")

directory <- "D:/MultiPAMS/rs-fmri/MultiPAMS_11102017_classic_preprocessing/results/firstlevel/ANALYSIS_01/"

CouplesToExtract <- "ROIsCouple.txt"
mat <- ExtractROIsValues(directory, CouplesToExtract)
mat <- as_data_frame(mat)
save(file = "rs_fmri_extraction_data.RData", mat)