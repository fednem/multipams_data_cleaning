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
load('iron_data.RData')
#read functional data

source("D:/r_script/ExtractROIsValues.r")
directory <- "D:/MultiPAMS/rs-fmri/MultiPAMS_11102017_classic_preprocessing/results/firstlevel/ANALYSIS_01/"
CouplesToExtract <- "ROIsCouple.txt"
mat <- ExtractROIsValues(directory, CouplesToExtract)
mat <- as_data_frame(mat)
second_level_covariates <- read_delim("D:/MultiPAMS/rs-fmri/selected_2nd_covariates.txt", delim = " ")
second_level_covariates <- second_level_covariates %>%
  mutate(group = if_else(group_1 == 0, if_else(group_2 == 0, "group_3", "group_2"), "group_1")) %>%
  select(-5, -6, -7)
mat <- bind_cols(mat, second_level_covariates) %>%
  filter(Subject != 24)
group <- mat$group [-24]

iron_data <- iron_data [-24]

mat$iron <- as.numeric(iron_data)

value_iron_nuisance_by_rois <- mat %>%
  select(-Condition) %>%
  gather(rois,value, -Subject, -QA_ValidScans, -QA_MeanMotion, -QA_MeanGlobal, -QA_GCOR_rest, -group, -iron) %>%
  split(., .$rois) %>%
  map(~lm(data = ., value ~ iron + QA_MeanMotion + QA_MeanGlobal + QA_GCOR_rest)) %>%
  map(~summary(.))

value_iron_nuisance_by_rois_coefficients <- mat %>%
  select(-Condition) %>%
  gather(rois,value, -Subject, -QA_ValidScans, -QA_MeanMotion, -QA_MeanGlobal, -QA_GCOR_rest, -group, -iron) %>%
  split(., .$rois) %>%
  map(~lm(data = ., value ~ iron + QA_MeanMotion + QA_MeanGlobal + QA_GCOR_rest)) %>%
  map(~summary(.)) %>%
  map(~`[[`(.,"coefficients")) %>%
  map_dbl(~`[`(.,17))


value_iron_nuisance_by_rois_coefficients_fdr <- p.adjust(value_iron_nuisance_by_rois_coefficients, "fdr")

group_effect_on_iron <- oneway.test(iron ~ group, data = mat)

#no significant effect of group, give it a plot anyway

bar_plot_iron_by_group <- mat %>%
    select(iron, group) %>%
    group_by(group) %>%
    summarise(mean = mean(iron), lower_ci = t.test(iron)$conf.int[1], upper_ci = t.test(iron)$conf.int[2]) %>%
    ggplot(aes(x = group, y = mean, fill = group)) + 
    geom_bar(stat = 'identity') + 
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), color = "black", width = .2)

print(bar_plot_iron_by_group)

#try model with interaction by group

rs_iron_group_ironXgroup_by_rois.anova <- mat %>%
  select(-Condition) %>%
  gather(rois,value, -Subject, -QA_ValidScans, -QA_MeanMotion, -QA_MeanGlobal, -QA_GCOR_rest, -group, -iron) %>%
  split(., .$rois) %>%
  map(~lm(data = ., value ~ iron + group + iron:group + QA_MeanMotion + QA_MeanGlobal + QA_GCOR_rest)) %>%
  map(~Anova(mod = ., t = 3))


