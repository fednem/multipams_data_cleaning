library(tidyverse)
library(car)
library(lsmeans)

#load relevant data
load('iron_data.RData')
load("rs_fmri_extraction_data.RData")
#load relevant data

second_level_covariates <- read_delim("D:/MultiPAMS/rs-fmri/selected_2nd_covariates.txt", delim = " ")
second_level_covariates <- second_level_covariates %>%
  mutate(group = if_else(group_1 == 0, if_else(group_2 == 0, "group_3", "group_2"), "group_1")) %>%
  select(-5, -6, -7)
mat <- bind_cols(mat, second_level_covariates) %>%
  filter(Subject != 24)

group = mat$group

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


#iron extraction and anlysis from single subject space
load("iron_data_from_single_subject_space.RData")
iron_data_single_subject_space <- iron_data [-24,]

hist(iron_data_single_subject_space$iron)
boxplot(iron_data_single_subject_space$iron)

group_effect_on_iron <- bind_cols(iron_data_single_subject_space, data_frame(group = group)) %>%
  oneway.test(data = ., iron ~ group)

#there's a significant effect of group

#try again without outlier
group_effect_on_iron_no_outlier <- bind_cols(iron_data_single_subject_space, data_frame(group = group)) %>%
  filter(iron < 33) %>%
  oneway.test(data = ., iron ~ group)

#without outlier is barely significant (p = .05077)

#plot effect

barplot_effect_of_group <- bind_cols(iron_data_single_subject_space, data_frame(group = group)) %>%
  group_by(group) %>%
  summarise(avg = mean(iron), lower_ci = t.test(iron)$conf.int[1], upper_ci = t.test(iron)$conf.int[2]) %>%
  ggplot(data = ., aes(x = group, y = avg, fill = group)) + 
    geom_bar(stat = "identity") + 
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = .2) + 
    ggtitle("Main Effect of Group\non Iron") + 
    ylab("Group") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))

pdf("main_effect_of_group_on_iron_single_subject_space.pdf", w = 5, h = 3)
barplot_effect_of_group
dev.off()

#post-hoc - note that this change from trend with tukey (.09) to marginally significant with fdr (.0566)
group_effect_on_iron_post_hoc <- bind_cols(iron_data_single_subject_space, data_frame(group = group)) %>%
  lm(data = ., iron ~ group) %>%
  lsmeans(., pairwise~group, adjust = "fdr")

#check effect of iron from single subject space on rs_fmri
mat$iron <- as.numeric(iron_data_single_subject_space$iron)

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

rs_iron_group_ironXgroup_by_rois.anova <- mat %>%
  select(-Condition) %>%
  gather(rois,value, -Subject, -QA_ValidScans, -QA_MeanMotion, -QA_MeanGlobal, -QA_GCOR_rest, -group, -iron) %>%
  split(., .$rois) %>%
  map(~lm(data = ., value ~ iron + group + iron:group + QA_MeanMotion + QA_MeanGlobal + QA_GCOR_rest)) %>%
  map(~Anova(mod = ., t = 3))

#control by group but don t include interaction
value_iron_nuisance_group_by_rois <- mat %>%
  select(-Condition) %>%
  gather(rois,value, -Subject, -QA_ValidScans, -QA_MeanMotion, -QA_MeanGlobal, -QA_GCOR_rest, -group, -iron) %>%
  split(., .$rois) %>%
  map(~lm(data = ., value ~ iron + QA_MeanMotion + QA_MeanGlobal + QA_GCOR_rest + group)) %>%
  map(~Anova(mod = ., t = 3))
  
#plot effect for 22 -54 -14 and 22 -54 -14
res_of_rs_fmri_over_nuisance_selcted_rois <- mat %>%
  select(-Condition) %>%
  gather(rois,value, -Subject, -QA_ValidScans, -QA_MeanMotion, -QA_MeanGlobal, -QA_GCOR_rest, -group, -iron) %>%
  split(., .$rois) %>%
  `[`(., names(value_iron_nuisance_by_rois) %in% c("main_effect_group.-22 -62 +2 _To_sn_bilateral_Pat",
                                                  "main_effect_group.+22 -54 -14 _To_sn_bilateral_Pat")) %>%
  map(~lm(data = ., value ~ QA_MeanMotion + QA_MeanGlobal + QA_GCOR_rest)) %>%
  map(~residuals(.))

effect_of_iron_on_residuals_rs <- as_data_frame(res_of_rs_fmri_over_nuisance_selcted_rois) %>% 
  bind_cols(data_frame(iron = as.numeric(mat$iron), group = mat$group)) %>%
  gather(., rois, value, -iron, -group) %>%
  split(., .$rois) %>%
  map(~ggplot(data = ., aes(x = iron, y = value, color = group)) + 
        geom_point(size = 2, alpha = .7) + 
        stat_smooth(aes(group = 1), method = "lm", alpha = .2) + 
        ggtitle("Effect of Iron on rs connectivity") +
        xlab("Iron") + 
        ylab("rs-connectvity with SN\nresiduals over QA variables") + 
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5)))
        
        

pdf("effect_of_iron_on_residuals_rs.pdf", w = 6, h = 4)
print(effect_of_iron_on_residuals_rs)
dev.off()

# check model without healthy control
value_iron_nuisance_by_rois_patients_only <- mat %>%
  select(-Condition) %>%
  gather(rois,value, -Subject, -QA_ValidScans, -QA_MeanMotion, -QA_MeanGlobal, -QA_GCOR_rest, -group, -iron) %>%
  filter(group == "group_1" | group == "group_2") %>%
  split(., .$rois) %>%
  map(~lm(data = ., value ~ iron + QA_MeanMotion + QA_MeanGlobal + QA_GCOR_rest)) %>%
  map(~summary(.))

rs_iron_group_ironXgroup_by_rois_patients_only.anova <- mat %>%
  select(-Condition) %>%
  gather(rois,value, -Subject, -QA_ValidScans, -QA_MeanMotion, -QA_MeanGlobal, -QA_GCOR_rest, -group, -iron) %>%
  filter(group == "group_1" | group == "group_2") %>%
  split(., .$rois) %>%
  map(~lm(data = ., value ~ iron + group + iron:group + QA_MeanMotion + QA_MeanGlobal + QA_GCOR_rest)) %>%
  map(~Anova(mod = ., t = 3))


#add levodopa and other clinical variable

load("clinical_data_sets.RData")

subjects_code_as_in_conn <- scan("MultiPAMS_subjects_code_as_in_conn.txt", "%s%")
subjects_code_as_in_conn <- subjects_code_as_in_conn [-24]
mat$code <- subjects_code_as_in_conn

correlation_ledd_iron <- left_join(mat, park_pams_final_plus_gateano_dataset, by = "code") %>%
  lm(data = ., ledd ~ iron) %>%
  summary(.)

correlation_disease_duration_iron <- left_join(mat, park_pams_final_plus_gateano_dataset, by = "code") %>%
  lm(data = ., iron ~ disease_duration) %>%
  summary(.)

correlation_scopa_iron <- left_join(mat, park_pams_final_plus_gateano_dataset, by = "code") %>%
  lm(data = ., scopa ~iron) %>%
  summary(.)

