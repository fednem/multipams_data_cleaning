library(tidyverse)
library(car)
library(lsmeans)

#load relevant data
load('iron_data.RData')
load("rs_fmri_extraction_data.RData")
load("clinical_data_sets.RData")
#load relevant data
iron_data <- as.numeric(iron_data)
second_level_covariates <- read_delim("D:/MultiPAMS/rs-fmri/selected_2nd_covariates.txt", delim = " ")
second_level_covariates <- second_level_covariates %>%
  mutate(group = if_else(group_1 == 0, if_else(group_2 == 0, "group_3", "group_2"), "group_1")) %>%
  select(-5, -6, -7)
mat <- bind_cols(mat, second_level_covariates) %>%
  filter(Subject != 24)
group = mat$group
iron_data <- iron_data [-24]
mat$iron <- as.numeric(iron_data)

subjects_code_as_in_conn <- scan("MultiPAMS_subjects_code_as_in_conn.txt", "%s%")
subjects_code_as_in_conn <- subjects_code_as_in_conn [-24]
mat$code <- subjects_code_as_in_conn

#correlate iron from your method to Gaetano's

correlation_iron_mine_iron_gaetano <- left_join(mat, park_pams_final_plus_gateano_dataset, by = "code") %>%
  filter(visit == 1) %>%
  with(., cor.test(iron, SN_r2s))

left_join(mat, park_pams_final_plus_gateano_dataset, by = "code") %>%
  filter(visit == 1) %>%
  with(., plot(iron, SN_r2s))


group_effect_on_iron_gaetano <- left_join(mat, park_pams_final_plus_gateano_dataset, by = "code") %>%
  filter(visit == 1) %>%
  oneway.test(SN_r2s ~ group.x, data = .)


group_effect_on_iron_mine <- oneway.test(iron ~ group, data = mat)

bar_plot_iron_mine_by_group <- mat %>%
  select(iron, group) %>%
  group_by(group) %>%
  summarise(mean = mean(iron), lower_ci = t.test(iron)$conf.int[1], upper_ci = t.test(iron)$conf.int[2]) %>%
  ggplot(aes(x = group, y = mean, fill = group)) + 
  geom_bar(stat = 'identity') + 
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), color = "black", width = .2)

print(bar_plot_iron_mine_by_group)

bar_plot_iron_gaetano_by_group <- park_pams_final_plus_gateano_dataset %>%
  select(SN_r2s, group) %>%
  group_by(group) %>%
  summarise(mean = mean(SN_r2s), lower_ci = t.test(SN_r2s)$conf.int[1], upper_ci = t.test(SN_r2s)$conf.int[2]) %>%
  ggplot(aes(x = group, y = mean, fill = group)) + 
  geom_bar(stat = 'identity') + 
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), color = "black", width = .2)

print(bar_plot_iron_gaetano_by_group)


#correlate iron from your method to Germaine's

iron_germaine <- read_delim("germaine_value.txt", delim = "\t")

correlation_iron_mine_iron_germaine <- right_join(mat, iron_germaine, by = "code") %>%
    with(., cor.test(iron, value))

right_join(mat, iron_germaine, by = "code") %>%
  with(., plot(iron, value))

right_join(mat, iron_germaine, by = "code") %>%
  ggplot(data = ., aes(x = iron, y = value, color = group, group = group)) + 
  geom_point() + 
  stat_smooth(method = "lm")


correlation_iron_gaetano_iron_germaine <- inner_join(filter(park_pams_final_plus_gateano_dataset, 
                                                        visit == 1), iron_germaine, by = "code") %>%
  with(., cor.test(value, SN_r2s))

inner_join(filter(park_pams_final_plus_gateano_dataset, 
                  visit == 1), iron_germaine, by = "code") %>%
  with(., plot(value, SN_r2s))


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


