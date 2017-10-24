options(java.parameters = "- Xmx1024m")
library(tidyverse)
library(XLConnect)
library(stringr)

xlsx_file <- "PAMS_DATA_pourPP.xlsx"
wb <- loadWorkbook(xlsx_file, create = FALSE, password = NULL)
park_demographic_info <- as_data_frame(readWorksheet(wb, sheet = 1, startRow = 1, startCol = 1, endRow = 32, endCol = 9))
park_first_visit <- as_data_frame(readWorksheet(wb, sheet = 1, startRow = 2, startCol = 10, endRow = 32, endCol = 18))
park_second_visit <- as_data_frame(readWorksheet(wb, sheet = 1, startRow = 2, startCol = 20, endRow = 32, endCol = 26))

park_demographic_info <- park_demographic_info %>%
  select(code = N..inclusion, sex = sexe, hand = Droitier.Gaucher, date_of_birth = ddn, 
         stad_park = Stade.parkinson.en.ON, desease_start = date.début.maladie,
         worse_side = côté.le.plus.atteint.de.la.maladie) %>%
  slice(-1) %>%
  mutate(code = gsub("-","_",code))

park_first_visit <- park_first_visit %>%
  mutate(code = park_demographic_info$code) %>%
  select(code, date_mri = Date.de.l.IRM, age = Age.au.moment.de.l.IRM, mmse = MMSE...30.,
         updrs = Score.UPDRS, beck = Score.Beck...63., moca = Score.Moca...30., scopa = Score.scopa...69.)  %>%
mutate(moca = as.numeric(moca), code = gsub("-","_",code))


park_second_visit <- park_second_visit %>%
  mutate(code = park_demographic_info$code, age = NA, mmse = NA) %>%
  select(code, date_mri = Date.de.l.IRM, age, mmse,
         updrs = Score.UPDRS, beck = Score.Beck...63., moca = Score.Moca...30., scopa = Score.Scopa...69.)

park_final <- bind_rows(left_join(park_demographic_info, park_first_visit, by = "code"), 
                        left_join(park_demographic_info, park_second_visit, by = "code"), .id = "visit")


pams_demographic_info <- as_data_frame(readWorksheet(wb, sheet = 2, startRow = 1, startCol = 1, endRow = 32, endCol = 10))
pams_first_visit <- as_data_frame(readWorksheet(wb, sheet = 2, startRow = 2, startCol = 11, endRow = 32, endCol = 19))
pams_second_visit <- as_data_frame(readWorksheet(wb, sheet = 2, startRow = 2, startCol = 21, endRow = 33, endCol = 26))
pams_second_visit[30,] <- NA

pams_demographic_info <- pams_demographic_info %>%
  select(code = N..inclusion, ams = Type.d.AMS, sex = sexe, hand = Droitier.Gaucher, date_of_birth = ddn,
         msa_diagnostic_level = diagnostic.MSA, deseaese_start = date.début.maladie, worse_side = côté.le.plus.atteint) %>%
  slice(-1)%>%
  mutate(code = gsub("-","_",code))


pams_first_visit <- pams_first_visit %>%
  mutate(code = pams_demographic_info$code) %>%
  select(code, date_mri = Date.IRM, age = Age.au.moment.de.l.IRM, mmse = MMSE...30., umsars = Score.UMSARS.2...56.,
         beck = Score.inventaire.dépressionde.Beck...63., moca = Score.de.Moca...30., scopa = Score.de.scopa...69.) %>%
  mutate(scopa = as.numeric(scopa))

pams_second_visit <- pams_second_visit %>%
  mutate(code = pams_demographic_info$code, age = NA, mmse = NA) %>%
  select(code, date_mri = Date.IRM, age, mmse, umsars = Score.UMSARS.2, beck = Score.Beck, moca = Score.Moca,
         scopa = Score.SCOPA)

pams_final <- bind_rows(left_join(pams_demographic_info, pams_first_visit, by = "code"), 
                        left_join(pams_demographic_info, pams_second_visit, by = "code"), .id = "visit")

shared_variables <- intersect(colnames(park_final), colnames(pams_final))

park_pams_final <- select(park_final, shared_variables) %>%
  bind_rows(select(pams_final, shared_variables), .id = "group") %>%
  mutate(group = ifelse(group == 1, "park", "pams"))

hc_xlsx_file <- "MultiPAMS_BdD_Temoins.xlsx"
hc_wb <- loadWorkbook(hc_xlsx_file, create = FALSE, password = NULL)
hc_code <- as_data_frame(readWorksheet(hc_wb, sheet = 1, startRow = 2, startCol = 2, endCol = 2, endRow = 32))
hc_sex_dob <- as_data_frame(readWorksheet(hc_wb, sheet = 1, startRow = 1, startCol = 3, endCol = 4, endRow = 32)) %>%
  slice(-1)

hc_visit <- as_data_frame(readWorksheet(hc_wb, sheet = 1, startRow = 2, startCol = 7, endRow = 32, endCol = 13))

hc_final <- bind_cols(hc_code, hc_sex_dob) %>%
  bind_cols(hc_visit) %>%
  mutate(visit = as.character(1)) %>%
  select(visit, code = N..inclusion, sex = sexe, date_of_birth = date.de..naissance, date_mri = Date.de.l.IRM,
         age = Age.au.moment.de.l.IRM, mmse = MMSE...30., beck = Dépression.de..Beck...63., scopa = SCOPA) %>%
  mutate(beck = as.numeric(beck))

shared_variables <- intersect(colnames(park_pams_final), colnames(hc_final))
shared_variables <- shared_variables[which(shared_variables != "group")]


park_pams_hc_final <- bind_rows(select(park_final, shared_variables), 
                                select(pams_final, shared_variables), 
                                select(hc_final, shared_variables), .id = "group") %>%
  mutate(group = ifelse(group == 1, "park", ifelse(group == 2, "pams", "hc")))

final_dataset_xlsx_file <-'database definitivo_21_04_2015.xlsx'
final_dataset_wb <- loadWorkbook(final_dataset_xlsx_file, create = FALSE, password = NULL)
final_dataset <- as_data_frame(readWorksheet(final_dataset_wb, sheet = 1)) %>%
  select(Col1, subgroup_tot, H_Y, age_at_onset, disease_duration = dis_dur, ledd = LEDD, 
         levodopa_daily_dose, umsars_P = UMSARS_P, umsars_C = UMSARS_C, SN_r2s) %>%
  slice(1:55)

final_code <- final_dataset$Col1 %>% 
  str_split(pattern = "_") %>%
  map(`[`,2:3) %>% 
  map_chr(paste, collapse = "_")

park_pams_final_plus_gateano_dataset <- bind_cols(data_frame(code = final_code), final_dataset) %>% left_join(park_pams_final, by = "code")

save(file = "clinical_data_sets.RData", park_pams_final, park_pams_hc_final, park_pams_final_plus_gateano_dataset)
