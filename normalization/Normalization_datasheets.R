# script to add ages in the normalized sheets

# libraries
library("readxl")
library("tidyverse")
library("ggridges")
library("eeptools")
library("lubridate")

# Load the spreadsheets and add columns
Q1 <- read_excel("Q1_norm_background_04.10.18.xlsx") 
## add age in days and weeks
Q1_corr <- Q1 %>% mutate(inf_AgeDays=0) %>% 
  mutate(inf_AgeWeeks=floor(inf_AgeDays/7)) %>% mutate(inf_Age="birth")
## corrects dates
Q1_corr <- Q1_corr %>% mutate(q_ResponseDate = as.Date(q_ResponseDate, format = "%d.%m.%Y")) %>%
  mutate(inf_DOB = as.Date(inf_DOB, format = "%d.%m.%Y")) %>% 
  mutate(inf_DueDate = as.Date(inf_DueDate, format = "%d.%m.%Y"))
## correct the Postal code
Q1_corr <- Q1_corr %>% mutate(env_PostalCode=str_pad(env_PostalCode, 5, pad = "0")) %>%
  mutate(env_PrevResidencePostalCode=str_pad(env_PrevResidencePostalCode, 5, pad = "0"))
## check for duplicated submissions
Q1_dup_fam <- Q1_corr %>% select(familly_ID) %>% group_by(familly_ID) %>% 
  tally() %>% ungroup() %>% filter(n>1)
Q1_dup <- left_join(Q1_dup_fam, Q1_corr)
## export as csv
write_csv(Q1_corr, "Q1_norm_background_04.10.18.csv")


Q2 <- read_excel("Q2_norm_Previous3months_15.08.20.xlsx") 
## add age in days and weeks
Q2_corr <- Q2 %>% separate(inf_Age, into = c("nb","kpl"), sep=" ", remove = FALSE) %>%
  mutate(kpl=ifelse(kpl=="vko", 7,kpl)) %>%
  mutate(kpl=ifelse(kpl=="kk", 30.417, kpl)) %>% 
  mutate(inf_AgeDays=floor(as.numeric(nb)*as.numeric(kpl))) %>%
  mutate(inf_AgeWeeks=floor(inf_AgeDays/7)) %>% 
  relocate(inf_AgeDays, .after = inf_Age) %>% 
  relocate(inf_AgeWeeks, .after = inf_AgeDays) %>%
  select(-nb, -kpl)
## corrects dates
Q2_corr <- Q2_corr %>% mutate(q_ResponseDate = as.Date(q_ResponseDate, format = "%d.%m.%Y")) %>%
  mutate(inf_DOB = as.Date(inf_DOB, format = "%d.%m.%Y")) %>% 
  mutate(inf_DueDate = as.Date(inf_DueDate, format = "%d.%m.%Y"))
## correct the Postal code
Q2_corr <- Q2_corr %>% mutate(env_PostalCode3m=str_pad(env_PostalCode3m, 5, pad = "0"))
## removed for unfinished submissions
Q2_corr <- Q2_corr %>% filter(!is.na(inf_Age))
## check for duplicated submissions
Q2_dup_fam <- Q2_corr %>% select(familly_ID, inf_AgeWeeks) %>% group_by(familly_ID, inf_AgeWeeks) %>% 
  tally() %>% ungroup() %>% filter(n>1)
Q2_dup <- left_join(Q2_dup_fam, Q2_corr) %>% select(-n)
Q2_dup$na_count <- apply(Q2_dup, 1, function(x) sum(is.na(x))) # check the most filled entry
Q2_duprem <- Q2_dup %>% group_by(familly_ID) %>% 
  filter(q_ResponseDate == min(q_ResponseDate))
Q2_corr <- anti_join(Q2_corr, Q2_duprem)
## export as csv
write_csv(Q2_corr, "Q2_norm_Previous3months_15.08.20.csv")


Q3 <- read_excel("Q3_norm_4to6months_26.01.20.xlsx") 
## add age in days and weeks
Q3_corr <- Q3 %>% separate(inf_Age, into = c("nb","kpl"), sep=" ", remove = FALSE) %>%
  mutate(kpl=ifelse(kpl=="vko", 7,kpl)) %>%
  mutate(kpl=ifelse(kpl=="kk", 30.417, kpl)) %>% 
  mutate(inf_AgeDays=floor(as.numeric(nb)*as.numeric(kpl))) %>%
  mutate(inf_AgeWeeks=floor(inf_AgeDays/7)) %>% 
  relocate(inf_AgeDays, .after = inf_Age) %>% 
  relocate(inf_AgeWeeks, .after = inf_AgeDays) %>%
  select(-nb, -kpl)
## corrects dates
Q3_corr <- Q3_corr %>% mutate(q_ResponseDate = as.Date(q_ResponseDate, format = "%d.%m.%Y")) %>%
  mutate(inf_DOB = as.Date(inf_DOB, format = "%d.%m.%Y")) %>% 
  mutate(inf_DueDate = as.Date(inf_DueDate, format = "%d.%m.%Y"))
## removed for unfinished submissions
Q3_corr <- Q3_corr %>% filter(!is.na(inf_Age))
## check for duplicated submissions
Q3_dup_fam <-Q3_corr %>% select(familly_ID, inf_AgeWeeks) %>% group_by(familly_ID, inf_AgeWeeks) %>% 
  tally() %>% ungroup() %>% filter(n>1)
## export as csv
write_csv(Q3_corr, "Q3_norm_4to6months_26.01.20.csv")



Q4 <- read_excel("Q4_norm_7to12months_27.01.21.xlsx") 
## add age in days and weeks
Q4_corr <- Q4 %>% separate(inf_Age, into = c("nb","kpl"), sep=" ", remove = FALSE) %>%
  mutate(kpl=ifelse(kpl=="vko", 7,kpl)) %>%
  mutate(kpl=ifelse(kpl=="kk", 30.417, kpl)) %>% 
  mutate(inf_AgeDays=floor(as.numeric(nb)*as.numeric(kpl))) %>%
  mutate(inf_AgeWeeks=floor(inf_AgeDays/7)) %>% 
  relocate(inf_AgeDays, .after = inf_Age) %>% 
  relocate(inf_AgeWeeks, .after = inf_AgeDays) %>%
  select(-nb, -kpl)
## corrects dates
Q4_corr <- Q4_corr %>% mutate(q_ResponseDate = as.Date(q_ResponseDate, format = "%d.%m.%Y")) %>%
  mutate(inf_DOB = as.Date(inf_DOB, format = "%d.%m.%Y")) %>% 
  mutate(inf_DueDate = as.Date(inf_DueDate, format = "%d.%m.%Y"))
## removed for unfinished submissions
Q4_corr <- Q4_corr %>% filter(!is.na(inf_Age))
## check for duplicated submissions
Q4_dup_fam <-Q4_corr %>% select(familly_ID, inf_AgeWeeks) %>% group_by(familly_ID, inf_AgeWeeks) %>% 
  tally() %>% ungroup() %>% filter(n>1)
## export as csv
write_csv(Q4_corr, "Q4_norm_7to12months_27.01.21.csv")


Q5y1 <- read_excel("Q5_norm_NutritionMotorDev_v2_yearI_28.01.21.xlsx") 
## add age in days and weeks
Q5y1_corr <- Q5y1 %>% separate(inf_Age, into = c("nb","kpl"), sep=" ", remove = FALSE) %>%
  mutate(kpl=ifelse(kpl=="vko", 7,kpl)) %>%
  mutate(kpl=ifelse(kpl=="kk", 30.417, kpl)) %>% 
  mutate(inf_AgeDays=floor(as.numeric(nb)*as.numeric(kpl))) %>%
  mutate(inf_AgeWeeks=floor(inf_AgeDays/7)) %>% 
  relocate(inf_AgeDays, .after = inf_Age) %>% 
  relocate(inf_AgeWeeks, .after = inf_AgeDays) %>%
  select(-nb, -kpl)
## corrects dates
Q5y1_corr <- Q5y1_corr %>% mutate(q_ResponseDate = as.Date(q_ResponseDate, format = "%d.%m.%Y")) %>%
  mutate(inf_DOB = as.Date(inf_DOB, format = "%d.%m.%Y")) %>% 
  mutate(inf_DueDate = as.Date(inf_DueDate, format = "%d.%m.%Y"))
## removed for unfinished submissions
Q5y1_corr <- Q5y1_corr %>% filter(!is.na(inf_Age))
## check for duplicated submissions
Q5y1_dup_fam <-Q5y1_corr %>% select(familly_ID, inf_AgeWeeks) %>% group_by(familly_ID, inf_AgeWeeks) %>% 
  tally() %>% ungroup() %>% filter(n>1)
## export as csv
write_csv(Q5y1_corr, "Q5_norm_NutritionMotorDev_v2_yearI_28.01.21.csv")


Q5y2 <- read_excel("Q5_norm_NutritionMotorDev_v2_yearII_28.01.21.xlsx") 
## add age in days and weeks
Q5y2_corr <- Q5y2 %>% separate(inf_Age, into = c("nb","kpl"), sep=" ", remove = FALSE) %>%
  mutate(kpl=ifelse(kpl=="vko", 7,kpl)) %>%
  mutate(kpl=ifelse(kpl=="kk", 30.417, kpl)) %>% 
  mutate(inf_AgeDays=floor(as.numeric(nb)*as.numeric(kpl))) %>%
  mutate(inf_AgeWeeks=floor(inf_AgeDays/7)) %>% 
  relocate(inf_AgeDays, .after = inf_Age) %>% 
  relocate(inf_AgeWeeks, .after = inf_AgeDays) %>%
  select(-nb, -kpl)
## corrects dates
Q5y2_corr <- Q5y2_corr %>% mutate(q_ResponseDate = as.Date(q_ResponseDate, format = "%d.%m.%Y")) %>%
  mutate(inf_DOB = as.Date(inf_DOB, format = "%d.%m.%Y")) %>% 
  mutate(inf_DueDate = as.Date(inf_DueDate, format = "%d.%m.%Y"))
## removed for unfinished submissions
Q5y2_corr <- Q5y2_corr %>% filter(!is.na(inf_Age))
## check for duplicated submissions
Q5y2_dup_fam <-Q5y2_corr %>% select(familly_ID, inf_AgeWeeks) %>% group_by(familly_ID, inf_AgeWeeks) %>% 
  tally() %>% ungroup() %>% filter(n>1)
## export as csv
write_csv(Q5y2_corr, "Q5_norm_NutritionMotorDev_v2_yearII_28.01.21.csv")


Q5y1B <- read_excel("Q5_norm_NutritionMotorDev_yearI_27.01.21.xlsx")
## add age in days and weeks
Q5y1B_corr <- Q5y1B %>% separate(inf_Age, into = c("nb","kpl"), sep=" ", remove = FALSE) %>%
  mutate(kpl=ifelse(kpl=="vko", 7,kpl)) %>%
  mutate(kpl=ifelse(kpl=="kk", 30.417, kpl)) %>% 
  mutate(inf_AgeDays=floor(as.numeric(nb)*as.numeric(kpl))) %>%
  mutate(inf_AgeWeeks=floor(inf_AgeDays/7)) %>% 
  relocate(inf_AgeDays, .after = inf_Age) %>% 
  relocate(inf_AgeWeeks, .after = inf_AgeDays) %>%
  select(-nb, -kpl)
## corrects dates
Q5y1B_corr <- Q5y1B_corr %>% mutate(q_ResponseDate = as.Date(q_ResponseDate, format = "%d.%m.%Y")) %>%
  mutate(inf_DOB = as.Date(inf_DOB, format = "%d.%m.%Y")) %>% 
  mutate(inf_DueDate = as.Date(inf_DueDate, format = "%d.%m.%Y"))
## removed for unfinished submissions
Q5y1B_corr <- Q5y1B_corr %>% filter(!is.na(inf_Age))
## check for duplicated submissions
Q5y1B_dup_fam <-Q5y1B_corr %>% select(familly_ID, inf_AgeWeeks) %>% group_by(familly_ID, inf_AgeWeeks) %>% 
  tally() %>% ungroup() %>% filter(n>1)
## export as csv
write_csv(Q5y1B_corr, "Q5_norm_NutritionMotorDev_yearI_27.01.21.csv")


Q5y2B <- read_excel("Q5_norm_NutritionMotorDev_yearII_28.01.21.xlsx") 
## add age in days and weeks
Q5y2B_corr <- Q5y2B %>% separate(inf_Age, into = c("nb","kpl"), sep=" ", remove = FALSE) %>%
  mutate(kpl=ifelse(kpl=="vko", 7,kpl)) %>%
  mutate(kpl=ifelse(kpl=="kk", 30.417, kpl)) %>% 
  mutate(inf_AgeDays=floor(as.numeric(nb)*as.numeric(kpl))) %>%
  mutate(inf_AgeWeeks=floor(inf_AgeDays/7)) %>% 
  relocate(inf_AgeDays, .after = inf_Age) %>% 
  relocate(inf_AgeWeeks, .after = inf_AgeDays) %>%
  select(-nb, -kpl)
## corrects dates
Q5y2B_corr <- Q5y2B_corr %>% mutate(q_ResponseDate = as.Date(q_ResponseDate, format = "%d.%m.%Y")) %>%
  mutate(inf_DOB = as.Date(inf_DOB, format = "%d.%m.%Y")) %>% 
  mutate(inf_DueDate = as.Date(inf_DueDate, format = "%d.%m.%Y"))
## removed for unfinished submissions
Q5y2B_corr <- Q5y2B_corr %>% filter(!is.na(inf_Age))
## check for duplicated submissions
Q5y2B_dup_fam <-Q5y2B_corr %>% select(familly_ID, inf_AgeWeeks) %>% group_by(familly_ID, inf_AgeWeeks) %>% 
  tally() %>% ungroup() %>% filter(n>1)
## export as csv
write_csv(Q5y2B_corr, "Q5_norm_NutritionMotorDev_yearII_28.01.21.csv")


Q6F <- read_excel("Q6_norm_ParentsF_FoodFrequency_28.01.21.xlsx") 
## add age in days and weeks
Q6F_corr <- Q6F %>% separate(inf_Age, into = c("nb","kpl"), sep=" ", remove = FALSE) %>%
  mutate(kpl=ifelse(kpl=="vko", 7,kpl)) %>%
  mutate(kpl=ifelse(kpl=="kk", 30.417, kpl)) %>% 
  mutate(inf_AgeDays=floor(as.numeric(nb)*as.numeric(kpl))) %>%
  mutate(inf_AgeWeeks=floor(inf_AgeDays/7)) %>% 
  relocate(inf_AgeDays, .after = inf_Age) %>% 
  relocate(inf_AgeWeeks, .after = inf_AgeDays) %>%
  select(-nb, -kpl)
## corrects dates
Q6F_corr <- Q6F_corr %>% mutate(q_ResponseDate = as.Date(q_ResponseDate, format = "%d.%m.%Y")) %>%
  mutate(inf_DOB = as.Date(inf_DOB, format = "%d.%m.%Y")) %>% 
  mutate(inf_DueDate = as.Date(inf_DueDate, format = "%d.%m.%Y"))
## removed for unfinished submissions
Q6F_corr <- Q6F_corr %>% filter(!is.na(inf_Age))
## check for duplicated submissions
Q6F_dup_fam <-Q6F_corr %>% select(familly_ID, inf_AgeWeeks) %>% group_by(familly_ID, inf_AgeWeeks) %>% 
  tally() %>% ungroup() %>% filter(n>1)
## export as csv
write_csv(Q6F_corr, "Q6_norm_ParentsF_FoodFrequency_28.01.21.csv")


Q6M <- read_excel("Q6_norm_ParentsM_FoodFrequency_28.01.21.xlsx")
## add age in days and weeks
Q6M_corr <- Q6M %>% separate(inf_Age, into = c("nb","kpl"), sep=" ", remove = FALSE) %>%
  mutate(kpl=ifelse(kpl=="vko", 7,kpl)) %>%
  mutate(kpl=ifelse(kpl=="kk", 30.417, kpl)) %>% 
  mutate(inf_AgeDays=floor(as.numeric(nb)*as.numeric(kpl))) %>%
  mutate(inf_AgeWeeks=floor(inf_AgeDays/7)) %>% 
  relocate(inf_AgeDays, .after = inf_Age) %>% 
  relocate(inf_AgeWeeks, .after = inf_AgeDays) %>%
  select(-nb, -kpl)
## corrects dates
Q6M_corr <- Q6M_corr %>% mutate(q_ResponseDate = as.Date(q_ResponseDate, format = "%d.%m.%Y")) %>%
  mutate(inf_DOB = as.Date(inf_DOB, format = "%d.%m.%Y")) %>% 
  mutate(inf_DueDate = as.Date(inf_DueDate, format = "%d.%m.%Y"))
## removed for unfinished submissions
Q6M_corr <- Q6M_corr %>% filter(!is.na(inf_Age))
## check for duplicated submissions
Q6M_dup_fam <-Q6M_corr %>% select(familly_ID, inf_AgeWeeks) %>% group_by(familly_ID, inf_AgeWeeks) %>% 
  tally() %>% ungroup() %>% filter(n>1)
## export as csv
write_csv(Q6M_corr, "Q6_norm_ParentsM_FoodFrequency_28.01.21.csv")


Q7 <- read_excel("Q7_norm_depression_unkown.xlsx")
## add age in days and weeks
Q7_corr <- Q7 %>% mutate(inf_Age="24 kk") %>%
  separate(inf_Age, into = c("nb","kpl"), sep=" ", remove = FALSE) %>%
  mutate(kpl=ifelse(kpl=="vko", 7,kpl)) %>%
  mutate(kpl=ifelse(kpl=="kk", 30.417, kpl)) %>% 
  mutate(inf_AgeDays=floor(as.numeric(nb)*as.numeric(kpl))) %>%
  mutate(inf_AgeWeeks=floor(inf_AgeDays/7)) %>% 
  relocate(inf_AgeDays, .after = inf_Age) %>% 
  relocate(inf_AgeWeeks, .after = inf_AgeDays) %>%
  select(-nb, -kpl)
## corrects dates
Q7_corr <- Q7_corr %>% mutate(q_ResponseDate = as.Date(q_ResponseDate, format = "%d.%m.%Y")) %>%
  mutate(inf_DOB = as.Date(inf_DOB, format = "%d.%m.%Y"))%>% 
  mutate(inf_DueDate = as.Date(inf_DueDate, format = "%d.%m.%Y"))
## removed for unfinished submissions
Q7_corr <- Q7_corr %>% filter(!is.na(inf_Age))
## check for duplicated submissions
Q7_dup_fam <- Q7_corr %>% select(familly_ID, inf_AgeWeeks) %>% group_by(familly_ID, inf_AgeWeeks) %>% 
  tally() %>% ungroup() %>% filter(n>1)
## export as csv
write_csv(Q7_corr, "Q7_norm_depression_unkown.csv")


Q8 <- read_excel("Q8_norm_HouseDustCollection_28.01.21.xlsx") 
## add age in days and weeks
Q8_corr <- Q8 %>% separate(inf_Age, into = c("nb","kpl"), sep=" ", remove = FALSE) %>%
  mutate(kpl=ifelse(kpl=="vko", 7,kpl)) %>%
  mutate(kpl=ifelse(kpl=="kk", 30.417, kpl)) %>% 
  mutate(inf_AgeDays=floor(as.numeric(nb)*as.numeric(kpl))) %>%
  mutate(inf_AgeWeeks=floor(inf_AgeDays/7)) %>% 
  relocate(inf_AgeDays, .after = inf_Age) %>% 
  relocate(inf_AgeWeeks, .after = inf_AgeDays) %>%
  select(-nb, -kpl)
## corrects dates
Q8_corr <- Q8_corr %>% mutate(q_ResponseDate = as.Date(q_ResponseDate, format = "%d.%m.%Y")) %>%
  mutate(inf_DOB = as.Date(inf_DOB, format = "%d.%m.%Y")) %>% 
  mutate(inf_DueDate = as.Date(inf_DueDate, format = "%d.%m.%Y"))
## removed for unfinished submissions
Q8_corr <- Q8_corr %>% filter(!is.na(inf_Age))
## check for duplicated submissions
Q8_dup_fam <- Q8_corr %>% select(familly_ID, inf_AgeWeeks) %>% group_by(familly_ID, inf_AgeWeeks) %>% 
  tally() %>% ungroup() %>% filter(n>1)
## export as csv
write_csv(Q8_corr, "Q8_norm_HouseDustCollection_28.01.21.csv")


Q9 <- read_excel("Q9_norm_0to3 months_02.02.2020.xlsx") 
## add age in days and weeks
Q9_corr <- Q9 %>% separate(inf_Age, into = c("nb","kpl"), sep=" ", remove = FALSE) %>%
  mutate(kpl=ifelse(kpl=="vko", 7,kpl)) %>%
  mutate(kpl=ifelse(kpl=="kk", 30.417, kpl)) %>% 
  mutate(inf_AgeDays=floor(as.numeric(nb)*as.numeric(kpl))) %>%
  mutate(inf_AgeWeeks=floor(inf_AgeDays/7)) %>% 
  relocate(inf_AgeDays, .after = inf_Age) %>% 
  relocate(inf_AgeWeeks, .after = inf_AgeDays) %>%
  select(-nb, -kpl)
## corrects dates
Q9_corr <- Q9_corr %>% mutate(q_ResponseDate = as.Date(q_ResponseDate, format = "%d.%m.%Y")) %>%
  mutate(inf_DOB = as.Date(inf_DOB, format = "%d.%m.%Y")) %>% 
  mutate(inf_DueDate = as.Date(inf_DueDate, format = "%d.%m.%Y"))
## removed for unfinished submissions
Q9_corr <- Q9_corr %>% filter(!is.na(inf_Age))
## check for duplicated submissions
Q9_dup_fam <- Q9_corr %>% select(familly_ID, inf_AgeWeeks) %>% group_by(familly_ID, inf_AgeWeeks) %>% 
  tally() %>% ungroup() %>% filter(n>1)
Q9_dup <- left_join(Q9_dup_fam, Q9_corr) %>% select(-n)
Q9_dup$na_count <- apply(Q9_dup, 1, function(x) sum(is.na(x))) # check the most filled entry
Q9_duprem <- Q9_dup %>% group_by(familly_ID) %>% 
  filter(q_ResponseDate == min(q_ResponseDate))
Q9_corr <- anti_join(Q9_corr, Q9_duprem)
## export as csv
write_csv(Q9_corr, "Q9_norm_0to3 months_02.02.2020.csv")

#### TO ADD ONCE I GET ALL DATA
Q10 <- read_excel("Q10_norm_MotherStress_02.09.20.xlsx")
## add age in days and weeks
Q10_corr <- Q10 %>% separate(inf_Age, into = c("nb","kpl"), sep=" ", remove = FALSE) %>%
  mutate(kpl=ifelse(kpl=="vko", 7,kpl)) %>%
  mutate(kpl=ifelse(kpl=="kk", 30.417, kpl)) %>% 
  mutate(inf_AgeDays=floor(as.numeric(nb)*as.numeric(kpl))) %>%
  mutate(inf_AgeWeeks=floor(inf_AgeDays/7)) %>% 
  relocate(inf_AgeDays, .after = inf_Age) %>% 
  relocate(inf_AgeWeeks, .after = inf_AgeDays) %>%
  select(-nb, -kpl)
## corrects dates
Q10_corr <- Q10_corr %>% mutate(q_ResponseDate = as.Date(q_ResponseDate, format = "%d.%m.%Y")) %>%
  mutate(inf_DOB = as.Date(inf_DOB, format = "%d.%m.%Y")) %>% 
  mutate(inf_DueDate = as.Date(inf_DueDate, format = "%d.%m.%Y"))
## removed for unfinished submissions
Q10_corr <- Q10_corr %>% filter(!is.na(inf_Age))
## check for duplicated submissions
Q10_dup_fam <- Q10_corr %>% select(familly_ID, inf_AgeWeeks) %>% group_by(familly_ID, inf_AgeWeeks) %>% 
  tally() %>% ungroup() %>% filter(n>1)
## export as csv
write_csv(Q10_corr, "Q10_norm_MotherStress_02.09.20.csv")

