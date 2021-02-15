# script to create the normalized sheets

# libraries
library("readxl")
library("tidyverse")
library("ggridges")
library("eeptools")
library("lubridate")

#######################################################################################################
# Load the spreadsheets and add columns
Q1 <- read_excel("../normalized_versions/translated/Q1_norm_background_04.10.18.xlsx") 
## add age in days and weeks
Q1_corr <- Q1 %>% mutate(inf_AgeDays=0) %>% 
  mutate(inf_AgeWeeks=floor(inf_AgeDays/7)) %>% 
  mutate(inf_Age="birth") %>%
  relocate(inf_AgeDays, .after = inf_Age) %>% 
  relocate(inf_AgeWeeks, .after = inf_AgeDays)
## corrects dates
Q1_corr <- Q1_corr %>% mutate(q_ResponseDate = as.Date(q_ResponseDate, format = "%d.%m.%Y")) %>%
  mutate(inf_DOB = as.Date(inf_DOB, format = "%d.%m.%Y")) %>% 
  mutate(inf_DueDate = as.Date(inf_DueDate, format = "%d.%m.%Y"))
## Correct Pregnancy duration
Q1_corr <- Q1_corr %>% separate('m_PregnancyDurationW+D', into = c("weeksPreg", "DaysPreg"), sep="\\+", 
                                remove=FALSE) %>% 
  mutate(DaysPreg = replace_na(DaysPreg, 0)) %>%
  mutate(m_PregnancyDurationD=(as.numeric(weeksPreg)*7)+as.numeric(DaysPreg)) %>% 
  select(-weeksPreg, -DaysPreg) %>%
  relocate(m_PregnancyDurationD, .after = 'm_PregnancyDurationW+D')
## correct the Postal code
Q1_corr <- Q1_corr %>% mutate(env_PostalCode=str_pad(env_PostalCode, 5, pad = "0")) %>%
  mutate(env_PrevResidencePostalCode=str_pad(env_PrevResidencePostalCode, 5, pad = "0"))
## check for duplicated submissions
Q1_dup_fam <- Q1_corr %>% select(familly_ID) %>% group_by(familly_ID) %>% 
  tally() %>% ungroup() %>% filter(n>1)
Q1_dup <- left_join(Q1_dup_fam, Q1_corr)
## export as csv
write_csv(Q1_corr, "../normalized_versions/corrected_translated/Q1_norm_background_04.10.18.csv")

#######################################################################################################
Q2_hints <- c("text","text","text","text","text","text","numeric","numeric","text","text","logical",
              "text","numeric","logical","text","numeric","numeric","numeric","numeric","numeric",
              "text","numeric","logical","text","logical","text","numeric","numeric","numeric","numeric","numeric",
              "text","numeric","numeric","text","logical","numeric","logical","numeric","logical","logical","text",
              "text","text","text","text","text","logical","text","text","text","numeric","numeric","text","text",
              "text","text","text","text","text","text","numeric","numeric","numeric","logical","text","text","text",
              "logical","text","numeric","numeric","numeric","numeric","text","text","text","text","text","text","text",
              "logical","text","text","text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric",
              "logical","text","text","text","text","text","text","text","logical","numeric","logical","numeric","numeric",
              "numeric","logical","numeric","logical","logical","logical","numeric","numeric","numeric","numeric","numeric",
              "numeric","numeric","text","text","numeric","numeric","text","text","text")
Q2 <- read_excel("../normalized_versions/translated/Q2_norm_Previous3months_15.08.20.xlsx", col_types = Q2_hints) 

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
write_csv(Q2_corr, "../normalized_versions/corrected_translated/Q2_norm_Previous3months_15.08.20.csv")

#######################################################################################################
Q3 <- read_excel("../normalized_versions/translated/Q3_norm_4to6months_26.01.20.xlsx") 
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
write_csv(Q3_corr, "../normalized_versions/corrected_translated/Q3_norm_4to6months_26.01.20.csv")


#######################################################################################################
Q4 <- read_excel("../normalized_versions/translated/Q4_norm_7to12months_27.01.21.xlsx") 
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
write_csv(Q4_corr, "../normalized_versions/corrected_translated/Q4_norm_7to12months_27.01.21.csv")

#######################################################################################################
Q5y1 <- read_excel("../normalized_versions/translated/Q5_norm_NutritionMotorDev_v2_yearI_28.01.21.xlsx") 
## add age in days and weeks
Q5y1_corr <- Q5y1 %>% separate(inf_Age, into = c("nb","kpl"), sep=" ", remove = FALSE) %>%
  mutate(kpl=ifelse(kpl=="vko", 7,kpl)) %>%
  mutate(kpl=ifelse(kpl=="kk", 30.417, kpl)) %>% 
  mutate(inf_AgeDays=floor(as.numeric(nb)*as.numeric(kpl))) %>%
  mutate(inf_AgeWeeks=floor(inf_AgeDays/7)) %>% 
  relocate(inf_AgeDays, .after = inf_Age) %>% 
  relocate(inf_AgeWeeks, .after = inf_AgeDays) %>%
  select(-nb, -kpl)
## correct period in start and stop
Q5y1_corr <- Q5y1_corr %>% separate(inf_PeriodFormula, into = c("inf_PeriodFormulaStart","inf_PeriodFormulaEnd"), sep="-", remove = FALSE)%>% 
  relocate(inf_PeriodFormulaStart, .after = inf_PeriodFormula) %>% 
  relocate(inf_PeriodFormulaEnd, .after = inf_PeriodFormulaStart)
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
write_csv(Q5y1_corr, "../normalized_versions/corrected_translated/Q5_norm_NutritionMotorDev_v2_yearI_28.01.21.csv")

#######################################################################################################
Q5y2 <- read_excel("../normalized_versions/translated/Q5_norm_NutritionMotorDev_v2_yearII_28.01.21.xlsx") 
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
write_csv(Q5y2_corr, "../normalized_versions/corrected_translated/Q5_norm_NutritionMotorDev_v2_yearII_28.01.21.csv")

#######################################################################################################
Q5y1B <- read_excel("../normalized_versions/translated/Q5_norm_NutritionMotorDev_yearI_27.01.21.xlsx")
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
## correct period in start and stop
Q5y1B_corr <- Q5y1B_corr %>% separate(inf_PeriodFormula, into = c("inf_PeriodFormulaStart","inf_PeriodFormulaEnd"), sep="-", remove = FALSE)%>% 
  relocate(inf_PeriodFormulaStart, .after = inf_PeriodFormula) %>% 
  relocate(inf_PeriodFormulaEnd, .after = inf_PeriodFormulaStart)
## removed for unfinished submissions
Q5y1B_corr <- Q5y1B_corr %>% filter(!is.na(inf_Age))
## check for duplicated submissions
Q5y1B_dup_fam <-Q5y1B_corr %>% select(familly_ID, inf_AgeWeeks) %>% group_by(familly_ID, inf_AgeWeeks) %>% 
  tally() %>% ungroup() %>% filter(n>1)
## export as csv
write_csv(Q5y1B_corr, "../normalized_versions/corrected_translated/Q5_norm_NutritionMotorDev_yearI_27.01.21.csv")

#######################################################################################################
Q5y2B <- read_excel("../normalized_versions/translated/Q5_norm_NutritionMotorDev_yearII_28.01.21.xlsx") 
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
write_csv(Q5y2B_corr, "../normalized_versions/corrected_translated/Q5_norm_NutritionMotorDev_yearII_28.01.21.csv")

#######################################################################################################
Q6F <- read_excel("../normalized_versions/translated/Q6_norm_ParentsF_FoodFrequency_28.01.21.xlsx") 
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
write_csv(Q6F_corr, "../normalized_versions/corrected_translated/Q6_norm_ParentsF_FoodFrequency_28.01.21.csv")

#######################################################################################################
Q6M <- read_excel("../normalized_versions/translated/Q6_norm_ParentsM_FoodFrequency_28.01.21.xlsx")
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
write_csv(Q6M_corr, "../normalized_versions/corrected_translated/Q6_norm_ParentsM_FoodFrequency_28.01.21.csv")

#######################################################################################################
Q7 <- read_excel("../normalized_versions/translated/Q7_norm_depression_unkown.xlsx")
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
write_csv(Q7_corr, "../normalized_versions/corrected_translated/Q7_norm_depression_unkown.csv")

#######################################################################################################
Q8 <- read_excel("../normalized_versions/translated/Q8_norm_HouseDustCollection_28.01.21.xlsx") 
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
write_csv(Q8_corr, "../normalized_versions/corrected_translated/Q8_norm_HouseDustCollection_28.01.21.csv")

#######################################################################################################
Q9 <- read_excel("../normalized_versions/translated/Q9_norm_0to3 months_02.02.2020.xlsx") 
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
write_csv(Q9_corr, "../normalized_versions/corrected_translated/Q9_norm_0to3 months_02.02.2020.csv")

#######################################################################################################
Q10y0 <- read_excel("../normalized_versions/translated/Q10_norm_MotherStress_Year0_02-09-21.xlsx")
## corrects dates
Q10y0_corr <- Q10y0 %>% mutate(q_ResponseDate = as.Date(q_ResponseDate, format = "%d.%m.%Y")) %>%
  mutate(inf_DOB = as.Date(inf_DOB, format = "%d.%m.%Y")) %>% 
  mutate(inf_DueDate = as.Date(inf_DueDate, format = "%d.%m.%Y"))
## check for duplicated submissions
Q10y0_dup_fam <- Q10y0_corr %>% select(familly_ID, inf_AgeWeeks) %>% group_by(familly_ID, inf_AgeWeeks) %>% 
  tally() %>% ungroup() %>% filter(n>1)
## export as csv
write_csv(Q10y0_corr, "../normalized_versions/corrected_translated/Q10_norm_MotherStress_Year0_02-09-21.csv")

#######################################################################################################
Q10y2 <- read_excel("../normalized_versions/translated/Q10_norm_MotherStress_Year2_02-09-20.xlsx")
## corrects dates
Q10y2_corr <- Q10y2 %>% mutate(q_ResponseDate = as.Date(q_ResponseDate, format = "%d.%m.%Y")) %>%
  mutate(inf_DOB = as.Date(inf_DOB, format = "%d.%m.%Y")) %>% 
  mutate(inf_DueDate = as.Date(inf_DueDate, format = "%d.%m.%Y"))
## add age in days and weeks
Q10y2_corr <- Q10y2_corr %>% mutate(inf_AgeDays= difftime(q_ResponseDate, inf_DOB)) %>%
  mutate(inf_AgeWeeks=floor(inf_AgeDays/7)) %>% 
  relocate(inf_AgeDays, .after = inf_DueDate) %>% 
  relocate(inf_AgeWeeks, .after = inf_AgeDays)
## check for duplicated submissions
Q10y2_dup_fam <- Q10y2_corr %>% select(familly_ID, inf_AgeWeeks) %>% group_by(familly_ID, inf_AgeWeeks) %>% 
  tally() %>% ungroup() %>% filter(n>1)
## export as csv
write_csv(Q10y2_corr, "../normalized_versions/corrected_translated/Q10_norm_MotherStress_Year2_02-09-20.csv")

