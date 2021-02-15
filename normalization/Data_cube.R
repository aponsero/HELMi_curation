# script to create the tables of the new datamodel

# libraries
library("readxl")
library("tidyverse")
library("ggridges")
library("eeptools")
library("lubridate")


# Load the spreadsheets
# --------------
Voc <- read_excel("../questionnaire_vocabulary.xlsx")
searchable <- read_excel("searchable_fields.xlsx")

Q1_max<-1055
Q1 <- read_csv("../normalized_versions/corrected_translated/Q1_norm_background_04.10.18.csv", guess_max=Q1_max) %>% 
  add_column(questionnaire="A_background")

Q2_max<-7242
Q2 <- read_csv("../normalized_versions/corrected_translated/Q2_norm_Previous3months_15.08.20.csv", guess_max=Q2_max) %>% 
  add_column(questionnaire="E_Previous3Months")

Q3_max<-6653
Q3 <- read_csv("../normalized_versions/corrected_translated/Q3_norm_4to6months_26.01.20.csv", guess_max=Q3_max) %>% 
  add_column(questionnaire="C_4to6months")

Q4_max<-5600
Q4 <- read_csv("../normalized_versions/corrected_translated/Q4_norm_7to12months_27.01.21.csv", guess_max=Q4_max) %>% 
  add_column(questionnaire="D_7to12months")

Q5y1_max<-431
Q5y2_max<-901
Q5y1B_max<-532
Q5y2B_max<-6
Q5y1 <- read_csv("../normalized_versions/corrected_translated/Q5_norm_NutritionMotorDev_v2_yearI_28.01.21.csv", guess_max=Q5y1_max) %>% 
  add_column(questionnaire="F1_NutritionMotorDev_year1_Version2")
Q5y2 <- read_csv("../normalized_versions/corrected_translated/Q5_norm_NutritionMotorDev_v2_yearII_28.01.21.csv", guess_max=Q5y2_max) %>% 
  add_column(questionnaire="F2_NutritionMotorDev_year2_Version2")
Q5y1B <- read_csv("../normalized_versions/corrected_translated/Q5_norm_NutritionMotorDev_yearI_27.01.21.csv", guess_max=Q5y1B_max) %>%
  add_column(questionnaire="F1_NutritionMotorDev_year1_Version1")
Q5y2B <- read_csv("../normalized_versions/corrected_translated/Q5_norm_NutritionMotorDev_yearII_28.01.21.csv", guess_max=Q5y2B_max) %>% 
  add_column(questionnaire="F2_NutritionMotorDev_year2_Version1")

Q6F_max<-1018
Q6M_max<-1357
Q6F <- read_csv("../normalized_versions/corrected_translated/Q6_norm_ParentsF_FoodFrequency_28.01.21.csv", guess_max=Q6F_max) %>% 
  add_column(questionnaire="G_ParentF_FoodFrequency")
Q6M <- read_csv("../normalized_versions/corrected_translated/Q6_norm_ParentsM_FoodFrequency_28.01.21.csv", guess_max=Q6M_max) %>% 
  add_column(questionnaire="G_ParentM_FoodFrequency")

Q7_max<-683
Q7 <- read_csv("../normalized_versions/corrected_translated/Q7_norm_depression_unkown.csv", guess_max=Q7_max) %>% 
  add_column(questionnaire="G_MotherDepression")

Q8_max<-268
Q8 <- read_csv("../normalized_versions/corrected_translated/Q8_norm_HouseDustCollection_28.01.21.csv", guess_max=Q8_max) %>% 
  add_column(questionnaire="H_HouseCollection")

Q9_max<-16953
Q9 <- read_csv("../normalized_versions/corrected_translated/Q9_norm_0to3 months_02.02.2020.csv", guess_max=Q9_max) %>% 
  add_column(questionnaire="B_0to3months")

Q10y0_max<-1076
Q10y2_max<-689
Q10y0 <- read_csv("../normalized_versions/corrected_translated/Q10_norm_MotherStress_Year0_02.09.21.csv", guess_max=Q10y0_max) %>% 
  add_column(questionnaire="G0_MotherStress")
Q10y2 <- read_csv("../normalized_versions/corrected_translated/Q10_norm_MotherStress_Year2_02.09.21.csv", guess_max=Q10y2_max) %>% 
  add_column(questionnaire="G2_MotherStress")
# --------------

# Generate the questionnaire table
# --------------

Q1_fam <- Q1 %>% group_by(questionnaire, familly_ID) %>% tally() %>% ungroup()
Q2_fam <- Q2 %>% group_by(questionnaire, familly_ID) %>% tally() %>% ungroup()
Q3_fam <- Q3 %>% group_by(questionnaire, familly_ID) %>% tally() %>% ungroup()
Q4_fam <- Q4 %>% group_by(questionnaire, familly_ID) %>% tally() %>% ungroup()
Q5y1_fam <- Q5y1 %>% group_by(questionnaire, familly_ID) %>% tally() %>% ungroup()
Q5y2_fam <- Q5y2 %>% group_by(questionnaire, familly_ID) %>% tally() %>% ungroup()
Q5y1B_fam <- Q5y1B %>% group_by(questionnaire, familly_ID) %>% tally() %>% ungroup()
Q5y2B_fam <- Q5y2B %>% group_by(questionnaire, familly_ID) %>% tally() %>% ungroup()
Q6F_fam <- Q6F %>% group_by(questionnaire, familly_ID) %>% tally() %>% ungroup()
Q6M_fam <- Q6M %>% group_by(questionnaire, familly_ID) %>% tally() %>% ungroup()
Q7_fam <- Q7 %>% group_by(questionnaire, familly_ID) %>% tally() %>% ungroup()
Q8_fam <- Q8 %>% group_by(questionnaire, familly_ID) %>% tally() %>% ungroup()
Q9_fam <- Q9 %>% group_by(questionnaire, familly_ID) %>% tally() %>% ungroup()
Q10y0_fam <- Q10y0 %>% group_by(questionnaire, familly_ID) %>% tally() %>% ungroup()
Q10y2_fam <- Q10y2 %>% group_by(questionnaire, familly_ID) %>% tally() %>% ungroup()

all_fam <- Q1_fam %>% add_row(Q2_fam) %>% 
  add_row(Q3_fam) %>% 
  add_row(Q4_fam) %>% 
  add_row(Q5y1_fam) %>% 
  add_row(Q5y2_fam) %>% 
  add_row(Q5y1B_fam) %>% 
  add_row(Q5y2B_fam) %>% 
  add_row(Q6F_fam) %>% 
  add_row(Q6M_fam) %>% 
  add_row(Q7_fam) %>% 
  add_row(Q8_fam) %>% 
  add_row(Q9_fam) %>%
  add_row(Q10y0_fam) %>% 
  add_row(Q10y2_fam) %>% 
  rename("nb_filled"="n")


all_fam_wide <- all_fam %>% pivot_wider(names_from=questionnaire, values_from=nb_filled) %>% replace(is.na(.), 0) %>%
  mutate(F1_NutritionMotorDev_year1=F1_NutritionMotorDev_year1_Version2+F1_NutritionMotorDev_year1_Version1)%>%
  mutate(F2_NutritionMotorDev_year2=F2_NutritionMotorDev_year2_Version2+F2_NutritionMotorDev_year2_Version1) %>%
  select(-F1_NutritionMotorDev_year1_Version2, -F1_NutritionMotorDev_year1_Version1, - F2_NutritionMotorDev_year2_Version2, - F2_NutritionMotorDev_year2_Version1)


# fixing FamillyID
all_fam_wide <- all_fam_wide %>% mutate(familly_ID=str_remove_all(familly_ID, "Perhe"))


write.csv(all_fam_wide, "../normalized_versions/DataCube/questionnaire.csv", row.names=FALSE)

# --------------

# Generate the baby main table
# --------------

baby_mainFields <- searchable %>% filter(entity=="Baby" & Included_interface==1) %>% pull(field_shortname)
baby_mainTable <-  Q1 %>% select(familly_ID, all_of(baby_mainFields))

# fixing the display of categories using the english translation of the fields
baby_mainCat <- searchable %>% filter(entity=="Baby" & Included_interface==1 & field_type=="categorical") %>%
  select(field_shortname, data_format, category_1:category_13) %>%
  separate(data_format, into=c("name1","name2","name3","name4","name5","name6","name7","name8","name9","name10",
                               "name11","name12","name13"), sep=",") %>%
  unite(new1, name1, category_1, sep = ",") %>%
  unite(new2, name2, category_2, sep = ",") %>%
  unite(new3, name3, category_3, sep = ",") %>%
  unite(new4, name4, category_4, sep = ",") %>%
  unite(new5, name5, category_5, sep = ",") %>%
  unite(new6, name6, category_6, sep = ",") %>%
  unite(new7, name7, category_7, sep = ",") %>%
  unite(new8, name8, category_8, sep = ",") %>%
  unite(new9, name9, category_9, sep = ",") %>%
  unite(new10, name10, category_10, sep = ",") %>%
  unite(new11, name11, category_11, sep = ",") %>%
  unite(new12, name12, category_12, sep = ",") %>%
  unite(new13, name13, category_13, sep = ",") %>%
  pivot_longer(cols = starts_with("new"), names_to="stuff",
               values_to="cat") %>%
  separate(cat, into=c("short", "cat_name"), sep=",") %>%
  select(-stuff) %>%
  filter(short!="NA")
baby_mainCat$short<- as.numeric(baby_mainCat$short)

baby_catFields <- searchable %>% filter(entity=="Baby" & Included_interface==1 & field_type=="categorical") %>% 
  pull(field_shortname)

baby_mainTableL <- baby_mainTable %>% pivot_longer(cols = baby_catFields, names_to="field_shortname",
                                                   values_to="short")
baby_mainTableL <- left_join(baby_mainTableL, baby_mainCat, by = c("field_shortname", "short")) %>%
  select(-short)

baby_mainTable <- baby_mainTableL %>% 
  pivot_wider(names_from = field_shortname, values_from=cat_name)

# fixing FamillyID
baby_mainTable <- baby_mainTable %>% mutate(familly_ID=str_remove_all(familly_ID, "Perhe"))

# printing table
write.csv(baby_mainTable, "../normalized_versions/DataCube/baby.csv", row.names=FALSE)

# --------------


# Generate the Familly main table
# --------------

familly_mainFields <- searchable %>% filter(entity=="Familly" & Included_interface==1) %>% pull(field_shortname)

familly_mainTable_Q1 <-  Q1 %>% select(familly_ID, any_of(familly_mainFields))
familly_mainTable_Q8 <-  Q8 %>% select(familly_ID, any_of(familly_mainFields))
familly_mainTable <- full_join(familly_mainTable_Q1,familly_mainTable_Q8, by="familly_ID")

# fixing the display of categories using the english translation of the fields
familly_mainCat <- searchable %>% filter(entity=="Familly" & Included_interface==1 & field_type=="categorical") %>%
  select(field_shortname, data_format, category_1:category_13) %>%
  separate(data_format, into=c("name1","name2","name3","name4","name5","name6","name7","name8","name9","name10",
                               "name11","name12","name13"), sep=",") %>%
  unite(new1, name1, category_1, sep = ",") %>%
  unite(new2, name2, category_2, sep = ",") %>%
  unite(new3, name3, category_3, sep = ",") %>%
  unite(new4, name4, category_4, sep = ",") %>%
  unite(new5, name5, category_5, sep = ",") %>%
  unite(new6, name6, category_6, sep = ",") %>%
  unite(new7, name7, category_7, sep = ",") %>%
  unite(new8, name8, category_8, sep = ",") %>%
  unite(new9, name9, category_9, sep = ",") %>%
  unite(new10, name10, category_10, sep = ",") %>%
  unite(new11, name11, category_11, sep = ",") %>%
  unite(new12, name12, category_12, sep = ",") %>%
  unite(new13, name13, category_13, sep = ",") %>%
  pivot_longer(cols = starts_with("new"), names_to="stuff",
               values_to="cat") %>%
  separate(cat, into=c("short", "cat_name"), sep=",") %>%
  select(-stuff) %>%
  filter(short!="NA")
familly_mainCat$short<- as.numeric(familly_mainCat$short)

familly_catFields <- searchable %>% filter(entity=="Familly" & Included_interface==1 
                                           & field_type=="categorical") %>% pull(field_shortname)

familly_mainTableL <- familly_mainTable %>% pivot_longer(cols = familly_catFields, names_to="field_shortname",
                                                   values_to="short")
familly_mainTableL <- left_join(familly_mainTableL, familly_mainCat, by = c("field_shortname", "short")) %>%
  select(-short)

familly_mainTable <- familly_mainTableL %>% 
  pivot_wider(names_from = field_shortname, values_from=cat_name)

# fixing FamillyID
familly_mainTable <- familly_mainTable %>% mutate(familly_ID=str_remove_all(familly_ID, "Perhe"))

# printing table
write.csv(familly_mainTable, "../normalized_versions/DataCube/familly.csv", row.names=FALSE)

# --------------


# Generate the Mother main table
# --------------

mother_mainFields <- searchable %>% filter(entity=="Mother" & Included_interface==1) %>% pull(field_shortname)

mother_mainTable <-  Q1 %>% select(familly_ID, any_of(mother_mainFields))

# fixing the display of categories using the english translation of the fields
mother_mainCat <- searchable %>% filter(entity=="Mother" & Included_interface==1 & field_type=="categorical") %>%
  select(field_shortname, data_format, category_1:category_13) %>%
  separate(data_format, into=c("name1","name2","name3","name4","name5","name6","name7","name8","name9","name10",
                               "name11","name12","name13"), sep=",") %>%
  unite(new1, name1, category_1, sep = ",") %>%
  unite(new2, name2, category_2, sep = ",") %>%
  unite(new3, name3, category_3, sep = ",") %>%
  unite(new4, name4, category_4, sep = ",") %>%
  unite(new5, name5, category_5, sep = ",") %>%
  unite(new6, name6, category_6, sep = ",") %>%
  unite(new7, name7, category_7, sep = ",") %>%
  unite(new8, name8, category_8, sep = ",") %>%
  unite(new9, name9, category_9, sep = ",") %>%
  unite(new10, name10, category_10, sep = ",") %>%
  unite(new11, name11, category_11, sep = ",") %>%
  unite(new12, name12, category_12, sep = ",") %>%
  unite(new13, name13, category_13, sep = ",") %>%
  pivot_longer(cols = starts_with("new"), names_to="stuff",
               values_to="cat") %>%
  separate(cat, into=c("short", "cat_name"), sep=",") %>%
  select(-stuff) %>%
  filter(short!="NA")
mother_mainCat$short<- as.numeric(mother_mainCat$short)

mother_catFields <- searchable %>% filter(entity=="Mother" & Included_interface==1 
                                           & field_type=="categorical") %>% pull(field_shortname)

mother_mainTableL <- mother_mainTable %>% pivot_longer(cols = all_of(mother_catFields), names_to="field_shortname",
                                                         values_to="short")
mother_mainTableL <- left_join(mother_mainTableL, mother_mainCat, by = c("field_shortname", "short")) %>%
  select(-short)

mother_mainTable <- mother_mainTableL %>% 
  pivot_wider(names_from = field_shortname, values_from=cat_name)

# fixing FamillyID
mother_mainTable <- mother_mainTable %>% mutate(familly_ID=str_remove_all(familly_ID, "Perhe"))

# printing table
write.csv(mother_mainTable, "../normalized_versions/DataCube/mother.csv", row.names=FALSE)

# --------------


# Generate the BioFather main table
# --------------

father_mainFields <- searchable %>% filter(entity=="BioFather" & Included_interface==1) %>% pull(field_shortname)

father_mainTable <-  Q1 %>% select(familly_ID, any_of(father_mainFields))

# fixing the display of categories using the english translation of the fields
father_mainCat <- searchable %>% filter(entity=="BioFather" & Included_interface==1 & field_type=="categorical") %>%
  select(field_shortname, data_format, category_1:category_13) %>%
  separate(data_format, into=c("name1","name2","name3","name4","name5","name6","name7","name8","name9","name10",
                               "name11","name12","name13"), sep=",") %>%
  unite(new1, name1, category_1, sep = ",") %>%
  unite(new2, name2, category_2, sep = ",") %>%
  unite(new3, name3, category_3, sep = ",") %>%
  unite(new4, name4, category_4, sep = ",") %>%
  unite(new5, name5, category_5, sep = ",") %>%
  unite(new6, name6, category_6, sep = ",") %>%
  unite(new7, name7, category_7, sep = ",") %>%
  unite(new8, name8, category_8, sep = ",") %>%
  unite(new9, name9, category_9, sep = ",") %>%
  unite(new10, name10, category_10, sep = ",") %>%
  unite(new11, name11, category_11, sep = ",") %>%
  unite(new12, name12, category_12, sep = ",") %>%
  unite(new13, name13, category_13, sep = ",") %>%
  pivot_longer(cols = starts_with("new"), names_to="stuff",
               values_to="cat") %>%
  separate(cat, into=c("short", "cat_name"), sep=",") %>%
  select(-stuff) %>%
  filter(short!="NA")
father_mainCat$short<- as.numeric(father_mainCat$short)

father_catFields <- searchable %>% filter(entity=="BioFather" & Included_interface==1 
                                          & field_type=="categorical") %>% pull(field_shortname)

father_mainTableL <- father_mainTable %>% pivot_longer(cols = all_of(father_catFields), names_to="field_shortname",
                                                       values_to="short")
father_mainTableL <- left_join(father_mainTableL, father_mainCat, by = c("field_shortname", "short")) %>%
  select(-short)

father_mainTable <- father_mainTableL %>% 
  pivot_wider(names_from = field_shortname, values_from=cat_name)

# fixing FamillyID
father_mainTable <- father_mainTable %>% mutate(familly_ID=str_remove_all(familly_ID, "Perhe"))

# printing table
write.csv(father_mainTable, "../normalized_versions/DataCube/father.csv", row.names=FALSE)

# --------------

# Generate the partner main table
# --------------

partner_mainFields <- searchable %>% filter(entity=="Partner" & Included_interface==1) %>% pull(field_shortname)

partner_mainTable <-  Q1 %>% select(familly_ID, any_of(partner_mainFields))

# fixing the display of categories using the english translation of the fields
partner_mainCat <- searchable %>% filter(entity=="Partner" & Included_interface==1 & field_type=="categorical") %>%
  select(field_shortname, data_format, category_1:category_13) %>%
  separate(data_format, into=c("name1","name2","name3","name4","name5","name6","name7","name8","name9","name10",
                               "name11","name12","name13"), sep=",") %>%
  unite(new1, name1, category_1, sep = ",") %>%
  unite(new2, name2, category_2, sep = ",") %>%
  unite(new3, name3, category_3, sep = ",") %>%
  unite(new4, name4, category_4, sep = ",") %>%
  unite(new5, name5, category_5, sep = ",") %>%
  unite(new6, name6, category_6, sep = ",") %>%
  unite(new7, name7, category_7, sep = ",") %>%
  unite(new8, name8, category_8, sep = ",") %>%
  unite(new9, name9, category_9, sep = ",") %>%
  unite(new10, name10, category_10, sep = ",") %>%
  unite(new11, name11, category_11, sep = ",") %>%
  unite(new12, name12, category_12, sep = ",") %>%
  unite(new13, name13, category_13, sep = ",") %>%
  pivot_longer(cols = starts_with("new"), names_to="stuff",
               values_to="cat") %>%
  separate(cat, into=c("short", "cat_name"), sep=",") %>%
  select(-stuff) %>%
  filter(short!="NA")
partner_mainCat$short<- as.numeric(partner_mainCat$short)

partner_catFields <- searchable %>% filter(entity=="Partner" & Included_interface==1 
                                          & field_type=="categorical") %>% pull(field_shortname)

partner_mainTableL <- partner_mainTable %>% pivot_longer(cols = all_of(partner_catFields), names_to="field_shortname",
                                                       values_to="short")
partner_mainTableL <- left_join(partner_mainTableL, partner_mainCat, by = c("field_shortname", "short")) %>%
  select(-short)

partner_mainTable <- partner_mainTableL %>% 
  pivot_wider(names_from = field_shortname, values_from=cat_name)

# fixing FamillyID
partner_mainTable <- partner_mainTable %>% mutate(familly_ID=str_remove_all(familly_ID, "Perhe"))

# printing table
write.csv(partner_mainTable, "../normalized_versions/DataCube/partner.csv", row.names=FALSE)

# --------------

