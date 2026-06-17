# ==============================================================================
# Part 1: Establishing the Occupational Middle-Class
# Data: O*NET & OEWS
# Author: Haya Adleh
# ==============================================================================

library(tidyverse)
library(spatstat.geom)

# ------ Part 1. 2015 Data — Education + Wages + Middle-Wage Classification -----

# ONET education datasets will be used to identify whether occupations require a 
# Bachelor's degree or higher or a lower than a Bachelor's level education.
# Link to source: https://www.onetcenter.org/db_releases.html


# --- Load O*NET Education Requirement 2015 Data for analysis ---
onet_education_data_2015 <- readxl::read_xlsx("Datasets/Education, Training, and Experience10.2015.xlsx")

# O*NET (Required Level of Education, Data Value) is a distribution
# across 12 categories, where Data Value = the % of workers who say that education
# category applies. Categories 1–5 are sub-baccalaureate (less than BA); 6–12 are BA+.

# Method of classification:
# 1. Sum category percentages into two buckets: Sub_BA and BA_Plus.
# 2. An occupation is "middle-eduction" if Sub_BA > BA_Plus

onet_education_cleaned_data_2015 <- onet_education_data_2015 %>%
  filter(`Element ID` == "2.D.1") %>%
  mutate(SOC6 = str_remove_all(str_sub(`O*NET-SOC Code`, 1, 7), "-")) %>%
  group_by(SOC6, Category) %>%
  # before aggregating into buckets, I will aggregate 8-digit occupations to the 6-digit level by averaging the scores
  summarise(avg_data_value = mean(`Data Value`, na.rm = TRUE), .groups = "drop") %>%
  mutate(bucket = ifelse(Category <= 5, "Sub_BA", "BA_Plus")) %>%
  group_by(SOC6, bucket) %>%
  summarise(bucket_pct = sum(avg_data_value), .groups = "drop") %>%
  pivot_wider(names_from = bucket, values_from = bucket_pct, values_fill = 0) %>%
  mutate(is_middle_edu = Sub_BA > BA_Plus,
         dominant_requirement = ifelse(is_middle_edu, "Sub-Baccalaureate", "Bachelor's or Higher"))

print(paste("2015 Non-BA Occupations:", sum(onet_education_cleaned_data_2015$dominant_requirement == "Sub-Baccalaureate", na.rm = TRUE)))
print(paste("2015 Total Occupations:", nrow(onet_education_cleaned_data_2015)))


# NOTE: I use a majority-rule threshold, not a hard cutoff, which
# I deemed to be better to occupations with mixed credential distributions.

# --- Load O*NET Skills 2015 Data for analysis ---
onet_skills_data_2015 <- readxl::read_xlsx("Datasets/Skills.10.2015.xlsx")

# ONET Skills datasets rates the importance of skills (1-5 scale) across 35 skill categories

# I will not use all 35 skills available. instead, I'll group them into 
# 5 bigger domains, per the PEW report: 
# https://www.pewresearch.org/social-trends/2020/01/30/methodology-28-2/

# create a mapping based on the Element IDs 
skill_mapping <- data.frame(
  Element_ID = c(
    # Social: Monitoring, Social Perceptiveness, Coordination, Persuasion, etc.
    "2.A.2.d", "2.B.1.a", "2.B.1.b", "2.B.1.c", "2.B.1.d", "2.B.1.e", "2.B.1.f",
    # Fundamental: Reading, Writing, Speaking, Listening, Math, Science
    "2.A.1.a", "2.A.1.b", "2.A.1.c", "2.A.1.d", "2.A.2.a", "2.A.2.b", "2.A.2.c", "2.B.4.e",
    # Analytical: Critical Thinking, Active Learning, Operations Analysis, etc.
    "2.A.1.e", "2.A.1.f", "2.B.2.i", "2.B.3.a", "2.B.3.b", "2.B.4.g", "2.B.3.e", "2.B.4.h",
    # Managerial: Personnel, Financial, Material Resources, Time Management
    "2.B.5.a", "2.B.5.b", "2.B.5.c", "2.B.5.d",
    # Mechanical: Equipment Selection, Installation, Programming, Troubleshooting, Repair
    "2.B.3.c", "2.B.3.d", "2.B.3.g", "2.B.3.h", "2.B.3.j", "2.B.3.k", "2.B.3.l", "2.B.3.m"),
  Family = c(rep("Social", 7), rep("Fundamental", 8), rep("Analytical", 8), 
    rep("Managerial", 4), rep("Mechanical", 8)))

# Aggregate skills into families
onet_skills_cleaned_2015 <- onet_skills_data_2015 %>%
  filter(`Scale ID` == "IM") %>% # focus on importance as per PEW report
  mutate(SOC6 = str_remove_all(str_sub(`O*NET-SOC Code`, 1, 7), "-")) %>%
  # before aggregating into families, I will aggregate 8-digit occupations to the 6-digit level by averaging the scores
  group_by(SOC6, `Element ID`) %>% 
  mutate(Data_Value = mean(`Data Value`, na.rm = TRUE)) %>% 
  # Now I will join by the skill family classification and aggregate 
  inner_join(skill_mapping, by = c("Element ID" = "Element_ID")) %>%
  group_by(SOC6, Family) %>%
  # find the average across skills in each family
  summarise(family_score = mean(Data_Value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Family, values_from = family_score)


# Now, calculate the relative importance
onet_skills_cleaned_2015 <- onet_skills_cleaned_2015 %>%
  mutate(Total_Skill_Sum = Social + Fundamental + Analytical + Managerial + Mechanical,
         Rel_Social = round(Social / Total_Skill_Sum, 2),
         Rel_Analytical = round(Analytical / Total_Skill_Sum, 2),
         Rel_Fundamental = round(Fundamental / Total_Skill_Sum, 2),
         Rel_Managerial = round(Managerial / Total_Skill_Sum, 2),
         Rel_Mechanical = round(Mechanical / Total_Skill_Sum, 2))


# Now I create one, all-encompassing file with SOC codes, education, & skills
education_skills_soc_2015 <- onet_education_cleaned_data_2015 %>% 
  full_join(onet_skills_cleaned_2015, by = "SOC6")

# --- OEWS Middle-wage-bands ---

# OEWS will be used to establish an employment-weighted median wage across occupations
# Find the datasets here: https://www.bls.gov/oes/tables.htm

# load the 2015 file
oews_2015 <- readxl::read_xlsx("Datasets/aMSA_M2015_dl.xlsx")  

# Filter to the NYC-Newark-Jersey City MSA and detailed occupation group (SOC-6) only
oews_nyc_2015 <- oews_2015 %>%
  filter(OCC_GROUP == "detailed",
         AREA_NAME == "New York-Newark-Jersey City, NY-NJ-PA") %>%
  filter(A_MEDIAN != c("*", "	#"), TOT_EMP != "**") %>% 
  mutate(SOC6 = str_remove_all(str_sub(OCC_CODE, 1, 7), "-"),
         A_MEDIAN = suppressWarnings(as.numeric(A_MEDIAN)),
         TOT_EMP = suppressWarnings(as.numeric(TOT_EMP))) %>%
  select(SOC6, OCC_TITLE, TOT_EMP, A_MEDIAN)

print(paste("Number of Occupations from OEWS that dont have wages:",
            sum(is.na(oews_nyc_2015$A_MEDIAN), na.rm = TRUE)))

# suppressed wages are filtered out. Note from BLS: https://www.bls.gov/oes/oes_ques.htm
# [estimates] "may be withheld from publication for .. failure to meet BLS 
# quality standards or the need to protect the confidentiality of our survey respondents."

# I chose an employment-weighted median to reflect where the a typical worker sits in the wage 
# distribution and to describe the middle of the workforce & not the middle of job categories.

oews_nyc_wages_2015 <- oews_nyc_2015 %>% filter(!is.na(A_MEDIAN))

# Band definition:
# Lower bound = 67% of weighted median
# Upper bound = 200% of weighted median

nyc_overall_median_2015 <- weighted.median(
  x = oews_nyc_wages_2015$A_MEDIAN,
  w = oews_nyc_wages_2015$TOT_EMP)

lower_bound_2015 <- nyc_overall_median_2015 * 0.67
upper_bound_2015 <- nyc_overall_median_2015 * 2.00

print(paste("2015 NYC-NJ-PA Metro Middle-Wage Range:", round(lower_bound_2015), "to", round(upper_bound_2015)))

# These multipliers are consistent across both years, so the band is defined
# relative to each year's own wage structure

# --- Classify 2015 Occupations as Middle-wage ---

# join OEWS data with ONET data
occ_id_2015_na <- oews_nyc_wages_2015 %>%
  left_join(education_skills_soc_2015, by = "SOC6")

# Quality check -- How many of OEWS SOCs were not identified in ONET??
print(paste("Number of Occupations from OEWS that is NA in ONET:",
            sum(is.na(occ_id_2015_na$is_middle_edu), na.rm = TRUE)))

print(paste("2015 OEWS Non-BA Occupations:", 
            sum(occ_id_2015_na$dominant_requirement == "Sub-Baccalaureate", na.rm = TRUE)))

manual_soc_list_2015 <- occ_id_2015_na %>% 
  filter(is.na(is_middle_edu)) %>% 
  pull(SOC6)

na_count <- sum(is.na(occ_id_2015_na$is_middle_edu))
total_count <- nrow(occ_id_2015_na)
na_pct <- na_count / total_count * 100

print(paste("Percent of Occupations from OEWS that are NA in ONET:",
            round(na_pct, 2), "%"))

# Since some occupations identified in OEWS don't have education flags, I did a manual diagnostic and found out that many 
# of these occupations are in the "All Other" groups. So I will do the following: 

# 1. identify occupations that have OEWS employment/wage figures but not ONET flags ---
# 2.Since there is no great technical way to assign education flags to those occupations from OEWS
# that did not map in ONET, I assign an education flag manually through searches on ONET & OEWS

# writexl::write_xlsx(occ_id_2015_na, "Outputs/occ_id_2015_na.xlsx")

# 38 occupations in OEWS did NOT have an education flag from ONET
script <- "Scripts/01B_mid_edu_assign.R"
lines <- readLines(script)[11:60]
code <- paste(lines, collapse = "\n")
eval(parse(text = code))

# After assigning that middle_education flag, occupations are identified as middle_wage_occp if:
# 1. Its annual median wage falls within the NYC middle-wage band AND
# 2. It is a middle-skill occupation (majority of workers don't need BA+)
occ_id_2015 <- occ_id_2015 %>%
  mutate(
    is_middle_wage = A_MEDIAN >= lower_bound_2015 &
      A_MEDIAN <= upper_bound_2015,
    is_middle_class_occ = is_middle_wage & is_middle_edu,
    SOC = SOC6)

# --- Audit: Assignment Summary by Source and Education ---
assignment_summary_2015 <- occ_id_2015 %>%
  group_by(assignment_source, is_middle_edu) %>%
  summarise(
    count = n(),
    avg_emp = round(mean(TOT_EMP, na.rm = TRUE)),
    .groups = "drop")

print("=== 2015 Education Classification Audit ===")
print(assignment_summary_2015)

# --- Diagnostic: Full Classification Breakdown ---
diagnoistic_2015 <- occ_id_2015 %>%
  mutate(classification_group = case_when(
    is_middle_edu == TRUE  & is_middle_wage == TRUE  ~ "1. Both TRUE (Middle Class)",
    is_middle_edu == TRUE  & is_middle_wage == FALSE ~ "2. Edu TRUE / Wage FALSE",
    is_middle_edu == FALSE & is_middle_wage == TRUE  ~ "3. Wage TRUE / Edu FALSE",
    is_middle_edu == FALSE & is_middle_wage == FALSE ~ "4. Both FALSE",
    TRUE ~ "Error/NA")) %>%
  group_by(classification_group) %>%
  summarise(
    occ_count = n(),
    med_wage = round(median(A_MEDIAN, na.rm = TRUE)),
    .groups = "drop")

cat("\nEducation totals by flag\n")
cat("Total Middle-Edu (TRUE): ", sum(occ_id_2015$is_middle_edu == TRUE, na.rm = TRUE), "\n")
cat("Total Middle-Wage (TRUE):", sum(occ_id_2015$is_middle_wage == TRUE, na.rm = TRUE), "\n\n")

cat("Final Diagnostic\n")
print(diagnoistic_2015)

# Check for remaining NA
cat("\nRemaining NA values in is_middle_edu:", 
    sum(is.na(occ_id_2015$is_middle_edu)), "\n")

# write.csv(occ_id_2015, "Outputs/occ_id_2015.csv")

# ------ Part 2. 2023 Data — Education + Wages + Middle-Wage Classification -----
# Identical methodology to Part 1, applied to the 2023 vintage.

# --- Load O*NET Education Requirement 2015 Data for analysis ---
onet_education_data_2023 <- readxl::read_xlsx("Datasets/Education, Training, and Experience11.2023.xlsx")

onet_education_cleaned_data_2023 <- onet_education_data_2023 %>%
  filter(`Element ID` == "2.D.1") %>%
  mutate(SOC6 = str_remove_all(str_sub(`O*NET-SOC Code`, 1, 7), "-")) %>%
  group_by(SOC6, Category) %>%
  summarise(avg_data_value = mean(`Data Value`, na.rm = TRUE), .groups = "drop") %>%
  mutate(bucket = ifelse(Category <= 5, "Sub_BA", "BA_Plus")) %>%
  group_by(SOC6, bucket) %>%
  summarise(bucket_pct = sum(avg_data_value), .groups = "drop") %>%
  pivot_wider(names_from = bucket, values_from = bucket_pct, values_fill = 0) %>%
  mutate(is_middle_edu  = Sub_BA > BA_Plus,
         dominant_requirement = ifelse(is_middle_edu, "Sub-Baccalaureate", "Bachelor's or Higher"))

print(paste("2023 Non-BA Count:", sum(onet_education_cleaned_data_2023$dominant_requirement == "Sub-Baccalaureate", na.rm = TRUE)))
print(paste("2023 Total Occupations:", nrow(onet_education_cleaned_data_2023)))

# --- Load O*NET Skills 2023 Data for analysis ---
onet_skills_data_2023 <- readxl::read_xlsx("Datasets/Skills11.2023.xlsx")

onet_skills_cleaned_2023 <- onet_skills_data_2023 %>%
  filter(`Scale ID` == "IM") %>% # focus on importance as per report
  mutate(SOC6 = str_remove_all(str_sub(`O*NET-SOC Code`, 1, 7), "-")) %>%
  group_by(SOC6, `Element ID`) %>% 
  mutate(Data_Value = mean(`Data Value`, na.rm = TRUE)) %>% 
  inner_join(skill_mapping, by = c("Element ID" = "Element_ID")) %>%
  group_by(SOC6, Family) %>%
  summarise(family_score = mean(Data_Value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Family, values_from = family_score)

# Now, calculate the relative importance
onet_skills_cleaned_2023 <- onet_skills_cleaned_2023 %>%
  mutate(Total_Skill_Sum = Social + Fundamental + Analytical + Managerial + Mechanical,
         Rel_Social = round(Social / Total_Skill_Sum, 2),
         Rel_Analytical = round(Analytical / Total_Skill_Sum, 2),
         Rel_Fundamental = round(Fundamental / Total_Skill_Sum, 2),
         Rel_Managerial = round(Managerial / Total_Skill_Sum, 2),
         Rel_Mechanical = round(Mechanical / Total_Skill_Sum, 2))

education_skills_soc_2023 <- onet_education_cleaned_data_2023 %>% 
  full_join(onet_skills_cleaned_2023, by = "SOC6")

# --- OEWS Middle-wage-bands ---
oews_2023 <- readxl::read_xlsx("Datasets/MSA_M2023_dl.xlsx")

oews_nyc_2023 <- oews_2023 %>%
  filter(O_GROUP == "detailed",             
         AREA_TITLE == "New York-Newark-Jersey City, NY-NJ-PA") %>% 
  mutate(SOC6 = str_remove_all(str_sub(OCC_CODE, 1, 7), "-"),
         A_MEDIAN = suppressWarnings(as.numeric(A_MEDIAN)),
         TOT_EMP = suppressWarnings(as.numeric(TOT_EMP))) %>%
  select(SOC6, OCC_TITLE, TOT_EMP, A_MEDIAN)

print(paste("Number of Occupations from OEWS that dont have wages:",
            sum(is.na(oews_nyc_2023$A_MEDIAN), na.rm = TRUE)))

oews_nyc_wages_2023 <- oews_nyc_2023 %>% filter(!is.na(A_MEDIAN))

nyc_overall_median_2023 <- weighted.median(
  x = oews_nyc_wages_2023$A_MEDIAN,
  w = oews_nyc_wages_2023$TOT_EMP)

lower_bound_2023 <- nyc_overall_median_2023 * 0.67
upper_bound_2023 <- nyc_overall_median_2023 * 2.00

print(paste("2023 Middle-Wage Range:", round(lower_bound_2023), "to", round(upper_bound_2023)))


# --- Classify 2023 Occupations as Middle-wage ---
occ_id_2023_na <- oews_nyc_wages_2023 %>%
  left_join(education_skills_soc_2023, by = "SOC6") 

# Quality check -- What % of OEWS was not identified in ONET??
print(paste("Number of Occupations from OEWS that is NA in ONET:",
            sum(is.na(occ_id_2023_na$is_middle_edu), na.rm = TRUE)))

na_count <- sum(is.na(occ_id_2023_na$is_middle_edu))
total_count <- nrow(occ_id_2023_na)
na_pct <- na_count / total_count * 100

print(paste("Percent of Occupations from OEWS that are NA in ONET:",
            round(na_pct, 2), "%"))

manual_soc_list_2023 <- occ_id_2023_na %>% 
  filter(is.na(is_middle_edu)) %>% 
  pull(SOC6)

# 88 occupations from OEWS did NOT have an education flag in ONET
script <- "Scripts/01B_mid_edu_assign.R"
lines <- readLines(script)[65:161]
code <- paste(lines, collapse = "\n")
eval(parse(text = code))

occ_id_2023 <- occ_id_2023 %>%
  mutate(
    is_middle_wage = A_MEDIAN >= lower_bound_2023 &
      A_MEDIAN <= upper_bound_2023,
    is_middle_class_occ = is_middle_wage & is_middle_edu,
    SOC = SOC6)


# --- Audit: Assignment Summary by Source and Education ---
assignment_summary_2023 <- occ_id_2023 %>%
  group_by(assignment_source, is_middle_edu) %>%
  summarise(
    count = n(),
    avg_emp = round(mean(TOT_EMP, na.rm = TRUE)),
    .groups = "drop")

print("=== 2023 Education Classification Audit ===")
print(assignment_summary_2023)

# --- Diagnostic: Full Classification Breakdown ---
diagnoistic_2023 <- occ_id_2023 %>%
  mutate(classification_group = case_when(
    is_middle_edu == TRUE  & is_middle_wage == TRUE  ~ "1. Both TRUE (Middle Class)",
    is_middle_edu == TRUE  & is_middle_wage == FALSE ~ "2. Edu TRUE / Wage FALSE",
    is_middle_edu == FALSE & is_middle_wage == TRUE  ~ "3. Wage TRUE / Edu FALSE",
    is_middle_edu == FALSE & is_middle_wage == FALSE ~ "4. Both FALSE",
    TRUE ~ "Error/NA")) %>%
  group_by(classification_group) %>%
  summarise(
    occ_count = n(),
    med_wage = round(median(A_MEDIAN, na.rm = TRUE)),
    .groups = "drop")

cat("\nEducation totals by flag\n")
cat("Total Middle-Edu (TRUE): ", sum(occ_id_2023$is_middle_edu == TRUE, na.rm = TRUE), "\n")
cat("Total Middle-Wage (TRUE):", sum(occ_id_2023$is_middle_wage == TRUE, na.rm = TRUE), "\n\n")

cat("Final Diagnostic\n")
print(diagnoistic_2023)

# Check for remaining NA
cat("\nRemaining NA values in is_middle_edu:", 
    sum(is.na(occ_id_2023$is_middle_edu)), "\n")

# write.csv(occ_id_2023, "Outputs/occ_id_2023.csv")

# save.image(file = "Environments/01_establish_mid_class.RData")

# See "Scripts\sensitivity_analysis.R" for sensitivity analysis
