# ==============================================================================
# Part 3.: ACS Middle-Class Labor Force Demographic Classification
# Using the mapped ACS middle-class occupation file, define ACS variables
# Author: Haya Adleh
# ==============================================================================

library(tidyverse)
library(spatstat.geom)

# Bring in the final mapped 2015 and 2023 ACS files -----
acs_2015_final <- read.csv("Outputs/acs_2015_final.csv")
acs_2023_final <- read.csv("Outputs/acs_2023_final.csv")

acs_year_join <- bind_rows(
  acs_2015_final %>% 
    mutate(is_middle_class = is_middle_class_occp,
           SERIALNO = as.character(SERIALNO),
           SPORDER = as.integer(SPORDER)),
  acs_2023_final %>% 
    mutate(is_middle_class = is_middle_class_occp,
           SERIALNO = as.character(SERIALNO),
           SPORDER = as.integer(SPORDER))) 

# Define the ACS variables
acs_classified <- acs_year_join %>%
  mutate(OCCP = as.numeric(OCCP),AGEP = as.numeric(AGEP), ADJINC = as.numeric(ADJINC),
         
         gender = if_else(SEX == "2", "Female", "Male"),
         
         race_eth = case_when(
           as.numeric(HISP) != 1 ~ "Hispanic",
           as.numeric(HISP) == 1 & RAC1P == "1" ~ "NH White",
           as.numeric(HISP) == 1 & RAC1P == "2" ~ "NH Black",
           as.numeric(HISP) == 1 & RAC1P == "6" ~ "NH Asian",
           as.numeric(HISP) == 1 ~ "Other",
           TRUE ~ NA_character_),
         
         citizenship = case_when(
           CIT %in% c("1", "2", "3") ~ "US Born",
           CIT == "4" ~ "Naturalized",
           CIT == "5" ~ "Non-citizen",
           TRUE ~ NA_character_
         ),
         
         education = case_when(
           SCHL %in% as.character(1:15) ~ "Less than HS",
           SCHL %in% c("16","17") ~ "HS/GED",
           SCHL %in% c("18","19","20") ~ "Some College",
           SCHL %in% c("21","22","23","24") ~ "BA+",
           TRUE ~ NA_character_
         ),
         
         age_group = case_when(
           AGEP < 16 ~ "Under 16",
           AGEP < 25 ~ "16 to 24 Years",
           AGEP < 45 ~ "25 to 44 Years",
           AGEP < 65 ~ "45 to 64 Years",
           TRUE ~ "65+"),
         
         emp_status = case_when(
           ESR %in% c("1", "2") ~ "Employed",
           ESR == "3" ~ "Unemployed",
           ESR == "6" ~ "NILF",
           TRUE ~ NA_character_
         ),
         
         in_lf = as.integer(ESR %in% c("1","2","3")),
         is_employed = as.integer(emp_status == "Employed"),
         
         occupation = case_when(
           between(OCCP, 10, 440) ~ "Management",
           between(OCCP, 500, 960) ~ "Business/Finance",
           between(OCCP, 1000, 1240) ~ "Computer/Math",
           between(OCCP, 1300, 1980) ~ "Engineering/Sciences",
           between(OCCP, 2000, 2060) ~ "Social Service",
           between(OCCP, 2100, 2180) ~ "Legal",
           between(OCCP, 2200, 2555) ~ "Education",
           between(OCCP, 2600, 2960) ~ "Arts/Media",
           between(OCCP, 3000, 3550) ~ "Healthcare Practitioners",
           between(OCCP, 3600, 3655) ~ "Healthcare Support",
           between(OCCP, 3700, 3960) ~ "Protective Service",
           between(OCCP, 4000, 4160) ~ "Food Prep",
           between(OCCP, 4200, 4255) ~ "Building Maintenance",
           between(OCCP, 4300, 4655) ~ "Personal Care",
           between(OCCP, 4700, 5940) ~ "Sales/Office",
           between(OCCP, 6200, 6950) ~ "Construction/Extraction",
           between(OCCP, 7000, 7640) ~ "Installation/Maintance/Repair",
           between(OCCP, 7700, 9760) ~ "Production/Transportation",
           between(OCCP, 9800, 9830) ~ "Military",
           TRUE ~ NA_character_
         ),
         
         weeks_50plus = case_when(
           year == 2015 & WKW == "1" ~ TRUE,
           year == 2023 & as.numeric(WKWN) >= 50 ~ TRUE,
           TRUE ~ FALSE
         ),
         
         hours34plus = as.numeric(WKHP) >= 34,
         
         is_full_time = if_else(weeks_50plus & hours34plus, TRUE, FALSE),
         
         adj_wage = WAGP * (as.numeric(ADJINC) / 1000000),
         has.earnings = as.integer(WAGP > 0 & !is.na(WAGP)))


# write_rds(acs_classified, "Outputs/acs_classified.rds")

# =================== FILTERING - CREATE ANALYSIS POOL ==========================================
# Now that we have the applied middle-class flags to our ACS population, it is time to think about who 
# we want to filter IN and OUT to perform our ACS analysis... I will perform sensitivity analysis to evaluate

cat("=== ACS Pre-filtering ===\n")
acs_classified %>% 
  group_by(year) %>%
  summarise(n_persons = n(), 
            weighted_persons = formatC(sum(PWGTP), format = "d", big.mark = ","),
            pct_mw = round(100 * sum(PWGTP[is_middle_class]) / sum(PWGTP), 1), 
            .groups = "drop") %>% 
  print()

# Create base pool: prime-age workers with positive earnings
base_pool <- acs_classified %>%
  filter(AGEP >= 18, AGEP <= 64,
         !is.na(OCCP), OCCP > 0, OCCP < 9800,
         adj_wage > 0, adj_wage < 500000,
         has.earnings == 1) %>%
  mutate(is_middle_class = replace_na(is_middle_class, FALSE))

# Full-time pool: primary analysis
analysis_pool <- base_pool %>%
  filter(is_full_time == TRUE)

# write_rds(analysis_pool, "Outputs/analysis_pool.rds")

# Part-time pool: supplementary analysis
parttime_pool <- base_pool %>%
  filter(is_full_time == FALSE)

# Document what each filter removes
cat("\n=== Filter Impact ===\n")
bind_rows(
  acs_classified %>%
    group_by(year) %>%
    summarise(pool = "All employed",
              n = formatC(n(), format = "d", big.mark = ","),
              weighted = formatC(sum(PWGTP), format = "d", big.mark = ","),
              pct_mw = paste0(round(100 * sum(PWGTP[is_middle_class == TRUE], na.rm = TRUE) / sum(PWGTP), 1), "%"),
              .groups = "drop"),
  
  base_pool %>%
    group_by(year) %>%
    summarise(pool = "Prime-age with wages",
              n = formatC(n(), format = "d", big.mark = ","),
              weighted = formatC(sum(PWGTP), format = "d", big.mark = ","),
              pct_mw = paste0(round(100 * sum(PWGTP[is_middle_class]) / sum(PWGTP), 1), "%"),
              .groups = "drop"),
  
  analysis_pool %>%
    group_by(year) %>%
    summarise(pool = "Full-time only",
              n = formatC(n(), format = "d", big.mark = ","),
              weighted = formatC(sum(PWGTP), format = "d", big.mark = ","),
              pct_mw = paste0(round(100 * sum(PWGTP[is_middle_class]) / sum(PWGTP), 1), "%"),
              .groups = "drop"),
  
  parttime_pool %>%
    group_by(year) %>%
    summarise(pool = "Part-time only",
              n = formatC(n(), format = "d", big.mark = ","),
              weighted = formatC(sum(PWGTP), format = "d", big.mark = ","),
              pct_mw = paste0(round(100 * sum(PWGTP[is_middle_class]) / sum(PWGTP), 1), "%"),
              .groups = "drop")) %>%
  arrange(year, pool) %>%
  print()

