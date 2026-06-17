# ==============================================================================
# Purpose: this script attempts to bridge the 2015 and 2023 files across occupations
# to track how middle-class definition changed over time and how many workers
# it affected. The tension is that the vintages chnaged, and so the bridge is 
# imperfect. Would love help bridging it better. 
# ==============================================================================

library(tidyverse)

# Bring in the final mapped 2015 and 2023 ACS files -----
acs_2015_final <- read.csv("Outputs/acs_2015_final.csv") 
names(acs_2015_final)

acs_2023_final <- read.csv("Outputs/acs_2023_final.csv")
names(acs_2023_final)

# Load the 2010-2018 OCCP crosswalk
occp_xwalk_raw <- readxl::read_excel("Datasets/OCCP_2010_to_2018_XWALK.xlsx", skip = 3)

# Clean column names
colnames(occp_xwalk_raw) <- c("SOC_2010", "OCCP_2010", "title_2010", 
                              "SOC_2018", "OCCP_2018", "title_2018")

# Clean and remove leading zeros
occp_xwalk <- occp_xwalk_raw %>%
  filter(!is.na(OCCP_2010) | !is.na(OCCP_2018)) %>%
  # Forward-fill 2010 codes for split mappings
  mutate(OCCP_2010 = zoo::na.locf(OCCP_2010, na.rm = FALSE)) %>%
  # Keep only rows with 2018 code
  filter(!is.na(OCCP_2018)) %>%
  # Convert to numeric to removes leading zeros
  mutate(
    OCCP_2010 = as.numeric(OCCP_2010),
    OCCP_2018 = as.numeric(OCCP_2018)
  ) %>%
  select(OCCP_2010, OCCP_2018, title_2010, title_2018)

occp_title <- occp_xwalk %>% 
  mutate(OCCP = as.numeric(OCCP_2010)) %>%
  filter(is.na(title_2010)) %>% 
  select(OCCP, title_2010) 
  

# ----------- collapse by 2015 occupation codes ---------------

# Aggregate 2015 by occupation
occ_2015 <- acs_2015_final %>%
  mutate(OCCP = as.numeric(OCCP)) %>%
  group_by(OCCP) %>%
  summarise(
    workers_2015 = sum(PWGTP, na.rm = TRUE),
    mc_workers_2015 = sum(PWGTP[is_middle_class_occp == TRUE], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    mc_pct_2015 = (mc_workers_2015 / workers_2015) * 100,
    is_mc_2015 = if_else(mc_pct_2015 > 50, TRUE, FALSE)
  ) 

cat(" Unique occupations:", n_distinct(occ_2015$OCCP), "\n")
cat(" Total workers:", formatC(sum(occ_2015$workers_2015), format="d", big.mark=","), "\n")
cat("2015 Original workers:", sum(acs_2015_final$PWGTP, na.rm=T), "\n")
cat(" MC workers:", formatC(sum(occ_2015$mc_workers_2015), format="d", big.mark=","), "\n")
cat(" MC occupations:", sum(occ_2015$is_mc_2015, na.rm=T), "\n")
cat(" Non-MC occupations:", sum(!occ_2015$is_mc_2015, na.rm=T), "\n\n")


# ----------- collapse by 2023 occupation codes ---------------

# Aggregate 2023 by occupation
occ_2023 <- acs_2023_final %>%
  mutate(OCCP = as.numeric(OCCP)) %>%
  group_by(OCCP) %>%
  summarise(
    workers_2023 = sum(PWGTP, na.rm = TRUE),
    mc_workers_2023 = sum(PWGTP[is_middle_class_occp == TRUE], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    mc_pct_2023 = (mc_workers_2023 / workers_2023) * 100,
    is_mc_2023 = if_else(mc_pct_2023 > 50, TRUE, FALSE)
  )

cat(" Aggregated 2023 (by OCCP):\n")
cat(" Unique occupations:", n_distinct(occ_2023$OCCP), "\n")
cat(" Total workers:", formatC(sum(occ_2023$workers_2023), format="d", big.mark=","), "\n")
cat(" MC workers:", formatC(sum(occ_2023$mc_workers_2023), format="d", big.mark=","), "\n")
cat(" MC occupations:", sum(occ_2023$is_mc_2023, na.rm=T), "\n")
cat(" Non-MC occupations:", sum(!occ_2023$is_mc_2023, na.rm=T), "\n\n")

# collapse 2018 occupation codes to 2010 vintage
occ_2023_collapsed <- occ_2023 %>%
  mutate(OCCP_2018 = as.numeric(OCCP)) %>%
  left_join(occp_xwalk, by = "OCCP_2018") %>%
  mutate(
    # If no crosswalk match, keep the original 2018 code
    OCCP_2010 = coalesce(OCCP_2010, OCCP_2018)
  ) %>%
  group_by(OCCP = OCCP_2010) %>%
  summarise(
    workers_2023    = sum(workers_2023,    na.rm = TRUE),
    mc_workers_2023 = sum(mc_workers_2023, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    mc_pct_2023 = (mc_workers_2023 / workers_2023) * 100,
    is_mc_2023  = if_else(mc_pct_2023 > 50, TRUE, FALSE)
  )

cat(" Aggregated 2023 (by OCCP):\n")
cat(" Unique occupations:", n_distinct(occ_2023_collapsed$OCCP), "\n")
cat(" Total workers:", formatC(sum(occ_2023_collapsed$workers_2023), format="d", big.mark=","), "\n")
cat(" MC workers:", formatC(sum(occ_2023_collapsed$mc_workers_2023), format="d", big.mark=","), "\n")
cat(" MC occupations:", sum(occ_2023_collapsed$is_mc_2023, na.rm=T), "\n")
cat(" Non-MC occupations:", sum(!occ_2023_collapsed$is_mc_2023, na.rm=T), "\n\n")


# ----------- join 2015 and 2023 files ---------------

comparison <- occ_2015 %>%
  full_join(occ_2023_collapsed, by = "OCCP", suffix = c("_2015", "_2023")) %>%
  mutate(
    workers_change = workers_2023 - workers_2015,
    mc_status_change = case_when(
      is_mc_2015 == TRUE & is_mc_2023 == TRUE ~ "Stayed MC",
      is_mc_2015 == TRUE & is_mc_2023 == FALSE ~ "Exited MC",
      is_mc_2015 == FALSE & is_mc_2023 == TRUE ~ "Entered MC",
      is_mc_2015 == FALSE & is_mc_2023 == FALSE ~ "Stayed Non-MC",
      TRUE ~ "Unmatched"
    )
  ) %>%
  select(OCCP, mc_status_change, everything()) %>%
  arrange(desc(abs(workers_change))) 

# check totals match with original ACS files
cat("Total 2023 workers:", formatC(sum(comparison$workers_2023, na.rm=T), format="d", big.mark=","), "\n")
cat("2023 workers:", sum(acs_2023_final$PWGTP, na.rm=T), "\n")

cat("Total 2015 workers:", formatC(sum(comparison$workers_2015, na.rm=T), format="d", big.mark=","), "\n")
cat("2015 Original workers:", sum(acs_2015_final$PWGTP, na.rm=T), "\n")


# writexl::write_xlsx(comparison, "Outputs/comparison_final.xlsx")

unmatched_2015 <- comparison %>% 
  filter(mc_status_change =="Unmatched", is.na(workers_2023)) %>% 
  print()

unmatched_2023 <- comparison %>% 
  filter(mc_status_change =="Unmatched", is.na(workers_2015)) %>% 
  print()


# ---------------------- Compare 2015 to 2023 occupations --------------------

comparison <- readxl::read_excel("Outputs/comparison_final.xlsx")

# summary
status_summary <- comparison %>%
  group_by(mc_status_change) %>%
  summarise(
    n_occupations = n(),
    workers_2015 = sum(workers_2015, na.rm=T),
    workers_2023 = sum(workers_2023, na.rm=T),
    net_change = workers_2023 - workers_2015,
    .groups = "drop"
  ) %>%
  mutate(pct_change = round((net_change / workers_2015 * 100), 1))

print(status_summary)

# Create seperate files
stayed_mc <- comparison %>% filter(mc_status_change == "Stayed MC")
exited_mc <- comparison %>% filter(mc_status_change == "Exited MC")
entered_mc <- comparison %>% filter(mc_status_change == "Entered MC")


# -------------------- Build summary table ---------------------
library(gt)
library(scales)

mc_summary <- comparison %>%
  filter(!is.na(workers_2015), !is.na(workers_2023)) %>%   # drop unmatched
  group_by(mc_status_change) %>%
  summarise(
    n_occupations = n(),
    workers_2015  = sum(workers_2015, na.rm = TRUE),
    workers_2023  = sum(workers_2023, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    net_change = workers_2023 - workers_2015,
    pct_change = net_change / workers_2015
  ) %>%
  mutate(mc_status_change = factor(mc_status_change, levels = c(
    "Stayed MC", "Entered MC", "Exited MC", "Stayed Non-MC"
  ))) %>%
  arrange(mc_status_change) %>%
  bind_rows(
    summarise(.,
              mc_status_change = factor("Total"),
              n_occupations    = sum(n_occupations),
              workers_2015     = sum(workers_2015),
              workers_2023     = sum(workers_2023),
              net_change       = sum(net_change),
              pct_change       = sum(net_change) / sum(workers_2015)
    )
  )

# ----------- looking at full-time/part-time rates ---------------

# For entered-MC occupations, what % are part-time?
entered_mc_worktype <- acs_2023_final %>%
  filter(OCCP %in% entered_mc$OCCP) %>%
  mutate(weeks_50plus = case_when(
    year == 2015 & WKW == "1" ~ TRUE,
    year == 2023 & as.numeric(WKWN) >= 50 ~ TRUE,
    TRUE ~ FALSE),
    
    hours34plus = as.numeric(WKHP) >= 34,
    
    is_full_time = if_else(weeks_50plus & hours34plus, TRUE, FALSE)
  ) %>%
  group_by(is_full_time) %>%
  summarise(workers = sum(PWGTP, na.rm = TRUE)) %>%
  mutate(pct = 100 * workers / sum(workers))

print(entered_mc_worktype)


# For exit-MC occupations, what % are part-time?
exit_mc_worktype <- acs_2023_final %>%
  filter(OCCP %in% exited_mc$OCCP) %>%
  mutate(weeks_50plus = case_when(
    year == 2015 & WKW == "1" ~ TRUE,
    year == 2023 & as.numeric(WKWN) >= 50 ~ TRUE,
    TRUE ~ FALSE),
    
    hours34plus = as.numeric(WKHP) >= 34,
    
    is_full_time = if_else(weeks_50plus & hours34plus, TRUE, FALSE)) %>%
  group_by(is_full_time) %>%
  summarise(workers = sum(PWGTP, na.rm = TRUE)) %>%
  mutate(pct = 100 * workers / sum(workers))

print(exit_mc_worktype)


# For stayed-MC occupations, what % are part-time?
stayed_mc_worktype <- acs_2015_final %>%
  filter(OCCP %in% stayed_mc$OCCP) %>%
  mutate(weeks_50plus = case_when(
    year == 2015 & WKW == "1" ~ TRUE,
    year == 2023 & as.numeric(WKWN) >= 50 ~ TRUE,
    TRUE ~ FALSE),
    
    hours34plus = as.numeric(WKHP) >= 34,
    
    is_full_time = if_else(weeks_50plus & hours34plus, TRUE, FALSE)) %>%
  group_by(is_full_time) %>%
  summarise(workers = sum(PWGTP, na.rm = TRUE)) %>%
  mutate(pct = 100 * workers / sum(workers))

print(stayed_mc_worktype)


# For entered-MC occupations, what % are part-time?
stayed_mc_worktype_2023 <- acs_2023_final %>%
  filter(OCCP %in% stayed_mc$OCCP) %>%
  mutate(weeks_50plus = case_when(
    year == 2015 & WKW == "1" ~ TRUE,
    year == 2023 & as.numeric(WKWN) >= 50 ~ TRUE,
    TRUE ~ FALSE),
    
    hours34plus = as.numeric(WKHP) >= 34,
    
    is_full_time = if_else(weeks_50plus & hours34plus, TRUE, FALSE)) %>%
  group_by(is_full_time) %>%
  summarise(workers = sum(PWGTP, na.rm = TRUE)) %>%
  mutate(pct = 100 * workers / sum(workers))

print(stayed_mc_worktype_2023)


overall_middle_class_worktype_2015 <- acs_2015_final %>%
  mutate(weeks_50plus = case_when(
    year == 2015 & WKW == "1" ~ TRUE,
    year == 2023 & as.numeric(WKWN) >= 50 ~ TRUE,
    TRUE ~ FALSE),
    
    hours34plus = as.numeric(WKHP) >= 34,
    
    is_full_time = if_else(weeks_50plus & hours34plus, TRUE, FALSE)) %>%
  group_by(is_full_time) %>%
  summarise(workers = sum(PWGTP, na.rm = TRUE)) %>%
  mutate(pct = 100 * workers / sum(workers))

print(overall_middle_class_worktype_2015)

overall_middle_class_worktype <- acs_2023_final %>%
  mutate(weeks_50plus = case_when(
    year == 2015 & WKW == "1" ~ TRUE,
    year == 2023 & as.numeric(WKWN) >= 50 ~ TRUE,
    TRUE ~ FALSE),
    
    hours34plus = as.numeric(WKHP) >= 34,
    
    is_full_time = if_else(weeks_50plus & hours34plus, TRUE, FALSE)) %>%
  group_by(is_full_time) %>%
  summarise(workers = sum(PWGTP, na.rm = TRUE)) %>%
  mutate(pct = 100 * workers / sum(workers))

print(overall_middle_class_worktype)

# check significance
library(srvyr)
library(survey)

acs_design <- acs_2023_final %>%
  filter(OCCP %in% entered_mc$OCCP | OCCP %in% exited_mc$OCCP | OCCP %in% stayed_mc$OCCP) %>%
  mutate(
    group = case_when(
      OCCP %in% entered_mc$OCCP ~ "Entered MC",
      OCCP %in% exited_mc$OCCP ~ "Exited MC",
      OCCP %in% stayed_mc$OCCP ~ "Stayed MC"
    ),
    weeks_50plus = if_else(as.numeric(WKWN) >= 50, TRUE, FALSE),
    hours34plus = as.numeric(WKHP) >= 34,
    is_full_time = if_else(weeks_50plus & hours34plus, "FT", "PT")
  ) %>%
  as_survey_design(weights = PWGTP)

svychisq(~group + is_full_time, design = acs_design, statistic = "Wald")
