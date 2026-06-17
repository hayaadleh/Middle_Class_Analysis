# ==============================================================================
# Part 2: ACS Labor Force Application
# Map ACS PUMS occupation codes to OEWS/ONET middle-class flags
# Author: Haya Adleh
# ==============================================================================

library(tidyverse)
library(spatstat.geom)

# ----------- OEWS/ONET Middle-Class Jobs classification -----------
# import the final OEWS/ONET middle-class classification files
occ_id_2015 <- read.csv("Outputs/occ_id_2015.csv") %>% 
  mutate(SOC6 = as.numeric(SOC6))
occ_id_2023 <- read.csv("Outputs/occ_id_2023.csv") %>% 
  mutate(SOC6 = as.numeric(SOC6))

# -------------  Setup and Load ---------------

# DON'T RUN THIS --- its too big. Below I run it, extract my wanted variables, 
# apply my filters, then save the individual yearly files. See below to upload them.
# 
# The dataset below has ACS microdata from 2014 through 2024, and it was originally 
# parsed from Anne's ACS folder and joined for an earlier analysis. It came from this
# script: "Z:\Adleh_work\ACS.Labor.Analysis - Copy\01_load_recode_1yr.R" lines 7:47
# 
# acs_data <- readRDS("Datasets/ACS_raw_2014-2024.rds")
# 
# # Filter to civilian employed only (ESR 1 = at work, 2 = with job not at work)
# prep_acs <- function(data, target_year) {
#   data %>%
#     filter(year == target_year, ESR %in% c("1", "2")) %>%
#     select(year, SERIALNO, SPORDER, PWGTP, OCCP, AGEP, SEX, RAC1P, HISP, CIT, SCHL, 
#            ESR, ADJINC, WAGP, WKWN, WKW, WKHP) %>% 
#     mutate(OCCP = as.numeric(OCCP))}
# 
# acs_2015 <- prep_acs(acs_data, 2015)
# acs_2023 <- prep_acs(acs_data, 2023)


# saveRDS(acs_2015, "Outputs/acs_2015.rds")
# saveRDS(acs_2023, "Outputs/acs_2023.rds")


# --------- START HERE -- upload the yearly ACS files
acs_2015 <- readRDS("Outputs/acs_2015.rds")
acs_2023 <- readRDS("Outputs/acs_2023.rds")

cat("ACS 2015 persons:", nrow(acs_2015), "\n")
cat("ACS 2023 persons:", nrow(acs_2023), "\n")


# ---------- ACS 2023: Map to OEWS Middle-Class Flag ------------

# Load NEM-to-ACS crosswalk 2023 
# this file maps BLS SOC codes (2018 vintage) to Census OCCP codes (2018 vintage).
soc_acs_crosswalk <- readxl::read_xlsx("Datasets/nem-occcode-acs-crosswalk.xlsx") %>%
  rename(SOC = 2, OCCP = 4) %>%
  mutate(SOC = str_remove_all(str_sub(SOC, 1, 7), "-"),
         OCCP = as.numeric(OCCP),
         SOC = as.numeric(SOC))

cat("SOC crosswalk rows:", nrow(soc_acs_crosswalk), "\n")
cat("Unique OCCP codes in SOC crosswalk:", n_distinct(soc_acs_crosswalk$OCCP), "\n")

# ---- First create an occupation look-up table that maps out SOC to OCCP
# With SOC to ACS crosswalk, join with the OEWS/ONET file using SOC
pre_agg_2023 <- soc_acs_crosswalk %>%
  rename(soc_2018 = SOC) %>%
  left_join(occ_id_2023, by = c("soc_2018" = "SOC6")) # Diagnose HERE

# NOTE: one OCCP code (4-digit) can contain multiple SOC codes (6-digits) with different MW flags.
# The question is: How do we compress multiple SOCs with different education 
# requirements into one OCCP category?

# I use employment-weighted majority. An OCCP is flagged middle-wage
# if > 60% of SOC employment in that OCCP group have middle-class flags
occp_lookup_2023 <- pre_agg_2023 %>% 
  group_by(OCCP) %>% 
  summarise(
    soc_match_count = sum(!is.na(is_middle_class_occ)), # Track if any SOC matched
    
    pct_middle = if_else(
      sum(TOT_EMP, na.rm = TRUE) > 0, # the condition
      sum(TOT_EMP[is_middle_class_occ == TRUE], na.rm = TRUE) / sum(TOT_EMP, na.rm = TRUE), # if TRUE, find % of middle-class from aggregated SOCS 
      mean(is_middle_class_occ, na.rm = TRUE)), # if FALSE, take majority
    
    is_middle_class_occp = if_else(
      sum(!is.na(is_middle_class_occ)) > 0, # Any SOC matched? 
      pct_middle >= 0.6, 
      NA),
    
    # Skills (weighted if employment exists, else un-weighted)
    avg_analytical = if_else(sum(TOT_EMP, na.rm = TRUE) > 0,
                             weighted.mean(Analytical, TOT_EMP, na.rm = TRUE),
                             mean(Analytical, na.rm = TRUE)),
    avg_fundamental = if_else(sum(TOT_EMP, na.rm = TRUE) > 0,
                              weighted.mean(Fundamental, TOT_EMP, na.rm = TRUE),
                              mean(Fundamental, na.rm = TRUE)),
    avg_managerial = if_else(sum(TOT_EMP, na.rm = TRUE) > 0,
                             weighted.mean(Managerial, TOT_EMP, na.rm = TRUE),
                             mean(Managerial, na.rm = TRUE)),
    avg_mechanical = if_else(sum(TOT_EMP, na.rm = TRUE) > 0,
                             weighted.mean(Mechanical, TOT_EMP, na.rm = TRUE),
                             mean(Mechanical, na.rm = TRUE)),
    avg_social = if_else(sum(TOT_EMP, na.rm = TRUE) > 0,
                         weighted.mean(Social, TOT_EMP, na.rm = TRUE),
                         mean(Social, na.rm = TRUE)),
    .groups = "drop") %>%
  mutate(Total_Skill_Sum = avg_analytical + avg_fundamental + avg_managerial + avg_mechanical + avg_social,
         Rel_Analytical = round(avg_analytical / Total_Skill_Sum, 2),
         Rel_Fundamental = round(avg_fundamental / Total_Skill_Sum, 2),
         Rel_Managerial = round(avg_managerial / Total_Skill_Sum, 2),
         Rel_Mechanical = round(avg_mechanical / Total_Skill_Sum, 2),
         Rel_Social = round(avg_social / Total_Skill_Sum, 2))
    
# Diagnostics for OCCP lookup table
cat("Total OCCP codes:", nrow(occp_lookup_2023), "\n")
cat("OCCP codes that matched to SOCs:", sum(occp_lookup_2023$soc_match_count > 0), "\n")
cat("OCCP codes with NO SOC match:", sum(occp_lookup_2023$soc_match_count == 0), "\n")
cat("Middle-class occupations (>= 60%):", sum(occp_lookup_2023$is_middle_class_occp == TRUE, na.rm = TRUE), "\n")
cat("Non-middle-class occupations (< 60%):", sum(occp_lookup_2023$is_middle_class_occp == FALSE, na.rm = TRUE), "\n\n")

# I have 21 ACS occupations from the crosswalk that did not have a match in ONET/OEWS
# I won't do anything about them now. Instead, I will join this look-up with my final ACS 
# survey and see how many of these NA occupations will reappear.

# Now I join the Occupation look-up table with my ACS 2023 file
acs_2023_final <- acs_2023 %>%
  left_join(occp_lookup_2023, by = "OCCP")

# Diagnostics for ACS person-level data
cat("=== ACS Person-Level Diagnostics ===\n")
cat("Total ACS persons from joined file:", nrow(acs_2023_final), "\n")
cat("Total ACS persons from original file *should match above*:", nrow(acs_2023), "\n")
cat("Unweighted % with SOC match:", 
    round(100 * mean(!is.na(acs_2023_final$soc_match_count) & acs_2023_final$soc_match_count > 0, na.rm = TRUE), 1), "%\n")
cat("Weighted % with SOC match:", 
    round(100 * sum(acs_2023_final$PWGTP[!is.na(acs_2023_final$soc_match_count) & acs_2023_final$soc_match_count > 0], na.rm = TRUE) / 
            sum(acs_2023_final$PWGTP, na.rm = TRUE), 1), "%\n")
cat("Weighted count of people with NO SOC match:", 
    formatC(sum(acs_2023_final$PWGTP[is.na(acs_2023_final$soc_match_count) | acs_2023_final$soc_match_count == 0], na.rm = TRUE), 
            format = "d", big.mark = ","), "\n")
cat("Weighted % in middle-class occupations:", 
    round(100 * sum(acs_2023_final$PWGTP[acs_2023_final$is_middle_class_occp == TRUE], na.rm = TRUE) / 
            sum(acs_2023_final$PWGTP, na.rm = TRUE), 1), "%\n")

# List unique OCCP codes without matches
na_occupations <- acs_2023_final %>%
  filter(is.na(soc_match_count) | soc_match_count == 0) %>%
  distinct(OCCP) %>%
  arrange(OCCP)

cat("\nUnique OCCP codes in ACS without SOC matches:", nrow(na_occupations), "\n")
print(na_occupations)

# 14 occupations reappear in the ACS join, they are: 
# 1800: Economist, 2700: Actor, 2723: Umpires, referees, and other sports officials, 
# 2751: Music directors and composers, 2752: Musicians and singers, 2755: Disc jockeys, except radio,
# 2770: Entertainers and performers, sports and related workers, all other, 2805: Broadcast announcers and radio disc jockeys,
# 2810: News analysts, reporters, and journalists, 3256: Nurse anesthetists, 6835: Explosives workers, ordnance handling experts, and blasters,
# 7160: Automotive glass installers and repairers, 8555: Other woodworkers, 9142: Taxi Drivers

# I will manually fill middle_class_flags here. 
acs_2023_final <- acs_2023_final %>%
  mutate(assignment_source = ifelse(is.na(is_middle_class_occp), 
                                    "Manual Assignment", "Primary Assignment")) %>%
  mutate(is_middle_class_occp = case_when(
    OCCP == 1800 ~ FALSE, # Economists require a BA+
    OCCP == 3256 ~ FALSE, # Nurse Anesthetics require a BA+
    OCCP == 2810 ~ FALSE, # News analysts TYPICALLY requires BA+
    OCCP %in% c(2700, 2751, 2752, 2755, 2770, 2805) ~ FALSE, # UNSURE -- Flagging art occupations as false
    OCCP == 2723 ~ TRUE,  # Umpires and referees is said to not need BA+ & median wage @ $38,820
    OCCP == 9142 ~ FALSE, # Taxi Drivers, does not require BA+ but median wage @ $35K -- https://www.bls.gov/oes/2023/may/oes_ny.htm
    OCCP == 7160 ~ TRUE,  # Automotive glass installers, no BA+ req & wage @ $50K
    OCCP == 8555 ~ TRUE,  # Other woodworkers, no BA+ req & wage @ $42K
    OCCP == 6835 ~ TRUE,  # Explosives workers/Blasters
    TRUE ~ is_middle_class_occp))

# Final diagnostics after manual fixes
cat("\n=== ACS 2023 Final Coverage (After Manual Fixes) ===\n")
acs_2023_final %>%
  summarise(
    total_persons = formatC(n(), format = "d", big.mark = ","),
    # Unweighted counts
    has_mc_flag = formatC(sum(!is.na(is_middle_class_occp)), format = "d", big.mark = ","),
    no_mc_flag = formatC(sum(is.na(is_middle_class_occp)), format = "d", big.mark = ","),
    mc_true = formatC(sum(is_middle_class_occp == TRUE, na.rm = TRUE), format = "d", big.mark = ","),
    mc_false = formatC(sum(is_middle_class_occp == FALSE, na.rm = TRUE), format = "d", big.mark = ","),
    # Weighted percentages
    pct_with_flag = paste0(round(100 * sum(PWGTP[!is.na(is_middle_class_occp)], na.rm = TRUE) / sum(PWGTP), 1), "%"),
    pct_mc_true = paste0(round(100 * sum(PWGTP[is_middle_class_occp == TRUE], na.rm = TRUE) / sum(PWGTP), 1), "%"),
    pct_mc_false = paste0(round(100 * sum(PWGTP[is_middle_class_occp == FALSE], na.rm = TRUE) / sum(PWGTP), 1), "%")
  ) %>% print()


cat("Middle-class workers as % of entire ACS sample:", 
    round(100 * sum(acs_2023_final$PWGTP[acs_2023_final$is_middle_class_occp == TRUE], na.rm = TRUE) /
            sum(acs_2023_final$PWGTP, na.rm = TRUE), 1), "%\n")

# the join is done.. save the file below
# write.csv(acs_2023_final, "Outputs/acs_2023_final.csv")

# ------- ACS 2015: Map to OEWS Middle-Wage Flag -----------
# identical methodology to above, just applied using the 2010 vintage to 2015 ACS file

# Load NEM-to-ACS crosswalk 2017 
# NEM-to-ACS crosswalk: maps BLS SOC codes (2010 vintage) to Census OCCP codes (2010 vintage).
soc_acs_crosswalk_2017 <- readxl::read_xlsx("Datasets/nem-occcode-cps-crosswalk 2017.xlsx") %>%
  rename(SOC = 2, OCCP = 4) %>%
  mutate(SOC = str_remove_all(str_sub(SOC, 1, 7), "-"),
         OCCP = as.numeric(OCCP),
         SOC = as.numeric(SOC))

cat("SOC crosswalk rows:", nrow(soc_acs_crosswalk_2017), "\n")
cat("Unique OCCP codes in NEM crosswalk:", n_distinct(soc_acs_crosswalk_2017$OCCP), "\n")

pre_agg_2015 <- soc_acs_crosswalk_2017 %>%
  rename(soc_2010 = SOC) %>%
  left_join(occ_id_2015, by = c("soc_2010" = "SOC6"))

occp_lookup_2015 <- pre_agg_2015 %>% 
  group_by(OCCP) %>%
  summarise(
    soc_match_count = sum(!is.na(is_middle_class_occ)),
    
    pct_middle = if_else(
      sum(TOT_EMP, na.rm = TRUE) > 0,
      sum(TOT_EMP[is_middle_class_occ == TRUE], na.rm = TRUE) / sum(TOT_EMP, na.rm = TRUE),
      mean(is_middle_class_occ, na.rm = TRUE)),
    
    is_middle_class_occp = if_else(
      sum(!is.na(is_middle_class_occ)) > 0,
      pct_middle >= 0.6,
      NA),
    
    avg_analytical = if_else(sum(TOT_EMP, na.rm = TRUE) > 0,
                             weighted.mean(Analytical, TOT_EMP, na.rm = TRUE),
                             mean(Analytical, na.rm = TRUE)),
    avg_fundamental = if_else(sum(TOT_EMP, na.rm = TRUE) > 0,
                              weighted.mean(Fundamental, TOT_EMP, na.rm = TRUE),
                              mean(Fundamental, na.rm = TRUE)),
    avg_managerial = if_else(sum(TOT_EMP, na.rm = TRUE) > 0,
                             weighted.mean(Managerial, TOT_EMP, na.rm = TRUE),
                             mean(Managerial, na.rm = TRUE)),
    avg_mechanical = if_else(sum(TOT_EMP, na.rm = TRUE) > 0,
                             weighted.mean(Mechanical, TOT_EMP, na.rm = TRUE),
                             mean(Mechanical, na.rm = TRUE)),
    avg_social = if_else(sum(TOT_EMP, na.rm = TRUE) > 0,
                         weighted.mean(Social, TOT_EMP, na.rm = TRUE),
                         mean(Social, na.rm = TRUE)),
    .groups = "drop") %>%
  mutate(Total_Skill_Sum = avg_analytical + avg_fundamental + avg_managerial + avg_mechanical + avg_social,
         Rel_Analytical = round(avg_analytical / Total_Skill_Sum, 2),
         Rel_Fundamental = round(avg_fundamental / Total_Skill_Sum, 2),
         Rel_Managerial = round(avg_managerial / Total_Skill_Sum, 2),
         Rel_Mechanical = round(avg_mechanical / Total_Skill_Sum, 2),
         Rel_Social = round(avg_social / Total_Skill_Sum, 2))

cat("Total OCCP codes:", nrow(occp_lookup_2015), "\n")
cat("OCCP codes that matched to SOCs:", sum(occp_lookup_2015$soc_match_count > 0), "\n")
cat("OCCP codes with NO SOC match:", sum(occp_lookup_2015$soc_match_count == 0), "\n")
cat("Middle-class occupations (>= 60%):", sum(occp_lookup_2015$is_middle_class_occp == TRUE, na.rm = TRUE), "\n")
cat("Non-middle-class occupations (< 60%):", sum(occp_lookup_2015$is_middle_class_occp == FALSE, na.rm = TRUE), "\n\n")

acs_2015_final <- acs_2015 %>%
  left_join(occp_lookup_2015, by = "OCCP")

cat("=== ACS Person-Level Diagnostics ===\n")
cat("Total ACS persons from joined file:", nrow(acs_2015_final), "\n")
cat("Total ACS persons from original file *should match above*:", nrow(acs_2015), "\n")
cat("Unweighted % with SOC match:", 
    round(100 * mean(!is.na(acs_2015_final$soc_match_count) & acs_2015_final$soc_match_count > 0, na.rm = TRUE), 1), "%\n")
cat("Weighted % with SOC match:", 
    round(100 * sum(acs_2015_final$PWGTP[!is.na(acs_2015_final$soc_match_count) & acs_2015_final$soc_match_count > 0], na.rm = TRUE) / 
            sum(acs_2015_final$PWGTP, na.rm = TRUE), 1), "%\n")
cat("Weighted count of people with NO SOC match:", 
    formatC(sum(acs_2015_final$PWGTP[is.na(acs_2015_final$soc_match_count) | acs_2015_final$soc_match_count == 0], na.rm = TRUE), 
            format = "d", big.mark = ","), "\n")
cat("Weighted % in middle-class occupations:", 
    round(100 * sum(acs_2015_final$PWGTP[acs_2015_final$is_middle_class_occp == TRUE], na.rm = TRUE) / 
            sum(acs_2015_final$PWGTP, na.rm = TRUE), 1), "%\n")

na_occupations <- acs_2015_final %>%
  filter(is.na(soc_match_count) | soc_match_count == 0) %>%
  distinct(OCCP) %>%
  arrange(OCCP)

cat("\nUnique OCCP codes in ACS without SOC matches:", nrow(na_occupations), "\n")
print(na_occupations)

# 16 occupations reappear in the ACS join, they are: 
# 10: Chief executives, 330: Gaming managers, 1320: Aerospace engineers, 
# 2060: Religious workers, all other, 2700: Actors, 2760: Entertainers and performers, sports and related workers, all other,
# 3120: NA, 5030: Communications equipment operators, all other, 5840: Insurance claims and policy processing clerks
# 6040: Graders and sorters, agricultural products, 6100: Fishing and hunting workers, 6130: Logging workers,
# 6720:Hazardous materials removal workers, 7420: Telecommunications line installers and repairers, 
# 8620: Water and liquid waste treatment plant and system operators, 9200: Locomotive engineers and operators

# I will manually fill middle_class_flags here. 
acs_2015_final <- acs_2015_final %>%
  mutate(assignment_source = ifelse(is.na(is_middle_class_occp), 
                                    "Manual Assignment", "Primary Assignment")) %>%
  mutate(is_middle_class_occp = case_when(
    OCCP == 10   ~ FALSE, # Chief executives, requires BA+
    OCCP == 1320 ~ FALSE, # Aerospace engineers, high median wages
    OCCP %in% c(2700, 2760) ~ FALSE, # Actors, Musicians, Entertainers --- UNSURE, flagged as NA
    OCCP == 330  ~ FALSE, # Gaming managers -- also UNSURE
    OCCP == 2060 ~ FALSE, # Religious workers, all other -- also UNSURE
    OCCP == 3120 ~ FALSE, # Podiatrist, requires BA+
    OCCP == 7420 ~ FALSE,  # Telecommunications line installers and repairers -- median wage @ $130K
    OCCP == 8620 ~ TRUE,  # Water and liquid waste treatment plant operators -- No BA+ and wage @ $66K
    OCCP == 9200 ~ TRUE,  # Locomotive engineers and operators, no BA+ req and wage @ $93K
    OCCP == 6720 ~ TRUE,  # Hazardous materials removal workers, no BA+ req and wage @ $72K
    OCCP == 5840 ~ TRUE,  # Insurance claims and policy processing clerks, no BA+ req and wage @ $57K
    OCCP == 5030 ~ TRUE,  # Communications equipment operators, median wage @ $50K
    OCCP %in% c(6040, 6100, 6130) ~ TRUE, # Agricultural/Logging/Fishing
    TRUE ~ is_middle_class_occp
  ))

cat("\n=== ACS 2015 Final Coverage (After Manual Fixes) ===\n")
acs_2015_final %>%
  summarise(
    total_persons = formatC(n(), format = "d", big.mark = ","),
    has_mc_flag = formatC(sum(!is.na(is_middle_class_occp)), format = "d", big.mark = ","),
    no_mc_flag = formatC(sum(is.na(is_middle_class_occp)), format = "d", big.mark = ","),
    mc_true = formatC(sum(is_middle_class_occp == TRUE, na.rm = TRUE), format = "d", big.mark = ","),
    mc_false = formatC(sum(is_middle_class_occp == FALSE, na.rm = TRUE), format = "d", big.mark = ","),
    pct_with_flag = paste0(round(100 * sum(PWGTP[!is.na(is_middle_class_occp)], na.rm = TRUE) / sum(PWGTP), 1), "%"),
    pct_mc_true = paste0(round(100 * sum(PWGTP[is_middle_class_occp == TRUE], na.rm = TRUE) / sum(PWGTP), 1), "%"),
    pct_mc_false = paste0(round(100 * sum(PWGTP[is_middle_class_occp == FALSE], na.rm = TRUE) / sum(PWGTP), 1), "%")
  ) %>% print()

cat("Middle-class workers as % of entire ACS sample:", 
    round(100 * sum(acs_2015_final$PWGTP[acs_2015_final$is_middle_class_occp == TRUE], na.rm = TRUE) /
            sum(acs_2015_final$PWGTP, na.rm = TRUE), 1), "%\n")

# write.csv(acs_2015_final, "Outputs/acs_2015_final.csv")

# ------ Create a summary table for a Progress Report -----
progress_summary <- data.frame(
  Metric = c("Total Sample (Persons)", "Weighted Match Quality (% of Workforce Covered)", "Middle-Wage Workforce Share (%)"),
  `2015_Result` = c(
    nrow(acs_2015_final),
    round(100 * sum(acs_2015_final$PWGTP[!is.na(acs_2015_final$soc_match_count) & acs_2015_final$soc_match_count > 0], na.rm = TRUE) / sum(acs_2015_final$PWGTP), 1),
    round(100 * sum(acs_2015_final$PWGTP[acs_2015_final$is_middle_class_occp == TRUE], na.rm = TRUE) / sum(acs_2015_final$PWGTP), 1)),
  `2023_Result` = c(
    nrow(acs_2023_final),
    round(100 * sum(acs_2023_final$PWGTP[!is.na(acs_2023_final$soc_match_count) & acs_2023_final$soc_match_count > 0], na.rm = TRUE) / sum(acs_2023_final$PWGTP), 1),
    round(100 * sum(acs_2023_final$PWGTP[acs_2023_final$is_middle_class_occp == TRUE], na.rm = TRUE) / sum(acs_2023_final$PWGTP), 1)))

print(progress_summary)

# See "Scripts\sensitivity_analysis.R" for sensitivity analysis