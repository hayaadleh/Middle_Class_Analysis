# =============================================================================
# Sensitivity Analysis
# Function: This analysis tests the robustness of middle-class 
# workforce estimates by varying two key classification parameters: 
# the upper wage threshold (1.5x vs 2.0x median) and the education requirement 
# strictness (simple majority vs 60% threshold). 

# The function applies these alternative parameters to both SOC-level occupation
# classifications and ACS person-level population estimates. At the SOC level,
# it identifies which occupations are reclassified under each scenario and quantifies
# employment impacts. At the ACS level, it recalculates the middle-class workforce share
# by re-aggregating SOC flags to OCCP codes using the same employment-weighted
# majority rule, then mapping updated flags to person records.
# =============================================================================



# Load these before running run_sensitivity_analysis():
library(tidyverse)

# 1. Median wage thresholds
nyc_overall_median_2015 <- 41950 
nyc_overall_median_2023 <- 57270 

# 2. OEWS/ONET occupation data with flags
occ_id_2015 <- read.csv("Outputs/occ_id_2015.csv")
occ_id_2023 <- read.csv("Outputs/occ_id_2023.csv")

# 3. ACS final person-level data (with manual fixes applied)
acs_2015_final <- read.csv("Outputs/acs_2015_final.csv")
acs_2023_final <- read.csv("Outputs/acs_2023_final.csv")


# 4. SOC-to-OCCP crosswalks
soc_acs_crosswalk_2017 <- readxl::read_xlsx("Datasets/nem-occcode-cps-crosswalk 2017.xlsx") %>%
  rename(SOC = 2, OCCP = 4) %>%
  mutate(SOC = str_remove_all(str_sub(SOC, 1, 7), "-"),
         OCCP = as.numeric(OCCP),
         SOC = as.numeric(SOC))

soc_acs_crosswalk <- readxl::read_xlsx("Datasets/nem-occcode-acs-crosswalk.xlsx") %>%
  rename(SOC = 2, OCCP = 4) %>%
  mutate(SOC = str_remove_all(str_sub(SOC, 1, 7), "-"),
         OCCP = as.numeric(OCCP),
         SOC = as.numeric(SOC))


# Sensitivity Analysis Function
run_sensitivity_analysis <- function(occ_id_df, acs_df, crosswalk_df, year) {
  
  overall_median <- if (year == 2015) nyc_overall_median_2015 else nyc_overall_median_2023
  
  scenarios <- list(
    Baseline = list(upper_mult = 2.00, sub_ba_cutoff = occ_id_df$BA_Plus),
    ScenA = list(upper_mult = 1.50, sub_ba_cutoff = 50),
    ScenB = list(upper_mult = 2.00, sub_ba_cutoff = 60)
  )
  
  cat("\n=== SOC-LEVEL SENSITIVITY (", year, ") ===\n")
  
  all_soc_results <- map(names(scenarios), function(scen_name) {
    params <- scenarios[[scen_name]]
    l_bound <- overall_median * 0.67
    u_bound <- overall_median * params$upper_mult
    
    occ_id_df %>%
      mutate(
        sens_is_middle_edu = Sub_BA > params$sub_ba_cutoff,
        sens_is_middle_wage = A_MEDIAN >= l_bound & A_MEDIAN <= u_bound,
        sens_is_middle_class = sens_is_middle_edu & sens_is_middle_wage)
  })
  names(all_soc_results) <- names(scenarios)
  
  soc_summary <- map_df(names(scenarios), function(scen) {
    df <- all_soc_results[[scen]]
    tibble(
      Scenario = scen,
      MC_Occupations = sum(df$sens_is_middle_class, na.rm = TRUE),
      MC_Employment = formatC(sum(df$TOT_EMP[df$sens_is_middle_class == TRUE], na.rm = TRUE), format = "d", big.mark = ",")
    )
  })
  print(soc_summary)
  
  baseline_soc <- all_soc_results[["Baseline"]] %>%
    select(SOC6, OCC_TITLE, A_MEDIAN, Sub_BA, base_mc = sens_is_middle_class)
  
  dropped_scenA <- all_soc_results[["ScenA"]] %>%
    select(SOC6, scen_mc = sens_is_middle_class) %>%
    inner_join(baseline_soc, by = "SOC6") %>%
    filter(base_mc == TRUE & scen_mc == FALSE) %>%
    select(SOC6, OCC_TITLE, A_MEDIAN, Sub_BA) %>%
    arrange(desc(A_MEDIAN))
  
  dropped_scenB <- all_soc_results[["ScenB"]] %>%
    select(SOC6, scen_mc = sens_is_middle_class) %>%
    inner_join(baseline_soc, by = "SOC6") %>%
    filter(base_mc == TRUE & scen_mc == FALSE) %>%
    select(SOC6, OCC_TITLE, A_MEDIAN, Sub_BA) %>%
    arrange(Sub_BA)
  
  cat("\n=== OCCUPATIONS DROPPED IN SCENARIO A (Wage Ceiling 1.5x) ===\n")
  cat("Total Lost:", nrow(dropped_scenA), "\n")
  print(tail(dropped_scenA, 15))
  
  cat("\n=== OCCUPATIONS DROPPED IN SCENARIO B (Sub_BA > 60%) ===\n")
  cat("Total Lost:", nrow(dropped_scenB), "\n")
  print(head(dropped_scenB, 15))
  
  cat("\n=== ACS-LEVEL SENSITIVITY (", year, ") ===\n")
  
  sim_base <- acs_df %>% 
    mutate(Year = year, sens_is_middle_class_occp = is_middle_class_occp) %>% 
    select(Year, OCCP, PWGTP, sens_is_middle_class_occp)
  
  sim_scenA <- map_df(c("ScenA"), function(scen_name) {
    params <- scenarios[[scen_name]]
    l_bound <- overall_median * 0.67
    u_bound <- overall_median * params$upper_mult
    
    dynamic_soc <- occ_id_df %>%
      mutate(
        sens_is_middle_edu = Sub_BA > params$sub_ba_cutoff,
        sens_is_middle_wage = A_MEDIAN >= l_bound & A_MEDIAN <= u_bound,
        sens_is_middle_class = sens_is_middle_edu & sens_is_middle_wage)
    
    dynamic_occp <- crosswalk_df %>%
      left_join(dynamic_soc, by = c("SOC" = "SOC6")) %>%
      group_by(OCCP) %>%
      summarise(
        pct_middle = if_else(
          sum(TOT_EMP, na.rm = TRUE) > 0,
          sum(TOT_EMP[sens_is_middle_class == TRUE], na.rm = TRUE) / sum(TOT_EMP, na.rm = TRUE),
          mean(sens_is_middle_class, na.rm = TRUE)),
        sens_is_middle_class_occp = if_else(
          sum(!is.na(sens_is_middle_class)) > 0,
          pct_middle >= 0.6,
          NA),
        .groups = "drop")
    
    acs_df %>%
      select(OCCP, PWGTP) %>%
      left_join(dynamic_occp, by = "OCCP") %>%
      mutate(Year = year,
             sens_is_middle_class_occp = coalesce(sens_is_middle_class_occp, FALSE))
  })
  
  sim_scenB <- map_df(c("ScenB"), function(scen_name) {
    params <- scenarios[[scen_name]]
    l_bound <- overall_median * 0.67
    u_bound <- overall_median * params$upper_mult
    
    dynamic_soc <- occ_id_df %>%
      mutate(
        sens_is_middle_edu = Sub_BA > params$sub_ba_cutoff,
        sens_is_middle_wage = A_MEDIAN >= l_bound & A_MEDIAN <= u_bound,
        sens_is_middle_class = sens_is_middle_edu & sens_is_middle_wage)
    
    dynamic_occp <- crosswalk_df %>%
      left_join(dynamic_soc, by = c("SOC" = "SOC6")) %>%
      group_by(OCCP) %>%
      summarise(
        pct_middle = if_else(
          sum(TOT_EMP, na.rm = TRUE) > 0,
          sum(TOT_EMP[sens_is_middle_class == TRUE], na.rm = TRUE) / sum(TOT_EMP, na.rm = TRUE),
          mean(sens_is_middle_class, na.rm = TRUE)),
        sens_is_middle_class_occp = if_else(
          sum(!is.na(sens_is_middle_class)) > 0,
          pct_middle >= 0.6,
          NA),
        .groups = "drop")
    
    acs_df %>%
      select(OCCP, PWGTP) %>%
      left_join(dynamic_occp, by = "OCCP") %>%
      mutate(Year = year,
             sens_is_middle_class_occp = coalesce(sens_is_middle_class_occp, FALSE))
  })
  
  all_scenarios <- bind_rows(
    sim_base %>% mutate(Scenario = "Baseline"),
    sim_scenA %>% mutate(Scenario = "ScenA"),
    sim_scenB %>% mutate(Scenario = "ScenB"))
  
  scenario_summary <- all_scenarios %>%
    group_by(Year, Scenario) %>%
    summarise(
      MC_Count = formatC(sum(PWGTP[sens_is_middle_class_occp == TRUE], na.rm = TRUE), format = "d", big.mark = ","),
      Total = formatC(sum(PWGTP), format = "d", big.mark = ","),
      MC_Share = paste0(round(sum(PWGTP[sens_is_middle_class_occp == TRUE], na.rm = TRUE) / sum(PWGTP) * 100, 1), "%"),
      .groups = "drop") %>%
    arrange(Year, Scenario)
  
  print(scenario_summary)
  
  return(scenario_summary)
}

results_2015 <- run_sensitivity_analysis(occ_id_2015, acs_2015_final, soc_acs_crosswalk_2017, 2015)
results_2023 <- run_sensitivity_analysis(occ_id_2023, acs_2023_final, soc_acs_crosswalk, 2023)

all_results <- bind_rows(results_2015, results_2023)
cat("\n=== COMBINED RESULTS ===\n")
print(all_results)


