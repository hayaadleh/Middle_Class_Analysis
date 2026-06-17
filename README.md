# The Middle-Class of New York City -- Read Me

**Purpose**: This project aims to understand the changing structures of the middle-class in New York City through an occupational lens using microdata from the American Community Survey, wage benchmarks from the Occupational Employment & Wage Statistics (OEWS), and education and skill requirements from O*NET datasets. It compares the middle-class workforce between 2015 and 2023, where middle-class is defined separately for each year as occupations that (1) require sub-bachelor's degree education and (2) have median wages falling within that year's middle-wage corridor (0.67x to 2.0x the year-specific overall median wage).

---
### Part 1: Establishing the Middle-Class occupational definition 
- **Script**: Z:\Adleh_work\NYC_Middle_Class_Analysis\Scripts\01_establish_mid_class.R
- **General Function**: This script assigns a middle-class occupational flag to 6-digit SOC occupations. First, each occupation is classified by an educational requirement as either requiring a bachelor’s degree or higher (BA_plus) or less than a bachelor’s degree (Sub_BA). Next, the script calculates the employment-weighted median wage of the OEWS sample for each year and defines a middle-wage corridor bounded by 0.67 times the median wage at the lower threshold and 2.00 times the median wage at the upper threshold. Finally, an occupation is classified as middle class if it both (1) requires no more than a Sub-BA education and (2) has a median wage that falls within the defined middle-wage corridor for that year.
- **Purpose**: The purpose of this classification is to identify occupations that provide middle-class earnings while remaining accessible to workers without a need for a bachelor’s degree.
- **Limitations**:
    - To solve for the mismatch that will occur between occupations present/absent between the ONET and OEWS files, I prioritized having complete OEWS data and left joined the ONET file to it. Because my middle-class definition depends on an employment-weighted median, preserving complete OEWS wage and employment information was prioritized over complete O*NET coverage.
    - Since I identified some occupations from OEWS that were missing an education flag from ONET, I manually assigned those flags using the search engine in ONET.
- **Datasets input**:
  - NYC_Middle_Class_Analysis\Datasets\Education, Training, and Experience10.2015.xlsx – click for source
  - NYC_Middle_Class_Analysis\Datasets\Education, Training, and Experience11.2023.xlsx  – click for source
  - NYC_Middle_Class_Analysis\Datasets\Skills.10.2015.xlsx – click for source
  - NYC_Middle_Class_Analysis\Datasets\Skills11.2023.xlsx – click for source
  - NYC_Middle_Class_Analysis\Datasets\MSA_M2015_dl.xlsx – click for source
  - NYC_Middle_Class_Analysis\Datasets\MSA_M2023_dl.xlsx – click for source
- **Final outputs**: The final outputs’ most important variables include each occupation’s 6-digit SOC code, occupation title, wages, middle education flag, middle wage flag, middle class flag, and skill scores.
  - NYC_Middle_Class_Analysis\Outputs\occ_id_2015.csv
  - NYC_Middle_Class_Analysis\Outputs\occ_id_2015.csv

---
### Part 2: Applying Occupational Middle-Class Flags to ACS Survey Data 
- **Script**: NYC_Middle_Class_Analysis\Scripts\02_acs_mid_class_assignment.R
- **General Function**: This script maps middle-class occupational flags from 6-digit SOC codes to ACS OCCP codes and applies them to individual-level ACS survey data. First, the script uses a crosswalk to link SOC codes to OCCP codes. Because one OCCP can contain multiple SOC codes with different middle-class classifications, the script aggregates SOC-level flags to the OCCP level using an employment-weighted majority rule. The script then joins this OCCP lookup table to person-level ACS data, assigning each worker a middle-class flag based on their occupation.
- **Purpose**: The purpose of this mapping is to translate occupation-level middle-class definitions into population-level estimates by linking SOC classifications to Census microdata, allowing for calculation of the size and composition of the middle-class workforce.
- **Limitations**:
  - A small number of OCCP codes (14 in 2023, 16 in 2015) did not match any SOC in the crosswalk or OEWS file. These occupations were manually classified using BLS median wage and education data to preserve population coverage.
  - The crosswalk vintages differ by year, reflecting Census occupation code updates, which introduces comparability issues across years. This is okay because when I analyze based on occupations, I will group them into bigger 2-digit codes.
- **Datasets input**:
  - NYC_Middle_Class_Analysis\Outputs\occ_id_2015.csv – source: 01_establish_mid_class.R
  - NYC_Middle_Class_Analysis\Outputs\occ_id_2023.csv csv – source: 01_establish_mid_class.R
  - NYC_Middle_Class_Analysis\Outputs\acs_2023.rds – pulled from Census
  - NYC_Middle_Class_Analysis\Outputs\acs_2015.rds – pulled from Census
  - NYC_Middle_Class_Analysis\Datasets\nem-occcode-acs-crosswalk.xlsx – click for source
  - NYC_Middle_Class_Analysis\Datasets\nem-occcode-cps-crosswalk 2017.xlsx – this crosswalk does not exist online anymore; I retrieved it from an email thread from Jihyun.
- **Final outputs**: The final outputs are population-level estimates linked to middle-class flags.
  - NYC_Middle_Class_Analysis\Outputs\acs_2015_final.csv
  - NYC_Middle_Class_Analysis\Outputs\acs_2023_final.csv
  - NYC_Middle_Class_Analysis\Outputs\02_acs_mid_class_assignment_diagnostics
---
### Part 2B: Sensitivity Analysis
- **Script**: NYC_Middle_Class_Analysis\Scripts\sensitivity_analysis.R
- **General Function**: This script tests the robustness of the middle-class workforce estimates by varying two key classification parameters: the upper wage threshold (1.5x vs 2.0x median) and the education requirement strictness (simple majority vs 60% threshold). The function applies these parameters to both SOC-level occupation classifications and ACS person-level population estimates. At the SOC level, it identifies which occupations are reclassified under each scenario and quantifies employment impacts. At the ACS level, it recalculates the middle-class workforce share by re-aggregating SOC flags to OCCP codes using the same employment-weighted majority rule, then mapping updated flags to person records.
- **Purpose**: The purpose is to test the robustness of the methodology applied and diagnose how the resulting middle-class pool changes as these parameters change.
- **Datasets input**:
  - NYC_Middle_Class_Analysis\Outputs\acs_2015_final.csv
  - NYC_Middle_Class_Analysis\Outputs\acs_2023_final.csv
  - NYC_Middle_Class_Analysis\Outputs\occ_id_2015.csv
  - NYC_Middle_Class_Analysis\Outputs\occ_id_2023.csv
  - NYC_Middle_Class_Analysis\Datasets\nem-occcode-acs-crosswalk.xlsx
  - NYC_Middle_Class_Analysis\Datasets\nem-occcode-cps-crosswalk 2017.xlsx
- **Final outputs**: The output displays the number of occupations that are affected by the changing parameters, identifies those occupations on the threshold, and calculates how those changes affect final ACS middle-class counts and shares, per year.
  - NYC_Middle_Class_Analysis\Outputs\sensitivity_analysis_output
---
### Part 3: ACS Middle-Class Labor Force Demographic Classification
- **Script**: NYC_Middle_Class_Analysis\Scripts\03_acs_definitions.R
- **General Function**: This script defines demographic and labor force variables for the ACS sample and creates filtered analysis pools. First, the script standardizes person-level characteristics including gender, race/ethnicity, citizenship, education, age groups, and employment status. It constructs occupational categories from OCCP codes using 2-digit groupings and calculates full-time status based on weeks worked and usual hours. The script then applies multiple filters to create distinct analysis pools: a base pool of prime-age workers (18-64) with positive earnings, a full-time pool (50+ weeks and 34+ hours per week) for primary analysis, and a part-time pool for supplementary analysis.
- **Purpose**: The purpose is to standardize demographic variables across survey years and construct relevant subsamples that isolate the core labor force for middle-class trend analysis while documenting the impact of each filtering decision on sample composition and middle-class share.
- **Limitations**:
  - Filtering to only full-time workers exclude part-time employment, which may undercount middle-class jobs in some sectors. The part-time pool is retained separately to assess this potential bias.
  - The wage cap at $500,000 removes extreme outliers but may exclude a small number of legitimate high earners in middle-class occupations.
- **Datasets input**:
  - NYC_Middle_Class_Analysis\Outputs\acs_2015_final.csv
  - NYC_Middle_Class_Analysis\Outputs\acs_2023_final.csv
- **Final outputs**: The final outputs are defined ACS population-level estimates linked to middle-class flags.
  - NYC_Middle_Class_Analysis\Outputs\acs_classified.rds -- This file preserves the entire employed sample
  - Z:\Adleh_work\NYC_Middle_Class_Analysis\Outputs\analysis_pool.rds -- This file is filtered for prime-age workers (18-64) with positive earnings & full-time work.

