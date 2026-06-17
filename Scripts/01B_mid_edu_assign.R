# ===============================================================================
# Purpose: Manually fill out missing education flags in the joined OEWS/ONET file

# Rationale: To solve for the mismatch that will occur between occupations present/absent between 
# the ONET and OEWS files, I prioritized having complete OEWS data and left joined the ONET 
# file to it. Because my middle-class definition depends on an employment-weighted median, 
# preserving complete OEWS wage and employment information was prioritized over complete O*NET coverage.
# ===============================================================================


# --- 2015 Manual Education Assignment using OCC_TITLE ---
occ_id_2015 <- occ_id_2015_na %>%
  # Mark assignment source
  mutate(assignment_source = ifelse(is.na(is_middle_edu), 
                                    "Manual Assignment", "O*NET Primary")) %>%
  mutate(is_middle_edu = case_when(
    # FALSE: Require BA+ or higher education
    OCC_TITLE == "Legislators" ~ FALSE,
    OCC_TITLE == "Life Scientists, All Other" ~ FALSE,
    OCC_TITLE == "Counselors, All Other" ~ FALSE,
    OCC_TITLE == "Social Workers, All Other" ~ FALSE,
    OCC_TITLE == "Community and Social Service Specialists, All Other" ~ FALSE,
    OCC_TITLE == "Legal Support Workers, All Other" ~ FALSE,
    OCC_TITLE == "Social Sciences Teachers, Postsecondary, All Other" ~ FALSE,
    OCC_TITLE == "Special Education Teachers, Preschool" ~ FALSE,
    OCC_TITLE == "Teachers and Instructors, All Other, Except Substitute Teachers" ~ FALSE,
    OCC_TITLE == "Substitute Teachers" ~ FALSE,
    OCC_TITLE == "Media and Communication Workers, All Other" ~ FALSE,
    OCC_TITLE == "Media and Communication Equipment Workers, All Other" ~ FALSE,
    OCC_TITLE == "Dentists, All Other Specialists" ~ FALSE,
    OCC_TITLE == "Therapists, All Other" ~ FALSE,
    
    # TRUE: Require Sub-BA or skilled trades/service work
    OCC_TITLE == "Drafters, All Other" ~ TRUE,
    OCC_TITLE == "Orderlies" ~ TRUE,
    OCC_TITLE == "First-Line Supervisors of Protective Service Workers, All Other" ~ TRUE,
    OCC_TITLE == "Cooks, All Other" ~ TRUE,
    OCC_TITLE == "Sales and Related Workers, All Other" ~ TRUE,
    OCC_TITLE == "Financial Clerks, All Other" ~ TRUE,
    OCC_TITLE == "Information and Record Clerks, All Other" ~ TRUE,
    OCC_TITLE == "Office and Administrative Support Workers, All Other" ~ TRUE,
    OCC_TITLE == "Helpers, Construction Trades, All Other" ~ TRUE,
    OCC_TITLE == "Precision Instrument and Equipment Repairers, All Other" ~ TRUE,
    OCC_TITLE == "Metal Workers and Plastic Workers, All Other" ~ TRUE,
    OCC_TITLE == "Woodworkers, All Other" ~ TRUE,
    OCC_TITLE == "Photographic Process Workers and Processing Machine Operators" ~ TRUE,
    OCC_TITLE == "Motor Vehicle Operators, All Other" ~ TRUE,
    OCC_TITLE == "Rail Transportation Workers, All Other" ~ TRUE,
    OCC_TITLE == "Transportation Workers, All Other" ~ TRUE,
    OCC_TITLE == "Material Moving Workers, All Other" ~ TRUE,
    OCC_TITLE == "Designers, All Other" ~ TRUE,  # Technical/product design; sub-BA skilled
    OCC_TITLE == "Food Preparation and Serving Related Workers, All Other" ~ TRUE,  # Service work
    OCC_TITLE == "Grounds Maintenance Workers, All Other" ~ TRUE,  # Maintenance work
    OCC_TITLE == "Personal Care and Service Workers, All Other" ~ TRUE,  # Service work
    OCC_TITLE == "Assemblers and Fabricators, All Other" ~ TRUE,  # Manufacturing
    OCC_TITLE == "Food Processing Workers, All Other" ~ TRUE,  # Food service/processing
    OCC_TITLE == "Textile, Apparel, and Furnishings Workers, All Other" ~ TRUE,  # Manufacturing
    
    # Default: keep existing value from O*NET
    TRUE ~ is_middle_edu )) 



# --- 2023 Manual Education Assignment using OCC_TITLE ---
occ_id_2023 <- occ_id_2023_na %>%
  mutate(assignment_source = ifelse(is.na(is_middle_edu), 
                                    "Manual Override", "O*NET Primary")) %>%
  mutate(is_middle_edu = case_when(
    # FALSE: Require BA+ or higher education
    OCC_TITLE == "Public Relations Managers" ~ FALSE,
    OCC_TITLE == "Legislators" ~ FALSE,
    OCC_TITLE == "Fundraising Managers" ~ FALSE,
    OCC_TITLE == "Facilities Managers" ~ FALSE,
    OCC_TITLE == "Education Administrators, All Other" ~ FALSE,
    OCC_TITLE == "Buyers and Purchasing Agents" ~ FALSE,
    OCC_TITLE == "Project Management Specialists" ~ FALSE,
    OCC_TITLE == "Accountants and Auditors" ~ FALSE,
    OCC_TITLE == "Property Appraisers and Assessors" ~ FALSE,
    OCC_TITLE == "Financial and Investment Analysts" ~ FALSE,
    OCC_TITLE == "Financial Risk Specialists" ~ FALSE,
    OCC_TITLE == "Software Developers" ~ FALSE,
    OCC_TITLE == "Marine Engineers and Naval Architects" ~ FALSE,
    OCC_TITLE == "Life Scientists, All Other" ~ FALSE,
    OCC_TITLE == "Psychologists, All Other" ~ FALSE,
    OCC_TITLE == "Geological Technicians, Except Hydrologic Technicians" ~ FALSE,
    OCC_TITLE == "Substance Abuse, Behavioral Disorder, and Mental Health Counselors" ~ FALSE,
    OCC_TITLE == "Counselors, All Other" ~ FALSE,
    OCC_TITLE == "Social Workers, All Other" ~ FALSE,
    OCC_TITLE == "Community and Social Service Specialists, All Other" ~ FALSE,
    OCC_TITLE == "Religious Workers, All Other" ~ FALSE,
    OCC_TITLE == "Social Sciences Teachers, Postsecondary, All Other" ~ FALSE,
    OCC_TITLE == "Postsecondary Teachers, All Other" ~ FALSE,
    OCC_TITLE == "Special Education Teachers, Kindergarten and Elementary School" ~ FALSE,
    OCC_TITLE == "Substitute Teachers, Short-Term" ~ FALSE,
    OCC_TITLE == "Teachers and Instructors, All Other" ~ FALSE,
    OCC_TITLE == "Teaching Assistants, Except Postsecondary" ~ FALSE,
    OCC_TITLE == "Educational Instruction and Library Workers, All Other" ~ FALSE,
    OCC_TITLE == "Media and Communication Workers, All Other" ~ FALSE,
    OCC_TITLE == "Lighting Technicians" ~ FALSE,
    OCC_TITLE == "Media and Communication Equipment Workers, All Other" ~ FALSE,
    OCC_TITLE == "Dentists, All Other Specialists" ~ FALSE,
    OCC_TITLE == "Surgeons, All Other" ~ FALSE,
    OCC_TITLE == "Clinical Laboratory Technologists and Technicians" ~ FALSE,
    OCC_TITLE == "Medical Dosimetrists" ~ FALSE,
    
    # TRUE: Require Sub-BA or skilled trades/service work
    OCC_TITLE == "Farmers, Ranchers, and Other Agricultural Managers" ~ TRUE,
    OCC_TITLE == "Entertainment and Recreation Managers, Except Gambling" ~ TRUE,
    OCC_TITLE == "Architectural and Civil Drafters" ~ TRUE,
    OCC_TITLE == "Drafters, All Other" ~ TRUE,
    OCC_TITLE == "Electrical and Electronic Engineering Technologists and Technicians" ~ TRUE,
    OCC_TITLE == "Calibration Technologists and Technicians" ~ TRUE,
    OCC_TITLE == "Surveying and Mapping Technicians" ~ TRUE,
    OCC_TITLE == "Hydrologic Technicians" ~ TRUE,
    OCC_TITLE == "Legal Support Workers, All Other" ~ TRUE,
    OCC_TITLE == "Artists and Related Workers, All Other" ~ TRUE,
    OCC_TITLE == "Designers, All Other" ~ TRUE,
    OCC_TITLE == "Emergency Medical Technicians" ~ TRUE,
    OCC_TITLE == "Paramedics" ~ TRUE,
    OCC_TITLE == "Medical Records Specialists" ~ TRUE,
    OCC_TITLE == "Health Information Technologists and Medical Registrars" ~ TRUE,
    OCC_TITLE == "First-Line Supervisors of Security Workers" ~ TRUE,
    OCC_TITLE == "First-Line Supervisors of Protective Service Workers, All Other" ~ TRUE,
    OCC_TITLE == "Cooks, All Other" ~ TRUE,
    OCC_TITLE == "Building Cleaning Workers, All Other" ~ TRUE,
    OCC_TITLE == "First-Line Supervisors of Entertainment and Recreation Workers, Except Gambling Services" ~ TRUE,
    OCC_TITLE == "Gambling Service Workers, All Other" ~ TRUE,
    OCC_TITLE == "Entertainment Attendants and Related Workers, All Other" ~ TRUE,
    OCC_TITLE == "Crematory Operators" ~ TRUE,
    OCC_TITLE == "Tour and Travel Guides" ~ TRUE,
    OCC_TITLE == "Personal Care and Service Workers, All Other" ~ TRUE,
    OCC_TITLE == "Sales Representatives of Services, Except Advertising, Insurance, Financial Services, and Travel" ~ TRUE,
    OCC_TITLE == "Sales and Related Workers, All Other" ~ TRUE,
    OCC_TITLE == "Communications Equipment Operators, All Other" ~ TRUE,
    OCC_TITLE == "Financial Clerks, All Other" ~ TRUE,
    OCC_TITLE == "Credit Authorizers, Checkers, and Clerks" ~ TRUE,
    OCC_TITLE == "Information and Record Clerks, All Other" ~ TRUE,
    OCC_TITLE == "Office and Administrative Support Workers, All Other" ~ TRUE,
    OCC_TITLE == "Helpers, Construction Trades, All Other" ~ TRUE,
    OCC_TITLE == "Miscellaneous Construction and Related Workers" ~ TRUE,
    OCC_TITLE == "Precision Instrument and Equipment Repairers, All Other" ~ TRUE,
    OCC_TITLE == "Electrical, Electronic, and Electromechanical Assemblers, Except Coil Winders, Tapers, and Finishers" ~ TRUE,
    OCC_TITLE == "Metal Workers and Plastic Workers, All Other" ~ TRUE,
    OCC_TITLE == "Textile, Apparel, and Furnishings Workers, All Other" ~ TRUE,
    OCC_TITLE == "First-Line Supervisors of Transportation and Material Moving Workers, Except Aircraft Cargo Handling Supervisors" ~ TRUE,
    OCC_TITLE == "Bus Drivers, School" ~ TRUE,
    OCC_TITLE == "Shuttle Drivers and Chauffeurs" ~ TRUE,
    OCC_TITLE == "Motor Vehicle Operators, All Other" ~ TRUE,
    OCC_TITLE == "Railroad Brake, Signal, and Switch Operators and Locomotive Firers" ~ TRUE,
    OCC_TITLE == "Aircraft Service Attendants" ~ TRUE,
    OCC_TITLE == "Transportation Workers, All Other" ~ TRUE,
    OCC_TITLE == "Material Moving Workers, All Other" ~ TRUE,
    OCC_TITLE == "Home Health and Personal Care Aides" ~ TRUE,
    OCC_TITLE == "School Bus Monitors" ~ TRUE,
    OCC_TITLE == "Food Preparation and Serving Related Workers, All Other" ~ TRUE,
    OCC_TITLE == "Grounds Maintenance Workers, All Other" ~ TRUE,
    OCC_TITLE == "Miscellaneous Assemblers and Fabricators" ~ TRUE,
    OCC_TITLE == "Food Processing Workers, All Other" ~ TRUE,
    OCC_TITLE == "Production Workers, All Other" ~ TRUE,
    # Default: keep existing value from O*NET
    TRUE ~ is_middle_edu)) 
