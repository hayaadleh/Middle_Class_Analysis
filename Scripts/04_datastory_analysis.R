# =============================================================================
# Data Story Analysis
# New York City's Middle Class
# June 11, 2026.
# =============================================================================

library(tidyverse)

# =============================================================================
# Main Body -- Middle Class Occupational Status Change
# =============================================================================

# this file comes from "Scripts/comparing_occupations.R"
comparison <- readxl::read_excel("Outputs/comparison_final.xlsx")

library(gt)
library(scales)

mc_summary <- comparison %>%
  filter(!is.na(workers_2015), !is.na(workers_2023)) %>%
  group_by(mc_status_change) %>%
  summarise(n_occupations = n(),
            workers_2015  = sum(workers_2015, na.rm = TRUE),
            workers_2023  = sum(workers_2023, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(pct_change = (workers_2023 - workers_2015) / workers_2015,
         mc_status_change = recode(mc_status_change, 
                                   "Stayed MC" = "Stayed Middle-Class",
                                   "Entered MC" = "Entered Middle-Class",
                                   "Exited MC" = "Exited Middle-Class",
                                   "Stayed Non-MC"= "Stayed Non-Middle-Class" )) %>%
  mutate(mc_status_change = factor(mc_status_change, levels = c( 
    "Stayed Middle-Class", "Entered Middle-Class", 
    "Exited Middle-Class", "Stayed Non-Middle-Class"))) %>%
  arrange(mc_status_change) %>%
  bind_rows(summarise(.,
                      mc_status_change = factor("Total"),
                      n_occupations = sum(n_occupations),
                      workers_2015 = sum(workers_2015), workers_2023 = sum(workers_2023),
                      pct_change = (sum(workers_2023) - sum(workers_2015)) / sum(workers_2015))) %>% 
  print()

# =============================================================================
# Paragraph 2 -- Full-Time vs Part-Time Analysis
# =============================================================================

acs_2015_final <- read.csv("Outputs/acs_2015_final.csv")
acs_2023_final <- read.csv("Outputs/acs_2023_final.csv")

# Create seperate files
stayed_mc <- comparison %>% filter(mc_status_change == "Stayed MC")
exited_mc <- comparison %>% filter(mc_status_change == "Exited MC")
entered_mc <- comparison %>% filter(mc_status_change == "Entered MC")

# For entered-MC occupations, what % are part-time?
entered_mc_worktype <- acs_2023_final %>%
  filter(OCCP %in% entered_mc$OCCP) %>%
  mutate(weeks_50plus = case_when(
    year == 2015 & WKW == "1" ~ TRUE,
    year == 2023 & as.numeric(WKWN) >= 50 ~ TRUE,
    TRUE ~ FALSE),
    hours34plus = as.numeric(WKHP) >= 34,
    is_full_time = if_else(weeks_50plus & hours34plus, TRUE, FALSE)) %>%
  group_by(is_full_time) %>%
  summarise(workers = sum(PWGTP, na.rm = TRUE)) %>%
  mutate(pct = 100 * workers / sum(workers))

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

# For stayed-MC occupations, what % are part-time?
stayed_mc_worktype_2015 <- acs_2015_final %>%
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

overall_middle_class_worktype_2023 <- acs_2023_final %>%
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
print(entered_mc_worktype)
print(stayed_mc_worktype_2015)
print(stayed_mc_worktype_2023)
print(overall_middle_class_worktype_2023)

pt_data <- bind_rows(
  entered_mc_worktype %>% mutate(group = "Entered Middle-Class"),
  exit_mc_worktype  %>% mutate(group = "Exited Middle-Class"),
  stayed_mc_worktype_2023  %>% mutate(group = "Stayed Middle-Class"),
  overall_middle_class_worktype_2023 %>% mutate(group = "All NYC Workers (2023)")) %>%
  mutate(type = if_else(is_full_time, "Full-time", "Part-time"),
         group = factor(group, levels = c(
           "Entered Middle-Class", "Exited Middle-Class",
           "Stayed Middle-Class", "All NYC Workers (2023)")))

fig2.fulltime_partime <- ggplot(pt_data, aes(x = group, y = pct, fill = type)) +
  geom_col(position = "stack", width = 0.5) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            position = position_stack(vjust = 0.5),
            size = 3.5, fontface = "bold", color = "white") +
  scale_fill_manual(values = c("Full-time" = "#104E8B", "Part-time" = "#e74c3c")) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title   = "Full-Time vs. Part-Time Share by Middle-Class Status",
       x = NULL, y = NULL, fill = NULL,
       caption = "Source: ACS PUMS microdata.") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 10, face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) 

print(fig2.fulltime_partime)
ggsave("Plots/fig2.fulltime_partime.png")

# =============================================================================
# Paragraph 3 -- Wage distribution
# =============================================================================

# Load the analysis pool, which is full-time workers aged 18-64 with positive earnings.
analysis_pool <- readRDS("Outputs/analysis_pool.RDS")

# Add CPI deflators to 2023 dollars
# Source: BLS CPI-U https://www.bls.gov/cpi/
cpi <- tibble(
  year  = c(2015, 2023),
  cpi_u = c(237.017, 304.702)) %>%
  mutate(deflator = 304.702 / cpi_u)

analysis_pool <- analysis_pool %>%
  left_join(cpi, by = "year") %>%
  mutate(adj_wage_real = adj_wage * deflator)

wt_quantile <- function(x, w, probs) {
  o <- order(x); x <- x[o]; w <- w[o]
  cum <- cumsum(w) / sum(w)
  sapply(probs, function(p) x[which(cum >= p)[1]])
}

# create NYC decile cuts by year
nyc_cuts <- analysis_pool %>%
  filter(adj_wage_real > 0, !is.na(adj_wage_real),
         year %in% c(2015, 2023)) %>%
  group_by(year) %>%
  summarise(cuts = list(wt_quantile(adj_wage_real, PWGTP,
                                    probs = seq(0, 1, 0.1))),
            .groups = "drop")

# Assign deciles and compute MC shares
wage_deciles <- analysis_pool %>%
  filter(is_middle_class == TRUE, adj_wage_real > 0,
         !is.na(adj_wage_real), year %in% c(2015, 2023)) %>%
  group_by(year) %>%
  group_modify(~ {
    cuts <- nyc_cuts$cuts[nyc_cuts$year == .y$year][[1]]
    .x %>% mutate(decile = cut(adj_wage_real, breaks = cuts,
                               labels = paste0("D", 1:10),
                               include.lowest = TRUE))
  }) %>%
  ungroup() %>%
  filter(!is.na(decile)) %>%
  group_by(year, decile) %>%
  summarise(workers = sum(PWGTP), .groups = "drop") %>%
  group_by(year) %>%
  mutate(share = workers / sum(workers)) %>%
  ungroup()

cat("=== Share of MC Workers: Bottom vs Top Half of NYC Distribution ===\n")
wage_deciles %>%
  mutate(half = if_else(as.integer(substr(decile, 2, 3)) <= 5,
                        "Bottom half (D1-D5)", "Top half (D6-D10)")) %>%
  group_by(year, half) %>%
  summarise(share = scales::percent(sum(share), accuracy = 0.1),
            .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = share) %>%
  print()

# Plot
fig3.wage_decile <- ggplot(wage_deciles, aes(x = decile, y = share, fill = as.factor(year))) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +
  geom_vline(xintercept = 5.5, linetype = "dashed", color = "grey40", linewidth = 0.6) +
  annotate("text", x = 2.8, y = max(wage_deciles$share) + 0.005,
           label = "Bottom half of NYC wages", size = 3, color = "grey40", fontface = "italic") +
  annotate("text", x = 7.8, y = max(wage_deciles$share) + 0.005,
           label = "Top half of NYC wages", size = 3, color = "grey40", fontface = "italic") +
  scale_fill_manual(values = c("2015" = "#104E8B", "2023" = "#e74c3c"),
                    labels = c("2015", "2023")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.08))) +
  labs(title = "Position of middle-class workers within the citywide wage distribution",
       x = "NYC Wage Decile (D1 = lowest 10%, D10 = highest 10%)",
       y = "Share of Middle-Class Workforce",
       fill = NULL,
       caption = "Source: ACS microdata. Full-time workers only. CPI-U adjusted to 2023 dollars.") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10, color = "grey30"),
        legend.position = "top",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())

print(fig3.wage_decile)
ggsave("Plots/fig3.wage_decile.png")


# =============================================================================
# Paragraph 4 -- Race and Wage
# =============================================================================

wt_med <- function(x, w) {
  o <- order(x); x <- x[o]; w <- w[o]
  x[which(cumsum(w)/sum(w) >= 0.5)[1]]
}

# MC wage distribution in 2023
mc_wages <- analysis_pool %>%
  filter(is_middle_class, year == 2023,
         adj_wage_real > 0, !is.na(adj_wage_real)) %>%
  select(adj_wage_real, PWGTP) %>%
  arrange(adj_wage_real) %>%
  mutate(cum_pct = cumsum(PWGTP) / sum(PWGTP) * 100)

# Per occupation: median wage + where it sits in MC distribution
occ <- analysis_pool %>%
  filter(is_middle_class, year == 2023, !is.na(occupation), 
         !is.na(race_eth), adj_wage_real > 0, !is.na(adj_wage_real)) %>%
  group_by(occupation) %>%
  summarise(occ_median = wt_med(adj_wage_real, PWGTP), 
            pct_nonwhite = sum(PWGTP[race_eth != "White"]) / sum(PWGTP) * 100,
            n_workers = sum(PWGTP), .groups = "drop") %>%
  mutate(mc_percentile = map_dbl(occ_median, function(w) {
    mc_wages$cum_pct[which.min(abs(mc_wages$adj_wage_real - w))]}))

# Benchmark: overall MC non-white share
b_nonwhite <- analysis_pool %>%
  filter(is_middle_class, year == 2023, !is.na(race_eth)) %>%
  summarise(p = sum(PWGTP[race_eth != "White"]) / sum(PWGTP) * 100) %>%
  pull(p)

cat(sprintf("MC avg non-white: %.1f%%\n\n", b_nonwhite))

occ %>%
  arrange(mc_percentile) %>%
  mutate(occ_median = scales::dollar(round(occ_median)),
         pct_nonwhite = paste0(round(pct_nonwhite, 1), "%"),
         mc_percentile = paste0(round(mc_percentile, 1), "th"),
         n_workers = format(round(n_workers), big.mark = ",")) %>%
  select(occupation, occ_median, mc_percentile, pct_nonwhite, n_workers) %>%
  print(n = Inf, width = Inf)

fig4.occp_race <- ggplot(occ, aes(x = pct_nonwhite, y = mc_percentile, size = n_workers)) +
  annotate("rect", xmin = b_nonwhite, xmax = 100, ymin = -Inf, ymax = 50,
           fill = "#c0392b", alpha = 0.04) +
  annotate("rect", xmin = 0, xmax = b_nonwhite, ymin = 50, ymax = Inf,
           fill = "#2c6fad", alpha = 0.04) +
  geom_vline(xintercept = b_nonwhite, linetype = "dashed",
             color = "grey40", linewidth = 0.5) +
  geom_hline(yintercept = 50, linetype = "dashed",
             color = "grey40", linewidth = 0.5) +
  annotate("text", x = 1, y = 51.5,
           label = "50th percentile of MC wages",
           hjust = 0, size = 2.8, color = "grey40") +
  annotate("text", x = b_nonwhite + 0.5, y = 1,
           label = paste0("MC avg non-white: ", round(b_nonwhite, 1), "%"),
           hjust = 0, size = 2.8, color = "grey40") +
  geom_point(aes(color = pct_nonwhite >= b_nonwhite & mc_percentile < 50),
             alpha = 0.82) +
  geom_label_repel(
    aes(label = occupation,
        color = pct_nonwhite >= b_nonwhite & mc_percentile < 50),
    size = 2.8, fontface = "bold", fill = "white",
    label.padding = unit(0.12, "lines"),
    box.padding = 0.35, max.overlaps = 25, show.legend = FALSE) +
  scale_color_manual(
    values = c("TRUE" = "#c0392b", "FALSE" = "#2c6fad"),
    labels = c("TRUE" = "High non-white, below MC median",
               "FALSE" = "Other occupations"),
    name = NULL) +
  scale_size_continuous(range = c(3, 16), labels = scales::comma, name = "Worker Count") +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100),
                     labels = function(x) paste0(x, "th percentile")) +
  scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100)) +
  labs(title = "Share of Non-White Workers by Occupation and Position in the Middle-Class Wage Distribution",
       subtitle = "Bubble size = workers | Dashed lines = Middle-Class wage median and racial composition average",
       x  = "% Non-White Workers in Occupation",
       y  = "Percentile in Middle-Class Wage Distribution",
       caption = "Source: ACS microdata. Middle-class workers only. Employment-weighted median. CPI-U adjusted to 2023 dollars.") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        legend.position  = "bottom",
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 9, color = "grey40"),
        plot.caption = element_text(size = 8, color = "grey50"))

print(fig4.occp_race)
ggsave("Plots/fig4.occp_race.png")
