# 02_funct_Plots.R
# Functions used to build the male vs. female visualizations.

# Shared colors so Male/Female look the same in every plot
sex_colors <- c("Male" = "#2C7FB8", "Female" = "#E6550D")

# ---------------------------------------------------------------------------
# plot_sex_comparison()
# Grouped bar chart (Male vs. Female) with 95% CI error bars.
# Inputs:
#   data      - data frame with columns age_group, outcome, sex, percentage,
#               ci_low, ci_high
#   title     - plot title (character)
# Output: a ggplot object
# ---------------------------------------------------------------------------
plot_sex_comparison <- function(data, title) {
  ggplot(data, aes(x = outcome, y = percentage, fill = sex)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                  position = position_dodge(width = 0.8), width = 0.2) +
    geom_text(aes(label = paste0(percentage, "%"), y = ci_high),
              position = position_dodge(width = 0.8), vjust = -0.5, size = 3) +
    facet_wrap(~ age_group, scales = "free_y") +
    scale_fill_manual(values = sex_colors) +
    scale_x_discrete(labels = scales::label_wrap(12)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(title = title, x = NULL, y = "Percent in the past year (%)",
         fill = "Sex",
         caption = "Source: SAMHSA NSDUH 2022-2024 (Appendix Table 1). Bars show 95% CIs.") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "top")
}

# ---------------------------------------------------------------------------
# plot_counts()
# Grouped bar chart of estimated number of people (in thousands).
# Inputs: data (needs number_thousands), title
# Output: a ggplot object
# ---------------------------------------------------------------------------
plot_counts <- function(data, title) {
  ggplot(data, aes(x = outcome, y = number_thousands, fill = sex)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    facet_wrap(~ age_group, scales = "free_y") +
    scale_fill_manual(values = sex_colors) +
    scale_x_discrete(labels = scales::label_wrap(12)) +
    scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.1))) +
    labs(title = title, x = NULL, y = "People (thousands)", fill = "Sex",
         caption = "Source: SAMHSA NSDUH 2022-2024 (Appendix Table 1).") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "top")
}

# ---------------------------------------------------------------------------
# compute_sex_differences()
# Builds one row per age group/outcome with the Female - Male difference
# (percentage points), an approximate 95% CI for that difference, and the
# female-to-male prevalence ratio.
# The standard error of each estimate is backed out of its published CI:
#   SE = (ci_high - ci_low) / (2 * 1.96)
# and the difference SE is sqrt(SE_f^2 + SE_m^2) (assumes independence).
# Input: data (same format as above). Output: a tibble.
# ---------------------------------------------------------------------------
compute_sex_differences <- function(data) {
  data %>%
    mutate(se = (ci_high - ci_low) / (2 * 1.96)) %>%
    select(age_group, outcome, sex, percentage, se) %>%
    pivot_wider(names_from = sex, values_from = c(percentage, se)) %>%
    mutate(diff_pp  = percentage_Female - percentage_Male,
           diff_se  = sqrt(se_Female^2 + se_Male^2),
           diff_low = diff_pp - 1.96 * diff_se,
           diff_high = diff_pp + 1.96 * diff_se,
           ratio_F_to_M = percentage_Female / percentage_Male)
}

# ---------------------------------------------------------------------------
# plot_sex_difference()
# Dot-and-whisker plot of Female - Male difference in percentage points.
# Points to the right of 0 = higher among females; left = higher among males.
# Input: diff_data (output of compute_sex_differences()), title
# Output: a ggplot object
# ---------------------------------------------------------------------------
plot_sex_difference <- function(diff_data, title) {
  ggplot(diff_data, aes(x = diff_pp, y = outcome, color = age_group)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_pointrange(aes(xmin = diff_low, xmax = diff_high),
                    position = position_dodge(width = 0.5)) +
    scale_color_manual(values = c("Adolescents (12-17)" = "#756BB1",
                                  "Adults (18+)" = "#31A354")) +
    scale_y_discrete(labels = scales::label_wrap(18)) +
    labs(title = title,
         x = "Female minus male (percentage points)", y = NULL, color = NULL,
         caption = "Approximate 95% CIs derived from published CIs.") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "top")
}
