#
# Main Script Preregistering Data Donation Studies
# Date: 2026-09-24

# We provide the full code but, due to copyright reasons, can only provide some data

library(tidyverse)
library(here)
library(styler)
library(systemfonts)

windowsFonts(Candara = windowsFont("Candara"))

# 01 Importing survey data ----------------------------------------------------------------

# run script to import survey data
source(paste0(here(), "/scripts/01_load_survey.r"))

# 02 Analyze survey data ----------------------------------------------------------------

## 02.1 Formal variables  ----------------------------------------------------------------

# N respondents
nrow(survey)

# timespan of survey
range(survey$date_start, na.rm = TRUE)

## 02.2 Engagement with open science practices  ----------------------------------------------------------------

# % of participants who ever engaged in open_science practices

# labels for the experience items
exp_labels <- c(
  exp_prereg         = "Preregistration/registered report",
  exp_share_code     = "Shared code",
  exp_share_data     = "Shared data",
  exp_share_material = "Shared materials",
  exp_replication    = "Replicated study",
  exp_open_access    = "Open-Access format",
  exp_other          = "Other practices"
)

survey |>
  select(exp_prereg:exp_other) |>
  summarise(across(everything(),
    list(
      pct1 = ~ round(100 * mean(.x == 1, na.rm = TRUE), 1),
      n    = ~ sum(!is.na(.x))
    ),
    .names = "{.col}__{.fn}"
  )) |>
  pivot_longer(everything(),
    names_to = c("variable", ".value"),
    names_sep = "__"
  ) |>
  mutate(label = exp_labels[variable]) |>
  ggplot(aes(x = pct1, y = reorder(label, pct1))) +
  geom_col(fill = "#346C83") +
  geom_text(aes(label = paste0(pct1, "%")),
    hjust = -0.15, size = 3.5, color = "#333333",
    family = "Candara"
  ) +
  scale_x_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.12))) +
  labs(
    x = NULL, y = NULL,
    title = NULL
  ) +
  theme_minimal(base_size = 12, base_family = "Candara") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank()
  )

# Plus other, open responses for other open science practices
survey |>
  filter(!is.na(exp_other_open)) |>
  pull(exp_other_open)

## 02.3 Reasons to not preregister  ----------------------------------------------------------------

# labels for the reason items
prereg_labels <- c(
  prereg_not_thought    = "Did not think about it",
  prereg_unsure_how     = "Unsure what to include",
  prereg_effort         = "Too much effort",
  prereg_uncertainty    = "Too many uncertainties/deviations",
  prereg_not_flexible   = "Would have lowered flexibility",
  prereg_incentive      = "Lacked incentives",
  prereg_not_applicable = "Not applicable to my study",
  prereg_other          = "Other reason"
)

survey |>
  select(prereg_not_thought:prereg_other) |>
  summarise(across(everything(),
    list(
      pct1 = ~ round(100 * mean(.x == 1, na.rm = TRUE), 1),
      n    = ~ sum(!is.na(.x))
    ),
    .names = "{.col}__{.fn}"
  )) |>
  pivot_longer(everything(),
    names_to = c("variable", ".value"),
    names_sep = "__"
  ) |>
  mutate(label = prereg_labels[variable]) |>
  ggplot(aes(x = pct1, y = reorder(label, pct1))) +
  geom_col(fill = "#346C83") +
  geom_text(aes(label = paste0(pct1, "%")),
    hjust = -0.15, size = 3.5, color = "#333333",
    family = "Candara"
  ) +
  scale_x_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.12))) +
  labs(
    x = NULL, y = NULL,
    title = "Reasons for not preregistering"
  ) +
  theme_minimal(base_size = 12, base_family = "Candara") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# Plus other, open responses for why preregistration was not done
survey |>
  filter(!is.na(prereg_other_open)) |>
  pull(prereg_other_open)

## 02.4 What a template should look like  ----------------------------------------------------------------

# % of elements that should be included in preregistration
survey |>
  describe(
    include_design, include_rq, include_sampling, include_data,
    include_measures, include_analysis, include_storage
  ) |>
  # reduce to relevant output
  select(Variable, N, Missing, M, SD)

# visualize seperately
include_labels <- c(
  include_design   = "Study information\n& design",
  include_rq       = "Research\nquestions",
  include_sampling = "Sampling",
  include_data     = "Data\nextraction",
  include_measures = "Measurement\ncreation",
  include_analysis = "Analysis",
  include_storage  = "Data use/\nstorage"
)

# visualize
survey |>
  select(include_design:include_storage) |>
  pivot_longer(everything(), names_to = "variable", values_to = "value") |>
  group_by(variable) |>
  summarise(
    M = mean(value, na.rm = TRUE),
    SD = sd(value, na.rm = TRUE),
    n = sum(!is.na(value)), .groups = "drop"
  ) |>
  mutate(
    label = include_labels[variable],
    label = fct_reorder(label, M, .desc = TRUE)
  ) |>
  ggplot(aes(x = label, y = M)) +
  geom_col(fill = "#346C83", width = 0.7) +
  geom_errorbar(aes(ymin = M - SD, ymax = M + SD),
    width = 0.2, color = "#333333"
  ) +
  geom_text(aes(y = M + SD, label = sprintf("%.2f", M)),
    vjust = -0.6, size = 3.3, color = "#333333",
    family = "Candara"
  ) +
  scale_y_continuous(
    limits = c(0, 6), breaks = 0:5,
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    x = NULL, y = "Mean importance (1–5)",
    title = "Importance of preregistration elements"
  ) +
  theme_minimal(base_size = 12, base_family = "Candara") +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 10)
  )

# Plus other, open responses for which elements should be included
survey |>
  filter(!is.na(include_other_open)) |>
  pull(include_other_open)

## 02.5 For which preregistration steps participants expect problems  ----------------------------------------------------------------

# % of steps where participant expect problems
problem_labels <- c(
  problem_design   = "Study information & design",
  problem_rq       = "Research questions",
  problem_sampling = "Sampling",
  problem_data     = "Data extraction",
  problem_measures = "Measurement creation",
  problem_analysis = "Analysis",
  problem_storage  = "Data use/storage",
  problem_other    = "Other steps"
)

survey |>
  select(problem_design:problem_other) |>
  summarise(across(everything(),
    list(
      pct1 = ~ round(100 * mean(.x == 1, na.rm = TRUE), 1),
      n    = ~ sum(!is.na(.x))
    ),
    .names = "{.col}__{.fn}"
  )) |>
  pivot_longer(everything(),
    names_to = c("variable", ".value"),
    names_sep = "__"
  ) |>
  mutate(label = problem_labels[variable]) |>
  ggplot(aes(x = pct1, y = reorder(label, pct1))) +
  geom_col(fill = "#346C83") +
  geom_text(aes(label = paste0(pct1, "%")),
    hjust = -0.15, size = 3.5, color = "#333333",
    family = "Candara"
  ) +
  scale_x_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.12))) +
  labs(
    x = NULL, y = NULL,
    title = "Steps where participants expect problems"
  ) +
  theme_minimal(base_size = 12, base_family = "Candara") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# no open responses here, so skipped

## 02.6 Which problems they expect  ----------------------------------------------------------------

# design: not mentioned, thus skipped

# research questions and hypotheses
survey |>
  filter(!is.na(problem_rq_open)) |>
  pull(problem_rq_open)

# sampling
survey |>
  filter(!is.na(problem_sampling_open)) |>
  pull(problem_sampling_open)

# data
survey |>
  filter(!is.na(problem_data_open)) |>
  pull(problem_data_open)

# measures
survey |>
  filter(!is.na(problem_measures_open)) |>
  pull(problem_measures_open)

# analysis
survey |>
  filter(!is.na(problem_analysis_open)) |>
  pull(problem_analysis_open)

# storage
survey |>
  filter(!is.na(problem_storage_open)) |>
  pull(problem_storage_open)

## 02.7 Any other ideas respondents had  ----------------------------------------------------------------
survey |>
  filter(!is.na(ideas)) |>
  pull(ideas)
