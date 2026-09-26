#
# Main Script Preregistering Data Donation Studies
# Date: 2026-09-24

# We provide the full code but, due to copyright reasons, can only provide some data

library(tidyverse)
library(here)
library(styler)

# 01 Importing survey data ----------------------------------------------------------------

#run script to import survey data
source(paste0(here(), "/scripts/01_load_survey.r"))

# 02 Analyze survey data ----------------------------------------------------------------

## 02.1 Formal variables  ----------------------------------------------------------------

# N respondents
nrow(survey)

# timespan of survey
range(survey$date_start, na.rm = TRUE)

## 02.2 Engagement with open science practices  ----------------------------------------------------------------

# % of participants who ever engaged in open_science practices
survey |>
  select(exp_prereg:exp_other) |>
  summarise(across(everything(),
                   list(
                     pct1 = ~ round(100 * mean(.x == 1, na.rm = TRUE), 1),
                     n    = ~ sum(!is.na(.x))
                   ),
                   .names = "{.col}__{.fn}")) |>
  pivot_longer(everything(),
               names_to = c("variable", ".value"),
               names_sep = "__")

# Plus other, open responses for other open science practices
survey |>
  filter(!is.na(exp_other_open)) |>
  pull(exp_other_open)

## 02.3 Reasons to not preregister  ----------------------------------------------------------------

# % of reasons for why no preregistration was done
survey |>
  select(prereg_not_thought:prereg_other) |>
  summarise(across(everything(),
                   list(
                     pct1 = ~ round(100 * mean(.x == 1, na.rm = TRUE), 1),
                     n    = ~ sum(!is.na(.x))
                   ),
                   .names = "{.col}__{.fn}")) |>
  pivot_longer(everything(),
               names_to = c("variable", ".value"),
               names_sep = "__")

# Plus other, open responses for why preregistration was not done
survey |>
  filter(!is.na(prereg_other_open)) |>
  pull(prereg_other_open)

## 02.4 What a template should look like  ----------------------------------------------------------------

# % of elements that should be included in preregistration
survey |>
  describe(include_design, include_rq, include_sampling, include_data,
           include_measures, include_analysis, include_storage)|>
  
  #reduce to relevant output
  select(Variable, N, Missing, M, SD)
  
# Plus other, open responses for which elements should be included
survey |>
  filter(!is.na(include_other_open)) |>
  pull(include_other_open)

## 02.5 For which preregistration steps participants expect problems  ----------------------------------------------------------------

# % of steps where participant expect problems
survey |>
  select(problem_design:problem_other) |>
  summarise(across(everything(),
                   list(
                     pct1 = ~ round(100 * mean(.x == 1, na.rm = TRUE), 1),
                     n    = ~ sum(!is.na(.x))
                   ),
                   .names = "{.col}__{.fn}")) |>
  pivot_longer(everything(),
               names_to = c("variable", ".value"),
               names_sep = "__")

#no open responses here, so skipped

## 02.6 Which problems they expect  ----------------------------------------------------------------

#design: not mentioned, thus skipped

# research questions and hypotheses
survey |>
  filter(!is.na(problem_rq_open)) |>
  pull(problem_rq_open)

#sampling
survey |>
  filter(!is.na(problem_sampling_open)) |>
  pull(problem_sampling_open)

#data
survey |>
  filter(!is.na(problem_data_open)) |>
  pull(problem_data_open)

#measures
survey |>
  filter(!is.na(problem_measures_open)) |>
  pull(problem_measures_open)

#analysis
survey |>
  filter(!is.na(problem_analysis_open)) |>
  pull(problem_analysis_open)

#storage
survey |>
  filter(!is.na(problem_storage_open)) |>
  pull(problem_storage_open)

## 02.7 Any other ideas respondents had  ----------------------------------------------------------------
survey |>
  filter(!is.na(ideas)) |>
  pull(ideas)