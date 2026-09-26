# Script reads in the survey data
# Variable descriptions read in as comments, indicator describtions as attributes

# Dieses Script liest eine CSV-Datendatei in GNU R ein.
# Beim Einlesen werden für alle Variablen Beschriftungen (comment) angelegt.
# Die Beschriftungen für Werte wird ebenfalls als Attribute (attr) abgelegt.

# 01.1 Importing data ----------------------------------------------------------------

# set encoding
options(encoding = "UTF-8")

# read in survey data
ds <- read.delim(
  file = paste0(here(), "/data/survey/survey_dd.csv"),
  encoding = "UTF-8", fileEncoding = "UTF-8",
  header = FALSE, sep = "\t", quote = "\"",
  dec = ".", row.names = NULL,
  col.names = c(
    "CASE", "SERIAL", "REF", "QUESTNNR", "MODE", "STARTED", "DD01", "DD02", "DD02_01",
    "DD02_02", "DD02_03", "DD02_04", "DD02_05", "DD02_06", "DD02_07", "DD02_07a", "DD03",
    "DD03_01", "DD03_02", "DD03_03", "DD03_04", "DD03_05", "DD03_06", "DD03_07", "DD03_08",
    "DD03_08a", "LP14", "TE02_01", "TE02_02", "TE02_03", "TE02_04", "TE02_05", "TE02_06",
    "TE02_07", "TE06_07", "TE03", "TE03_01", "TE03_02", "TE03_03", "TE03_04", "TE03_05",
    "TE03_06", "TE03_07", "TE03_08", "TE07_02", "TE07_03", "TE07_04", "TE07_05", "TE07_06",
    "TE07_07", "TE04_01", "TIME001", "TIME002", "TIME003", "TIME004", "TIME005", "TIME006",
    "TIME007", "TIME008", "TIME009", "TIME_SUM", "MAILSENT", "LASTDATA", "STATUS",
    "FINISHED", "Q_VIEWER", "LASTPAGE", "MAXPAGE", "MISSING", "MISSREL", "TIME_RSI"
  ),
  as.is = TRUE,
  colClasses = c(
    CASE = "numeric", SERIAL = "character", REF = "character", QUESTNNR = "character",
    MODE = "factor", STARTED = "POSIXct", DD01 = "numeric", DD02 = "numeric",
    DD02_01 = "logical", DD02_02 = "logical", DD02_03 = "logical", DD02_04 = "logical",
    DD02_05 = "logical", DD02_06 = "logical", DD02_07 = "logical",
    DD02_07a = "character", DD03 = "numeric", DD03_01 = "logical", DD03_02 = "logical",
    DD03_03 = "logical", DD03_04 = "logical", DD03_05 = "logical", DD03_06 = "logical",
    DD03_07 = "logical", DD03_08 = "logical", DD03_08a = "character", LP14 = "numeric",
    TE02_01 = "numeric", TE02_02 = "numeric", TE02_03 = "numeric", TE02_04 = "numeric",
    TE02_05 = "numeric", TE02_06 = "numeric", TE02_07 = "numeric",
    TE06_07 = "character", TE03 = "numeric", TE03_01 = "logical", TE03_02 = "logical",
    TE03_03 = "logical", TE03_04 = "logical", TE03_05 = "logical", TE03_06 = "logical",
    TE03_07 = "logical", TE03_08 = "logical", TE07_02 = "character",
    TE07_03 = "character", TE07_04 = "character", TE07_05 = "character",
    TE07_06 = "character", TE07_07 = "character", TE04_01 = "character",
    TIME001 = "integer", TIME002 = "integer", TIME003 = "integer", TIME004 = "integer",
    TIME005 = "integer", TIME006 = "integer", TIME007 = "integer", TIME008 = "integer",
    TIME009 = "integer", TIME_SUM = "integer", MAILSENT = "POSIXct",
    LASTDATA = "POSIXct", STATUS = "character", FINISHED = "logical",
    Q_VIEWER = "logical", LASTPAGE = "numeric", MAXPAGE = "numeric",
    MISSING = "numeric", MISSREL = "numeric", TIME_RSI = "numeric"
  ),
  skip = 1,
  check.names = TRUE, fill = TRUE,
  strip.white = FALSE, blank.lines.skip = TRUE,
  comment.char = "",
  na.strings = ""
)

# 01.2 Adding labels, etc. to survey data ----------------------------------------------------------------

# set attributes
attr(ds, "project") <- "dd-open-science"
attr(ds, "description") <- "DataDonation-Prereg"
attr(ds, "date") <- "2026-08-01"
attr(ds, "server") <- "https://www.soscisurvey.de"

# Variable und Value Labels
ds$DD01 <- factor(ds$DD01,
  levels = c("1", "2", "-9"),
  labels = c("Yes", "No", "[NA] Not answered"),
  ordered = FALSE
)

ds$LP14 <- factor(ds$LP14,
  levels = c("1", "2", "-9"),
  labels = c("Yes", "No", "[NA] Not answered"),
  ordered = FALSE
)

# Variable descriptions
comment(ds$SERIAL) <- "Serial number (if provided)"
comment(ds$REF) <- "Reference (if provided in link)"
comment(ds$QUESTNNR) <- "Questionnaire that has been used in the interview"
comment(ds$MODE) <- "Interview mode"
comment(ds$STARTED) <- "Time the interview has started (Europe/Berlin)"
comment(ds$DD01) <- "dd-experience"
comment(ds$DD02) <- "open-science-experience: Residual option (negative) or number of selected options"
comment(ds$DD02_01) <- "open-science-experience: Preregistration/registered report"
comment(ds$DD02_02) <- "open-science-experience: Shared code (e.g., for analysis, for extracting data)"
comment(ds$DD02_03) <- "open-science-experience: Shared data"
comment(ds$DD02_04) <- "open-science-experience: Shared materials (e.g., questionnaire)"
comment(ds$DD02_05) <- "open-science-experience: Replicated study"
comment(ds$DD02_06) <- "open-science-experience: Used an open-access publication format (e.g., open access paper, preprint)"
comment(ds$DD02_07) <- "open-science-experience: Other practices, namely ..."
comment(ds$DD02_07a) <- "open-science-experience: Other practices, namely ... (free text)"
comment(ds$DD03) <- "preregistratio-no: Residual option (negative) or number of selected options"
comment(ds$DD03_01) <- "preregistratio-no: I simply did not think about it"
comment(ds$DD03_02) <- "preregistratio-no: I was unsure about what needs to be included in the preregistration"
comment(ds$DD03_03) <- "preregistratio-no: Preregistration required too much effort (time, resources, etc.)"
comment(ds$DD03_04) <- "preregistratio-no: The planned data donation study included too many uncertainties/potential deviations (e.g., changes in DDP, expected sample size) to preregister"
comment(ds$DD03_05) <- "preregistratio-no: Preregistration would have lowered flexibility in how the data could be used"
comment(ds$DD03_06) <- "preregistratio-no: Preregistration lacked incentives (e.g., from journals, the academic community)"
comment(ds$DD03_07) <- "preregistratio-no: Preregistration did not seem applicable to my study (e.g., study was just a pretest)"
comment(ds$DD03_08) <- "preregistratio-no: Other reason, namely ..."
comment(ds$DD03_08a) <- "preregistratio-no: Other reason, namely ... (free text)"
comment(ds$LP14) <- "Consent"
comment(ds$TE02_01) <- "content template: Study information and design (e.g., authors, study type)"
comment(ds$TE02_02) <- "content template: Research questions (e.g., RQs, hypotheses)"
comment(ds$TE02_03) <- "content template: Sampling (e.g., sampling strategy, expected sample size)"
comment(ds$TE02_04) <- "content template: Data extraction (e.g., selection of platforms, selection of variables, data donation tool)"
comment(ds$TE02_05) <- "content template: Measurement creation (e.g., processing methods for anonymization, index building)"
comment(ds$TE02_06) <- "content template: Analysis (e.g., descriptive/inferential analysis)"
comment(ds$TE02_07) <- "content template: Data use/storage (e.g., storing of digital traces, sharing for further use)"
comment(ds$TE06_07) <- "content template (other): Other aspect, namely ..."
comment(ds$TE03) <- "Problems steps: Residual option (negative) or number of selected options"
comment(ds$TE03_01) <- "Problems steps: Study information and design (e.g., authors, study type)"
comment(ds$TE03_02) <- "Problems steps: Research questions (e.g., RQs, hypotheses)"
comment(ds$TE03_03) <- "Problems steps: Sampling (e.g., sampling strategy, expected sample size)"
comment(ds$TE03_04) <- "Problems steps: Data extraction (e.g., selection of platforms, selection of variables, data donation tool)"
comment(ds$TE03_05) <- "Problems steps: Measurement creation (e.g., processing methods for anonymization, index building)"
comment(ds$TE03_06) <- "Problems steps: Analysis (e.g., descriptive/inferential analysis)"
comment(ds$TE03_07) <- "Problems steps: Data use/storage (e.g., storing of digital traces, sharing for further use)"
comment(ds$TE03_08) <- "Problems steps: Other steps, namely ..."
comment(ds$TE07_02) <- "Problems steps (open): Research questions (e.g., RQs, hypotheses)"
comment(ds$TE07_03) <- "Problems steps (open): Sampling (e.g., sampling strategy, expected sample size)"
comment(ds$TE07_04) <- "Problems steps (open): Data extraction (e.g., selection of platforms, selection of variables, data donation tool)"
comment(ds$TE07_05) <- "Problems steps (open): Measurement creation (e.g., processing methods for anonymization, index building)"
comment(ds$TE07_06) <- "Problems steps (open): Analysis (e.g., descriptive/inferential analysis)"
comment(ds$TE07_07) <- "Problems steps (open): Data use/storage (e.g., storing of digital traces, sharing for further use)"
comment(ds$TE04_01) <- "further ideas: [01]"
comment(ds$TIME001) <- "Time spent on page 1"
comment(ds$TIME002) <- "Time spent on page 2"
comment(ds$TIME003) <- "Time spent on page 3"
comment(ds$TIME004) <- "Time spent on page 4"
comment(ds$TIME005) <- "Time spent on page 5"
comment(ds$TIME006) <- "Time spent on page 6"
comment(ds$TIME007) <- "Time spent on page 7"
comment(ds$TIME008) <- "Time spent on page 8"
comment(ds$TIME009) <- "Time spent on page 9"
comment(ds$TIME_SUM) <- "Time spent overall (except outliers)"
comment(ds$MAILSENT) <- "Time when the invitation mailing was sent (personally identifiable recipients, only)"
comment(ds$LASTDATA) <- "Time when the data was most recently updated"
comment(ds$STATUS) <- "Interview status marker"
comment(ds$FINISHED) <- "Has the interview been finished (reached last page)?"
comment(ds$Q_VIEWER) <- "Did the respondent only view the questionnaire, omitting mandatory questions?"
comment(ds$LASTPAGE) <- "Last page that the participant has handled in the questionnaire"
comment(ds$MAXPAGE) <- "Hindmost page handled by the participant"
comment(ds$MISSING) <- "Missing answers in percent"
comment(ds$MISSREL) <- "Missing answers (weighted by relevance)"
comment(ds$TIME_RSI) <- "Completion Speed (relative)"

# 01.3 Reducing cases/variables  ----------------------------------------------------------------

# Reduce to relevant cases
survey <- ds |>
  # only those who gave consent (LP14) & ever conducted a data donation study (DD01) & finished survey
  filter(LP14 == "Yes" & DD01 == "Yes" & STATUS == "complete")

# Reduce to relevant variables
survey <- survey |>
  select(CASE, STARTED, DD02_01:DD02_07a, DD03_01:TE06_07, TE03_01:TE04_01, -LP14)

# clean house
rm(ds)

# 01.4 Adding clearer variable names ----------------------------------------------------------------

survey <- survey %>%
  rename(
    case = CASE,
    date_start = STARTED,
    exp_prereg = DD02_01,
    exp_share_code = DD02_02,
    exp_share_data = DD02_03,
    exp_share_material = DD02_04,
    exp_replication = DD02_05,
    exp_open_access = DD02_06,
    exp_other = DD02_07,
    exp_other_open = DD02_07a,
    prereg_not_thought = DD03_01,
    preg_unsure_how = DD03_02,
    preg_effort = DD03_03,
    preg_uncertainty = DD03_04,
    preg_not_flexible = DD03_05,
    preg_incentive = DD03_06,
    preg_not_applicable = DD03_07,
    preg_other = DD03_08,
    prereg_other_open = DD03_08a,
    include_design = TE02_01,
    include_rq = TE02_02,
    include_sampling = TE02_03,
    include_data = TE02_04,
    include_measures = TE02_05,
    include_analysis = TE02_06,
    include_storage = TE02_07,
    include_other_open = TE06_07,
    problem_design = TE03_01,
    problem_rq = TE03_02,
    problem_sampling = TE03_03,
    problem_data = TE03_04,
    problem_measures = TE03_05,
    problem_analysis = TE03_06,
    problem_storage = TE03_07,
    problem_other = TE03_08,
    problem_rq_open = TE07_02, # important: TE07_01 missing because no one who completed quest. clicked "yes" for TE03 item
    problem_sampling_open = TE07_03,
    problem_data_open = TE07_04,
    problem_measures_open = TE07_05,
    problem_analysis_open = TE07_06,
    problem_storage_open = TE07_07,
    ideas = TE04_01
  )

# 01.5 Changing type for analysis ----------------------------------------------------------------

survey <- survey |>
  mutate(across(where(is.logical), as.numeric))
