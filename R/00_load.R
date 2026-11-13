# Loads the tables extract_steps.py wrote and sets the run's mode.
#
# The run's mode is the environment variable ANALYSIS_CONDITIONS: "masked" (the default) until the
# analysis is frozen, "real" after (README, Freeze). Each table carries the kind of conditions table
# the extractor was given, and a run refuses tables made with the other kind. Masked runs write to
# results/masked/ (git-ignored); only the real run writes to results/.

if (!file.exists("renv.lock")) stop("run the analysis from Q1-DSS/analysis/, where renv.lock is")

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
})
source("R/utils.R")

MODE <- Sys.getenv("ANALYSIS_CONDITIONS", "masked")
if (!MODE %in% c("masked", "real")) stop("ANALYSIS_CONDITIONS must be masked or real")
OUT <- if (MODE == "real") "results" else file.path("results", "masked")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

SEED <- 20261111
CONDITIONS <- c("C1", "C2", "C3")
CONDITION_NAMES <- c(C1 = "manual-only", C2 = "step-card", C3 = "spatial-cue")
TASKS <- c("condition_repair", "cue_free_repair")
TASK_NAMES <- c(condition_repair = "condition repair", cue_free_repair = "cue-free repair")

# The sensitivity analysis that DEVIATIONS.txt fixes: P03's cue-free repair ended in step 7 (item 4);
# it is analysed as registered.
WITHOUT_P03_CUE_FREE <- "P03"

read_table <- function(name, ...) {
  path <- file.path("data", name)
  if (!file.exists(path)) stop(path, " is missing: run extract_steps.py first")
  x <- read_csv(path, show_col_types = FALSE, progress = FALSE, na = "", ...)
  if ("conditions_source" %in% names(x) && nrow(x) > 0 && any(x$conditions_source != MODE)) {
    stop(path, " was made with the ", unique(x$conditions_source), " conditions, but this run is ", MODE)
  }
  x
}

steps <- read_table("steps.csv", col_types = cols(.default = col_guess(), participant = col_character(),
                                                   condition = col_character(), step_id = col_character(),
                                                   completion_reason = col_character(), row_detail = col_character())) |>
  mutate(
    condition = factor(condition, levels = CONDITIONS),
    task = factor(task, levels = TASKS),
    participant = factor(participant),
    step_f = factor(step)
  )
tasks <- read_table("tasks.csv", col_types = cols(.default = col_guess(), participant = col_character(),
                                                  condition = col_character(), missed = col_character(),
                                                  notes = col_character())) |>
  mutate(condition = factor(condition, levels = CONDITIONS), task = factor(task, levels = TASKS))
lookups <- read_table("lookups.csv", col_types = cols(.default = col_guess(), participant = col_character(),
                                                      condition = col_character(), pages = col_character())) |>
  mutate(condition = factor(condition, levels = CONDITIONS), task = factor(task, levels = TASKS))
participants <- read_table("participants.csv", col_types = cols(.default = col_guess(), participant = col_character(),
                                                                condition = col_character())) |>
  mutate(condition = factor(condition, levels = CONDITIONS))
not_analysed <- read_table("not_analysed_tasks.csv", col_types = cols(.default = col_character()))

isolation_work <- read_table("isolation_work.csv", col_types = cols(.default = col_guess(), participant = col_character(),
                                                                    condition = col_character(), events = col_character())) |>
  mutate(condition = factor(condition, levels = CONDITIONS))

# The participant forms (extract_forms.py). A masked run goes ahead without them; the real run needs them.
FORMS_READY <- file.exists("data/forms.csv") && file.exists("data/exclusions.csv")
if (MODE == "real" && !FORMS_READY) stop("the real run needs data/forms.csv and data/exclusions.csv: run extract_forms.py")
if (FORMS_READY) {
  forms <- read_table("forms.csv", col_types = cols(.default = col_guess(), participant = col_character(), condition = col_character(),
                                                     f2_keep_only_one = col_character())) |>
    mutate(condition = factor(condition, levels = CONDITIONS))
  exclusions <- read_table("exclusions.csv", col_types = cols(.default = col_character(), excluded = col_integer()))
  forms_unjoined <- read_table("forms_unjoined.csv", col_types = cols(.default = col_character()))
  if (any(!participants$participant %in% exclusions$participant)) stop("data/exclusions.csv is older than participants.csv: rerun extract_forms.py")
} else {
  forms <- NULL
  exclusions <- data.frame(participant = participants$participant, f5_excluded = NA_character_, f5_reason = NA_character_,
                           excluded = 0L, ruling = "no forms yet")
}

# Exclusions (registration item 6), applied to every table. participants_run keeps everyone run, for
# the report of exclusions by condition.
EXCLUDED <- exclusions$participant[exclusions$excluded == 1]
participants_run <- participants
keep_analysed <- function(x) {
  out <- x |> filter(!as.character(participant) %in% EXCLUDED)
  if (is.factor(out$participant)) out$participant <- droplevels(out$participant)
  out
}
steps <- keep_analysed(steps)
tasks <- keep_analysed(tasks)
lookups <- keep_analysed(lookups)
participants <- keep_analysed(participants)
isolation_work <- keep_analysed(isolation_work)
if (!is.null(forms)) forms <- keep_analysed(forms)

if (anyNA(steps$condition) || anyNA(tasks$condition)) stop("a participant has no condition in the conditions table")
if (any(steps$auto == 1 & !is.na(steps$d))) stop("a step the machine completed has a decision time")
