# POST HOC. Not part of the analysis frozen at analysis-frozen-20261112 and not registered: added on
# 16 November 2026, after the conditions were unmasked, at the author's request, and reported apart
# from the planned results. run_all.sh does not run it.
#
# Two checks that a reader of the paper would ask for:
#   1. Hypothesis 1 on the steps each participant reached, so that a step the 15-minute limit kept
#      out no longer counts as not correct.
#   2. The time between the two repairs (participants.csv, gap_min: the condition repair's end to the
#      opening of the cue-free repair's log, as the frozen tables give it): by condition, and the frozen and participant-means models of Hypothesis 5 with it added,
#      in minutes and, so that the few long gaps weigh less, as log2 minutes.
# Neither replaces a confirmatory test.
#
#     ANALYSIS_CONDITIONS=real Rscript R/09_posthoc_revision_checks.R

source("R/00_load.R")

POSTHOC_OUT <- file.path(OUT, "posthoc")
dir.create(POSTHOC_OUT, recursive = TRUE, showWarnings = FALSE)

FORMULA_CORRECT <- correct ~ condition + step_f + (1 | participant)
FORMULA_DECISION <- log(d) ~ condition + step_f + (1 | participant)
FORMULA_POSTHOC <- mean_centred ~ condition
# Each form of the interval: its column, and the change in it that its ratio refers to.
INTERVAL_FORMS <- list(
  "minutes" = list(term = "interval_10", unit = "per 10 minutes"),
  "log2 minutes" = list(term = "interval_log2", unit = "per doubling")
)
with_term <- function(formula, term) update(formula, as.formula(paste(". ~ . +", term)))

# ---- 1. Hypothesis 1 without the steps the limit kept out ----------------------------------------

condition_repair <- steps |> filter(task == "condition_repair")
h1_rows <- list(
  "every step (as frozen)" = condition_repair,
  "steps reached" = condition_repair |> filter(row_detail != "not reached")
)

h1_check <- bind_rows(lapply(names(h1_rows), function(label) {
  rows <- h1_rows[[label]] |> droplevels()
  fitted <- fit_lme4(FORMULA_CORRECT, rows, family = binomial)
  est <- condition_contrasts(fitted$fit, "C2 vs C1")
  per_condition <- rows |>
    group_by(participant, condition) |>
    summarise(steps = n(), share = mean(correct), .groups = "drop") |>
    group_by(condition) |>
    summarise(steps = sum(steps), percent_correct = 100 * mean(share), .groups = "drop") |>
    tidyr::pivot_wider(names_from = condition, values_from = c(steps, percent_correct))
  data.frame(rows = label, contrast = "C2 vs C1", odds_ratio = est$ratio, ci_low = est$ci_low,
             ci_high = est$ci_high, p = est$p, converged = fitted$converged, singular = fitted$singular) |>
    bind_cols(per_condition)
})) |>
  mutate(analysis = "post hoc", p_text = fmt_p(p), mode = MODE)

# ---- 2. The time between the two repairs and Hypothesis 5 ----------------------------------------

intervals <- participants |>
  transmute(participant = as.character(participant), condition = as.character(condition), interval_min = gap_min)
if (anyNA(intervals$interval_min) || any(intervals$interval_min <= 0)) {
  stop("a participant has no time between the repairs, or one of 0 or less: check gap_min in participants.csv")
}

pair <- intervals |> filter(condition %in% c("C2", "C3"))
balance <- wilcox.test(interval_min ~ condition, data = pair, exact = FALSE)
interval_summary <- intervals |>
  group_by(condition) |>
  summarise(participants = n(), median_min = median(interval_min), min_min = min(interval_min),
            max_min = max(interval_min), .groups = "drop") |>
  mutate(test = "Wilcoxon rank-sum, spatial-cue vs step-card", p = balance$p.value, p_text = fmt_p(balance$p.value),
         analysis = "post hoc, descriptive", mode = MODE)

rows_h5 <- steps |>
  filter(task == "cue_free_repair", !is.na(d)) |>
  mutate(participant = as.character(participant)) |>
  left_join(intervals |> select(participant, interval_min), by = "participant") |>
  mutate(interval_10 = interval_min / 10, interval_log2 = log2(interval_min)) |>
  droplevels()

# The interval's own coefficient as a ratio of decision times, with its t-based 95% CI.
interval_term <- function(fit, form, df) {
  if (is.null(form)) {
    return(data.frame(interval_unit = "", interval_ratio = NA_real_, interval_ci_low = NA_real_,
                      interval_ci_high = NA_real_, interval_p = NA_real_))
  }
  co <- summary(fit)$coefficients[form$term, ]
  half <- qt(0.975, df) * co[["Std. Error"]]
  data.frame(interval_unit = form$unit, interval_ratio = exp(co[["Estimate"]]),
             interval_ci_low = exp(co[["Estimate"]] - half), interval_ci_high = exp(co[["Estimate"]] + half),
             interval_p = co[[grep("^Pr", names(co), value = TRUE)]])
}

mixed_h5 <- function(label) {
  form <- INTERVAL_FORMS[[label]]
  formula <- if (is.null(form)) FORMULA_DECISION else with_term(FORMULA_DECISION, form$term)
  fitted <- fit_lme4(formula, rows_h5)
  est <- condition_contrasts(fitted$fit, "C3 vs C2")
  df <- if (is.null(form)) NA else summary(fitted$fit)$coefficients[form$term, "df"]
  data.frame(model = "frozen mixed model", covariate = label, ratio = est$ratio, ci_low = est$ci_low,
             ci_high = est$ci_high, p = est$p, converged = fitted$converged, singular = fitted$singular) |>
    bind_cols(interval_term(fitted$fit, form, df))
}

# Each log decision time minus that step's mean over all participants, averaged per participant (as in 07).
participant_means <- rows_h5 |>
  group_by(step_f) |>
  mutate(centred = log(d) - mean(log(d))) |>
  ungroup() |>
  group_by(participant, condition, interval_10, interval_log2) |>
  summarise(mean_centred = mean(centred), .groups = "drop")

participant_h5 <- function(label) {
  form <- INTERVAL_FORMS[[label]]
  formula <- if (is.null(form)) FORMULA_POSTHOC else with_term(FORMULA_POSTHOC, form$term)
  fitted <- fit_plain(function() lm(formula, data = participant_means), "lm")
  if (!fitted$converged) stop("the post hoc model gave a warning: ", fitted$attempts$warnings)
  est <- condition_contrasts(fitted$fit, "C3 vs C2")
  data.frame(model = "participant means", covariate = label, ratio = est$ratio, ci_low = est$ci_low,
             ci_high = est$ci_high, p = est$p, converged = TRUE, singular = FALSE) |>
    bind_cols(interval_term(fitted$fit, form, fitted$fit$df.residual))
}

covariates <- c("none", names(INTERVAL_FORMS))
interval_h5 <- bind_rows(lapply(covariates, mixed_h5), lapply(covariates, participant_h5)) |>
  mutate(contrast = "C3 vs C2", analysis = "post hoc", p_text = fmt_p(p), interval_p_text = fmt_p(interval_p),
         mode = MODE, .after = covariate)

readr::write_csv(h1_check, file.path(POSTHOC_OUT, "posthoc_h1_reached_steps.csv"), na = "")
readr::write_csv(interval_summary, file.path(POSTHOC_OUT, "posthoc_interval_by_condition.csv"), na = "")
readr::write_csv(interval_h5, file.path(POSTHOC_OUT, "posthoc_interval_h5.csv"), na = "")
cat(sprintf("Post hoc (%s conditions): Hypothesis 1 on reached steps, the interval between repairs and Hypothesis 5; %s/posthoc_{h1_reached_steps,interval_*}.csv\n",
            MODE, POSTHOC_OUT))
