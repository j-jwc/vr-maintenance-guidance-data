# POST HOC. Not part of the analysis frozen at analysis-frozen-20261112 and not registered: added on
# 15 November 2026, after the conditions were unmasked, at the author's request, and reported apart
# from the planned results. run_all.sh does not run it.
#
# Two checks of Hypothesis 5 against a reading in which the difference comes from the isolation
# steps skipped with Next in the condition repair rather than from the spatial cues:
#   1. the frozen model and the participant-means model on the cue-free repair's steps 1 to 3 alone
#      (the steps it shares with the condition repair), beside steps 4 to 9 (as in 07);
#   2. both models without every step-card and spatial-cue participant who skipped an isolation step
#      in the condition repair (isolation_work.csv, isolation_step_skipped), with the per-step ratios.
# The registration removes no participant for this, so none of it replaces the confirmatory test.
#
#     ANALYSIS_CONDITIONS=real Rscript R/08_posthoc_isolation_skip.R

source("R/00_load.R")

POSTHOC_OUT <- file.path(OUT, "posthoc")
dir.create(POSTHOC_OUT, recursive = TRUE, showWarnings = FALSE)

FORMULA_DECISION <- log(d) ~ condition + step_f + (1 | participant)
FORMULA_POSTHOC <- mean_centred ~ condition

rows_h5 <- steps |> filter(task == "cue_free_repair", !is.na(d)) |> droplevels()

mixed_h5 <- function(rows) {
  fitted <- fit_lme4(FORMULA_DECISION, rows)
  est <- condition_contrasts(fitted$fit, "C3 vs C2")
  data.frame(model = "frozen mixed model", ratio = est$ratio, ci_low = est$ci_low, ci_high = est$ci_high,
             p = est$p, converged = fitted$converged, singular = fitted$singular)
}

# Each log decision time minus that step's mean over all participants, averaged per participant (as in 07).
participant_h5 <- function(rows) {
  means <- rows |>
    group_by(step_f) |>
    mutate(centred = log(d) - mean(log(d))) |>
    ungroup() |>
    group_by(participant, condition) |>
    summarise(steps = n(), mean_centred = mean(centred), .groups = "drop")
  fitted <- fit_plain(function() lm(FORMULA_POSTHOC, data = means), "lm")
  if (!fitted$converged) stop("the post hoc model gave a warning: ", fitted$attempts$warnings)
  est <- condition_contrasts(fitted$fit, "C3 vs C2")
  data.frame(model = "participant means", ratio = est$ratio, ci_low = est$ci_low, ci_high = est$ci_high,
             p = est$p, converged = TRUE, singular = FALSE)
}

both_models <- function(rows, steps_label, without_label) {
  bind_rows(mixed_h5(rows), participant_h5(rows)) |>
    mutate(steps = steps_label, without = without_label, .before = 1)
}

skippers <- isolation_work |>
  filter(task == "condition_repair", condition %in% c("C2", "C3"), isolation_step_skipped == 1) |>
  select(participant, condition) |>
  mutate(participant = as.character(participant), condition = as.character(condition))
if (nrow(skippers) == 0) stop("no step-card or spatial-cue participant skipped an isolation step: check isolation_work.csv")
skipper_label <- paste(skippers$participant, collapse = " ")

shared <- rows_h5 |> filter(step <= 3) |> droplevels()
not_shared <- rows_h5 |> filter(step >= 4) |> droplevels()
without_skippers <- rows_h5 |> filter(!participant %in% skippers$participant) |> droplevels()

checks <- bind_rows(
  both_models(rows_h5, "1 to 9", ""),
  both_models(shared, "1 to 3", ""),
  both_models(not_shared, "4 to 9", ""),
  both_models(without_skippers, "1 to 9", skipper_label),
  both_models(without_skippers |> filter(step <= 3) |> droplevels(), "1 to 3", skipper_label),
  both_models(without_skippers |> filter(step >= 4) |> droplevels(), "4 to 9", skipper_label)
) |>
  mutate(analysis = "post hoc", contrast = "C3 vs C2", p_text = fmt_p(p), mode = MODE)

per_step <- function(rows, label) {
  rows |>
    filter(condition %in% c("C2", "C3")) |>
    group_by(step, condition) |>
    summarise(n = n(), geometric_mean_s = exp(mean(log(d))), .groups = "drop") |>
    tidyr::pivot_wider(names_from = condition, values_from = c(n, geometric_mean_s)) |>
    mutate(ratio_c3_c2 = geometric_mean_s_C3 / geometric_mean_s_C2, shared = step <= 3, rows = label)
}
steps_table <- bind_rows(per_step(rows_h5, "all"), per_step(without_skippers, paste("without", skipper_label))) |>
  mutate(analysis = "post hoc, descriptive", mode = MODE)

readr::write_csv(skippers |> mutate(mode = MODE), file.path(POSTHOC_OUT, "posthoc_isolation_skippers.csv"), na = "")
readr::write_csv(checks, file.path(POSTHOC_OUT, "posthoc_isolation_skip_checks.csv"), na = "")
readr::write_csv(steps_table, file.path(POSTHOC_OUT, "posthoc_isolation_skip_per_step.csv"), na = "")
cat(sprintf("Post hoc (%s conditions): Hypothesis 5 without isolation skippers (%s); %s/posthoc_isolation_skip_*.csv\n",
            MODE, skipper_label, POSTHOC_OUT))
