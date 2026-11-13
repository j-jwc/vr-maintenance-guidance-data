# POST HOC. Not part of the analysis frozen at analysis-frozen-20261112 and not registered: added on
# 13 November 2026, after the conditions were unmasked, at the author's request, and reported apart
# from the planned results. run_all.sh does not run it.
#
# Checks of Hypothesis 5 that a reader of Fig. 3 would ask for: the participant as the unit (as in
# 06_posthoc_participant_means.R), the frozen model refitted without each step-card and spatial-cue
# participant in turn, and the spatial-cue to step-card ratio of geometric mean decision times per
# step of the cue-free repair. The registration removes no outliers, so none of this replaces the
# confirmatory test.
#
#     ANALYSIS_CONDITIONS=real Rscript R/07_posthoc_h5.R

source("R/00_load.R")

POSTHOC_OUT <- file.path(OUT, "posthoc")
dir.create(POSTHOC_OUT, recursive = TRUE, showWarnings = FALSE)

FORMULA_DECISION <- log(d) ~ condition + step_f + (1 | participant)
FORMULA_POSTHOC <- mean_centred ~ condition

# The rows of the frozen Hypothesis 5 model.
rows_h5 <- steps |> filter(task == "cue_free_repair", !is.na(d)) |> droplevels()
not_shared <- function(rows) rows |> filter(step >= 4) |> droplevels()

mixed_h5 <- function(rows) {
  fitted <- fit_lme4(FORMULA_DECISION, rows)
  est <- condition_contrasts(fitted$fit, "C3 vs C2")
  data.frame(model = "frozen mixed model", ratio = est$ratio, ci_low = est$ci_low, ci_high = est$ci_high,
             p = est$p, converged = fitted$converged, singular = fitted$singular)
}

# Each log decision time minus that step's mean over all participants, averaged per participant.
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

geometric_means <- rows_h5 |>
  group_by(participant, condition) |>
  summarise(steps = n(), geometric_mean_s = exp(mean(log(d))), .groups = "drop") |>
  arrange(condition, desc(geometric_mean_s))
slowest <- geometric_means |> filter(condition == "C3") |> slice(1) |> pull(participant) |> as.character()
without_slowest <- rows_h5 |> filter(participant != slowest) |> droplevels()

checks <- bind_rows(
  cbind(steps = "1 to 9", without = "", participant_h5(rows_h5)),
  cbind(steps = "4 to 9", without = "", participant_h5(not_shared(rows_h5))),
  cbind(steps = "1 to 9", without = slowest, mixed_h5(without_slowest)),
  cbind(steps = "1 to 9", without = slowest, participant_h5(without_slowest))
) |>
  mutate(analysis = "post hoc", contrast = "C3 vs C2", p_text = fmt_p(p), mode = MODE)

in_pair <- geometric_means |> filter(condition %in% c("C2", "C3"))
leave_one_out <- bind_rows(lapply(as.character(in_pair$participant), function(id) {
  rows <- rows_h5 |> filter(participant != id) |> droplevels()
  m <- mixed_h5(rows)
  p <- participant_h5(rows)
  data.frame(without = id, condition = as.character(in_pair$condition[in_pair$participant == id]),
             mixed_ratio = m$ratio, mixed_ci_low = m$ci_low, mixed_ci_high = m$ci_high, mixed_p = m$p,
             mixed_singular = m$singular,
             participant_ratio = p$ratio, participant_ci_low = p$ci_low, participant_ci_high = p$ci_high,
             participant_p = p$p)
})) |> mutate(analysis = "post hoc", mode = MODE)

per_step <- function(rows, label) {
  rows |>
    filter(condition %in% c("C2", "C3")) |>
    group_by(step, condition) |>
    summarise(n = n(), geometric_mean_s = exp(mean(log(d))), .groups = "drop") |>
    tidyr::pivot_wider(names_from = condition, values_from = c(n, geometric_mean_s)) |>
    mutate(ratio_c3_c2 = geometric_mean_s_C3 / geometric_mean_s_C2, shared = step <= 3, rows = label)
}
steps_table <- bind_rows(per_step(rows_h5, "all"), per_step(without_slowest, paste("without", slowest))) |>
  mutate(analysis = "post hoc, descriptive", mode = MODE)

readr::write_csv(checks, file.path(POSTHOC_OUT, "posthoc_h5_checks.csv"), na = "")
readr::write_csv(leave_one_out, file.path(POSTHOC_OUT, "posthoc_h5_leave_one_out.csv"), na = "")
readr::write_csv(geometric_means |> mutate(mode = MODE), file.path(POSTHOC_OUT, "posthoc_h5_participant_geomeans.csv"), na = "")
readr::write_csv(steps_table, file.path(POSTHOC_OUT, "posthoc_h5_per_step.csv"), na = "")
cat(sprintf("Post hoc (%s conditions): Hypothesis 5 checks, slowest spatial-cue participant %s; %s/posthoc_h5_*.csv\n",
            MODE, slowest, POSTHOC_OUT))
