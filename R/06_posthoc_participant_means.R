# POST HOC. Not part of the analysis frozen at analysis-frozen-20261112 and not registered: added on
# 12 November 2026, after the conditions were unmasked, at the author's request, and reported apart
# from the planned results. run_all.sh does not run it.
#
# The frozen decision-time model of the condition repair (Hypotheses 2 and 3) estimated the
# participant variance at zero (a singular fit), so its contrasts treat each step as independent.
# This check takes the participant as the unit instead: each log decision time minus that step's mean
# over all participants, so that conditions with different steps timed stay comparable, averaged per
# participant, then a linear model on condition with the same two planned contrasts.
#
#     ANALYSIS_CONDITIONS=real Rscript R/06_posthoc_participant_means.R

source("R/00_load.R")

POSTHOC_OUT <- file.path(OUT, "posthoc")
dir.create(POSTHOC_OUT, recursive = TRUE, showWarnings = FALSE)

# The rows of the frozen Hypotheses 2 and 3 model.
rows <- steps |> filter(task == "condition_repair", !is.na(d))

participant_means <- rows |>
  group_by(step_f) |>
  mutate(centred = log(d) - mean(log(d))) |>
  ungroup() |>
  group_by(participant, condition) |>
  summarise(steps = n(), mean_centred = mean(centred), .groups = "drop")

FORMULA_POSTHOC <- mean_centred ~ condition
fitted <- fit_plain(function() lm(FORMULA_POSTHOC, data = participant_means), "lm")
if (!fitted$converged) stop("the post hoc model gave a warning: ", fitted$attempts$warnings)

checks <- c("C2 vs C1" = "H2", "C3 vs C2" = "H3")
est <- condition_contrasts(fitted$fit, names(checks))
posthoc <- bind_rows(lapply(seq_len(nrow(est)), function(i) {
  pair <- strsplit(est$contrast[i], " vs ")[[1]]
  in_pair <- function(c) participant_means$condition == c
  data.frame(
    analysis = "post hoc",
    checks = checks[[est$contrast[i]]],
    repair = "condition repair",
    outcome = "decision time, step-centred log, mean per participant",
    effect = "ratio of geometric means",
    contrast = est$contrast[i],
    contrast_names = paste(CONDITION_NAMES[pair[1]], "vs", CONDITION_NAMES[pair[2]]),
    ratio = est$ratio[i], ci_low = est$ci_low[i], ci_high = est$ci_high[i],
    log_estimate = est$log_estimate[i], se = est$se[i], df = est$df[i], statistic = est$statistic[i],
    p = est$p[i], p_text = fmt_p(est$p[i]),
    participants_first = sum(in_pair(pair[1])), participants_second = sum(in_pair(pair[2])),
    steps_first = sum(participant_means$steps[in_pair(pair[1])]),
    steps_second = sum(participant_means$steps[in_pair(pair[2])]),
    formula = paste(deparse(FORMULA_POSTHOC), collapse = ""),
    mode = MODE
  )
}))

readr::write_csv(posthoc, file.path(POSTHOC_OUT, "posthoc_participant_means_contrasts.csv"), na = "")
cat(sprintf("Post hoc (%s conditions): participant means of step-centred log decision time, condition repair, %d participants; %s/posthoc_participant_means_contrasts.csv\n",
            MODE, nrow(participant_means), POSTHOC_OUT))
