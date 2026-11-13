# Hypotheses 1 to 5: the confirmatory tests, as registered (AsPredicted #321454, item 5).
#
# Every model is a step-level mixed model with a random intercept per participant and the step as a
# fixed factor, fitted on all three conditions of the repair it concerns, with the planned contrast
# taken from it with emmeans. Every test is two-sided; the five p values are Holm-adjusted together
# at a family-wise alpha of .05.
#
# Rows each model uses:
# - Hypothesis 1: every step of the condition repair, steps never reached included as not correct.
# - Hypotheses 2, 3 and 5: the steps the script gives a decision time. Steps the machine completed
#   have none, and a step with no first correct action has none; none is imputed.
# - Hypothesis 4: the steps the script times, those that were the current step for a stretch of
#   their own. A step never reached, or passed over in the manual-only condition, has an error count
#   of 0 by construction, never having been the current step.

source("R/00_load.R")

condition_repair <- steps |> filter(task == "condition_repair")
cue_free_repair <- steps |> filter(task == "cue_free_repair")

FORMULA_CORRECT <- correct ~ condition + step_f + (1 | participant)
FORMULA_DECISION <- log(d) ~ condition + step_f + (1 | participant)
FORMULA_ERRORS <- errors ~ condition + step_f + (1 | participant)

data_h1 <- condition_repair
data_h23 <- condition_repair |> filter(!is.na(d))
data_h4 <- condition_repair |> filter(!is.na(t))
data_h5 <- cue_free_repair |> filter(!is.na(d))

fit_h1 <- fit_lme4(FORMULA_CORRECT, data_h1, family = binomial)
fit_h23 <- fit_lme4(FORMULA_DECISION, data_h23)
fit_h4 <- fit_nb(FORMULA_ERRORS, data_h4)
fit_h5 <- fit_lme4(FORMULA_DECISION, data_h5)

hypotheses <- tribble(
  ~hypothesis, ~repair, ~outcome, ~effect, ~fitted, ~data, ~contrast,
  "H1", "condition repair", "step correct", "odds ratio", fit_h1, data_h1, "C2 vs C1",
  "H2", "condition repair", "decision time", "ratio of geometric means", fit_h23, data_h23, "C2 vs C1",
  "H3", "condition repair", "decision time", "ratio of geometric means", fit_h23, data_h23, "C3 vs C2",
  "H4", "condition repair", "errors per step", "rate ratio", fit_h4, data_h4, "C3 vs C2",
  "H5", "cue-free repair", "decision time", "ratio of geometric means", fit_h5, data_h5, "C3 vs C2"
)

confirmatory <- bind_rows(lapply(seq_len(nrow(hypotheses)), function(i) {
  h <- hypotheses[i, ]
  fitted <- h$fitted[[1]]
  data <- h$data[[1]]
  est <- condition_contrasts(fitted$fit, h$contrast)
  pair <- strsplit(h$contrast, " vs ")[[1]]
  n <- sapply(pair, function(c) n_distinct(data$participant[data$condition == c]))
  rows <- sapply(pair, function(c) sum(data$condition == c))
  data.frame(
    hypothesis = h$hypothesis, repair = h$repair, outcome = h$outcome, effect = h$effect,
    contrast = h$contrast,
    contrast_names = paste(CONDITION_NAMES[pair[1]], "vs", CONDITION_NAMES[pair[2]]),
    estimate = est$ratio, ci_low = est$ci_low, ci_high = est$ci_high,
    log_estimate = est$log_estimate, se = est$se, df = est$df, statistic = est$statistic,
    p = est$p,
    participants_first = n[[1]], participants_second = n[[2]],
    rows_first = rows[[1]], rows_second = rows[[2]],
    icc = icc(fitted$fit),
    converged = fitted$converged
  )
}))
confirmatory$p_holm <- p.adjust(confirmatory$p, method = "holm")
confirmatory$p_text <- fmt_p(confirmatory$p)
confirmatory$p_holm_text <- fmt_p(confirmatory$p_holm)
confirmatory$percent_change <- (confirmatory$estimate - 1) * 100
confirmatory$detectable_d_05 <- mapply(detectable_d, confirmatory$participants_first, confirmatory$participants_second, MoreArgs = list(alpha = .05))
confirmatory$detectable_d_01 <- mapply(detectable_d, confirmatory$participants_first, confirmatory$participants_second, MoreArgs = list(alpha = .01))
confirmatory$mode <- MODE

fits <- bind_rows(
  fit_record("H1: step correct, condition repair", FORMULA_CORRECT, data_h1, fit_h1),
  fit_record("H2, H3: log decision time, condition repair", FORMULA_DECISION, data_h23, fit_h23),
  fit_record("H4: errors per step, condition repair", FORMULA_ERRORS, data_h4, fit_h4),
  fit_record("H5: log decision time, cue-free repair", FORMULA_DECISION, data_h5, fit_h5)
)
fits$mode <- MODE

# Model-estimated decision time per condition, for the figure (03_descriptives.R).
decision_means <- bind_rows(
  condition_means(fit_h23$fit) |> mutate(task = "condition_repair", model = "H2, H3"),
  condition_means(fit_h5$fit) |> mutate(task = "cue_free_repair", model = "H5")
)
decision_means$mode <- MODE

write_result(confirmatory, "confirmatory.csv")
write_result(fits, "confirmatory_models.csv")
write_result(decision_means, "decision_time_means.csv")

if (!all(fits$converged)) {
  warning("a confirmatory model did not converge after the optimizer settings: see confirmatory_models.csv", call. = FALSE)
}
cat(sprintf("Confirmatory (%s conditions): %d models fitted, %d converged; results in %s/\n",
            MODE, nrow(fits), sum(fits$converged), OUT))
