# Exploratory, secondary and sensitivity analyses. None of these carries a confirmatory claim; their
# p values are not Holm-adjusted and are labelled exploratory in every output.
#
# By-condition analyses report the study's two planned contrasts, step-card vs manual-only and
# spatial-cue vs step-card. Rows per model follow 01_confirmatory.R: decision
# and execution time on the steps the script gives one; errors on the steps it times; step
# correctness on every step.
#
# The participant-level measures from the forms (Hypotheses 8 to 10 and Hypothesis 5's recall items)
# are scored with R/forms.R from data/forms.csv (extract_forms.py) and reported with an effect size
# and 95% CI (registration item 8): for a pair of conditions the probability of superiority beside
# the Mann-Whitney p, for all three epsilon squared beside the Kruskal-Wallis p, and for Hypothesis
# 10 Dunn's test, Holm-adjusted across its three pairs.

source("R/00_load.R")
source("R/forms.R")
suppressPackageStartupMessages(library(MASS, include.only = "glm.nb"))

condition_repair <- steps |> filter(task == "condition_repair")
cue_free_repair <- steps |> filter(task == "cue_free_repair")
PLANNED <- c("C2 vs C1", "C3 vs C2")

FORMULA_CORRECT <- correct ~ condition + step_f + (1 | participant)
FORMULA_DECISION <- log(d) ~ condition + step_f + (1 | participant)
FORMULA_DECISION_NET <- log(d_net) ~ condition + step_f + (1 | participant)
FORMULA_EXECUTION <- log(e) ~ condition + step_f + (1 | participant)
FORMULA_ERRORS <- errors ~ condition + step_f + (1 | participant)

contrasts_out <- list()
records <- list()

# Fits one model, records it, and keeps the named contrasts with the analysis they belong to.
run <- function(analysis, outcome, formula, data, kind, which) {
  fitted <- switch(kind,
    linear = fit_lme4(formula, data),
    logistic = fit_lme4(formula, data, family = binomial),
    negbin = fit_nb(formula, data)
  )
  records[[length(records) + 1]] <<- cbind(analysis = analysis, fit_record(outcome, formula, data, fitted))
  est <- condition_contrasts(fitted$fit, which)
  contrasts_out[[length(contrasts_out) + 1]] <<- cbind(analysis = analysis, outcome = outcome, est,
                                                       converged = fitted$converged)
  invisible(fitted)
}

# Hypothesis 6: execution time, spatial-cue vs step-card, with an equivalence test (ratio of
# geometric means within 0.80 to 1.25, its 90% CI inside the bounds).
data_e <- condition_repair |> filter(!is.na(e))
fit_h6 <- run("H6 execution time", "log execution time, condition repair", FORMULA_EXECUTION, data_e, "linear", "C3 vs C2")
h6_tost <- cbind(analysis = "H6 execution time, equivalence (TOST)", condition_tost(fit_h6$fit, "C3 vs C2"))

# Hypothesis 7: manual lookups in the condition repair, per participant. Count: negative binomial.
# Total time: linear model of log time among the participants who looked the manual up at all, since
# a participant with no lookup has no log time; how many had none is reported
# per condition.
lookup_tasks <- tasks |> filter(task == "condition_repair")
FORMULA_LOOKUPS <- n_lookups ~ condition
fit_h7_count <- fit_plain(function() MASS::glm.nb(FORMULA_LOOKUPS, data = lookup_tasks), "MASS::glm.nb")
records[[length(records) + 1]] <- cbind(analysis = "H7 manual lookups", fit_record("lookup count, condition repair", FORMULA_LOOKUPS, lookup_tasks, fit_h7_count))
contrasts_out[[length(contrasts_out) + 1]] <- cbind(analysis = "H7 manual lookups", outcome = "lookup count (rate ratio)",
                                                    condition_contrasts(fit_h7_count$fit, PLANNED), converged = fit_h7_count$converged)
lookup_time <- lookup_tasks |> filter(m > 0)
FORMULA_LOOKUP_TIME <- log(m) ~ condition
fit_h7_time <- fit_plain(function() lm(FORMULA_LOOKUP_TIME, data = lookup_time), "lm")
records[[length(records) + 1]] <- cbind(analysis = "H7 manual lookups", fit_record("log lookup time, participants with a lookup", FORMULA_LOOKUP_TIME, lookup_time, fit_h7_time))
contrasts_out[[length(contrasts_out) + 1]] <- cbind(analysis = "H7 manual lookups", outcome = "lookup time among participants with a lookup (ratio of geometric means)",
                                                    condition_contrasts(fit_h7_time$fit, PLANNED), converged = fit_h7_time$converged)
manual_use <- lookup_tasks |>
  group_by(condition) |>
  summarise(participants = n(), with_lookup = sum(n_lookups > 0),
            lookups_mean = mean(n_lookups), lookups_sd = sd(n_lookups), lookups_median = median(n_lookups),
            time_mean_s = mean(m), time_sd_s = sd(m), time_median_s = median(m), .groups = "drop") |>
  mutate(condition_name = CONDITION_NAMES[as.character(condition)])

# Hypothesis 8: work before isolation in each repair, per participant (any at all, 0 or 1), logistic.
isolation <- steps |>
  group_by(participant, condition, task) |>
  summarise(in_steps = sum(safety_skips), .groups = "drop") |>
  left_join(tasks |> select(participant, task, safety_skips_outside), by = c("participant", "task")) |>
  mutate(work_before_isolation = in_steps + safety_skips_outside, any = as.integer(work_before_isolation > 0))
FORMULA_ISOLATION <- any ~ condition
for (repair in TASKS) {
  d_rep <- isolation |> filter(task == repair)
  fitted <- fit_plain(function() glm(FORMULA_ISOLATION, family = binomial, data = d_rep), "glm binomial")
  records[[length(records) + 1]] <- cbind(analysis = "H8 work before isolation", fit_record(paste("any work before isolation,", TASK_NAMES[repair]), FORMULA_ISOLATION, d_rep, fitted))
  contrasts_out[[length(contrasts_out) + 1]] <- cbind(analysis = "H8 work before isolation", outcome = paste("any work before isolation,", TASK_NAMES[repair], "(odds ratio)"),
                                                      condition_contrasts(fitted$fit, PLANNED), converged = fitted$converged)
}
isolation_summary <- isolation |>
  group_by(task, condition) |>
  summarise(participants = n(), with_any = sum(any), events_total = sum(work_before_isolation),
            events_median = median(work_before_isolation), .groups = "drop") |>
  mutate(condition_name = CONDITION_NAMES[as.character(condition)], repair = TASK_NAMES[as.character(task)])

# Condition by repair interaction on log decision time, both repairs in one model. The step factor is
# nested in the repair; the cue-free repair has no step 10, so lme4 drops that empty column. The
# coefficients give how each contrast changes from the condition repair to the cue-free repair, as a
# ratio of the two ratios of geometric means.
both <- steps |> filter(!is.na(d)) |> mutate(condition = relevel(condition, ref = "C2"))
FORMULA_INTERACTION <- log(d) ~ condition * task + task:step_f + (1 | participant)
fit_int <- fit_lme4(FORMULA_INTERACTION, both)
records[[length(records) + 1]] <- cbind(analysis = "Condition by repair interaction", fit_record("log decision time, both repairs", FORMULA_INTERACTION, both, fit_int))
coefs <- summary(fit_int$fit)$coefficients
interaction_row <- function(term, label, sign) {
  est <- sign * coefs[term, "Estimate"]
  se <- coefs[term, "Std. Error"]
  df <- coefs[term, "df"]
  half <- qt(0.975, df) * se
  data.frame(term = term, change = label, ratio_of_ratios = exp(est), ci_low = exp(est - half), ci_high = exp(est + half),
             log_estimate = est, se = se, df = df, p = coefs[term, "Pr(>|t|)"])
}
interaction <- rbind(
  interaction_row("conditionC3:taskcue_free_repair", "C3 vs C2, cue-free repair over condition repair", 1),
  interaction_row("conditionC1:taskcue_free_repair", "C2 vs C1, cue-free repair over condition repair", -1)
)
interaction$converged <- fit_int$converged

# Secondary measures for Hypothesis 5: errors and step correctness on the cue-free repair.
run("H5 secondary", "errors per step, cue-free repair", FORMULA_ERRORS, cue_free_repair |> filter(!is.na(t)), "negbin", "C3 vs C2")
run("H5 secondary", "step correct, cue-free repair", FORMULA_CORRECT, cue_free_repair, "logistic", "C3 vs C2")

# Decision time with manual reading removed, for Hypotheses 2 and 3.
run("Decision time without manual reading (H2, H3)", "log decision time net of manual reading, condition repair",
    FORMULA_DECISION_NET, condition_repair |> filter(!is.na(d_net)), "linear", PLANNED)

# Sensitivity: the cue-free repair's steps 4 to 9 only, which it does not share with the condition repair.
steps_4_9 <- cue_free_repair |> filter(step >= 4)
run("Sensitivity: cue-free repair steps 4 to 9", "log decision time (H5)", FORMULA_DECISION, steps_4_9 |> filter(!is.na(d)), "linear", "C3 vs C2")
run("Sensitivity: cue-free repair steps 4 to 9", "errors per step", FORMULA_ERRORS, steps_4_9 |> filter(!is.na(t)), "negbin", "C3 vs C2")
run("Sensitivity: cue-free repair steps 4 to 9", "step correct", FORMULA_CORRECT, steps_4_9, "logistic", "C3 vs C2")

# Sensitivity: Hypothesis 5 without P03's cue-free repair (DEVIATIONS item 4).
run(paste("Sensitivity: H5 without", WITHOUT_P03_CUE_FREE, "cue-free repair"), "log decision time (H5)", FORMULA_DECISION,
    cue_free_repair |> filter(!is.na(d), participant != WITHOUT_P03_CUE_FREE), "linear", "C3 vs C2")

# Sensitivity: Hypothesis 4 on every step line, a step never reached or passed over counted as 0 errors.
run("Sensitivity: H4 on every step line", "errors per step (H4)", FORMULA_ERRORS, condition_repair, "negbin", "C3 vs C2")

# Total repair time, gross (T) and with manual reading removed (T_net), per repair: not hypothesised;
# a Kruskal-Wallis test across the three conditions, as planned before data collection, beside the
# descriptives of 03_descriptives.R.
total_time <- bind_rows(lapply(TASKS, function(repair) {
  d <- tasks |> filter(task == repair)
  bind_rows(lapply(c("T", "T_net"), function(v) {
    k <- kruskal.test(d[[v]], d$condition)
    data.frame(repair = TASK_NAMES[[repair]], measure = v, participants = sum(!is.na(d[[v]])),
               statistic = unname(k$statistic), df = unname(k$parameter), p = k$p.value)
  }))
}))

# Participant-level measures from the forms.
form_pairs <- form_kruskal <- form_dunn <- NULL
if (!is.null(forms)) {
  scored <- score_forms(forms, tasks)
  if (MODE == "real" && anyNA(scored[!is.na(scored$sus), c("recall_item1", "recall_item2", "recall_item3")])) {
    stop("a recall answer is not scored yet: finish data/recall_coding.csv")
  }
  pairs_for <- function(analysis, measure, pairs) {
    bind_rows(lapply(pairs, function(p) {
      ab <- strsplit(p, " vs ")[[1]]
      cbind(analysis = analysis, measure = measure, pair_effect(scored[[measure]], scored$condition, ab[1], ab[2], SEED))
    }))
  }
  form_pairs <- bind_rows(
    pairs_for("H8 safety ordering", "recall_item3", PLANNED),
    bind_rows(lapply(c("recall_item1", "recall_item2", "recall_item4", "recall_score"),
                     function(m) pairs_for("H5 secondary, recall", m, "C3 vs C2"))),
    bind_rows(lapply(c("gap_condition_repair", "gap_cue_free_repair"), function(m) pairs_for("H9 confidence calibration", m, PLANNED)))
  )
  H10 <- c("tlx_condition_repair", "tlx_cue_free_repair", "sus", "f2_seq", "f3_seq")
  form_kruskal <- bind_rows(
    bind_rows(lapply(c("gap_condition_repair", "gap_cue_free_repair"),
                     function(m) cbind(analysis = "H9 confidence calibration", measure = m, kruskal_effect(scored[[m]], scored$condition, SEED)))),
    bind_rows(lapply(H10, function(m) cbind(analysis = "H10 workload, usability, ease", measure = m, kruskal_effect(scored[[m]], scored$condition, SEED))))
  )
  form_dunn <- bind_rows(lapply(H10, function(m) {
    dn <- dunn_holm(scored[[m]], scored$condition)
    effects <- bind_rows(lapply(seq_len(nrow(dn)), function(i) pair_effect(scored[[m]], scored$condition, dn$first[i], dn$second[i], SEED)))
    cbind(analysis = "H10 workload, usability, ease", measure = m, dn,
          superiority = effects$superiority, ci_low = effects$ci_low, ci_high = effects$ci_high)
  }))
}

exploratory <- bind_rows(contrasts_out) |>
  mutate(label = "exploratory", p_text = fmt_p(p), mode = MODE)
models <- bind_rows(records) |> mutate(mode = MODE)

write_result(exploratory, "exploratory_contrasts.csv")
write_result(models, "exploratory_models.csv")
write_result(h6_tost |> mutate(label = "exploratory", mode = MODE), "h6_equivalence.csv")
write_result(manual_use |> mutate(label = "exploratory", mode = MODE), "manual_use_by_condition.csv")
write_result(isolation_summary |> mutate(label = "exploratory", mode = MODE), "h8_isolation_by_condition.csv")
write_result(interaction |> mutate(p_text = fmt_p(p), label = "exploratory", mode = MODE), "interaction.csv")
write_result(total_time |> mutate(p_text = fmt_p(p), label = "exploratory", mode = MODE), "total_time_kruskal.csv")
if (!is.null(forms)) {
  write_result(form_pairs |> mutate(p_text = fmt_p(p), label = "exploratory", mode = MODE), "forms_pairs.csv")
  write_result(form_kruskal |> mutate(p_text = fmt_p(p), label = "exploratory", mode = MODE), "forms_kruskal.csv")
  write_result(form_dunn |> mutate(p_text = fmt_p(p), p_holm_text = fmt_p(p_holm), label = "exploratory", mode = MODE), "forms_dunn.csv")
} else {
  cat("Forms: data/forms.csv not present; Hypotheses 8 (recall), 9 and 10 and the recall items not analysed\n")
}

if (!all(models$converged)) {
  warning("exploratory models not converged after the optimizer settings: ",
          paste(models$model[!models$converged], collapse = "; "), call. = FALSE)
}
cat(sprintf("Exploratory (%s conditions): %d models, %d converged; results in %s/\n",
            MODE, nrow(models), sum(models$converged), OUT))
