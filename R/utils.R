# Helpers shared by the analysis scripts: model fitting with recorded convergence, planned
# contrasts, intraclass correlation, the detectable effect, and number formatting.

suppressPackageStartupMessages({
  library(lme4)
  library(lmerTest)
  library(glmmTMB)
  library(emmeans)
})

# p as reported: three decimals without the leading zero, or < .001.
fmt_p <- function(p) {
  ifelse(is.na(p), NA_character_, ifelse(p < .001, "< .001", sub("^0", "", sprintf("%.3f", p))))
}

fmt_num <- function(x, digits = 2) ifelse(is.na(x), NA_character_, formatC(x, format = "f", digits = digits))

# Runs a fit, keeping its warnings and messages instead of printing them, so that each one is recorded.
captured <- function(fit_fun) {
  warnings <- character()
  messages <- character()
  value <- withCallingHandlers(
    fit_fun(),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      messages <<- c(messages, trimws(conditionMessage(m)))
      invokeRestart("muffleMessage")
    }
  )
  list(value = value, warnings = warnings, messages = messages)
}

lme4_converged <- function(fit) {
  length(fit@optinfo$conv$lme4$messages) == 0 && length(fit@optinfo$warnings) == 0
}

# Fits a linear (lmerTest) or logistic (glmer) mixed model. The default optimizer first; on a
# convergence warning, bobyqa with more evaluations, as the analysis plan says. No term is ever
# dropped. Returns the fit with a record of every attempt.
fit_lme4 <- function(formula, data, family = NULL) {
  control <- if (is.null(family)) lme4::lmerControl else lme4::glmerControl
  attempts <- list(
    list(label = "default optimizer", control = control()),
    list(label = "bobyqa, maxfun 200000", control = control(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
  )
  tried <- list()
  for (a in attempts) {
    run <- captured(function() {
      if (is.null(family)) lmerTest::lmer(formula, data = data, control = a$control)
      else lme4::glmer(formula, data = data, family = family, control = a$control)
    })
    ok <- lme4_converged(run$value) && !any(grepl("converge", run$warnings))
    tried[[length(tried) + 1]] <- data.frame(attempt = a$label, converged = ok,
                                             warnings = paste(run$warnings, collapse = " | "),
                                             messages = paste(run$messages, collapse = " | "))
    if (ok) break
  }
  list(fit = run$value, converged = ok, attempts = do.call(rbind, tried), singular = lme4::isSingular(run$value))
}

# Fits a negative binomial (nbinom2) mixed model with glmmTMB: its default optimizer first, then
# more iterations, then BFGS. Converged means the optimizer reports success and the Hessian is
# positive definite.
fit_nb <- function(formula, data) {
  attempts <- list(
    list(label = "default (nlminb)", control = glmmTMBControl()),
    list(label = "nlminb, 10000 iterations", control = glmmTMBControl(optCtrl = list(iter.max = 1e4, eval.max = 1e4))),
    list(label = "optim BFGS", control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))
  )
  tried <- list()
  for (a in attempts) {
    run <- captured(function() glmmTMB(formula, data = data, family = nbinom2, control = a$control))
    ok <- run$value$fit$convergence == 0 && isTRUE(run$value$sdr$pdHess)
    tried[[length(tried) + 1]] <- data.frame(attempt = a$label, converged = ok,
                                             warnings = paste(run$warnings, collapse = " | "),
                                             messages = paste(run$messages, collapse = " | "))
    if (ok) break
  }
  list(fit = run$value, converged = ok, attempts = do.call(rbind, tried), singular = FALSE)
}

# Fits a participant-level model (lm, glm, MASS::glm.nb) with its warnings recorded. Converged
# means no warning and, for a GLM, the fitting algorithm's own convergence flag.
fit_plain <- function(fit_fun, label) {
  run <- captured(fit_fun)
  flag <- if (is.null(run$value$converged)) TRUE else isTRUE(run$value$converged)
  ok <- flag && length(run$warnings) == 0
  list(fit = run$value, converged = ok, singular = FALSE,
       attempts = data.frame(attempt = label, converged = ok,
                             warnings = paste(run$warnings, collapse = " | "),
                             messages = paste(run$messages, collapse = " | ")))
}

# Planned contrasts of condition, on the model's link scale, exponentiated: a ratio of geometric
# means for log time, an odds ratio for the logistic model, a rate ratio for the negative binomial.
# Linear mixed models use Satterthwaite degrees of freedom; the others are asymptotic (z).
CONTRASTS <- list(
  "C2 vs C1" = c(-1, 1, 0),
  "C3 vs C2" = c(0, -1, 1),
  "C3 vs C1" = c(-1, 0, 1)
)

condition_contrasts <- function(fit, which, level = 0.95) {
  args <- list(fit, specs = ~ condition)
  if (inherits(fit, "lmerMod")) args$lmer.df <- "satterthwaite"
  emm <- do.call(emmeans, args)
  con <- contrast(emm, method = CONTRASTS[which])
  s <- as.data.frame(summary(con, infer = c(TRUE, TRUE), level = level))
  lower <- grep("LCL|lower", names(s), value = TRUE)
  upper <- grep("UCL|upper", names(s), value = TRUE)
  stat <- grep("ratio$", names(s), value = TRUE)
  data.frame(
    contrast = as.character(s$contrast),
    log_estimate = s$estimate, se = s$SE, df = s$df,
    statistic = s[[stat]],
    ratio = exp(s$estimate), ci_low = exp(s[[lower]]), ci_high = exp(s[[upper]]),
    p = s$p.value,
    row.names = NULL
  )
}

# Estimated marginal mean per condition of a log-time model, back-transformed: a geometric mean in
# seconds with its 95% CI, averaged over the steps.
condition_means <- function(fit, level = 0.95) {
  args <- list(fit, specs = ~ condition)
  if (inherits(fit, "lmerMod")) args$lmer.df <- "satterthwaite"
  s <- as.data.frame(summary(do.call(emmeans, args), infer = c(TRUE, FALSE), level = level))
  data.frame(condition = as.character(s$condition), geometric_mean_s = exp(s$emmean),
             ci_low = exp(s$lower.CL), ci_high = exp(s$upper.CL), df = s$df, row.names = NULL)
}

# Two one-sided tests on the contrast's log ratio against symmetric bounds (log 0.80 = -log 1.25),
# with the 90% CI of the ratio.
condition_tost <- function(fit, which, bound = 1.25) {
  args <- list(fit, specs = ~ condition)
  if (inherits(fit, "lmerMod")) args$lmer.df <- "satterthwaite"
  emm <- do.call(emmeans, args)
  con <- contrast(emm, method = CONTRASTS[which])
  eq <- as.data.frame(test(con, delta = log(bound), side = "equivalence"))
  ci <- as.data.frame(confint(con, level = 0.90))
  data.frame(
    contrast = as.character(eq$contrast),
    ratio = exp(eq$estimate),
    ci90_low = exp(ci$lower.CL), ci90_high = exp(ci$upper.CL),
    bound_low = 1 / bound, bound_high = bound,
    p_tost = eq$p.value,
    inside = exp(ci$lower.CL) > 1 / bound & exp(ci$upper.CL) < bound,
    row.names = NULL
  )
}

# Intraclass correlation of the participant intercept. Linear: on the log scale. Logistic: on the
# latent scale (residual variance pi^2 / 3). Negative binomial (log link): the lognormal
# approximation of Nakagawa, Johnson and Schielzeth (2017), with the mean count taken at the
# average fixed-effect linear predictor.
icc <- function(fit) {
  if (!inherits(fit, c("glmmTMB", "merMod"))) return(NA_real_)
  if (inherits(fit, "glmmTMB")) {
    vu <- as.numeric(VarCorr(fit)$cond$participant)
    theta <- sigma(fit)
    lambda <- exp(mean(predict(fit, re.form = NA, type = "link")) + vu / 2)
    return(vu / (vu + log(1 + 1 / lambda + 1 / theta)))
  }
  vu <- as.numeric(VarCorr(fit)$participant)
  if (inherits(fit, "glmerMod")) return(vu / (vu + pi^2 / 3))
  vu / (vu + sigma(fit)^2)
}

# Smallest standardised difference (Cohen's d) a two-sided two-sample t test detects with the
# given power, from the noncentral t distribution as G*Power computes it.
detectable_d <- function(n1, n2, alpha, power = 0.80) {
  df <- n1 + n2 - 2
  crit <- qt(1 - alpha / 2, df)
  power_at <- function(d) {
    ncp <- d * sqrt(n1 * n2 / (n1 + n2))
    pt(-crit, df, ncp) + 1 - pt(crit, df, ncp)
  }
  uniroot(function(d) power_at(d) - power, c(0.01, 10), tol = 1e-10)$root
}

# Participants and rows per condition that a model used.
model_counts <- function(data) {
  data |>
    dplyr::group_by(condition) |>
    dplyr::summarise(participants = dplyr::n_distinct(participant), rows = dplyr::n(), .groups = "drop")
}

# A one-row record of how a model was fitted.
fit_record <- function(name, formula, data, fitted) {
  counts <- model_counts(data)
  data.frame(
    model = name,
    formula = paste(deparse(formula, width.cutoff = 500), collapse = ""),
    rows = nrow(data),
    participants = dplyr::n_distinct(data$participant),
    per_condition = paste(sprintf("%s: %d participants, %d rows", counts$condition, counts$participants, counts$rows), collapse = "; "),
    converged = fitted$converged,
    singular = fitted$singular,
    attempt_used = tail(fitted$attempts$attempt, 1),
    attempts = paste(sprintf("%s (%s)", fitted$attempts$attempt, ifelse(fitted$attempts$converged, "converged", "not converged")), collapse = "; "),
    warnings = paste(unique(unlist(strsplit(fitted$attempts$warnings[fitted$attempts$warnings != ""], " \\| "))), collapse = " | "),
    messages = paste(unique(unlist(strsplit(fitted$attempts$messages[fitted$attempts$messages != ""], " \\| "))), collapse = " | "),
    icc = icc(fitted$fit)
  )
}

write_result <- function(x, name) {
  readr::write_csv(x, file.path(OUT, name), na = "")
}
