# Scoring rules for the participant forms, as fixed before data collection, and
# Dunn's post hoc test for Hypothesis 10, and the effect sizes of the participant-level measures.
# These functions take item values; extract_forms.py finds each item in the exports by its header.

# System Usability Scale (Brooke 1996): items 1 to 10, each 1 to 5. Odd items minus 1, even items
# 5 minus the answer, summed and multiplied by 2.5, 0 to 100. A missing item leaves the score missing.
score_sus <- function(items) {
  items <- as.matrix(items)
  if (ncol(items) != 10) stop("the SUS has 10 items")
  if (any(items < 1 | items > 5, na.rm = TRUE)) stop("SUS items run from 1 to 5")
  odd <- items[, c(1, 3, 5, 7, 9), drop = FALSE] - 1
  even <- 5 - items[, c(2, 4, 6, 8, 10), drop = FALSE]
  2.5 * (rowSums(odd) + rowSums(even))
}

# Raw NASA-TLX: the mean of the six subscales, each 0 to 100 in steps of 5, unweighted. Performance is
# asked as 0 perfect to 100 failure, so no subscale is reversed.
TLX_SUBSCALES <- c(mental = "Mental demand", physical = "Physical demand", temporal = "Temporal demand",
                   performance = "Performance", effort = "Effort", frustration = "Frustration")
score_tlx <- function(subscales) {
  subscales <- as.matrix(subscales)
  if (ncol(subscales) != 6) stop("the raw NASA-TLX has six subscales")
  if (any(subscales < 0 | subscales > 100 | subscales %% 5 != 0, na.rm = TRUE)) stop("TLX subscales run from 0 to 100 in steps of 5")
  rowMeans(subscales)
}

# Recall item 4: the participant orders four steps of the drive roller procedure (shown as 7, 3, 9,
# 5; done as 3, 5, 7, 9). One point per pair of steps placed in the right order relative to each
# other, 0 to 6; a tie or a blank loses that pair. `positions` has one column per step, named by step
# number, holding the position the participant gave it.
RECALL_ORDER <- c("3", "5", "7", "9")
score_recall_order <- function(positions) {
  positions <- as.matrix(positions)
  if (!setequal(colnames(positions), RECALL_ORDER)) stop("recall item 4 needs positions for steps 3, 5, 7 and 9")
  pairs <- utils::combn(RECALL_ORDER, 2)
  right <- apply(pairs, 2, function(p) {
    earlier <- positions[, p[1]]
    later <- positions[, p[2]]
    !is.na(earlier) & !is.na(later) & earlier < later
  })
  rowSums(matrix(right, nrow = nrow(positions)))
}

# The recall score for Hypothesis 5's secondary analysis: the mean of item 1 out of 3, item 2 out of 3
# and item 4 out of 6, from 0 to 1. Item 3 serves Hypothesis 8 and is not in it.
score_recall <- function(item1, item2, item4) {
  if (any(item1 < 0 | item1 > 3 | item2 < 0 | item2 > 3 | item4 < 0 | item4 > 6, na.rm = TRUE)) stop("a recall item is out of range")
  (item1 / 3 + item2 / 3 + item4 / 6) / 3
}

# Confidence calibration (Hypothesis 9): the stated percentage of steps done correctly against step
# accuracy, both 0 to 100. The absolute gap is the measure; the signed gap (positive when the
# participant overestimates) is reported descriptively.
calibration_gap <- function(stated, correct_steps, n_steps) {
  accuracy <- 100 * correct_steps / n_steps
  data.frame(accuracy = accuracy, signed_gap = stated - accuracy, absolute_gap = abs(stated - accuracy))
}

# Dunn's (1964) test of every pair of groups after a Kruskal-Wallis test: mean ranks over the pooled
# sample, with the tie correction, two-sided normal p values, Holm-adjusted across the pairs. Written
# here so that no package is added for it; for two groups z^2 equals the Kruskal-Wallis statistic.
dunn_holm <- function(x, g) {
  keep <- !is.na(x) & !is.na(g)
  x <- x[keep]
  g <- droplevels(as.factor(g[keep]))
  n <- length(x)
  r <- rank(x)
  ties <- table(x)
  s2 <- n * (n + 1) / 12 - sum(ties^3 - ties) / (12 * (n - 1))
  mean_rank <- tapply(r, g, mean)
  size <- tapply(r, g, length)
  pairs <- utils::combn(levels(g), 2)
  out <- data.frame(
    first = pairs[2, ], second = pairs[1, ],
    n_first = as.integer(size[pairs[2, ]]), n_second = as.integer(size[pairs[1, ]]),
    mean_rank_first = as.numeric(mean_rank[pairs[2, ]]), mean_rank_second = as.numeric(mean_rank[pairs[1, ]])
  )
  out$z <- (out$mean_rank_first - out$mean_rank_second) / sqrt(s2 * (1 / out$n_first + 1 / out$n_second))
  out$p <- 2 * stats::pnorm(-abs(out$z))
  out$p_holm <- stats::p.adjust(out$p, method = "holm")
  out
}

# Effect sizes for the participant-level secondary measures, which the registration (item 8) reports
# with an effect size and 95% CI. Each CI is a percentile bootstrap that resamples participants
# within each condition (10,000 resamples), as the expert forecasts' CIs do.
BOOT_RESAMPLES <- 10000

# The probability of superiority (Vargha and Delaney's A): the chance that a participant of the first
# group scores above one of the second, ties counting half. 0.5 is no difference. It is the effect
# size of the Mann-Whitney test, whose p it is reported with.
superiority <- function(x, y) mean(outer(x, y, ">")) + 0.5 * mean(outer(x, y, "=="))

# Epsilon squared for the Kruskal-Wallis test, H / (n - 1), 0 to 1 (Tomczak and Tomczak 2014).
epsilon_squared <- function(x, g) unname(stats::kruskal.test(x, g)$statistic) / (length(x) - 1)

boot_percentile <- function(data, statistic, strata, seed) {
  set.seed(seed)
  b <- boot::boot(data, function(d, i) statistic(d[i, , drop = FALSE]), R = BOOT_RESAMPLES, strata = strata)
  # Every resample equal (all answers alike): boot.ci gives nothing, and the interval is that value.
  if (length(unique(b$t[, 1])) == 1) return(c(b$t[1, 1], b$t[1, 1]))
  ci <- boot::boot.ci(b, type = "perc", conf = 0.95)
  if (is.null(ci)) return(c(NA_real_, NA_real_))
  ci$percent[1, 4:5]
}

# One pair of conditions: sizes, medians, A with its CI and the Mann-Whitney p (normal approximation
# with the tie and continuity corrections, as wilcox.test gives it when there are ties).
pair_effect <- function(x, g, first, second, seed) {
  keep <- !is.na(x) & g %in% c(first, second)
  d <- data.frame(x = x[keep], g = as.character(g[keep]))
  a <- d$x[d$g == first]
  b <- d$x[d$g == second]
  if (length(a) == 0 || length(b) == 0) {
    return(data.frame(contrast = paste(first, "vs", second), n_first = length(a), n_second = length(b),
                      median_first = NA, median_second = NA, superiority = NA, ci_low = NA, ci_high = NA, p = NA))
  }
  ci <- boot_percentile(d, function(s) superiority(s$x[s$g == first], s$x[s$g == second]), factor(d$g), seed)
  p <- suppressWarnings(stats::wilcox.test(a, b, exact = FALSE, correct = TRUE)$p.value)
  data.frame(contrast = paste(first, "vs", second), n_first = length(a), n_second = length(b),
             median_first = stats::median(a), median_second = stats::median(b),
             superiority = superiority(a, b), ci_low = ci[1], ci_high = ci[2], p = p)
}

# All conditions at once: Kruskal-Wallis with epsilon squared and its CI.
kruskal_effect <- function(x, g, seed) {
  keep <- !is.na(x) & !is.na(g)
  d <- data.frame(x = x[keep], g = droplevels(as.factor(g[keep])))
  k <- stats::kruskal.test(d$x, d$g)
  ci <- boot_percentile(d, function(s) epsilon_squared(s$x, s$g), d$g, seed)
  data.frame(participants = nrow(d), groups = nlevels(d$g), H = unname(k$statistic), df = unname(k$parameter),
             p = k$p.value, epsilon_squared = epsilon_squared(d$x, d$g), ci_low = ci[1], ci_high = ci[2])
}

# Every participant-level measure, scored from data/forms.csv, with step accuracy from the logs for
# the calibration gaps. One row per participant.
score_forms <- function(forms, tasks) {
  order_positions <- as.matrix(forms[, paste0("recall_order_step", RECALL_ORDER)])
  colnames(order_positions) <- RECALL_ORDER
  accuracy <- tasks |>
    dplyr::select(participant, task, correct_steps, n_steps) |>
    tidyr::pivot_wider(names_from = task, values_from = c(correct_steps, n_steps))
  out <- forms |> dplyr::left_join(accuracy, by = "participant")
  out$sus <- score_sus(forms[, sprintf("sus_%d", 1:10)])
  out$tlx_condition_repair <- score_tlx(forms[, paste0("f2_tlx_", names(TLX_SUBSCALES))])
  out$tlx_cue_free_repair <- score_tlx(forms[, paste0("f3_tlx_", names(TLX_SUBSCALES))])
  out$recall_item2 <- out$recall_item2_part1 + out$recall_item2_part2 + out$recall_item2_part3
  out$recall_item4 <- ifelse(rowSums(!is.na(order_positions)) == 0, NA, score_recall_order(order_positions))
  out$recall_score <- score_recall(out$recall_item1, out$recall_item2, out$recall_item4)
  for (repair in c("condition_repair", "cue_free_repair")) {
    stated <- out[[if (repair == "condition_repair") "f2_stated_correct_pct" else "f3_stated_correct_pct"]]
    gap <- calibration_gap(stated, out[[paste0("correct_steps_", repair)]], out[[paste0("n_steps_", repair)]])
    out[[paste0("accuracy_", repair)]] <- gap$accuracy
    out[[paste0("gap_", repair)]] <- gap$absolute_gap
    out[[paste0("signed_gap_", repair)]] <- gap$signed_gap
  }
  out
}
