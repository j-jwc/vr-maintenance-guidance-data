# Data and code: Decisional Guidance and the Reliance Effect of Spatial Cues among Novices Performing Maintenance in Virtual Reality

This repository holds the anonymised session logs of the pre-registered experiment reported in the
paper, and the Python code that computes the paper's measures from them. The registration is on
AsPredicted, #321454 (anonymous version: https://aspredicted.org/pj5pd9.pdf).

39 participants each carried out two repairs of a full-size virtual parcel sorter: the condition
repair, in one of three conditions, then the cue-free repair, with step cards alone for everyone.

| Condition | Code in the logs |
|---|---|
| Manual-only | `C1` |
| Step-card | `C2` |
| Spatial-cue | `C3` |

## Contents

| Path | Contents |
|---|---|
| `logs/` | 78 session logs, one per participant and repair: `P01_condition_repair.csv` to `P39_cue_free_repair.csv`. |
| `data/` | The tables the models were fitted to: one row per step, repair, manual lookup and participant, the work before isolation in each condition repair, and the repairs run but not analysed. |
| `CODEBOOK.md` | The conditions and repairs, how the logs were anonymised, and every log and table column. |
| `session_metrics.py` | The study's measurement script, unchanged. From one log it computes, for each step, the time on the step, the decision time, the execution time, both also with manual reading removed, the wrong parts and wrong tools (errors), the work before isolation, and whether the step is correct; then the repair's totals and its manual lookups. |
| `extract_steps.py` | The script that ran `session_metrics.py` on every analysed log and laid its output out as the analysis tables (one row per step, repair, lookup and participant). It also computes the post hoc count of participants who worked on the later steps before completing isolation (`isolation_work`). |

## Computing the measures

Python 3.10 or later, standard library only.

    python3 session_metrics.py logs/P01_condition_repair.csv

prints one line per step and then the repair's totals. Every time is in seconds, on the log's clock
with the pauses taken out. Each anonymised log gives exactly the measures of the original log.

`extract_steps.py` reads the original logs, under their original file names, and the study's
participant list, which are not shared. It therefore shows how the analysis tables were made rather
than running on these copies. Its build identifier was removed, as it was from the logs.

## Reproducing the analyses

| Script | Paper |
|---|---|
| `R/01_confirmatory.R` | Hypotheses 1 to 5, Holm-adjusted (Table 4), and the model estimates of Fig. 3 |
| `R/02_exploratory.R` | Hypotheses 6 and 7, work before isolation (Hypothesis 8), the interaction of condition and repair, total time, and the sensitivity analyses |
| `R/06_posthoc_participant_means.R` | The post hoc check of Hypotheses 2, 3 and 5 on participant means |
| `R/07_posthoc_h5.R` | The post hoc robustness checks of Hypothesis 5 and its decision time per step (Supplementary Table S2) |

`R/00_load.R` loads the tables, `R/utils.R` fits the models and records their convergence, and
`R/forms.R` holds the scoring rules of the forms.

R 4.5.3, with the packages pinned in `renv.lock`. From this folder:

    Rscript -e 'renv::restore()'
    Rscript R/01_confirmatory.R
    Rscript R/02_exploratory.R
    Rscript R/06_posthoc_participant_means.R
    Rscript R/07_posthoc_h5.R

The scripts are those of the analysis, unchanged except that one sensitivity analysis, which the
paper does not report, was taken out. The models were built and checked on shuffled
condition labels before the real labels were read, and the scripts keep that switch: the run on
the real labels also read the participant forms, which are not shared, so here the scripts run in
their default setting and write to `results/masked/`. The tables in `data/` carry the real
conditions, so the values written there are the paper's: on these tables, the 15 result files
matched the analysis's own row for row, apart from the rows of the sensitivity analysis taken out.

## Not included

The participant forms (background, ratings, recall quiz), the manufacturer's manual pages, and the
logs of tasks that were not analysed. Without the forms, the recall part of Hypothesis 8, Hypotheses
9 and 10 (confidence calibration, workload and usability), ease, the recall quiz, the participant
table and the descriptive tables cannot be rerun from this repository.
