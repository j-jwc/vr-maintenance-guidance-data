# Codebook: anonymised share package


39 participants, 78 session logs (one per participant and repair), collected on 7 days.

## Files

- `logs/<participant>_<repair>.csv`: the session log of one repair, as the headset wrote it, anonymised (below).
- `data/steps.csv`, `data/tasks.csv`, `data/lookups.csv`, `data/participants.csv`: the analysis tables, as
  `extract_steps.py` wrote them from the logs with `session_metrics.py`'s measures.
- `data/isolation_work.csv`: work before isolation in the condition repair.
- `data/not_analysed_tasks.csv`: the repairs run but not analysed, and why.

## Conditions and repairs

- `C1`: manual-only condition.
- `C2`: step-card condition.
- `C3`: spatial-cue condition.
- `condition_repair` (`task_id` `aided` in the logs): the first 10 steps of a drive roller replacement, with the participant's own condition.
- `cue_free_repair` (`task_id` `transfer` in the logs): a nine-step belt frame replacement, with step cards only for every participant.

## Anonymisation

- Removed from every log: the build version, the headset's model and id, and the time the file was opened.
- Clocks: in each log, `elapsed_s` counts seconds from the file's first row, and `utc_time` keeps each
  row's offset from the file's first row on a clock that starts at 1970-01-01T00:00:00.000Z. No measure
  depends on the clocks' origin: each log gives the same measures before and after the shift.
- Tables: clock times are removed; `session_day` numbers the days of data collection from 1; the
  gap between a participant's two repairs is kept, in minutes (`gap_min`).
  `not_analysed_tasks.csv` gives no file names.
- Not included: the participant forms, the manual and its pages, and the logs not analysed.
- `P01_cue_free_repair.csv`: the log's `participant_code` reads P06: P01's complete second attempt at the cue-free repair, run under code P06. It is analysed as P01's; the attempt it replaces is not shared.

## Log columns

| Column | Meaning |
|---|---|
| `record` | Row type: `step`, `procedure`, `practice`, `session` or `event`. |
| `utc_time` | When the row was written, on the shifted clock (Anonymisation). |
| `elapsed_s` | Seconds from the file's first row to when the row was written. |
| `scene` | The scene shown. |
| `procedure` | The procedure's title. Empty outside a procedure. |
| `step_index` | The step's position in the procedure, from 1. On an `event` row, the active step (in `C1`, the step expected next). |
| `step_id` | The step's number as its card shows it. Empty when `step_index` is. |
| `duration_s` | For `step`, `procedure`, `practice` and `session` rows, seconds from start to end, paused time included. |
| `first_action_s` | For those rows, seconds from the start to the first action within it; empty when there was none. |
| `auto_hint` | For those rows, `yes` if an automatic hint came during it, else `no`. |
| `object` | On an event: the part or tool picked, tried, worked or pointed at; on `tool_applied`, the tool; on `condition_applied`, the condition; on `cue_shown` and `cue_hidden`, what the cue is on; on `manual_lookup_end`, the pages read, numbered as the PDF counts them. |
| `expected` | On `wrong_tool_picked`, the tools the active step needs; on `wrong_part_attempt`, the step's fasteners (`none` when it needs none); on `safety_skip`, what was not yet done (`zone`, `isolation` or both); on `tool_applied`, the part the tool was used on; on `condition_applied`, the guidance flags as applied. |
| `detail` | On `step`, `procedure` and `practice` rows, how it ended (`completed`, `left early`, `timed out`, and for a step passed over `skipped` or `completed out of order`); on `session` rows, `start` or `ended`; on an `event` row, the event's name. |
| `practice` | The practice under way; empty outside one. |
| `schema_version` | The columns' version, `1`. |
| `participant_code` | The participant code. |
| `condition` | The condition assigned to the participant. |
| `guidance` | The guidance the repair runs with: the condition on the condition repair, `C2` on the cue-free repair. |
| `task_id` | `aided` (condition repair) or `transfer` (cue-free repair). |
| `task_order` | The repair's place in the session, from 1. |
| `time_cap_s` | The repair's time cap in seconds. |
| `completion_reason` | On a completed `step` row, `user` or `auto`; `skipped` on a skipped step; on a `procedure` row, `completed`, `capped`, `timed_out` or `left early`. |
| `active_duration_s` | `duration_s` with paused time taken out. |
| `cue_level` | On a first correct action and `tool_applied`, the level of the cue on show on what was acted on; `1` on `cue_shown` and `cue_hidden`. |
| `cue_type` | With `cue_level`, the kind of cue: `part_tag` or `hint_ladder` (the target cue). |
| `manual_lookup_s` | On `step`, `procedure`, `practice` and `session` rows, the time spent on manual lookups, pauses taken out; on `manual_lookup_end`, that lookup's length. |

## Table columns

Times are in seconds on the pause-adjusted clock. An empty time has its reason in the matching `*_note` column.

`data/steps.csv`: `participant`, `condition`, `task` (repair), `step` (position, from 1), `step_id`;
`t` (time the step was current), `d` (decision time: step start to the first correct action), `e`
(execution time: first correct action to completion), `d_net` and `t_net` (with manual reading
removed); `correct` (1 when completed, in order, not skipped, with no wrong tool and no work before
isolation while current); `wrong_parts` (refused fastener attempts), `wrong_tools` (wrong tools
picked), `errors` (their sum), `safety_skips` (work before isolation); `completion_reason`;
`row_detail` (how the step line ended); `auto` (1 when the app completed the step because an earlier
action had already done it); `t_note`, `d_note`, `e_note`.

`data/tasks.csv`: `participant`, `condition`, `task`, `file` (the shared log); `T` (repair time),
`m` (manual lookup time), `T_net` (`T` without `m`), `active` (active time); `outcome`
(`completed`, `capped`, `timed_out`, `left early`), `completed` (1 or 0); `correct_steps`,
`n_steps`, `missed` (steps never reached); `n_lookups`; `wrong_parts_outside`, `wrong_tools_outside`,
`safety_skips_outside` (events while no step was current); `notes`; `pauses`, `pauses_over_60s`,
`longest_pause_s` (the log's pauses, from each `paused` event to the `resumed` after it).

`data/lookups.csv`: `participant`, `condition`, `task`, `lookup` (its number), `step` (the active
step), `start_s` (from the repair's start), `length_s`, `pages` (pages read, as the PDF counts them).

`data/participants.csv`: `participant`, `condition`, `session_day`, `gap_min` (minutes from the end
of the condition repair to the opening of the cue-free repair's log).

`data/isolation_work.csv`, one row per condition repair: `participant`, `condition`, `task`;
`isolation_complete_s` (when steps 1 and 2, blocking off the area and locking the main switch, were
both completed, in seconds after the repair's start on the log's `elapsed_s` clock; empty when either
was skipped or never completed); `isolation_step_skipped` (1 when step 1 or 2 was ever skipped);
`work_before_isolation_complete` (the work events on step 3 or later, and completions of such steps,
before that moment); `events` (those events, each with its step and time).

`data/not_analysed_tasks.csv`, the repairs run but not analysed: `participant` (empty for the
researcher's own runs), `task`, `outcome`, `last_step`, `active` (active time), `reason`.
