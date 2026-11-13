#!/usr/bin/env python3
"""Writes the study's per-step, per-task and per-lookup tables from the task logs, as session_metrics.py measures them.

Every measure comes from vendor/session_metrics.py, the study build's script (at the
registered build freeze), imported unchanged: its md5 is checked before it is
imported. This file only decides which log is whose task (the first-day rules of DEVIATIONS.txt,
items 1 to 3) and lays the script's report out as tables.

The condition is never read from a log or a file name, both of which carry the real one: it comes
from the conditions table given with --conditions (data/conditions_masked.csv until the analysis
is frozen, data/conditions_real.csv after), and the table's kind is written beside it.

Outputs, in data/:
- steps.csv: one row per step of each analysed task, steps never reached included;
- tasks.csv: one row per analysed task;
- lookups.csv: one row per manual lookup the log records;
- participants.csv: one row per participant, with the gap between the two repairs;
- files.csv: every task file found, analysed or not, and why;
- not_analysed_tasks.csv: the task files with a task that are not analysed, as the script measures them;
- isolation_work.csv: per condition repair, the work that reached step 3 or later before steps 1 and 2
  were both done (work before isolation; isolation_work below).

    python3 extract_steps.py --conditions data/conditions_masked.csv
"""

import argparse
import csv
import hashlib
import importlib.util
import json
import re
import sys
from datetime import datetime
from pathlib import Path

HERE = Path(__file__).resolve().parent
DATA = HERE / "data"
VENDORED = HERE / "vendor" / "session_metrics.py"
VENDORED_MD5 = "5303609cefab354580f357eb27e04b21"
# The registered build: every study log must come from it (DEVIATIONS.txt, preamble).
BUILD = "removed from this anonymised copy, as from the logs"
TASK_FILE = re.compile(r"^(P\d{2})_[^_]+_(aided|transfer)_(\d{8})_(\d{6})\.csv$")
TASK_NAMES = {"aided": "condition_repair", "transfer": "cue_free_repair"}

# DEVIATIONS.txt, item 1: P01's complete second attempt at the cue-free repair ran under code P06.
REASSIGNED = {
    "P06_C2_transfer_20261102_032710.csv": ("P01", "P01's complete second attempt at the cue-free repair, run under code P06 (DEVIATIONS item 1)"),
}
# DEVIATIONS.txt, items 1 to 3: task files of day 1 that are no participant's analysed task.
NOT_ANALYSED = {
    "P01_C3_transfer_20261102_024248.csv": ("P01", "P01's first attempt at the cue-free repair, ended in step 2 when she was called away; reported, not analysed (DEVIATIONS item 1)"),
    "P05_C1_aided_20261102_031126.csv": ("", "code P05 used by the researcher on day 1 and returned; no participant (DEVIATIONS item 2)"),
    "P06_C2_aided_20261102_032350.csv": ("", "run by the researcher under code P06 on day 1 to reach P01's second attempt; no participant (DEVIATIONS item 2)"),
    "P04_C3_transfer_20261102_031103.csv": ("P04", "opened and closed before Start, so it holds no task (DEVIATIONS item 3)"),
}
# Codes P05 and P06 of day 1 belong to no participant (DEVIATIONS item 2): any such file must be named above.
RETURNED_CODES = {("P05", "20261102"), ("P06", "20261102")}

STEP_COLUMNS = (
    "participant", "condition", "conditions_source", "task", "step", "step_id", "t", "d", "e", "d_net", "t_net",
    "correct", "wrong_parts", "wrong_tools", "errors", "safety_skips", "completion_reason", "row_detail", "auto",
    "t_note", "d_note", "e_note",
)
TASK_COLUMNS = (
    "participant", "condition", "conditions_source", "task", "file", "file_opened_utc", "task_start_utc", "task_end_utc",
    "T", "m", "T_net", "active", "outcome", "completed", "correct_steps", "n_steps", "missed", "n_lookups",
    "wrong_parts_outside", "wrong_tools_outside", "safety_skips_outside", "notes",
    "pauses", "pauses_over_60s", "longest_pause_s",
)
# Pauses longer than this are counted per file, a check on how often a session was held up.
LONG_PAUSE_S = 60.0
LOOKUP_COLUMNS = ("participant", "condition", "conditions_source", "task", "lookup", "step", "start_s", "length_s", "pages")
# Work before isolation, as the author defined it on 10 November 2026: in the condition
# repair, step 1 sets up the zone and step 2 isolates the machine. The moment both are completed is
# the moment isolation is complete; a step 1 or 2 that was ever skipped counts as never completed.
# Before that moment, every correct work event on step 3 or later, or a completed row of such a step,
# is work reaching the work steps. safety_skip is not this count: it counts attempts, refused or not.
ISOLATION_STEPS = ("1", "2")
WORK_EVENTS = ("correct_part_grabbed", "correct_interaction", "tool_applied")
DONE_DETAILS = ("completed", "completed out of order")
ISOLATION_COLUMNS = ("participant", "condition", "conditions_source", "task", "isolation_complete_s", "isolation_step_skipped",
                     "work_before_isolation_complete", "events")
PARTICIPANT_COLUMNS = (
    "participant", "condition", "conditions_source", "session_date", "condition_repair_end_utc", "cue_free_repair_opened_utc", "gap_min",
)
FILE_COLUMNS = ("file", "day", "code_in_name", "task", "participant", "analysed", "reason")
NOT_ANALYSED_COLUMNS = ("file", "participant", "task", "file_opened_utc", "task_start_utc", "task_end_utc", "outcome", "last_step", "active", "reason")


def load_metrics():
    """The study build's session_metrics.py, after checking that it is the unchanged file."""
    digest = hashlib.md5(VENDORED.read_bytes()).hexdigest()
    if digest != VENDORED_MD5:
        raise SystemExit(f"{VENDORED} has md5 {digest}, not {VENDORED_MD5}: it is not the study build's script, unchanged")
    spec = importlib.util.spec_from_file_location("session_metrics", VENDORED)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def utc(text):
    return datetime.strptime(text, "%Y-%m-%dT%H:%M:%S.%fZ")


def value(x):
    """A measured time as a number for the CSV, or empty when the script gives a reason instead."""
    return repr(x) if isinstance(x, float) else ""


def note(x):
    return "" if isinstance(x, float) else str(x)


def read_conditions(path):
    with open(path, newline="", encoding="utf-8") as f:
        reader = csv.DictReader(f)
        if reader.fieldnames != ["participant", "condition", "source"]:
            raise SystemExit(f"{path} must have the columns participant, condition, source")
        rows = list(reader)
    sources = {r["source"] for r in rows}
    if len(sources) != 1 or not sources <= {"masked", "real"}:
        raise SystemExit(f"{path}: source must be one of masked or real, the same on every row")
    return {r["participant"]: r["condition"] for r in rows}, sources.pop()


def read_roster(path):
    """The participants present: entries of study-main.json not removed and with the condition repair done."""
    entries = json.loads(path.read_text(encoding="utf-8"))
    return [e for e in entries if not e.get("removed") and e.get("aided_done")]


def assign(files):
    """Each task file's participant, whether it is analysed, and why; one analysed file per participant and task."""
    assigned = []
    for path in files:
        m = TASK_FILE.match(path.name)
        code, task, day = m.group(1), m.group(2), m.group(3)
        if path.name in REASSIGNED:
            participant, reason = REASSIGNED[path.name]
            assigned.append((path, code, task, participant, True, reason))
        elif path.name in NOT_ANALYSED:
            participant, reason = NOT_ANALYSED[path.name]
            assigned.append((path, code, task, participant, False, reason))
        elif (code, day) in RETURNED_CODES:
            raise SystemExit(f"{path.name}: a day-1 file under a returned code that no rule names (DEVIATIONS item 2)")
        else:
            assigned.append((path, code, task, code, True, ""))
    return assigned


def task_times(rows):
    """When the file opened, when Start was pressed, and when the task ended (its procedure row, or the file's last row)."""
    opened = rows[0]["utc_start"] or rows[0]["utc_time"]
    start = next((r["utc_time"] for r in rows if r.event == "task_start"), "")
    end = next((r["utc_time"] for r in rows if r["record"] == "procedure"), rows[-1]["utc_time"])
    return opened, start, end


def check_identity(path, rows, task, participant_in_log):
    first = rows[0]
    if first["task_id"] != task:
        raise SystemExit(f"{path.name}: task_id {first['task_id']} does not match its name")
    if first["participant_code"] != participant_in_log:
        raise SystemExit(f"{path.name}: participant_code {first['participant_code']} does not match its name")
    builds = {r["build_version"] for r in rows if r["build_version"]}
    if builds != {BUILD}:
        raise SystemExit(f"{path.name}: build {sorted(builds)}, not the registered {BUILD}")
    if task == "transfer" and first["guidance"] != "C2":
        raise SystemExit(f"{path.name}: a cue-free repair must run with the step-card flags (guidance C2)")


def pause_counts(pauses):
    """The file's pauses, as the script finds them: how many, how many over 60 s, the longest in seconds."""
    lengths = [b - a for a, b in pauses]
    return len(lengths), sum(1 for x in lengths if x > LONG_PAUSE_S), repr(max(lengths)) if lengths else ""


def isolation_work(rows):
    """Steps 1 and 2's completion, seconds after task_start on the log's clock (empty if never), whether
    either was skipped, and the work events on step 3 or later before it (ISOLATION_STEPS above)."""
    started = next(i for i, row in enumerate(rows) if row.event == "task_start")
    task, zero = rows[started:], rows[started].elapsed
    steps = [row for row in task if row["record"] == "step"]
    skipped = any(row["detail"] == "skipped" and row["step_index"] in ISOLATION_STEPS for row in steps)
    done = {}
    for row in steps:
        if row["detail"] in DONE_DETAILS and row["step_index"] in ISOLATION_STEPS:
            done.setdefault(row["step_index"], row.elapsed)
    moment = None if skipped or set(done) != set(ISOLATION_STEPS) else max(done.values())
    work = [row for row in task
            if (moment is None or row.elapsed < moment) and row["step_index"].isdigit() and int(row["step_index"]) >= 3
            and (row.event in WORK_EVENTS or (row["record"] == "step" and row["detail"] in DONE_DETAILS))]
    found = " | ".join(f"step {row['step_index']} {row.event or row['detail']} {row['object']} at {row.elapsed - zero:.3f} s".replace("  ", " ")
                       for row in work)
    return "" if moment is None else f"{moment - zero:.3f}", int(skipped), len(work), found


def write(path, columns, rows):
    with open(path, "w", newline="", encoding="utf-8") as f:
        writer = csv.writer(f, lineterminator="\n")
        writer.writerow(columns)
        writer.writerows(rows)


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("--conditions", type=Path, required=True, help="data/conditions_masked.csv or data/conditions_real.csv")
    parser.add_argument("--data", type=Path, default=DATA, help="the folder holding logs/ and study-main.json (default data/)")
    args = parser.parse_args(argv)

    metrics = load_metrics()
    conditions, source = read_conditions(args.conditions)
    roster = read_roster(args.data / "study-main.json")
    present = [e["code"] for e in roster]
    missing = sorted(set(present) - set(conditions))
    if missing:
        raise SystemExit(f"{args.conditions} has no condition for {', '.join(missing)}: rebuild it for the participants present")

    files = sorted((p for p in (args.data / "logs").glob("*/*.csv") if TASK_FILE.match(p.name)), key=lambda p: p.name)
    assigned = assign(files)

    steps, tasks, lookups, file_rows, not_analysed, ends, isolation = [], [], [], [], [], {}, []
    seen = {}
    for path, code, task, participant, analysed, reason in assigned:
        rows = metrics.read_log(path)
        has_task = any(r.event == "task_start" for r in rows)
        file_rows.append((path.name, path.parent.name, code, TASK_NAMES[task], participant, int(analysed), reason))
        if not analysed:
            if has_task:
                report = metrics.measure(rows, path.name)
                opened, start, end = task_times(rows)
                last = max((int(s.index) for s in report.steps if s.index.isdigit()), default="")
                not_analysed.append((path.name, participant, TASK_NAMES[task], opened, start, end, report.outcome, last, repr(report.active), reason))
            continue
        if not has_task:
            raise SystemExit(f"{path.name} has no task_start and no rule names it: decide with DEVIATIONS.txt before analysing")
        if participant not in present:
            raise SystemExit(f"{path.name}: {participant} is not a participant present in study-main.json")
        check_identity(path, rows, task, code)
        key = (participant, task)
        if key in seen:
            raise SystemExit(f"{participant} has two analysed {TASK_NAMES[task]} files: {seen[key]} and {path.name}")
        seen[key] = path.name

        report = metrics.measure(rows, path.name)
        condition, label = conditions[participant], TASK_NAMES[task]
        for line in report.lines:
            s = line.step
            reasons = [r for r in line.reason.split(", ") if r and r != "-"]
            steps.append((
                participant, condition, source, label, line.number, s.step_id, value(s.t), value(s.d), value(s.e), value(s.d_net), value(s.t_net),
                line.correct, line.wrong_parts, line.wrong_tools, line.errors, line.safety_skips, line.reason, line.detail,
                int("auto" in reasons), note(s.t), note(s.d), note(s.e),
            ))
        opened, start, end = task_times(rows)
        tasks.append((
            participant, condition, source, label, path.name, opened, start, end, repr(report.T), repr(report.m), repr(report.T_net),
            repr(report.active), report.outcome, "" if report.finished is None else int(report.finished),
            sum(l.correct for l in report.lines), report.total, " ".join(map(str, report.missed or [])), len(report.lookups),
            *report.outside, " | ".join(report.notes),
            *pause_counts(metrics.spans(rows, "paused", "resumed")),
        ))
        for n, lookup in enumerate(report.lookups, start=1):
            lookups.append((participant, condition, source, label, n, lookup.step, repr(lookup.start), value(lookup.length), lookup.pages))
        ends[key] = (opened, end)
        if task == "aided":
            isolation.append((participant, condition, source, label, *isolation_work(rows)))

    absent = [(p, t) for p in present for t in ("aided", "transfer") if (p, t) not in seen
              and next(e for e in roster if e["code"] == p).get("transfer_done" if t == "transfer" else "aided_done")]
    if absent:
        raise SystemExit("no analysed file for " + ", ".join(f"{p} {TASK_NAMES[t]}" for p, t in absent) + ": a lost log is an exclusion reason; record it before analysing")

    participants = []
    for p in present:
        aided, transfer = ends.get((p, "aided")), ends.get((p, "transfer"))
        gap = (utc(transfer[0]) - utc(aided[1])).total_seconds() / 60 if aided and transfer else None
        participants.append((p, conditions[p], source, aided[0][:10] if aided else "", aided[1] if aided else "",
                             transfer[0] if transfer else "", "" if gap is None else repr(gap)))

    args.data.mkdir(exist_ok=True)
    write(args.data / "steps.csv", STEP_COLUMNS, steps)
    write(args.data / "tasks.csv", TASK_COLUMNS, tasks)
    write(args.data / "lookups.csv", LOOKUP_COLUMNS, lookups)
    write(args.data / "participants.csv", PARTICIPANT_COLUMNS, participants)
    write(args.data / "files.csv", FILE_COLUMNS, file_rows)
    write(args.data / "not_analysed_tasks.csv", NOT_ANALYSED_COLUMNS, not_analysed)
    write(args.data / "isolation_work.csv", ISOLATION_COLUMNS, isolation)
    print(f"{len(present)} participants, {len(tasks)} tasks, {len(steps)} step rows, {len(lookups)} lookups; "
          f"{sum(1 for f in file_rows if not f[5])} task files not analysed; conditions {source}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
