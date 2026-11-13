#!/usr/bin/env python3
"""Prints a study task's step times, cues and error counts from its session log, as the study measures them.

It reads one study task's file from the headset (xr/LOGGING.md describes it) and prints, for each
step: its time t, its decision time d to the first correct action (the first of
correct_part_grabbed, correct_tool_picked and correct_interaction), its execution time e = t - d,
the cue on show at that first correct action, its wrong parts, wrong tools and safety skips, and
how it was completed, whether it is correct (1 or 0) and its errors (wrong parts plus wrong tools).
A step is correct when it has a completed row (by the participant or by the machine), has no skipped
row and no completed out of order row, and has no wrong_tool_picked and no safety_skip event with
its step_index; a step timed out, left early, skipped or never reached is 0. Each step of the task
has one line, 1 to its last step, the steps never reached included; a step with two rows (skipped,
then completed out of order) keeps one line that names both in its completion column. Then the task: its task_start, its time T over the steps the participant
completed (completion_reason user), its manual time m and its net time T_net, how it ended, and
the steps missed and its count of correct steps out of the task's steps (correct steps 7/10). Every time is on the log's clock with the pauses taken out. Steps completed by
the machine (completion_reason auto) have no d or e. Rows before task_start (the tutorial) are
not part of the task.

A skipped row whose step has a step_start of its own and a duration (the step cards' Next, which
skips the step under way) ends that step's stretch, so it has a t, and a d if a correct action came
before the skip. A skipped row with no step_start of its own or no duration (a step passed over in
C1) was never under way on its own and has no time. A completed out of order row with no duration
(a step skipped from the card and done later) has no time either, since its time is in its skipped
row, and the stretch of the step under way goes on past it.

The manual time comes from the log's own manual_lookup events; with --lookups it comes from the
researcher's stopwatch sheet instead, a CSV with the columns participant_code, task, start_s and
end_s (seconds from the participant's Start press, which writes task_start). Each lookup is taken
out of t and d of the step it falls in, and the net values are printed beside the gross ones.
The log's own lookups are listed last, each with its step, its length and, for a manual read in
the headset, the pages read in it. A value that cannot be worked out names the event it lacks.

    python3 xr/Tools/session_metrics.py T01_C3_aided_20261025_093000.csv
    python3 xr/Tools/session_metrics.py T01_C3_aided_20261025_093000.csv --lookups lookups.csv

Standard library only.
"""

import argparse
import csv
import sys
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, List, Optional, Tuple, Union

SCHEMA_VERSION = "1"
NEEDED = (
    "record", "elapsed_s", "utc_time", "step_index", "step_id", "object", "detail", "procedure", "schema_version",
    "participant_code", "condition", "guidance", "task_id", "time_cap_s", "completion_reason", "active_duration_s",
    "cue_level", "cue_type",
)
CORRECT = ("correct_part_grabbed", "correct_tool_picked", "correct_interaction")
LOOKUP_COLUMNS = ("participant_code", "task", "start_s", "end_s")
# How many steps each study task has, so the steps never started can be named: the aided task
# stops at the drive roller repair's 10th step (StudyTask.StepCap), the transfer task runs all 9 of the belt frame's.
TASK_STEPS = {"aided": 10, "transfer": 9}
# A step row that ends the stretch from its step_start. A skipped row ends it only when the step was under way (skipped_under_way).
ENDED = ("completed", "completed out of order", "timed out", "left early")
DONE = ("completed", "completed out of order")
FINISHED = ("completed", "capped")
NO_CORRECT = "no correct_* event"

# A value, or what keeps it from being worked out.
Value = Union[float, str]


class LogError(Exception):
    """The file is not a session log this script can read."""


@dataclass
class Row:
    fields: dict
    elapsed: float

    def __getitem__(self, name):
        return self.fields.get(name) or ""

    @property
    def event(self):
        return self["detail"] if self["record"] == "event" else ""


@dataclass
class Step:
    index: str
    step_id: str
    detail: str
    reason: str
    t: Value
    t_net: Value
    d: Value
    d_net: Value
    e: Value
    cue: str
    wrong_parts: int
    wrong_tools: int
    safety_skips: int

    @property
    def counted(self):
        """Whether the step's time counts in T: completed, with an action of the participant's in it, and timed (a step done after its skip is not)."""
        return self.detail in DONE and self.reason == "user" and not isinstance(self.t, str)


@dataclass
class Line:
    """One step number's line in the table: all its rows merged, with its verdict."""
    number: str
    step: Step
    detail: str
    reason: str
    wrong_parts: int
    wrong_tools: int
    safety_skips: int
    correct: int

    @property
    def errors(self):
        return self.wrong_parts + self.wrong_tools


@dataclass
class Lookup:
    """One of the log's own manual lookups in the task."""
    start: float
    step: str
    length: Value
    pages: str


@dataclass
class Report:
    name: str
    participant: str
    condition: str
    guidance: str
    task: str
    time_cap: str
    task_start: str
    lookups_from: str
    steps: List[Step]
    outside: Tuple[int, int, int]
    T: float
    T_net: float
    m: float
    m_log: Optional[float]
    active: float
    outcome: str
    finished: Optional[bool]
    missed: Optional[List[int]]
    lines: List[Line]
    total: Optional[int]
    notes: List[str] = field(default_factory=list)
    lookups: List[Lookup] = field(default_factory=list)


def read_log(path):
    """The file's rows, in the order they were written."""
    with open(path, newline="", encoding="utf-8") as f:
        reader = csv.DictReader(f)
        missing = [c for c in NEEDED if c not in (reader.fieldnames or [])]
        if missing:
            raise LogError(f"{path} is not a session log: it has no {', '.join(missing)} column")
        rows = []
        for n, fields in enumerate(reader, start=2):
            try:
                rows.append(Row(fields, float(fields["elapsed_s"])))
            except (TypeError, ValueError):
                raise LogError(f"{path}, line {n}: elapsed_s is not a number") from None
    if not rows:
        raise LogError(f"{path} has no rows")
    return rows


def read_lookups(path, participant, task):
    """The stopwatch's lookups for one participant's task, as (start, end) seconds from task_start."""
    with open(path, newline="", encoding="utf-8") as f:
        reader = csv.DictReader(f)
        missing = [c for c in LOOKUP_COLUMNS if c not in (reader.fieldnames or [])]
        if missing:
            raise LogError(f"{path} has no {', '.join(missing)} column")
        lookups = []
        for n, fields in enumerate(reader, start=2):
            if fields["participant_code"].strip() != participant or fields["task"].strip() != task:
                continue
            try:
                start, end = float(fields["start_s"]), float(fields["end_s"])
            except (TypeError, ValueError):
                raise LogError(f"{path}, line {n}: start_s and end_s must be numbers") from None
            if end < start:
                raise LogError(f"{path}, line {n}: end_s comes before start_s")
            lookups.append((start, end))
    return lookups


def spans(rows, opened, closed):
    """The stretches from each opened event to the closed event after it; one still open ends with the file."""
    found, start = [], None
    for row in rows:
        if row.event == opened and start is None:
            start = row.elapsed
        elif row.event == closed and start is not None:
            found.append((start, row.elapsed))
            start = None
    if start is not None:
        found.append((start, rows[-1].elapsed))
    return found


def overlap(lo, hi, intervals):
    return sum(max(0.0, min(hi, b) - max(lo, a)) for a, b in intervals)


def active(lo, hi, pauses):
    """Seconds from lo to hi with the pauses taken out."""
    return max(0.0, hi - lo - overlap(lo, hi, pauses))


def looked_up(lo, hi, lookups, pauses):
    """Seconds from lo to hi spent on manual lookups, their pauses taken out."""
    return sum(active(max(lo, a), min(hi, b), pauses) for a, b in lookups if min(hi, b) > max(lo, a))


def lookups_in(task, task_start):
    """
    The log's own lookups in the task: when each began, in seconds from task_start, its step, its
    length (manual_lookup_s, pauses out) and the pages read in it (manual_lookup_end's object).
    """
    found, opened = [], None
    for row in task:
        if row.event == "manual_lookup_start" and opened is None:
            opened = row
        elif row.event == "manual_lookup_end" and opened is not None:
            length = number(row["manual_lookup_s"])
            found.append(Lookup(opened.elapsed - task_start.elapsed, opened["step_index"],
                                length if length is not None else "no manual_lookup_s", row["object"]))
            opened = None
    if opened is not None:
        found.append(Lookup(opened.elapsed - task_start.elapsed, opened["step_index"], "no manual_lookup_end", ""))
    return found


def number(text):
    try:
        return float(text)
    except (TypeError, ValueError):
        return None


def measure(rows, name, stopwatch=None):
    """
    The task's measures from its rows. A step's stretch runs from its step_start to its row, so in
    C1, where step_start follows each step done and the next step expected may not be the one done
    next, a step's decision time runs from the step done before it, as its duration does.
    """
    first = rows[0]
    started = next((i for i, row in enumerate(rows) if row.event == "task_start"), None)
    if started is None:
        raise LogError(f"{name} has no task_start event: Start was never pressed while it was open, so it holds no task to measure")
    task_start = rows[started]
    task = rows[started:]
    pauses = spans(rows, "paused", "resumed")
    log_lookups = spans(task, "manual_lookup_start", "manual_lookup_end")
    notes = []
    if first["schema_version"] != SCHEMA_VERSION:
        notes.append(f"schema_version {first['schema_version'] or 'missing'}, not {SCHEMA_VERSION}: the columns may not mean what this script reads them as")

    if stopwatch is None:
        lookups, lookups_from = log_lookups, "the log's manual_lookup events"
    else:
        lookups = [(task_start.elapsed + a, task_start.elapsed + b) for a, b in stopwatch]
        lookups_from = "the researcher's stopwatch (lookups.csv)"
        if not stopwatch:
            notes.append(f"lookups.csv has no rows for {first['participant_code']} {first['task_id']}: m is 0")

    steps, outside = [], [0, 0, 0]
    opened, opened_index, events = None, "", []
    procedure = None
    for row in task:
        if row.event == "step_start":
            opened, opened_index, events = row.elapsed, row["step_index"], []
        elif row["record"] == "step":
            if late_after_skip(row):
                steps.append(untimed_step(row))
            elif row["detail"] in ENDED or skipped_under_way(row, opened, opened_index):
                steps.append(timed_step(row, opened, events, pauses, lookups))
                opened, events = None, []
            else:
                steps.append(untimed_step(row))
        elif row["record"] == "procedure":
            procedure = row
        elif row.event:
            if opened is not None:
                events.append(row)
            else:
                count(outside, row)

    T = sum(s.t for s in steps if s.counted)
    T_net = sum(s.t_net for s in steps if s.counted)
    # The task is over at its procedure's row (its last step, its step cap or its time cap); a file cut short ends where it ends.
    end = procedure.elapsed if procedure is not None else task[-1].elapsed
    m = looked_up(task_start.elapsed, end, lookups, pauses)
    m_log = looked_up(task_start.elapsed, end, log_lookups, pauses) if stopwatch is not None else None
    if procedure is None:
        outcome, finished = "no procedure row: the file ends without one, so the app did not end the task", None
    else:
        outcome = procedure["completion_reason"] or f"procedure row {procedure['detail']}, with no completion_reason"
        finished = procedure["completion_reason"] in FINISHED
    total = TASK_STEPS.get(first["task_id"])
    flagged = {int(r["step_index"]) for r in task if r.event in ("wrong_tool_picked", "safety_skip") and r["step_index"].isdigit()}
    done = {int(s.index) for s in steps if s.detail in DONE and s.index.isdigit()}
    missed = [k for k in range(1, total + 1) if k not in done] if total else None
    return Report(
        name=name, participant=first["participant_code"], condition=first["condition"], guidance=first["guidance"],
        task=first["task_id"], time_cap=first["time_cap_s"], task_start=f"{task_start['utc_time']} (elapsed_s {task_start.elapsed:.3f})",
        lookups_from=lookups_from, steps=steps, outside=tuple(outside), T=T, T_net=T_net, m=m, m_log=m_log,
        active=active(task_start.elapsed, end, pauses), outcome=outcome, finished=finished, missed=missed, lines=lines_of(steps, total, flagged), total=total, notes=notes,
        lookups=lookups_in(task, task_start))


def skipped_under_way(row, opened, index):
    """Whether a skipped row ends the stretch of its own step: the step began with a step_start and the row has a duration."""
    return (row["detail"] == "skipped" and opened is not None and row["step_index"] == index
            and number(row["active_duration_s"]) is not None)


def late_after_skip(row):
    """Whether a step row is a step skipped from the card and done later: completed out of order with no duration, as C1's never is."""
    return row["detail"] == "completed out of order" and number(row["active_duration_s"]) is None


def lines_of(steps, total, flagged):
    """
    One line per step number, 1 to the task's last step when it is known, in step order. A step's
    rows are merged: its times are those of its last row with a time (the one T counts), its wrong
    parts, wrong tools and safety skips are summed. It is correct when completed, never skipped
    or completed out of order, and no wrong_tool_picked or safety_skip event carries its index.
    """
    rows = {}
    for s in steps:
        rows.setdefault(s.index, []).append(s)
    numbers = sorted(rows, key=lambda k: (not k.isdigit(), int(k) if k.isdigit() else 0, k))
    if total:
        numbers = sorted(set(numbers) | {str(k) for k in range(1, total + 1)}, key=lambda k: (not k.isdigit(), int(k) if k.isdigit() else 0, k))
    lines = []
    for n in numbers:
        group = rows.get(n)
        if group is None:
            what = "not reached: no time"
            lines.append(Line(n, Step(n, "", "not reached", "", what, what, what, what, what, "", 0, 0, 0), "not reached", "-", 0, 0, 0, 0))
            continue
        details = [s.detail or "unnamed" for s in group]
        timed = [s for s in group if isinstance(s.t, float)]
        shown = (timed or group)[-1]
        correct = int("completed" in details and "skipped" not in details and "completed out of order" not in details
                      and not (n.isdigit() and int(n) in flagged))
        lines.append(Line(n, shown, ", ".join(details), ", ".join(dict.fromkeys(s.reason for s in group if s.reason)) or "-",
                          sum(s.wrong_parts for s in group), sum(s.wrong_tools for s in group), sum(s.safety_skips for s in group), correct))
    return lines


def count(counts, row):
    if row.event == "wrong_part_attempt":
        counts[0] += 1
    elif row.event == "wrong_tool_picked":
        counts[1] += 1
    elif row.event == "safety_skip":
        counts[2] += 1


def timed_step(row, opened, events, pauses, lookups):
    """A step that was under way, from its step_start to its row."""
    counts = [0, 0, 0]
    for event in events:
        count(counts, event)
    t = number(row["active_duration_s"])
    if t is None:
        t = "no active_duration_s on the step row"
    correct = next((e for e in events if e.event in CORRECT), None)
    if opened is None:
        d = d_net = "no step_start event"
        cue = d
        t_net = t if isinstance(t, str) else t - looked_up(row.elapsed - t, row.elapsed, lookups, pauses)
    else:
        t_net = t if isinstance(t, str) else t - looked_up(opened, row.elapsed, lookups, pauses)
        if correct is None:
            d = d_net = cue = NO_CORRECT
        else:
            d = active(opened, correct.elapsed, pauses)
            d_net = d - looked_up(opened, correct.elapsed, lookups, pauses)
            cue = f"{correct['cue_level']} {correct['cue_type']}" if correct["cue_level"] else "none on show"
    if row["completion_reason"] == "auto":
        d = d_net = cue = "auto: excluded"
    if row["detail"] not in DONE:
        e = f"step {row['detail']}"
    elif isinstance(d, str):
        e = d
    elif isinstance(t, str):
        e = t
    else:
        e = max(0.0, t - d)
    return Step(row["step_index"], row["step_id"], row["detail"], row["completion_reason"], t, t_net, d, d_net, e, cue, *counts)


def untimed_step(row):
    """A step passed over in C1, which was never under way on its own, or one skipped from the card and done later."""
    what = f"{row['detail'] or 'unnamed'}: no time"
    return Step(row["step_index"], row["step_id"], row["detail"], row["completion_reason"], what, what, what, what, what, "", 0, 0, 0)


def cell(value):
    return f"{value:.3f}" if isinstance(value, float) else str(value)


def table(header, lines):
    widths = [max(len(h), *(len(l[i]) for l in lines)) if lines else len(h) for i, h in enumerate(header)]
    out = ["  ".join(h.ljust(w) for h, w in zip(header, widths)).rstrip()]
    out += ["  ".join(c.ljust(w) for c, w in zip(l, widths)).rstrip() for l in lines]
    return out


def render(report):
    out = [
        f"File         {report.name}",
        f"Participant  {report.participant or '-'}   condition {report.condition or '-'}   guidance {report.guidance or '-'}   "
        f"task {report.task or '-'}   time cap {report.time_cap or '-'} s",
        f"task_start   {report.task_start}",
        f"Manual time  from {report.lookups_from}",
        "",
    ]
    header = ("step", "id", "row", "reason", "t", "t_net", "d", "d_net", "e", "cue at first correct", "wrong parts", "wrong tools", "safety skips", "correct", "errors")
    lines = [
        (l.number, l.step.step_id or "-", l.detail, l.reason, cell(l.step.t), cell(l.step.t_net), cell(l.step.d), cell(l.step.d_net),
         cell(l.step.e), l.step.cue or "-", str(l.wrong_parts), str(l.wrong_tools), str(l.safety_skips), str(l.correct), str(l.errors))
        for l in report.lines
    ]
    out += table(header, lines) if lines else ["No step rows after task_start: no step_start or step row was written."]
    out.append("")
    if any(report.outside):
        out.append("Outside any step: wrong parts {}, wrong tools {}, safety skips {}".format(*report.outside))
    totals = [sum(getattr(s, a) for s in report.steps) + o for a, o in zip(("wrong_parts", "wrong_tools", "safety_skips"), report.outside)]
    counted = sum(1 for s in report.steps if s.counted)
    m = f"{report.m:.3f} s" + (f"   (the log's own lookups: {report.m_log:.3f} s)" if report.m_log is not None else "")
    out += [
        f"T            {report.T:.3f} s over {counted} step(s) with completion_reason user",
        f"T_net        {report.T_net:.3f} s   (T less the manual lookups in those steps)",
        f"m            {m}",
        f"Active time  {report.active:.3f} s from task_start to the task's end, pauses taken out",
        "Totals       wrong parts {}, wrong tools {}, safety skips {}".format(*totals),
        f"Outcome      {report.outcome}" + ("" if report.finished is None else "   (completed)" if report.finished else "   (not completed)"),
    ]
    if report.missed is not None:
        out.append(f"Missed       {' '.join(map(str, report.missed)) if report.missed else 'none'}")
    else:
        out.append(f"Missed       task_id {report.task or 'missing'} names no study task, so the steps never started are not known")
    right = sum(l.correct for l in report.lines)
    out.append(f"Correct      correct steps {right}/{report.total}" if report.total
               else f"Correct      correct steps {right} (task_id {report.task or 'missing'} names no study task, so the task's steps are not known)")
    if any(s.cue == NO_CORRECT or s.d == NO_CORRECT for s in report.steps):
        out.append(f"{NO_CORRECT}: none of {', '.join(CORRECT)} came in the step")
    out.append("")
    if report.lookups:
        out.append(f"Lookups      {len(report.lookups)} in the log, start_s from task_start; pages read in the headset, as the PDF numbers them")
        out += table(("lookup", "step", "start_s", "length_s", "pages read"), [
            (str(n), l.step or "-", cell(l.start), cell(l.length), l.pages or "-") for n, l in enumerate(report.lookups, start=1)
        ])
    else:
        out.append("Lookups      none in the log")
    out += [f"Note         {n}" for n in report.notes]
    return "\n".join(out) + "\n"


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("log", type=Path, help="one study task's session log, as copied off the headset")
    parser.add_argument("--lookups", type=Path, help="the researcher's stopwatch sheet: participant_code, task, start_s, end_s")
    args = parser.parse_args(argv)
    try:
        rows = read_log(args.log)
        stopwatch = read_lookups(args.lookups, rows[0]["participant_code"], rows[0]["task_id"]) if args.lookups else None
        report = measure(rows, args.log.name, stopwatch)
    except (OSError, LogError) as e:
        print(e, file=sys.stderr)
        return 1
    sys.stdout.write(render(report))
    return 0


if __name__ == "__main__":
    sys.exit(main())
