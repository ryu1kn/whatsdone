---
name: work-on-backlog
description: Execute one backlog iteration in this repository using docs/TASKS.yaml as the source of truth. Use when asked to "work on backlog", "pick the next task", "continue autonomous work", or to implement a pending task with dependency-aware selection, strict backlog-edit rules, validation gates, and task status/audit updates.
---

# Work On Backlog

## Overview

Execute exactly one safe backlog iteration at a time.
Keep the repository mergeable and green: run `mise run gate` and leave git status clean by committing valid changes or reverting abandoned work.
Do exactly one of two modes per session: implement one actionable task, or perform strict backlog maintenance only.

## Workflow

1. Load `docs/TASKS.yaml`.
2. Select the highest-priority `todo` task whose `depends_on` tasks are all `done`.
3. Decide mode:
`Option A`: implement the selected task if acceptance is executable within one session.
`Option B`: perform backlog maintenance only (split/clarify) if task size is `large`, likely exceeds `max_sessions`, or acceptance is not executable.
Do not implement product code in `Option B`.
4. Apply only authorized backlog edits.
5. Run repository gate(s) until green.
6. Record audit/progress updates required by the mode.

## Option A: Implement One Task

1. Change selected task `status` from `todo` to `doing`.
2. Implement only what is needed for that task's acceptance criteria.
3. Add or update tests so acceptance is executable.
4. Run `mise run gate` and fix failures until green.
5. Commit with message format: `<task-id>: <title>`.
6. Mark task `done`.
7. Append one JSON line to `docs/backlog_audit.ndjson` describing the change.
8. Add a short entry to `claude-progress.txt`.

## Option B: Strict Backlog Maintenance

Use only when implementation is unsafe or non-executable in one session.
Allowed edits:
- Status transitions: `todo -> doing -> done`, `doing -> blocked`, `todo/doing -> superseded`.
- Split one task into 2-8 smaller tasks (`micro/small/medium`) using `depends_on` and `supersedes`.
- Add up to 3 clearly necessary prerequisite tasks.
- Use next sequential IDs for new tasks (no letter suffixes).

Forbidden edits:
- Deleting tasks.
- Changing `schema_version`, `size_profiles`, or `backlog_policy`.
- Renaming or re-IDing existing tasks except via `supersedes`.
- Rewriting most acceptance criteria without explicit justification.

If any task fields beyond status are changed, add `audit.change_reason` and append one JSON line to `docs/backlog_audit.ndjson` with: `timestamp`, `session_id` (if known), `tasks_changed`, and `reason`.

## Blocking Rule

If gate cannot be made green for the task after 2 focused attempts:
- mark task `blocked` with a precise reason
- split into smaller tasks or add missing prerequisites within edit limits
- stop implementation for this iteration

## Completion Output

End with exactly one line:
- `RESULT: OK`
- `RESULT: BLOCKED`
- `RESULT: BACKLOG_UPDATED`

Do not output long prose for per-iteration result reporting.
Emit `<promise>COMPLETE</promise>` only if all tasks are done, `mise run gate` passes, and git status is clean.

## References

- `docs/TASKS.yaml`: source of truth for tasks, dependencies, and statuses.
