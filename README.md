# org-roam-dailies-tasklog

Automatically record Org work sessions in org-roam daily notes. Task headings
stay clean: Emacs displays the time range and duration as overlays, with the
actual clock data stored in a `LOGBOOK` drawer.

## Setup

Requires Emacs 27.1+ and org-roam 2+. Configure org-roam first, then:

```elisp
(add-to-list 'load-path "/path/to/org-roam-dailies-tasklog")
(require 'org-roam-dailies-tasklog)
(setq org-clock-into-drawer "LOGBOOK")
(org-roam-dailies-tasklog-mode 1)
```

## Everyday use

1. On a source task, run `org-clock-in`. Each clock-in creates a new session.
2. Run `org-clock-out`. The same session gets its final clock time and duration.
3. Change the source task's state normally with `org-todo`. The latest session
   follows state changes, including DONE back to TODO or removal of the state.
4. Open the daily note with `org-roam-dailies-goto-today`.
5. Follow its **Original task** link, or run
   `org-roam-dailies-tasklog-visit-source`, to return to the source task.

Daily files are named `YYYY-MM-DD.org` inside `org-roam-dailies-directory`.
Missing notes get a simple title; org-roam capture templates are not used.
The global mode watches tasks in all Org buffers, not only org-roam files.

## Identity, history, and saving

The package adds an Org `ID` to a source task if needed, plus `TASKLOG_SESSION`
and `TASKLOG_FILE` properties. Daily entries contain `TASKLOG_SOURCE_ID`,
`TASKLOG_SESSION`, and bookkeeping properties for their state and clock line.
Matching never falls back to title text: identical titles and task renames are
safe. A session keeps the title it had when created.

A session stays in the daily file where it started, even if clock-out happens
after midnight. Its stored destination also works after reopening Emacs, provided
you saved the source task and restored the running clock using Org's own clock
persistence settings. **Save the source file normally** to persist its IDs and
session properties. The package automatically saves the daily file, including
any pending edits already in that buffer.

The latest session follows source state changes. Earlier sessions keep their
historical states. Changing a daily copy is a local journal edit; it does not
change the source and does not generate another log entry. The next source
update may overwrite the latest copy's state. Use the source link to manage
the actual task. Custom TODO keywords should be configured in both files if
you want Org to recognize and style them consistently.

Completing a task without a previous session creates a clock-free entry.
`org-roam-dailies-tasklog-completion-states` controls which states trigger this
(default: `("DONE")`). Other untracked state changes do not create entries.

Updates change only the state prefix, managed clock line, and bookkeeping
properties. Notes before or after the clock drawer, other properties, and child
headings stay in place. Deleting a session entry causes the next update to
recreate it. Moving or deleting the destination file is not tracked automatically.

Old entries without session IDs are left untouched rather than guessing which
task they belong to. Old literal timing headings do not get duplicate overlays;
new sessions use the new format.

## Display options

```elisp
(setq org-roam-dailies-tasklog-time-format "%H:%M" ; default "%I:%M %p"
      org-roam-dailies-tasklog-compact-duration t ; 1h 30m rather than 1:30
      org-roam-dailies-tasklog-time-column 50)    ; nil: five spaces after title
```

Run `org-roam-dailies-tasklog-refresh` after changing display options.
Times have the `shadow` face. A running clock shows `→ running`; this is not a
live elapsed-time counter. Long titles always get at least five spaces before
timing. Overlays refresh around edited entries instead of reparsing the entire
note, and disappear when the global mode is disabled. They are not exported or
copied as text; the LOGBOOK remains available to other Org tools.

## Tests

With dependencies installed in your Emacs package directory:

```sh
emacs --batch -Q \
  --eval '(progn (require (quote package)) (package-initialize))' \
  -L . -l test/org-roam-dailies-tasklog-test.el \
  -f ert-run-tests-batch-and-exit
```

Tests use temporary files and exercise real Org clock/state hooks, duplicate
names, renaming, midnight routing, persisted session metadata, note preservation,
history, source links, and overlay behavior.
