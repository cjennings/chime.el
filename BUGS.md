# Known Issues

## Recurring Events Show Duplicate Entries in Tooltip

**Status**: Confirmed
**Priority**: Medium
**Affects**: Tooltip display
**First Reported**: 2025-10-29

### Description

Events with daily repeaters (e.g., `+1d`) appear multiple times in the tooltip, once for each day within the lookahead window.

### Example

```org
** Daily Wrap Up
<2025-06-17 Tue 22:00 +1d>
```

With a tooltip lookahead of 8760 hours (1 year), this single event appears ~365 times in the tooltip:

```
Daily Wrap Up at 10:00 PM (in 6h 5m)
Daily Wrap Up at 10:00 PM (in 6h 5m)
Daily Wrap Up at 10:00 PM (in 6h 5m)
... [18 times visible in tooltip, 376 more events total]
```

### Root Cause

1. `org-agenda-list` expands recurring events into multiple instances within the agenda span
2. Each expanded instance gets a separate `org-marker` pointing to the same headline
3. `chime--update-modeline` treats each marker as a unique event
4. Current deduplication logic (lines 878-907) only takes the soonest timestamp **per event object**, but doesn't deduplicate across multiple event objects with the same title

### Technical Details

**Current deduplication approach** (chime.el:878-907):
```elisp
;; For events with multiple timestamps, only include the soonest one
;; to avoid duplicate entries (e.g., when events are rescheduled)
(dolist (event events)
  (let* ((all-times (cdr (assoc 'times event)))
         ;; Find soonest timestamp for THIS event
         (soonest-time-info nil))
    ;; ... only adds event once with soonest timestamp
    (push (list event soonest-time-info soonest-minutes-for-event) upcoming)))
```

This works for a single event with multiple timestamps (e.g., org-gcal reschedules), but NOT for multiple event objects with the same title from recurring expansions.

### Proposed Solution

Add title-based deduplication after the per-event deduplication:

```elisp
;; After collecting all upcoming events, deduplicate by title
;; Keep only the soonest occurrence of each unique title
(setq upcoming
      (let ((title-hash (make-hash-table :test 'equal)))
        (dolist (item upcoming)
          (let* ((event (car item))
                 (title (cdr (assoc 'title event)))
                 (minutes (caddr item))
                 (existing (gethash title title-hash)))
            ;; Only keep if this is the first occurrence or soonest so far
            (when (or (not existing)
                      (< minutes (caddr existing)))
              (puthash title item title-hash))))
        (hash-table-values title-hash)))
```

### Workaround

Users can reduce `chime-tooltip-lookahead-hours` to limit the number of recurring instances shown:

```elisp
;; Default is 8760 hours (1 year)
;; Reduce to 168 hours (1 week) to show only 7 daily repeats
(setq chime-tooltip-lookahead-hours 168)
```

### Test Plan

Add tests to `test-chime-tooltip-bugs.el`:
- Recurring daily event should appear once in tooltip
- Recurring weekly event should appear once per week
- Multiple different events with same title should both appear
- Edge case: Same title at different times on same day

### References

- User report: 2025-10-29 session
- Related code: `chime--update-modeline` (chime.el:858-940)
- Existing tests: test-chime-modeline.el (has some no-duplicate tests but may not cover this case)
