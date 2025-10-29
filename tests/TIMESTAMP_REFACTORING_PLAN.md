# Test Timestamp Refactoring Plan

## Problem Statement

All tests currently use hardcoded timestamps (primarily from October 2025). These tests will fail once we pass those dates. We need to refactor all time-dependent tests to use **dynamic timestamps** based on relative time relationships.

## Current State (as of 2025-10-28)

### Test Inventory
- **Total tests:** 339 across 23 test files
- **Files with hardcoded dates:** 15 out of 23 files
- **Most common hardcoded date:** 2025-10-24 (118 occurrences)

### Test File Breakdown
```
 30 tests - test-chime-sanitize-title.el
 30 tests - test-chime-notification-text.el
 25 tests - test-chime-timestamp-parse.el
 24 tests - test-chime-time-left.el
 19 tests - test-chime-all-day-events.el
 18 tests - test-chime-update-modeline.el
 18 tests - test-chime-modeline.el
 18 tests - test-chime-has-timestamp.el
 17 tests - test-chime-timestamp-within-interval-p.el
 16 tests - test-chime-gather-info.el
 13 tests - test-chime-overdue-todos.el
 11 tests - test-chime-process-notifications.el
 11 tests - test-chime-format-event-for-tooltip.el
 10 tests - test-chime-notifications.el
 10 tests - test-chime-group-events-by-day.el
 10 tests - test-chime-check-event.el
 10 tests - test-chime-apply-blacklist.el
  9 tests - test-chime-notify.el
  9 tests - test-chime-extract-time.el
  9 tests - test-chime-check-interval.el
  9 tests - test-chime-apply-whitelist.el
  7 tests - test-chime-tooltip-bugs.el
  6 tests - test-chime-whitelist-blacklist-conflicts.el
```

### Files with Hardcoded Dates (Need Refactoring)
1. test-chime-all-day-events.el
2. test-chime-check-event.el
3. test-chime-extract-time.el
4. test-chime-format-event-for-tooltip.el
5. test-chime-gather-info.el
6. test-chime-group-events-by-day.el
7. test-chime-has-timestamp.el
8. test-chime-notifications.el
9. test-chime-notification-text.el
10. test-chime-process-notifications.el
11. test-chime-sanitize-title.el
12. test-chime-timestamp-parse.el
13. test-chime-update-modeline.el
14. test-chime-modeline.el
15. test-chime-overdue-todos.el

### Files WITHOUT Hardcoded Dates (Already Good)
- test-chime-apply-blacklist.el
- test-chime-apply-whitelist.el
- test-chime-check-interval.el
- test-chime-notify.el
- test-chime-timestamp-within-interval-p.el
- test-chime-time-left.el
- test-chime-tooltip-bugs.el
- test-chime-whitelist-blacklist-conflicts.el

## Documentation Template for Each Test

Before refactoring, each test needs this documentation added to its docstring:

```elisp
(ert-deftest test-name ()
  "Brief description of what this test proves.

TIME RELATIONSHIPS:
  Current time: <relative time description>
  Event 1: <relative time description>
  Event 2: <relative time description>
  ...

DAY-OF-WEEK REQUIREMENTS:
  <Does this test care about day names? If so, why?>

SPECIAL PROPERTIES:
  - All-day event? Yes/No
  - Timed event? Yes/No
  - Repeating? Yes/No (+1w, .+1d, etc.)
  - Range? Yes/No

EXPECTED BEHAVIOR:
  <What should happen given the time relationships?>

CURRENT IMPLEMENTATION (as of 2025-10-28):
  Mock current-time: 2025-10-24 14:00
  Event timestamps: <2025-10-24 Thu 16:00>, etc.

REFACTORING NOTES:
  <Any special considerations for making this dynamic?>"
  ...)
```

## Common Time Relationship Patterns

Based on initial analysis, tests commonly use these relationships:

1. **"Now" relative:**
   - Event in 2 hours
   - Event 1 hour ago
   - Event in 5 minutes
   - Event 30 minutes ago

2. **Day relative:**
   - Event today at 2pm
   - Event tomorrow at 9am
   - Event yesterday
   - Event next week
   - Event 3 days from now

3. **Mixed scenarios:**
   - Multiple events spanning several days
   - Events both past and future
   - Events at specific times within the day

## Phase 1: Documentation (Current Week)

### Goals
1. Document time relationships for ALL 15 files with hardcoded dates
2. Identify common patterns
3. Design helper function API

### Process
For each test file:
1. Read through all tests
2. Add documentation to each test's docstring
3. Note patterns in a central location
4. Commit after each file

### Checklist
- [ ] test-chime-all-day-events.el (19 tests)
- [ ] test-chime-check-event.el (10 tests)
- [ ] test-chime-extract-time.el (9 tests)
- [ ] test-chime-format-event-for-tooltip.el (11 tests)
- [ ] test-chime-gather-info.el (16 tests)
- [ ] test-chime-group-events-by-day.el (10 tests)
- [ ] test-chime-has-timestamp.el (18 tests)
- [ ] test-chime-notifications.el (10 tests)
- [ ] test-chime-notification-text.el (30 tests)
- [ ] test-chime-process-notifications.el (11 tests)
- [ ] test-chime-sanitize-title.el (30 tests)
- [ ] test-chime-timestamp-parse.el (25 tests)
- [ ] test-chime-update-modeline.el (18 tests)
- [ ] test-chime-modeline.el (18 tests)
- [ ] test-chime-overdue-todos.el (13 tests)

**Total to document:** 228 tests

## Phase 2: Infrastructure Design

### Helper Functions Needed

Based on patterns, we'll need:

```elisp
;; Core time generation
(defun test-time-now ()
  "Return a base 'now' time that's always in the future.
Uses actual current time + 30 days to ensure validity.")

(defun test-time-at (days hours minutes)
  "Return time relative to test-time-now.
DAYS, HOURS, MINUTES can be positive (future) or negative (past).")

;; Convenience functions
(defun test-time-today-at (hour minute)
  "Time for today at HOUR:MINUTE.")

(defun test-time-yesterday-at (hour minute)
  "Time for yesterday at HOUR:MINUTE.")

(defun test-time-tomorrow-at (hour minute)
  "Time for tomorrow at HOUR:MINUTE.")

(defun test-time-days-ago (days &optional hour minute)
  "Time for DAYS ago, optionally at HOUR:MINUTE.")

(defun test-time-days-from-now (days &optional hour minute)
  "Time for DAYS from now, optionally at HOUR:MINUTE.")

;; Timestamp string generation
(defun test-timestamp-string (time &optional all-day-p)
  "Convert Emacs time to org timestamp string.
If ALL-DAY-P, omit time component: <2025-10-24 Thu>
Otherwise include time: <2025-10-24 Thu 14:00>")

(defun test-timestamp-range-string (start-time end-time)
  "Create range timestamp: <2025-10-24 Thu>--<2025-10-27 Sun>")

(defun test-timestamp-repeating (time repeater)
  "Add repeater to timestamp: <2025-10-24 Thu +1w>")

;; Mock helpers
(defmacro with-test-time (base-time &rest body)
  "Execute BODY with mocked current-time returning BASE-TIME.")

;; Day-of-week utilities (for tests that care)
(defun test-time-ensure-weekday (time weekday)
  "Adjust TIME to ensure it falls on WEEKDAY (0=Sun, 1=Mon, ...).")
```

### File Location
Create: `tests/testutil-time.el`

## Phase 3: Incremental Refactoring

### Strategy
1. Refactor ONE file at a time
2. Run full test suite after each file
3. Commit after successful refactoring
4. Document any issues/learnings

### Order of Refactoring
Start with simpler files first:

1. **test-chime-overdue-todos.el** (13 tests) - We just wrote these, fresh in mind
2. **test-chime-all-day-events.el** (19 tests) - Clear time relationships
3. **test-chime-check-event.el** (10 tests) - Smaller file
4. **test-chime-extract-time.el** (9 tests) - Timestamp focused
5. Continue with remaining files...

### Refactoring Checklist Per File
- [ ] Document all tests (Phase 1)
- [ ] Identify helper functions needed
- [ ] Refactor tests to use dynamic times
- [ ] Run tests: `make test FILE=test-chime-xxx.el`
- [ ] Run full suite: `make test`
- [ ] Commit with message: "refactor(tests): dynamic timestamps for test-chime-xxx"
- [ ] Update this document with learnings

## Phase 4: Validation

### Success Criteria
- [ ] All 339 tests pass
- [ ] No hardcoded 2025 dates remain in test files
- [ ] Tests can be run on any date and pass
- [ ] Helper functions are well-documented
- [ ] Test execution time hasn't significantly increased

### Validation Tests
1. Run full suite on current date
2. Mock system time to be 1 year in future, run suite
3. Mock system time to be 6 months in future, run suite

## Key Insights to Capture

### What Makes Time Testing Complex

1. **Org timestamp format dependencies**
   - Day names must match dates: `<2025-10-28 Tue>` (not Mon!)
   - org-parse-time-string validates this

2. **Time zone considerations**
   - encode-time uses local timezone
   - Need consistent timezone in tests?

3. **Boundary conditions**
   - Midnight rollovers
   - Month boundaries
   - Year boundaries
   - Daylight saving time transitions?

4. **Test interdependencies**
   - Do any tests depend on specific calendar dates?
   - Are there edge cases around specific dates?

### Questions to Answer During Documentation

For each test, ask:
1. **Why this specific date?** Was it arbitrary or intentional?
2. **Does day-of-week matter?** Or just the time relationship?
3. **Are there calendar edge cases?** (month end, year end, leap year)
4. **What's the minimum time window?** (minutes? hours? days?)
5. **Can this be purely relative?** Or does it need absolute dates?

## Progress Tracking

### Documentation Progress
- [ ] Phase 1 started: YYYY-MM-DD
- [ ] Phase 1 completed: YYYY-MM-DD
- Files documented: 0/15

### Refactoring Progress
- [ ] Infrastructure created: YYYY-MM-DD
- [ ] First file refactored: YYYY-MM-DD
- [ ] Phase 3 completed: YYYY-MM-DD
- Files refactored: 0/15

### Learnings Log
(Add notes as we go)

## References

- Original discussion: [Today's conversation]
- Test utilities: tests/testutil-general.el (existing)
- Org timestamp format: info:org#Timestamps
