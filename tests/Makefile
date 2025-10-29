# Makefile for chime.el test suite
# Usage:
#   make test              - Run all tests
#   make test-file FILE=overdue - Run tests in one file
#   make test-one TEST=name     - Run one specific test
#   make test-unit         - Run unit tests only
#   make test-integration  - Run integration tests only
#   make clean             - Remove byte-compiled files

# Configuration
EMACS ?= emacs
EMACSFLAGS = --batch -Q
TESTFLAGS = -l ert

# Dependency paths (adjust if needed)
ELPA_DIR = $(HOME)/.emacs.d/elpa
DASH_DIR = $(shell find $(ELPA_DIR) -maxdepth 1 -name 'dash-*' -type d 2>/dev/null | head -1)
ALERT_DIR = $(shell find $(ELPA_DIR) -maxdepth 1 -name 'alert-*' -type d 2>/dev/null | head -1)
ASYNC_DIR = $(shell find $(ELPA_DIR) -maxdepth 1 -name 'async-*' -type d 2>/dev/null | head -1)

# Build load path
LOADPATH = -L $(DASH_DIR) -L $(ALERT_DIR) -L $(ASYNC_DIR)

# Test files
ALL_TESTS = $(wildcard test-*.el)
UNIT_TESTS = $(filter-out test-chime-gcal% test-chime-notifications.el test-chime-process-notifications.el,$(ALL_TESTS))
INTEGRATION_TESTS = test-chime-notifications.el test-chime-process-notifications.el

# Colors for output (if terminal supports it)
RED = \033[0;31m
GREEN = \033[0;32m
YELLOW = \033[1;33m
NC = \033[0m # No Color

.PHONY: all test test-file test-one test-unit test-integration validate lint clean help check-deps

# Default target
all: test

# Check if dependencies are available
check-deps:
	@if [ -z "$(DASH_DIR)" ]; then \
		echo "$(RED)Error: dash package not found in $(ELPA_DIR)$(NC)"; \
		exit 1; \
	fi
	@if [ -z "$(ALERT_DIR)" ]; then \
		echo "$(RED)Error: alert package not found in $(ELPA_DIR)$(NC)"; \
		exit 1; \
	fi
	@if [ -z "$(ASYNC_DIR)" ]; then \
		echo "$(RED)Error: async package not found in $(ELPA_DIR)$(NC)"; \
		exit 1; \
	fi
	@echo "$(GREEN)✓ All dependencies found$(NC)"

# Run all tests
test: check-deps
	@echo "$(YELLOW)Running all tests ($(words $(ALL_TESTS)) files, ~339 tests)...$(NC)"
	@$(EMACS) $(EMACSFLAGS) $(LOADPATH) $(TESTFLAGS) \
		--eval "(dolist (f (directory-files \".\" t \"^test-.*\\\\.el$$\")) (load f))" \
		--eval '(ert-run-tests-batch-and-exit)' 2>&1 | tee test-output.log
	@if [ $$? -eq 0 ]; then \
		echo "$(GREEN)✓ All tests passed!$(NC)"; \
	else \
		echo "$(RED)✗ Some tests failed. See test-output.log for details.$(NC)"; \
		exit 1; \
	fi

# Run tests in one file
test-file: check-deps
ifndef FILE
	@echo "$(RED)Error: FILE not specified$(NC)"
	@echo "Usage: make test-file FILE=overdue"
	@echo "       make test-file FILE=test-chime-overdue-todos.el"
	@exit 1
endif
	@TESTFILE=$$(find . -maxdepth 1 -name "*$(FILE)*.el" -type f | head -1); \
	if [ -z "$$TESTFILE" ]; then \
		echo "$(RED)Error: No test file matching '$(FILE)' found$(NC)"; \
		exit 1; \
	fi; \
	echo "$(YELLOW)Running tests in $$TESTFILE...$(NC)"; \
	$(EMACS) $(EMACSFLAGS) $(LOADPATH) $(TESTFLAGS) -l "$$TESTFILE" \
		--eval '(ert-run-tests-batch-and-exit)' 2>&1 | tee test-file-output.log; \
	if [ $$? -eq 0 ]; then \
		echo "$(GREEN)✓ All tests in $$TESTFILE passed!$(NC)"; \
	else \
		echo "$(RED)✗ Some tests failed.$(NC)"; \
		exit 1; \
	fi

# Run one specific test
test-one: check-deps
ifndef TEST
	@echo "$(RED)Error: TEST not specified$(NC)"
	@echo "Usage: make test-one TEST=pilot"
	@echo "       make test-one TEST=test-overdue-has-passed-time-today-all-day"
	@exit 1
endif
	@echo "$(YELLOW)Searching for test matching '$(TEST)'...$(NC)"
	@TESTFILE=$$(grep -l "ert-deftest.*$(TEST)" test-*.el 2>/dev/null | head -1); \
	if [ -z "$$TESTFILE" ]; then \
		echo "$(RED)Error: No test matching '$(TEST)' found$(NC)"; \
		exit 1; \
	fi; \
	TESTNAME=$$(grep "ert-deftest.*$(TEST)" "$$TESTFILE" | sed 's/^(ert-deftest \([^ ]*\).*/\1/' | head -1); \
	echo "$(YELLOW)Running test '$$TESTNAME' in $$TESTFILE...$(NC)"; \
	$(EMACS) $(EMACSFLAGS) $(LOADPATH) $(TESTFLAGS) -l "$$TESTFILE" \
		--eval "(ert-run-tests-batch-and-exit \"$$TESTNAME\")" 2>&1; \
	if [ $$? -eq 0 ]; then \
		echo "$(GREEN)✓ Test $$TESTNAME passed!$(NC)"; \
	else \
		echo "$(RED)✗ Test $$TESTNAME failed.$(NC)"; \
		exit 1; \
	fi

# Run only unit tests
test-unit: check-deps
	@echo "$(YELLOW)Running unit tests ($(words $(UNIT_TESTS)) files)...$(NC)"
	@for testfile in $(UNIT_TESTS); do \
		echo "  Testing $$testfile..."; \
	done
	@$(EMACS) $(EMACSFLAGS) $(LOADPATH) $(TESTFLAGS) \
		$(foreach file,$(UNIT_TESTS),-l $(file)) \
		--eval '(ert-run-tests-batch-and-exit)' 2>&1 | tee test-unit-output.log
	@if [ $$? -eq 0 ]; then \
		echo "$(GREEN)✓ All unit tests passed!$(NC)"; \
	else \
		echo "$(RED)✗ Some unit tests failed.$(NC)"; \
		exit 1; \
	fi

# Run only integration tests
test-integration: check-deps
	@echo "$(YELLOW)Running integration tests ($(words $(INTEGRATION_TESTS)) files)...$(NC)"
	@$(EMACS) $(EMACSFLAGS) $(LOADPATH) $(TESTFLAGS) \
		$(foreach file,$(INTEGRATION_TESTS),-l $(file)) \
		--eval '(ert-run-tests-batch-and-exit)' 2>&1 | tee test-integration-output.log
	@if [ $$? -eq 0 ]; then \
		echo "$(GREEN)✓ All integration tests passed!$(NC)"; \
	else \
		echo "$(RED)✗ Some integration tests failed.$(NC)"; \
		exit 1; \
	fi

# Count tests
count:
	@echo "Test file inventory:"
	@for f in $(ALL_TESTS); do \
		count=$$(grep -c "^(ert-deftest" "$$f" 2>/dev/null || echo 0); \
		printf "%3d tests - %s\n" "$$count" "$$f"; \
	done | sort -rn
	@total=$$(find . -name "test-*.el" -exec grep -c "^(ert-deftest" {} \; | awk '{sum+=$$1} END {print sum}'); \
	echo "$(GREEN)Total: $$total tests across $(words $(ALL_TESTS)) files$(NC)"

# List all available tests
list:
	@echo "Available tests:"
	@grep -h "^(ert-deftest" test-*.el | sed 's/^(ert-deftest \([^ ]*\).*/  \1/' | sort

# Validate Emacs Lisp syntax
validate:
	@echo "$(YELLOW)Validating Emacs Lisp syntax...$(NC)"
	@failed=0; \
	total=0; \
	for file in ../chime.el test-*.el testutil-*.el; do \
		if [ -f "$$file" ] && [ ! -d "$$file" ]; then \
			total=$$((total + 1)); \
			output=$$($(EMACS) --batch $(LOADPATH) --eval "(progn \
				(setq byte-compile-error-on-warn nil) \
				(find-file \"$$file\") \
				(condition-case err \
					(progn \
						(check-parens) \
						(message \"✓ $$file - parentheses balanced\")) \
					(error \
						(message \"✗ $$file: %s\" (error-message-string err)) \
						(kill-emacs 1))))" 2>&1 | grep -E '(✓|✗)'); \
			if [ $$? -eq 0 ]; then \
				echo "$(GREEN)$$output$(NC)"; \
			else \
				echo "$(RED)$$output$(NC)"; \
				failed=$$((failed + 1)); \
			fi; \
		fi; \
	done; \
	if [ $$failed -eq 0 ]; then \
		echo "$(GREEN)✓ All $$total files validated successfully$(NC)"; \
	else \
		echo "$(RED)✗ $$failed of $$total files failed validation$(NC)"; \
		exit 1; \
	fi

# Comprehensive linting with elisp-lint
lint:
	@echo "$(YELLOW)Running elisp-lint...$(NC)"
	@$(EMACS) --batch --eval "(progn \
		(require 'package) \
		(package-initialize) \
		(require 'elisp-lint))" \
		-f elisp-lint-files-batch \
		--no-checkdoc \
		../chime.el test-*.el testutil-*.el 2>&1; \
	if [ $$? -eq 0 ]; then \
		echo "$(GREEN)✓ Linting completed successfully$(NC)"; \
	else \
		echo "$(RED)✗ Linting found issues (see above)$(NC)"; \
		exit 1; \
	fi

# Clean byte-compiled files
clean:
	@echo "$(YELLOW)Cleaning byte-compiled files...$(NC)"
	@rm -f *.elc ../*.elc
	@rm -f test-output.log test-file-output.log test-unit-output.log test-integration-output.log
	@echo "$(GREEN)✓ Cleaned$(NC)"

# Show help
help:
	@echo "Chime Test Suite Makefile"
	@echo ""
	@echo "Usage:"
	@echo "  make test                      - Run all tests (339 tests)"
	@echo "  make test-file FILE=overdue    - Run tests in one file (fuzzy match)"
	@echo "  make test-one TEST=pilot       - Run one specific test (fuzzy match)"
	@echo "  make test-unit                 - Run unit tests only"
	@echo "  make test-integration          - Run integration tests only"
	@echo "  make validate                  - Validate Emacs Lisp syntax (parens balance)"
	@echo "  make lint                      - Comprehensive linting with elisp-lint"
	@echo "  make count                     - Count tests per file"
	@echo "  make list                      - List all test names"
	@echo "  make clean                     - Remove byte-compiled files and logs"
	@echo "  make check-deps                - Verify all dependencies are installed"
	@echo "  make help                      - Show this help message"
	@echo ""
	@echo "Examples:"
	@echo "  make test                                    # Run everything"
	@echo "  make test-file FILE=overdue                  # Run test-chime-overdue-todos.el"
	@echo "  make test-one TEST=pilot                     # Run the pilot test"
	@echo "  make test-one TEST=test-overdue-has-passed   # Run specific test"
	@echo ""
	@echo "Environment variables:"
	@echo "  EMACS      - Emacs executable (default: emacs)"
	@echo "  ELPA_DIR   - ELPA package directory (default: ~/.emacs.d/elpa)"
