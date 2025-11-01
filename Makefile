# Makefile for chime.el
#
# Usage:
#   make test              - Run all tests (unit + integration)
#   make test-unit         - Run unit tests only
#   make test-integration  - Run integration tests only
#   make test-file FILE=test-chime-notify.el  - Run specific test file
#   make test-name TEST=test-chime-check-*    - Run tests matching pattern
#   make clean             - Remove generated files

# Emacs binary to use (override with: make EMACS=emacs29 test)
EMACS ?= emacs

# Test directories and files
TEST_DIR = tests
UNIT_TESTS = $(filter-out $(TEST_DIR)/test-integration-%.el, $(wildcard $(TEST_DIR)/test-*.el))
INTEGRATION_TESTS = $(wildcard $(TEST_DIR)/test-integration-*.el)
ALL_TESTS = $(UNIT_TESTS) $(INTEGRATION_TESTS)

# Emacs batch flags
EMACS_BATCH = $(EMACS) --batch --no-site-file --no-site-lisp

# Colors for output (if terminal supports it)
COLOR_GREEN = \033[0;32m
COLOR_RED = \033[0;31m
COLOR_BLUE = \033[0;34m
COLOR_RESET = \033[0m

.PHONY: help test test-all test-unit test-integration test-file test-name clean

# Default target
help:
	@echo "Chime.el Test Targets:"
	@echo ""
	@echo "  make test              - Run all tests (unit + integration)"
	@echo "  make test-unit         - Run unit tests only ($(words $(UNIT_TESTS)) files)"
	@echo "  make test-integration  - Run integration tests only ($(words $(INTEGRATION_TESTS)) files)"
	@echo "  make test-file FILE=<filename>  - Run specific test file"
	@echo "  make test-name TEST=<pattern>   - Run tests matching pattern"
	@echo "  make clean             - Remove generated files"
	@echo ""
	@echo "Examples:"
	@echo "  make test-file FILE=test-chime-notify.el"
	@echo "  make test-name TEST=test-chime-check-early-return"
	@echo "  make EMACS=emacs29 test   # Use specific Emacs version"

# Run all tests
test: test-all

test-all:
	@echo "$(COLOR_BLUE)Running all tests ($(words $(ALL_TESTS)) files)...$(COLOR_RESET)"
	@$(MAKE) test-unit
	@$(MAKE) test-integration
	@echo "$(COLOR_GREEN)✓ All tests complete$(COLOR_RESET)"

# Run unit tests only
test-unit:
	@echo "$(COLOR_BLUE)Running unit tests ($(words $(UNIT_TESTS)) files)...$(COLOR_RESET)"
	@failed=0; \
	for test in $(UNIT_TESTS); do \
		echo "  Testing $$test..."; \
		(cd $(TEST_DIR) && $(EMACS_BATCH) -l ert -l $$(basename $$test) -f ert-run-tests-batch-and-exit) || failed=$$((failed + 1)); \
	done; \
	if [ $$failed -eq 0 ]; then \
		echo "$(COLOR_GREEN)✓ All unit tests passed$(COLOR_RESET)"; \
	else \
		echo "$(COLOR_RED)✗ $$failed unit test file(s) failed$(COLOR_RESET)"; \
		exit 1; \
	fi

# Run integration tests only
test-integration:
	@echo "$(COLOR_BLUE)Running integration tests ($(words $(INTEGRATION_TESTS)) files)...$(COLOR_RESET)"
	@failed=0; \
	for test in $(INTEGRATION_TESTS); do \
		echo "  Testing $$test..."; \
		(cd $(TEST_DIR) && $(EMACS_BATCH) -l ert -l $$(basename $$test) -f ert-run-tests-batch-and-exit) || failed=$$((failed + 1)); \
	done; \
	if [ $$failed -eq 0 ]; then \
		echo "$(COLOR_GREEN)✓ All integration tests passed$(COLOR_RESET)"; \
	else \
		echo "$(COLOR_RED)✗ $$failed integration test file(s) failed$(COLOR_RESET)"; \
		exit 1; \
	fi

# Run specific test file
# Usage: make test-file FILE=test-chime-notify.el
test-file:
ifndef FILE
	@echo "$(COLOR_RED)Error: FILE parameter required$(COLOR_RESET)"
	@echo "Usage: make test-file FILE=test-chime-notify.el"
	@exit 1
endif
	@echo "$(COLOR_BLUE)Running tests in $(FILE)...$(COLOR_RESET)"
	@cd $(TEST_DIR) && $(EMACS_BATCH) -l ert -l $(FILE) -f ert-run-tests-batch-and-exit
	@echo "$(COLOR_GREEN)✓ Tests in $(FILE) complete$(COLOR_RESET)"

# Run specific test by name/pattern
# Usage: make test-name TEST=test-chime-check-early-return
#        make test-name TEST="test-chime-check-*"
test-name:
ifndef TEST
	@echo "$(COLOR_RED)Error: TEST parameter required$(COLOR_RESET)"
	@echo "Usage: make test-name TEST=test-chime-check-early-return"
	@echo "       make test-name TEST='test-chime-check-*'"
	@exit 1
endif
	@echo "$(COLOR_BLUE)Running tests matching pattern: $(TEST)...$(COLOR_RESET)"
	@cd $(TEST_DIR) && $(EMACS_BATCH) \
		-l ert \
		$(foreach test,$(ALL_TESTS),-l $(notdir $(test))) \
		--eval '(ert-run-tests-batch-and-exit "$(TEST)")'
	@echo "$(COLOR_GREEN)✓ Tests matching '$(TEST)' complete$(COLOR_RESET)"

# Clean generated files
clean:
	@echo "Cleaning generated files..."
	@find . -name "*.elc" -delete
	@find $(TEST_DIR) -name "chime-test-*" -delete
	@echo "$(COLOR_GREEN)✓ Clean complete$(COLOR_RESET)"
