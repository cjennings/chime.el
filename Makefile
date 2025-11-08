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
	@echo "[i] Running all tests ($(words $(ALL_TESTS)) files)..."
	@$(MAKE) test-unit
	@$(MAKE) test-integration
	@echo "[✓] All tests complete"

# Run unit tests only
test-unit:
	@echo "[i] Running unit tests ($(words $(UNIT_TESTS)) files)..."
	@failed=0; \
	for test in $(UNIT_TESTS); do \
		echo "  Testing $$test..."; \
		(cd $(TEST_DIR) && $(EMACS_BATCH) -l ert -l $$(basename $$test) -f ert-run-tests-batch-and-exit) || failed=$$((failed + 1)); \
	done; \
	if [ $$failed -eq 0 ]; then \
		echo "[✓] All unit tests passed"; \
	else \
		echo "[✗] $$failed unit test file(s) failed"; \
		exit 1; \
	fi

# Run integration tests only
test-integration:
	@echo "[i] Running integration tests ($(words $(INTEGRATION_TESTS)) files)..."
	@failed=0; \
	for test in $(INTEGRATION_TESTS); do \
		echo "  Testing $$test..."; \
		(cd $(TEST_DIR) && $(EMACS_BATCH) -l ert -l $$(basename $$test) -f ert-run-tests-batch-and-exit) || failed=$$((failed + 1)); \
	done; \
	if [ $$failed -eq 0 ]; then \
		echo "[✓] All integration tests passed"; \
	else \
		echo "[✗] $$failed integration test file(s) failed"; \
		exit 1; \
	fi

# Run specific test file
# Usage: make test-file FILE=test-chime-notify.el
test-file:
ifndef FILE
	@echo "[✗] Error: FILE parameter required"
	@echo "Usage: make test-file FILE=test-chime-notify.el"
	@exit 1
endif
	@echo "[i] Running tests in $(FILE)..."
	@cd $(TEST_DIR) && $(EMACS_BATCH) -l ert -l $(FILE) -f ert-run-tests-batch-and-exit
	@echo "[✓] Tests in $(FILE) complete"

# Run specific test by name/pattern
# Usage: make test-name TEST=test-chime-check-early-return
#        make test-name TEST="test-chime-check-*"
test-name:
ifndef TEST
	@echo "[✗] Error: TEST parameter required"
	@echo "Usage: make test-name TEST=test-chime-check-early-return"
	@echo "       make test-name TEST='test-chime-check-*'"
	@exit 1
endif
	@echo "[i] Running tests matching pattern: $(TEST)..."
	@cd $(TEST_DIR) && $(EMACS_BATCH) \
		-l ert \
		$(foreach test,$(ALL_TESTS),-l $(notdir $(test))) \
		--eval '(ert-run-tests-batch-and-exit "$(TEST)")'
	@echo "[✓] Tests matching '$(TEST)' complete"

# Clean generated files
clean:
	@echo "[i] Cleaning generated files..."
	@find . -name "*.elc" -delete
	@find $(TEST_DIR) -name "chime-test-*" -delete
	@echo "[✓] Clean complete"
