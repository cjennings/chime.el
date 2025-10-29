# elisp-lint Installation and Setup

**Purpose:** Comprehensive linting for Emacs Lisp code, including indentation, whitespace, and package metadata validation.

## Installation

### Option 1: Via MELPA (Recommended)

1. **Add MELPA to your Emacs config** (if not already present):
   ```elisp
   (require 'package)
   (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
   (package-initialize)
   ```

2. **Install elisp-lint:**
   ```elisp
   M-x package-refresh-contents
   M-x package-install RET elisp-lint RET
   ```

### Option 2: Manual Installation

1. **Clone the repository:**
   ```bash
   cd ~/.emacs.d/
   git clone https://github.com/gonewest818/elisp-lint.git
   ```

2. **Load in your Emacs config:**
   ```elisp
   (add-to-list 'load-path "~/.emacs.d/elisp-lint")
   (require 'elisp-lint)
   ```

## Dependencies

elisp-lint requires:
- **dash.el** - Installed automatically via package manager

## Command-Line Usage

### Basic Validation

Validate all .el files in current directory:
```bash
emacs -Q --batch -l elisp-lint.el -f elisp-lint-files-batch *.el
```

Validate specific files:
```bash
emacs -Q --batch -l elisp-lint.el -f elisp-lint-files-batch chime.el tests/test-*.el
```

### Disabling Specific Checks

Disable indentation check:
```bash
emacs -Q --batch -l elisp-lint.el -f elisp-lint-files-batch --no-indent *.el
```

Disable byte-compile check:
```bash
emacs -Q --batch -l elisp-lint.el -f elisp-lint-files-batch --no-byte-compile *.el
```

Available validators to disable:
- `--no-indent` - Skip indentation validation
- `--no-fill-column` - Skip line length validation
- `--no-trailing-whitespace` - Skip whitespace validation
- `--no-byte-compile` - Skip byte compilation
- `--no-package-lint` - Skip package metadata validation
- `--no-checkdoc` - Skip documentation validation

### Multiple Disabled Checks

```bash
emacs -Q --batch -l elisp-lint.el -f elisp-lint-files-batch \
  --no-byte-compile --no-checkdoc *.el
```

## Configuration

### Project-Level Configuration

Create `.dir-locals.el` in project root:

```elisp
((emacs-lisp-mode . ((fill-column . 80)
                     (indent-tabs-mode . nil)
                     (elisp-lint-ignored-validators . ("byte-compile"))
                     (elisp-lint-indent-specs . ((describe . 1)
                                                 (it . 1))))))
```

This configuration:
- Sets line length to 80 characters
- Uses spaces instead of tabs
- Disables byte-compile validator
- Defines custom indentation for `describe` and `it` macros

### File-Level Configuration

Add to top of .el file:
```elisp
;; -*- fill-column: 80; indent-tabs-mode: nil; -*-
```

## Integrating with chime.el Development

### Add to Makefile

Add `lint` target to `tests/Makefile`:

```makefile
# Comprehensive linting with elisp-lint (if installed)
lint:
	@echo "$(YELLOW)Running elisp-lint...$(NC)"
	@if command -v elisp-lint >/dev/null 2>&1; then \
		emacs -Q --batch -l elisp-lint.el -f elisp-lint-files-batch \
			--no-checkdoc ../chime.el test-*.el testutil-*.el; \
	else \
		echo "$(RED)elisp-lint not found. Install via MELPA or see ELISP_LINT_SETUP.md$(NC)"; \
		exit 1; \
	fi
```

### Use in CI/CD

Add to GitHub Actions workflow:

```yaml
- name: Install elisp-lint
  run: |
    emacs --batch --eval "(progn
      (require 'package)
      (add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)
      (package-initialize)
      (package-refresh-contents)
      (package-install 'elisp-lint))"

- name: Lint Emacs Lisp
  run: |
    emacs -Q --batch -l elisp-lint.el -f elisp-lint-files-batch *.el tests/*.el
```

## What elisp-lint Checks

1. **Indentation** - Verifies code is indented according to emacs-lisp-mode
2. **Tabs vs Spaces** - Ensures consistent use based on `indent-tabs-mode`
3. **Trailing Whitespace** - Detects lines with trailing whitespace
4. **Fill Column** - Warns about lines exceeding configured width
5. **Byte Compilation** - Checks for compilation warnings/errors
6. **Package Metadata** - Validates package headers (if present)
7. **Checkdoc** - Validates docstring format and style

## Comparison with Built-in Validation

### Current `make validate` (Built-in)
- ✓ Fast (no dependencies)
- ✓ Checks parentheses balance
- ✓ Basic byte-compile check
- ✗ No indentation checking
- ✗ No whitespace checking
- ✗ No documentation validation

### `elisp-lint` (If Installed)
- ✓ All checks from built-in
- ✓ Comprehensive indentation validation
- ✓ Whitespace and formatting checks
- ✓ Documentation style validation
- ✗ Requires installation
- ✗ Slightly slower

## Recommendation

Use **both**:
1. **`make validate`** - Quick syntax check during development (always available)
2. **`elisp-lint`** - Comprehensive linting before major commits/releases (optional but recommended)

The pre-commit hook uses `make validate` for speed and reliability, while `elisp-lint` can be run manually for deeper analysis.

## Troubleshooting

### "Cannot open load file: elisp-lint"

elisp-lint is not installed. Install via MELPA or manually (see Installation above).

### "Package 'dash' is not available"

Update package archives:
```elisp
M-x package-refresh-contents
```

Then reinstall elisp-lint.

### False Positives on Indentation

Some macros have non-standard indentation. Configure in `.dir-locals.el`:
```elisp
(elisp-lint-indent-specs . ((my-custom-macro . 1)))
```

## Resources

- **GitHub Repository:** https://github.com/gonewest818/elisp-lint
- **MELPA Package:** https://melpa.org/#/elisp-lint
- **Related Tool (package-lint):** https://github.com/purcell/package-lint
