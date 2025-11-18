;;; testutil-general.el ---  -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; This library provides general helper functions and constants for managing
;; test directories and files across test suites.
;;
;; It establishes a user-local hidden directory as the root for all test assets,
;; provides utilities to create this directory safely, create temporary files
;; and subdirectories within it, and clean up after tests.
;;
;; This library should be required by test suites to ensure consistent,
;; reliable, and isolated file-system resources.
;;
;;; Code:

(defconst chime-test-base-dir
  (expand-file-name "~/.temp-chime-tests/")
  "Base directory for all CHIME test files and directories.
All test file-system artifacts should be created under this hidden
directory in the user's home. This avoids relying on ephemeral system
directories like /tmp and reduces flaky test failures caused by external
cleanup.")

(defun chime-create-test-base-dir ()
  "Create the test base directory `chime-test-base-dir' if it does not exist.
Returns the absolute path to the test base directory.
Signals an error if creation fails."
  (let ((dir (file-name-as-directory chime-test-base-dir)))
	(unless (file-directory-p dir)
	  (make-directory dir t))
	(if (file-directory-p dir) dir
	  (error "Failed to create test base directory %s" dir))))

(defun chime-create--directory-ensuring-parents (dirpath)
  "Create nested directories specified by DIRPATH.
Error if DIRPATH exists already.
Ensure DIRPATH is within `chime-test-base-dir`."
  (let* ((base (file-name-as-directory chime-test-base-dir))
		 (fullpath (expand-file-name dirpath base)))
	(unless (string-prefix-p base fullpath)
	  (error "Directory path %s is outside base test directory %s" fullpath base))
	(when (file-exists-p fullpath)
	  (error "Directory path already exists: %s" fullpath))
	(make-directory fullpath t)
	fullpath))

(defun chime-create--file-ensuring-parents (filepath content &optional executable)
  "Create file at FILEPATH (relative to `chime-test-base-dir`) with CONTENT.
Error if file exists already.
Create parent directories as needed.
If EXECUTABLE is non-nil, set execute permissions on file.
Ensure FILEPATH is within `chime-test-base-dir`."
  (let* ((base (file-name-as-directory chime-test-base-dir))
		 (fullpath (expand-file-name filepath base))
		 (parent-dir (file-name-directory fullpath)))
	(unless (string-prefix-p base fullpath)
	  (error "File path %s is outside base test directory %s" fullpath base))
	(when (file-exists-p fullpath)
	  (error "File already exists: %s" fullpath))
	(unless (file-directory-p parent-dir)
	  (make-directory parent-dir t))
	(with-temp-buffer
	  (when content
		(insert content))
	  (write-file fullpath))
	(when executable
	  (chmod fullpath #o755))
	fullpath))

(defun chime-create-directory-or-file-ensuring-parents (path &optional content executable)
  "Create a directory or file specified by PATH relative to `chime-test-base-dir`.
If PATH ends with a slash, create nested directories.
Else create a file with optional CONTENT.
If EXECUTABLE is non-nil and creating a file, set executable permissions.
Error if the target path already exists.
Return the full created path."
  (let ((is-dir (string-suffix-p "/" path)))
	(if is-dir
		(chime-create--directory-ensuring-parents path)
	  (chime-create--file-ensuring-parents path content executable))))


;; (defun chime-create-file-with-content-ensuring-parents (filepath content &optional executable)
;;   "Create a file at FILEPATH with CONTENT, ensuring parent directories exist.
;; FILEPATH will be relative to `chime-test-base-dir'.
;; Signals an error if the file already exists.
;; If EXECUTABLE is non-nil, set executable permission on the file.
;; Errors if the resulting path is outside `chime-test-base-dir`."
;;   (let* ((base (file-name-as-directory chime-test-base-dir))
;; 		 (fullpath (if (file-name-absolute-p filepath)
;; 					   (expand-file-name filepath)
;; 					 (expand-file-name filepath base))))
;; 	(unless (string-prefix-p base fullpath)
;; 	  (error "File path %s is outside base test directory %s" fullpath base))
;; 	(let ((parent-dir (file-name-directory fullpath)))
;; 	  (when (file-exists-p fullpath)
;; 		(error "File already exists: %s" fullpath))
;; 	  (unless (file-directory-p parent-dir)
;; 		(make-directory parent-dir t))
;; 	  (with-temp-buffer
;; 		(insert content)
;; 		(write-file fullpath))
;; 	  (when executable
;; 		(chmod fullpath #o755))
;; 	  fullpath)))

(defun chime-fix-permissions-recursively (dir)
  "Recursively set read/write permissions for user under DIR.
Directories get user read, write, and execute permissions to allow recursive
operations."
  (when (file-directory-p dir)
	(dolist (entry (directory-files-recursively dir ".*" t))
	  (when (file-exists-p entry)
		(let* ((attrs (file-attributes entry))
			   (is-dir (car attrs))
			   (mode (file-modes entry))
			   (user-r (logand #o400 mode))
			   (user-w (logand #o200 mode))
			   (user-x (logand #o100 mode))
			   new-mode)
		  (setq new-mode mode)
		  (unless user-r
			(setq new-mode (logior new-mode #o400)))
		  (unless user-w
			(setq new-mode (logior new-mode #o200)))
		  (when is-dir
			;; Ensure user-execute for directories
			(unless user-x
			  (setq new-mode (logior new-mode #o100))))
		  (unless (= mode new-mode)
			(set-file-modes entry new-mode)))))))

(defun chime-delete-test-base-dir ()
  "Recursively delete test base directory `chime-test-base-dir' and contents.
Ensures all contained files and directories have user read/write permissions
so deletion is not blocked by permissions.
After deletion, verifies that the directory no longer exists.
Signals an error if the directory still exists after deletion attempt."
  (let ((dir (file-name-as-directory chime-test-base-dir)))
	(when (file-directory-p dir)
	  (chime-fix-permissions-recursively dir)
	  (delete-directory dir t))
	(when (file-directory-p dir)
	  (error "Test base directory %s still exists after deletion" dir))))

(defun chime-create-temp-test-file (&optional prefix)
  "Create a uniquely named temporary file under `chime-test-base-dir'.
Optional argument PREFIX is a string to prefix the filename, defaults
to \"tempfile-\". Returns the absolute path to the newly created empty file.
Errors if base test directory cannot be created or file creation fails."
  (let ((base (chime-create-test-base-dir))
		(file nil))
	(setq file (make-temp-file (expand-file-name (or prefix "tempfile-") base)))
	(unless (file-exists-p file)
	  (error "Failed to create temporary test file under %s" base))
	file))

(defun chime-create-test-subdirectory (subdir)
  "Ensure subdirectory SUBDIR (relative to `chime-test-base-dir') exists.
Creates parent directories as needed.
Returns the absolute path to the subdirectory.
Signals an error if creation fails.
SUBDIR must be a relative path string."
  (let* ((base (chime-create-test-base-dir))
		 (fullpath (expand-file-name subdir base)))
	(unless (file-directory-p fullpath)
	  (make-directory fullpath t))
	(if (file-directory-p fullpath) fullpath
	  (error "Failed to create test subdirectory %s" subdir))))

(defun chime-create-temp-test-file-with-content (content &optional prefix)
  "Create uniquely named temp file in =chime-test-base-dir= and write CONTENT to it.
Optional PREFIX is a filename prefix string, default \"tempfile-\".
Returns absolute path to the created file."
  (let ((file (chime-create-temp-test-file prefix)))
	(with-temp-buffer
	  (insert content)
	  (write-file file))
	file))

(provide 'testutil-general)
;;; testutil-general.el ends here.
