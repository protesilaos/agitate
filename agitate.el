;;; agitate.el --- Extras for diff-mode, vc-git, log-edit, log-view -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/agitate
;; Version: 0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: convenience, version control, git

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Another Git Interface Trying to Agitate Tranquil Emacsers.
;;
;; THIS IS A WORK-IN-PROGRESS.  Consult the README.org of this
;; repository for further documentation.  Or visit its web page:
;; <https://protesilaos.com/emacs/agitate>.

;;; Code:

(require 'diff)
(require 'log-edit)
(require 'log-view)
(require 'vc)
(require 'vc-git)

(defgroup agitate ()
  "Extras for `diff-mode', vc-git, `log-edit-mode', `log-view-mode'."
  :group 'diff
  :group 'vc)

;; Inspired by <https://gitmoji.dev/>, though I think most of those
;; are superfluous.  Less is more.
(defcustom agitate-log-edit-emoji-collection
  '(":art: Refine"
    ":bug: Fix"
    ":memo: Document"
    ":rocket: Update"
    ":skull: Delete"
    ":sparkles: Add")
  "Completion candidates for `agitate-log-edit-emoji-commit'.
It is recommended to use the :EMOJI: notation, as it works even
in terminals that cannot output Unicode.  Relevant applications
will render those as their corresponding graphical emoji."
  :type '(repeat string)
  :group 'agitate)

;; Check <https://www.conventionalcommits.org/en/v1.0.0/>.
(defcustom agitate-log-edit-conventional-commits-collection
  '("build" "chore" "ci" "docs" "feat" "fix" "perf" "polish"
    "refactor" "revert" "style" "test" "types" "workflow")
  "Completion candidates for `agitate-log-edit-conventional-commit'."
  :type '(repeat string)
  :group 'agitate)

(defcustom agitate-log-edit-informative-show-files t
  "Show applicable files with `agitate-log-edit-informative-mode'."
  :type 'boolean
  :group 'agitate)

(defcustom agitate-log-edit-informative-show-root-log nil
  "Show root revision log with `agitate-log-edit-informative-mode'.
Place the window below the one which displays the `log-edit'
buffer."
  :type 'boolean
  :group 'agitate)

(defcustom agitate-log-limit 100
  "Limit logs to this natural number.
This is like `vc-log-show-limit', but with a much more
conservative default value."
  :type 'natnum
  :group 'agitate)

(defun agitate--completion-table-no-sort (candidates &optional category annotation)
  "Make completion table for CANDIDATES with sorting disabled.
CATEGORY is the completion category.
ANNOTATION is an annotation function."
  (lambda (str pred action)
    (if (eq action 'metadata)
        `(metadata (display-sort-function . identity)
                   (cycle-sort-function . identity)
                   (annotation-function . ,annotation)
                   (category . ,category))
      (complete-with-action action candidates str pred))))

;;;; Commands for diff-mode

(defvar-local agitate--refine-diff-state nil
  "Current state of `agitate-diff-refine-cycle'.")

;;;###autoload
(defun agitate-diff-refine-cycle ()
  "Cycle current, all, or no refined (word-wise) diff highlighting.

Upon first invocation, refine the diff hunk at point or, when
none exists, the one closest to it.  On second call, operate on
the entire buffer.  And on the third time, remove all word-wise
fontification."
  (interactive nil diff-mode)
  (when-let (((derived-mode-p 'diff-mode))
             (point (point)))
    (pcase agitate--refine-diff-state
      ('current
       (setq-local diff-refine 'font-lock)
       (font-lock-flush)
       (goto-char point)
       (setq agitate--refine-diff-state 'all)
       (message "Diff refine ALL"))
      ('all
       (setq-local diff-refine nil)
       (revert-buffer)
       (goto-char point)
       (recenter)
       (setq agitate--refine-diff-state nil)
       (message "Diff refine NONE"))
      (_
       (diff-refine-hunk)
       (setq agitate--refine-diff-state 'current)
       (message "Diff refine CURRENT")))))

;;;###autoload
(defun agitate-diff-buffer-or-file ()
  "Produce a diff against the file or latest revision.

If the buffer is modified, produce a diff that compares its state
to that of the corresponding file.  In simple terms, show the
latest unsaved changes.

If the buffer is not modified, produce a diff of the file
relative to its latest revision."
  (interactive)
  (if (buffer-modified-p)
      (diff-buffer-with-file (current-buffer))
    (vc-diff)))

;;;###autoload
(defun agitate-diff-narrow-dwim (&optional narrow-file)
  "Narrow to diff hunk or file and widen when already narrowed.
By default narrow to the focused diff hunk.  With optional
NARROW-FILE as a prefix argument, operate on the current file
instead."
  (interactive "P")
  (when (derived-mode-p 'diff-mode)
    (cond
     ((buffer-narrowed-p)
      (widen)
      (message "WIDENED the view"))
     (narrow-file
      (diff-restrict-view narrow-file)
      (message "Narrowed to FILE"))
     (t
      (diff-restrict-view)
      (message "Narrowed to diff HUNK")))))

(defvar outline-minor-mode-highlight)

;;;###autoload
(defun agitate-diff-enable-outline-minor-mode ()
  "Enable `outline-minor-mode' with appropriate tweaks for diffs.

This basically gives you folding of diff hunks by means of the
`outline-cycle' command.

Add this function to the `diff-mode-hook'."
  (require 'outline)
  (let ((outline-minor-mode-highlight nil))
    (when (derived-mode-p 'diff-mode)
      (outline-minor-mode 1))))

;;;; Commands for log-edit (commit messages)

(defun agitate--log-edit-extract-file (with-file-extension)
  "Return file from `log-edit-files' without or WITH-FILE-EXTENSION."
  (when-let* ((files (log-edit-files))
              (file (if (length> files 1)
                        (completing-read "Derive shortname from: " files nil t)
                      (car files)))
              (name (file-name-nondirectory file)))
    (if with-file-extension
        name
      (file-name-sans-extension name))))

;;;###autoload
(defun agitate-log-edit-insert-file-name (&optional with-file-extension)
  "Insert at point file name sans directory from `log-edit-files'.

If multiple files are involved, prompt with completion for one
among them.

With optional prefix argument WITH-FILE-EXTENSION, include the
file extension.  Else omit it."
  (interactive "P" log-edit-mode)
  (insert (format "%s: " (agitate--log-edit-extract-file with-file-extension))))

(defvar agitate--log-edit-emoji-commit-history nil
  "Minibuffer history of `agitate-log-edit-emoji-commit'.")

;;;###autoload
(defun agitate-log-edit-emoji-commit ()
  "Insert emoji commit message at point.
Prompt for entry among `agitate-log-edit-emoji-collection'."
  (declare (interactive-only t))
  (interactive)
  (insert
   (completing-read
    "Select type of commit+emoji: "
    agitate-log-edit-emoji-collection nil t nil
    'agitate--log-edit-emoji-commit-history)))

(defvar agitate--log-edit-conventional-commits-history nil
  "Minibuffer history of `agitate-log-edit-conventional-commit'.")

(defun agitate-log-edit-conventional-commit ()
  "Insert conventional commit message at point.
Prompt for entry among those declared in
`agitate-log-edit-conventional-commits-collection'."
  (declare (interactive-only t))
  (interactive)
  (insert
   (concat
    (completing-read
     "Select type of conventional commit: "
     agitate-log-edit-conventional-commits-collection nil t nil
     'agitate--log-edit-conventional-commits-history)
    ": ")))

;;;;; log-edit "informative" window configuration mode

;;;###autoload
(define-minor-mode agitate-log-edit-informative-mode
  "Apply a specific window configuation when entering `log-edit'.

Show the `log-edit' window on the left, with the corresponding
diff on the right.

If `agitate-log-edit-informative-show-root-log' is non-nil, run
`vc-print-root-log' subject to `agitate-log-limit'.  Display it
below the `log-edit' window.

If `agitate-log-edit-informative-show-files' is non-nil, show the
`log-edit-files' further below.

Restore the last window configuration when finalising `log-edit',
either with `log-edit-kill-buffer' or `log-edit-done'.

Always kill the buffer used to perform the editing.  Ignore the
user option `log-edit-keep-buffer'."
  :init-value nil
  :global t
  (if agitate-log-edit-informative-mode
      (progn
        (add-hook 'vc-before-checkin-hook #'agitate--log-edit-informative-save-windows)
        (add-hook 'log-edit-hook #'agitate--log-edit-informative-setup)
        (add-hook 'vc-checkin-hook #'agitate--log-edit-informative-restore)
        (add-hook 'vc-checkin-hook #'agitate--log-edit-informative-kill-buffer))
    (remove-hook 'vc-before-checkin-hook #'agitate--log-edit-informative-save-windows)
    (remove-hook 'log-edit-hook #'agitate--log-edit-informative-setup)
    (remove-hook 'vc-checkin-hook #'agitate--log-edit-informative-restore)
    (remove-hook 'vc-checkin-hook #'agitate--log-edit-informative-kill-buffer)))

(defvar agitate--previous-window-configuration nil
  "Store the last window configuration.")

(defvar agitate--previous-window-point nil
  "Store the last window `point'.")

(defvar agitate--previous-window nil
  "Store the last window.")

(defun agitate--log-edit-informative-save-windows ()
  "Save `current-window-configuration'."
  (let ((win (get-buffer-window)))
    (setq agitate--previous-window win
          agitate--previous-window-point (window-point win)
          agitate--previous-window-configuration (current-window-configuration))))

(defun agitate--log-edit-informative-setup ()
  "Set up informative `log-edit' window configuration."
  (delete-other-windows)
  (add-hook 'kill-buffer-hook #'agitate--log-edit-informative-restore nil t)
  ;; FIXME 2022-10-19: Fails in an empty repo.  It is not nice to use
  ;; `ignore-errors', as we should not display any window in such a
  ;; scenario.  Which VC function can check for a repo without
  ;; revisions?
  (ignore-errors
    (save-selected-window
      (log-edit-show-diff))
    (if agitate-log-edit-informative-show-files
        (log-edit-show-files)
      (log-edit-hide-buf log-edit-files-buf))
    (when agitate-log-edit-informative-show-root-log
      (save-selected-window
        (let ((display-buffer-alist
               (cons (list (cons 'derived-mode 'log-view-mode)
                           (list 'display-buffer-below-selected))
                     display-buffer-alist)))
          (vc-print-root-log agitate-log-limit))))))

;; FIXME 2022-10-20: Sometimes we get an error like:
;;
;; window-normalize-window: #<window 153> is not a live window
;;
;; What is the cause?
(defun agitate--log-edit-informative-restore ()
  "Restore `agitate--previous-window-configuration' and clean state."
  (when agitate--previous-window-configuration
    (set-window-configuration
     agitate--previous-window-configuration
     :dont-set-frame
     :dont-set-miniwindow)
    (setq agitate--previous-window-configuration nil))
  (when (and agitate--previous-window
             agitate--previous-window-point
             (window-live-p agitate--previous-window))
    (select-window agitate--previous-window)
    (goto-char agitate--previous-window-point)
    (setq agitate--previous-window nil
          agitate--previous-window-point nil)))

(defun agitate--log-edit-informative-kill-buffer ()
  "Kill the vc-log buffer."
  ;; TODO 2022-10-19: More robust way to get this buffer?
  (when-let ((buf (get-buffer "*vc-log*")))
    (kill-buffer buf)))

;;;; Commands for log-view (listings of commits)

;;;###autoload
(defun agitate-log-view-kill-revision ()
  "Append to `kill-ring' log-view revision at or around point.

When the log-view is in the short format (one compact line per
revision), the revision is the one on the current line.  If the
revision is expanded with `log-view-expanded-log-entry-function'
and point is somewhere inside the expanded text, the revision is
still the same.

When the log-view is in the long format (detailed view where each
revision spans several lines), the revision is the one pertinent
to the text at point."
  (interactive)
  (when-let ((revision (cadr (log-view-current-entry (point) t))))
    (kill-new (format "%s" revision))
    (message "Copied: %s" revision)))

(defun agitate--log-view-on-revision-p (&optional pos)
  "Return non-nil if optional POS is on a revision line.
When POS is nil, use `point'."
  (when-let ((point (or pos (point)))
             ((not (log-view-inside-comment-p point))))
    (save-excursion
      (goto-char (line-beginning-position))
      (looking-at log-view-message-re))))

(defun agitate--log-view-revision-expanded-bounds (&optional back)
  "Return position of expanded log-view message.
With optional BACK, find the beginning, else the end."
  (let ((motion (if back
                    (list 're-search-backward 'log-view-msg-prev 1)
                  (list 're-search-forward 'log-view-msg-next -1))))
    (save-excursion
      (funcall (nth 0 motion) log-view-message-re nil t)
      (forward-line (nth 2 motion))
      (point))))

(defun agitate--log-view-kill-message (pos)
  "Do what `agitate-log-view-kill-revision-expanded' describes for POS."
  (kill-new
   (buffer-substring-no-properties
    (agitate--log-view-revision-expanded-bounds :back)
    (agitate--log-view-revision-expanded-bounds)))
  (message "Copied message of `%s' revision"
           (save-excursion (cadr (log-view-current-entry pos t)))))

;;;###autoload
(defun agitate-log-view-kill-revision-expanded ()
  "Append to `kill-ring' expanded message of log-view revision at point."
  (interactive nil log-view-mode)
  (let ((pos (point))
        opos)
    (when (agitate--log-view-on-revision-p pos)
      (setq opos (point))
      (forward-line 1)
      (setq pos (point)))
    (if (log-view-inside-comment-p pos)
        (agitate--log-view-kill-message pos)
      (goto-char opos))))

;;;; Commands for vc-git (Git backend for the Version Control framework)

(defun agitate--vc-git-get-hash-from-string (string)
  "Return commit hash from beginning of STRING."
  (when (string-match "\\b\\([0-9a-z]+\\)\\(\s+\\)?" string)
    (match-string 1 string)))

(defun agitate--vc-git-commit-prompt (&optional file)
  "Prompt for Git commit and return it as a string.
With optional FILE, limit the commits to those pertinent to it.

The number of revisions in the log is controlled by the user
option `agitate-log-limit'."
  (let* ((prompt (if file
                     (format "Select revision of `%s': " file)
                   "Select revision: "))
         (default-directory (vc-root-dir)))
    (completing-read
     prompt
     (agitate--completion-table-no-sort
      (process-lines
       vc-git-program "log"
       (format "-n %d" agitate-log-limit)
       "--pretty=format:%h  %ad  %an: %s"
       "--date=short"
       (or file "--")))
     nil t)))

;;;###autoload
(defun agitate-vc-git-find-revision ()
  "Find revision of current file, visiting it in a buffer.
Prompt with completion for the revision.

The number of revisions in the log is controlled by the user
option `agitate-log-limit'.

Pro tip: if you are using the `embark' package, you can produce a
snapshot of the minibuffer prompt.  Then use the resulting buffer
to browse through the file's history."
  (declare (interactive-only t))
  (interactive)
  (when-let* ((fileset (vc-deduce-fileset))
              (file (caadr fileset))
              (revision (agitate--vc-git-get-hash-from-string
                         (agitate--vc-git-commit-prompt
                          file))))
    (pop-to-buffer (vc-find-revision file revision (car fileset)))))

(defun agitate--vc-git-show-revert (&rest args)
  "Run `vc-git--call' with ARGS.
This is a helper for git-show(1) `revert-buffer-function'."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (apply 'vc-git--call args)
    (goto-char (point-min))))

;;;###autoload
(defun agitate-vc-git-show (&optional current-file)
  "Prompt for commit and run `git-show(1)' on it.
With optional CURRENT-FILE as prefix argument, limit the commits
to those pertaining to the current file.

The number of revisions in the log is controlled by the user
option `agitate-log-limit'.

Pro tip: if you are using the `embark' package, you can produce a
snapshot of the minibuffer prompt.  Then use the resulting buffer
to browse through the available commits."
  (declare (interactive-only t))
  (interactive "P")
  (when-let ((file (caadr (vc-deduce-fileset))))
    (let* ((f (when current-file file))
           (revision (agitate--vc-git-get-hash-from-string
                      (agitate--vc-git-commit-prompt
                       f)))
           (buf "*agitate-vc-git-show*")
           (inhibit-read-only t))
      (with-current-buffer (get-buffer-create buf)
        (erase-buffer))
      (vc-git--call buf "show" "--patch-with-stat" revision)
      (with-current-buffer (pop-to-buffer buf)
        (diff-mode)
        (setq-local revert-buffer-function
                    (lambda (_ignore-auto _noconfirm)
                        (agitate--vc-git-show-revert buf "show" "--patch-with-stat" revision)))
        (goto-char (point-min))))))

(defun agitate--vc-git-tag-prompt ()
  "Prompt for Git tag."
  (when-let* ((default-directory (vc-root-dir)))
    (completing-read
     "Select tag: "
     (agitate--completion-table-no-sort
      (process-lines
       vc-git-program "tag"
       "--"))
     nil t)))

;;;###autoload
(defun agitate-vc-git-show-tag (tag)
  "Run `git-show(1)' on Git TAG.
When called interactively, prompt for TAG using minibuffer
completion.

Pro tip: if you are using the `embark' package, you can produce a
snapshot of the minibuffer prompt.  Then use the resulting buffer
to browse through the available tags."
  (interactive (list (agitate--vc-git-tag-prompt)))
  (let* ((buf "*agitate-vc-git-show*")
         (inhibit-read-only t))
    (with-current-buffer (get-buffer-create buf)
        (erase-buffer))
    (vc-git--call buf "show" tag)
    (with-current-buffer (pop-to-buffer buf)
      (diff-mode)
      (setq-local revert-buffer-function
                  (lambda (_ignore-auto _noconfirm)
                    (let ((inhibit-read-only t))
                      (agitate--vc-git-show-revert buf "show" tag))))
      (goto-char (point-min)))))

(defun agitate--vc-git-format-patch-single-commit ()
  "Help `agitate-vc-git-format-patch-single' with its COMMIT."
  (if-let ((default-value (cadr (log-view-current-entry (point) t))))
      default-value
    (agitate--vc-git-get-hash-from-string (agitate--vc-git-commit-prompt))))

;;;###autoload
(defun agitate-vc-git-format-patch-single (commit)
  "Format patch for a single COMMIT.

If in a log-view buffer, the COMMIT is the one at point.  For the
details of how that is determined, read the doc string of
`agitate-log-view-kill-revision'.

If there is no such commit at point, prompt for COMMIT using
minibuffer completion.

Output the patch file to the return value of the function
`vc-root-dir'.

The number of revisions in the log is controlled by the user
option `agitate-log-limit'.

For Emacs 29, consider using `vc-prepare-patch'."
  (interactive (list (agitate--vc-git-format-patch-single-commit)))
  ;; TODO 2022-09-27: Handle the output directory better.  Though I am
  ;; not sure how people work with those.  I normally use the root of
  ;; the current repo (and then clean it) or put everything in the
  ;; ~/Desktop or some dedicated "patches" directory.
  (when-let* ((root (vc-root-dir))
              (default-directory root))
    (vc-git--call nil "format-patch" "-1" commit "--")))

;;;###autoload
(defun agitate-vc-git-format-patch-n-from-head (number)
  "Format patches covering NUMBER of commits from current HEAD.
This is the eqvuivalent of: git format-patch -NUMBER.

For Emacs 29, consider using `vc-prepare-patch'."
  (interactive (list (read-number "git format-patch -NUMBER: ")))
  (if (natnump number)
      (vc-git--call nil "format-patch" (format "-%d" number))
    (user-error "NUMBER must satisfy `natnump'; `%s' does not" number)))

;;;###autoload
(defun agitate-vc-git-grep (regexp)
  "Run `git-grep(1)' for REGEXP in `vc-root-dir'.
This is a simple wrapper around `vc-git-grep' to streamline the
basic task of searching for a regexp in the current Git
repository.  Use the original `vc-git-grep' for its other
arguments."
  (interactive (list (read-regexp "git-grep: " nil 'vc-git-history)))
  (vc-git-grep regexp "*" (vc-root-dir)))

(defun agitate--vc-git-prompt-remote ()
  "Helper prompt for `agitate-git-push'."
  (when-let ((remotes (process-lines vc-git-program "remote")))
    (if (length> remotes 1)
        (completing-read "Select Git remote: " remotes nil t)
      (car remotes))))

(defvar agitate--vc-git-kill-commit-message-history nil
  "Minibuffer history of `agitate-vc-git-kill-commit-message'.")

(defun agitate--vc-git-kill-commit-message-prompt ()
  "Helper prompt for `agitate-vc-git-kill-commit-message'."
  (let* ((default-value (cadr (log-view-current-entry (point) t)))
         (prompt (if default-value
                     (format "Commit HASH [%s]: " default-value)
                   "Commit HASH: "))
         (default-directory (vc-root-dir)))
    (completing-read
     prompt
     (agitate--completion-table-no-sort
      (process-lines
       vc-git-program "log"
       (format "-n %d" agitate-log-limit)
       "--pretty=format:%h  %ad  %an: %s"
       "--date=short"
       "--"))
     nil t nil
     'agitate--vc-git-kill-commit-message-history default-value)))

;;;###autoload
(defun agitate-vc-git-kill-commit-hash ()
  "Append to `kill-ring' hash of commit.
Prompt for commit using minibuffer completion.  The number of
revisions in the log is controlled by the user option
`agitate-log-limit'.

To kill the message of the commit, use the command
`agitate-vc-git-kill-commit-message'."
  (declare (interactive-only t))
  (interactive)
  (let ((hash (agitate--vc-git-get-hash-from-string
               (agitate--vc-git-kill-commit-message-prompt))))
    (kill-new hash)
    (message "Added `%s' to `kill-ring'" hash)))

;;;###autoload
(defun agitate-vc-git-kill-commit-message (hash)
  "Append to `kill-ring' message of commit with HASH identifier.
When called interactively, prompt for HASH using minibuffer
completion.

When point is in a log-view buffer, make the revision at point
the default value of the prompt (though also see the command
`agitate-log-view-kill-revision-expanded').

The number of revisions in the log is controlled by the user
option `agitate-log-limit'.

To kill only the commit hash, use the command
`agitate-vc-git-kill-commit-hash'."
  (interactive
   (list
    (agitate--vc-git-get-hash-from-string
     (agitate--vc-git-kill-commit-message-prompt))))
  (kill-new
   (with-temp-buffer
     (vc-git--call t "show" "--stat" "--no-color" hash "--")
     (buffer-substring-no-properties (point-min) (point-max))))
  (message "Added %s commit message to `kill-ring'" hash))

;; TODO 2022-09-27: We can have something similar which prompts for a
;; branch to push to.  There are lots of possibilities.  The idea is
;; that the user can pick the function they are most likely to use as
;; their default.  Then they can rely on PROMPT to modify its
;; behaviour.

;;;###autoload
(defun agitate-vc-git-push-prompt-for-remote (prompt)
  "Behave like `vc-git-push' but prompt for a remote, if needed.
The meaning of PROMPT is the same as that of `vc-git-push'.  In
such a case, do not prompt for a remote.

To use this function add it as an override advice to
`vc-git-push'."
  (vc-git--pushpull "push" prompt (list (agitate--vc-git-prompt-remote))))

(provide 'agitate)
;;; agitate.el ends here
