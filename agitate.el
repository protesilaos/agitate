;;; agitate.el --- Extras for diff-mode, vc-git, log-edit, log-view -*- lexical-binding: t -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: NOT-YET-AVAILABLE Development <~protesilaos/NOT-YET-AVAILABLE@lists.sr.ht>
;; URL: https://git.sr.ht/~protesilaos/NOT-YET-AVAILABLE
;; Mailing-List: https://lists.sr.ht/~protesilaos/NOT-YET-AVAILABLE
;; Version: 0.0.0
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

;;; Code:

(require 'diff)
(require 'log-edit)
(require 'log-view)
(require 'vc-git)

(defgroup agitate ()
  "Extras for `diff-mode', vc-git, `log-edit-mode', `log-view-mode'."
  :group 'diff
  :group 'vc)

;;;; Commands for diffs

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

;;;###autoload
(defun agitate-diff-kill-dwim ()
  "PROTOTYPE.

Kill hunk or remove the plus/minus signs in current line/region.

When the region is active, remove the plus or minus sign at the
start of each line.

When the region is not active but point is on a line that starts
with a plus or minus sign, remove that sign.

Removing the plus or minus sign means any subsequent commit will
not account for them.

If no region is active and the point is not on a line that starts
with the plus or minus sign, call `diff-hunk-kill' interactively."
  (interactive nil diff-mode)
  (unless mark-ring                  ; needed when entering a new buffer
    (push-mark (point) t nil))
  (when-let (((derived-mode-p 'diff-mode))
             (inhibit-read-only t))
    (cond
     ((region-active-p)
      (replace-regexp-in-region "^[+-]" " " (region-beginning) (region-end)))
     ((progn (goto-char (line-beginning-position)) (looking-at "^[+-]"))
      (replace-match " "))
     (t (call-interactively #'diff-hunk-kill)))))

(defvar outline-minor-mode-highlight)

;;;###autoload
(defun agitate-enable-outline-minor-mode ()
  "Enable `outline-minor-mode' with appropriate tweaks for diffs.

This basically gives you folding of diff hunks by means of the
`outline-cycle' command.

Add this function to the `diff-mode-hook'."
  (require 'outline)
  (let ((outline-minor-mode-highlight nil))
    (when (derived-mode-p 'diff-mode)
      (outline-minor-mode 1))))

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

;;;; Commands for log-edit (commit messages)

(defun agitate--log-edit-extract-file (with-file-extension)
  "Return file from `log-edit-files' without or WITH-FILE-EXTENSION."
  (when-let* ((files (log-edit-files))
              (file (if (length> files 1)
                        (completing-read "Derive shortname from: " files nil t)
                      (car files)))
              (name (file-name-nondirectory file)))
    (if with-file-extension
        file
      (file-name-sans-extension file))))

;;;###autoload
(defun agitate-log-edit-insert-file-name (&optional with-file-extension)
  "Insert at point file name sans directory from `log-edit-files'.

If multiple files are involved, prompt with completion for one
among them.

With optional prefix argument WITH-FILE-EXTENSION, include the
file extension.  Else omit it."
  (interactive "P" log-edit-mode)
  (insert (format "%s: " (agitate--log-edit-extract-file with-file-extension))))

;;;; Commands for vc-git (Git backend for the Version Control framework)

;;;###autoload
(defun agitate-git-grep (regexp)
  "Run `git-grep(1)' for REGEXP in `vc-root-dir'.
This is a simple wrapper around `vc-git-grep' to streamline the
basic task of searching for a regexp in the current Git
repository.  Use the original `vc-git-grep' for its other
features."
  (interactive (list (read-regexp "git-grep: " nil 'vc-git-history)))
  (vc-git-grep regexp "*" (vc-root-dir)))

(defun agitate--vc-git-prompt-remote ()
  "Helper prompt for `agitate-git-push'."
  (when-let ((remotes (process-lines vc-git-program "remote")))
    (if (length> remotes 1)
        (completing-read "Select Git remote: " remotes nil t)
      (car remotes))))

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
  (vc-git--pushpull "push" prompt (unless prompt `(,(agitate--vc-git-prompt-remote)))))

(provide 'agitate)
;;; agitate.el ends here
