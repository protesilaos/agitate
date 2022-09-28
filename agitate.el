;;; agitate.el --- Extras for diff-mode, vc-git, log-edit, log-view -*- lexical-binding: t -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Agitate Development <~protesilaos/agitate@lists.sr.ht>
;; URL: https://git.sr.ht/~protesilaos/agitate
;; Mailing-List: https://lists.sr.ht/~protesilaos/agitate
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

  ;; TODO 2022-09-28: Maybe have a do-what-I-mean behaviour to expand
  ;; the entry if necessary?  Or maybe expand it to get the message
  ;; and then contract it again?  Keeping it simple seems appropriate,
  ;; but we will see how this evolves.

;;;###autoload
(defun agitate-log-view-kill-revision-expanded ()
  "PROTOTYPE Append to `kill-ring' expanded message of log-view revision at point."
  (interactive nil log-view-mode)
  (let ((pos (point)))
    ;; TODO 2022-09-28: Also test when we are on the line of the
    ;; revision, with the expanded entry right below.
    (when (log-view-inside-comment-p pos)
      (kill-new
       (buffer-substring-no-properties
        (agitate--log-view-revision-expanded-bounds :back)
        (agitate--log-view-revision-expanded-bounds)))
      (message "Copied message of `%s' revision"
               (cadr (log-view-current-entry pos t))))))

;;;; Commands for vc-git (Git backend for the Version Control framework)

(defun agitate--vc-git-get-hash-from-string (string)
  "Return commit hash from beginning of STRING."
  (when (string-match "\\b\\([0-9a-z]+\\)\\(\s+\\)?" string)
    (match-string 1 string)))

(defun agitate--vc-git-commit-prompt (&optional file long)
  "Prompt for Git commit and return it as a string.
With optional FILE, limit the commits to those pertinent to it.
With optional LONG do not abbreviate commit hashes."
  (let* ((prompt (if file
                     (format "Select revision of `%s': " file)
                   "Select revision: "))
         (commit-format (if long "--pretty=oneline" "--oneline"))
         (default-directory (vc-root-dir)))
    (completing-read
     prompt
     (process-lines vc-git-program "log" commit-format (or file "--"))
     nil t)))

(defvar agitate-vc-git-show-buffer "*agitate-vc-git-show*"
  "Buffer for showing a git commit.")

;;;###autoload
(defun agitate-vc-git-show (&optional current-file)
  "PROTOTYPE.

Prompt for commit and run `git-show(1)' on it.
With optional CURRENT-FILE as prefix argument, limit the commits
to those pertaining to the current file."
  (declare (interactive-only t))
  (interactive "P")
  (when-let* ((file (caadr (vc-deduce-fileset))) ; FIXME 2022-09-27: Better way to get current file?
              (revision (agitate--vc-git-get-hash-from-string
                         (agitate--vc-git-commit-prompt
                          (when current-file file))))
              (buf "*agitate-vc-git-show*"))
    (apply 'vc-git-command (get-buffer-create buf) nil (when current-file file)
           (list "show" "--patch-with-stat" revision))
    ;; TODO 2022-09-27: What else do we need to set up in such a
    ;; buffer?
    (with-current-buffer (pop-to-buffer buf)
      (diff-mode))))

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
`vc-root-dir'."
  (interactive (list (agitate--vc-git-format-patch-single-commit)))
  ;; TODO 2022-09-27: Handle the output directory better.  Though I am
  ;; not sure how people work with those.  I normally use the root of
  ;; the current repo (and then clean it) or put everything in the
  ;; ~/Desktop or some dedicated "patches" directory.
  (when-let* ((root (vc-root-dir))
              (default-directory root))
    (apply 'vc-git-command nil nil nil
           (list "format-patch" "-1" commit "--"))))

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
     (process-lines vc-git-program "log" "--oneline" "--")
     nil t nil
     'agitate--vc-git-kill-commit-message-history default-value)))

;;;###autoload
(defun agitate-vc-git-kill-commit-message (hash)
  "Append to `kill-ring' message of commit with HASH identifier.
When called interactively, prompt for HASH using minibuffer
completion.

When point is in a log-view buffer, make the revision at point
the default value of the prompt.

This is useful to quote a past commit message."
  (interactive
   (list
    (agitate--vc-git-get-hash-from-string
     (agitate--vc-git-kill-commit-message-prompt))))
  (kill-new
   (with-temp-buffer
     (apply 'vc-git-command t nil nil (list "show" hash "--stat" "--no-color" "--"))
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
  (vc-git--pushpull "push" prompt (unless prompt `(,(agitate--vc-git-prompt-remote)))))

(provide 'agitate)
;;; agitate.el ends here
