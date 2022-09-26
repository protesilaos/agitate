;;; agitate.el --- WORK-IN-PROGRESS -*- lexical-binding: t -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: NOT-YET-AVAILABLE Development <~protesilaos/NOT-YET-AVAILABLE@lists.sr.ht>
;; URL: https://git.sr.ht/~protesilaos/NOT-YET-AVAILABLE
;; Mailing-List: https://lists.sr.ht/~protesilaos/NOT-YET-AVAILABLE
;; Version: 0.0.0
;; Package-Requires: ((emacs "29.1"))
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

(require 'log-edit)
(require 'vc-git)

(defgroup agitate ()
  "Work-in-progress."
  :group 'vc)

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

(defun agitate--vc-git-prompt-remote ()
  "Helper prompt for `agitate-git-push'."
  (when-let ((remotes (process-lines vc-git-program "remote")))
    (if (length> remotes 1)
        (completing-read "Select Git remote: " remotes nil t)
      (car remotes))))

;;;###autoload
(defun agitate-vc-git-push (prompt)
  "Substitute for `vc-git-push' with the same meaning for PROMPT."
  (vc-git--pushpull "push" prompt (unless prompt `(,(agitate--vc-git-prompt-remote)))))

(provide 'agitate)
;;; agitate.el ends here
