;;; git-walktree-mode.el --- Major-mode and minor-mode for git-walktree   -*- lexical-binding: t; -*-

;; Author: 10sr <8.slashes [at] gmail [dot] com>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Major-mode and minor-mode for git-walktree buffer.

;; git-walktree-mode: Major-mode for git-walktree tree buffer
;; git-walktree-minor-mode: Minor-mode for git-walktree blob buffer


;;; Code:

(require 'cl-lib)

(require 'git-walktree-utils)

;; These variables are defined in git-walktree.el
(defvar git-walktree-current-commitish)
(defvar git-walktree-current-path)
(defvar git-walktree-object-full-sha1)

(declare-function git-walktree--open-noselect
                  "git-walktree")

;; TODO: Move to another file?
(defun git-walktree-checkout-blob (object dest)
  "Checkout OBJECT into path DEST.
This function overwrites DEST without asking."
  (let ((status (call-process git-walktree-git-executable
                              nil  ; INFILE
                              (list :file dest)  ; DESTINATION
                              nil  ; DISPLAY
                              "cat-file"  ; ARGS
                              "-p"
                              object)))
    (unless (eq status 0)
      (error "Checkout failed"))))

;; TODO git-walktree-checkout-tree
;; git read-tree --prefix=rescue --index-output=idx 2f9912a
;; GIT_INDEX_FILE=idx git checkout-index -a
;; Or
;; GIT_INDEX_FILE=idx git read-tree 2f9912a
;; GIT_INDEX_FILE=idx git checkout-index -a --prefix=rescue/  # Require last slash



;; git-walktree-mode (major-mode)

(defun git-walktree-mode--move-to-file ()
  "Move point to file field of ls-tree output in current line.
This function do nothing when current line is not ls-tree output."
  (interactive)
  (save-match-data
    (when (save-excursion
            (goto-char (point-at-bol))
            (re-search-forward git-walktree-ls-tree-line-regexp
                               (point-at-eol) t))
      (goto-char (match-beginning 4)))))

(defun git-walktree-mode-next-line (&optional arg try-vscroll)
  "Move cursor vertically down ARG lines and move to file field if found.

For TRY-VSCROLL see doc of `move-line'."
  (interactive "^p\np")
  (or arg (setq arg 1))
  (line-move arg nil nil try-vscroll)
  (git-walktree-mode--move-to-file)
  )

(defun git-walktree-mode-previous-line (&optional arg try-vscroll)
  "Move cursor vertically up ARG lines and move to file field if found.

For TRY-VSCROLL see doc of `move-line'."
  (interactive "^p\np")
  (or arg (setq arg 1))
  (line-move (- arg) nil nil try-vscroll)
  (git-walktree-mode--move-to-file)
  )

(defun git-walktree-mode--get (&optional safe)
  "Get object entry info at current line.
This fucntion throws error If entry not available.

If optional arg SAFE is non-nil, do not error when no entry on current line,
instead return nil."
  (or (git-walktree--parse-lstree-line (buffer-substring-no-properties (point-at-bol)
                                                                       (point-at-eol)))
      (unless safe
        (error "No object entry on current line"))))


(defun git-walktree-mode-open-this ()
  "Open git object of current line."
  (interactive)
  (let ((info (git-walktree-mode--get)))
    (cl-assert info)
    (switch-to-buffer
     (if (string= (plist-get info
                             :type)
                  "commit")
         ;; For submodule cd to that directory and intialize
         ;; TODO: Provide way to go back to known "parent" repository
         (with-temp-buffer
           (cd (plist-get info :file))
           (git-walktree--open-noselect (plist-get info
                                                   :object)
                                        nil
                                        (plist-get info
                                                   :object)))
       (git-walktree--open-noselect git-walktree-current-commitish
                                    (git-walktree--join-path (plist-get info
                                                                        :file)
                                                             git-walktree-current-path)
                                    (plist-get info
                                               :object))))))

(defalias 'git-walktree-mode-goto-revision
  'git-walktree-open)

(cl-defun git-walktree-mode-checkout-to (dest)
  "Checkout blob or tree at point into DEST."
  (declare (interactive-only t))
  (interactive
   (list (let ((default (expand-file-name git-walktree-current-path
                                          git-walktree-repository-root))
               (info (git-walktree-mode--get t)))
           (when info
             (setq default
                   (expand-file-name (plist-get info :file)
                                     default)))
           (read-file-name "Checkout to: "
                           default
                           default))))
  (setq dest
        (expand-file-name dest))
  (let ((info (git-walktree-mode--get)))
    ;; TODO: When point is not on ls-tree lines, checkout current tree object
    (cl-assert info)
    (pcase (plist-get info :type)

      ("blob"
       ;; When DEST is a directory append the name to DEST
       (when (file-directory-p dest)
         (let* ((name (plist-get info :file))
                (name (file-name-nondirectory name)))
           (setq dest (expand-file-name name dest))))
       (when (and (file-exists-p dest)
                  (not (yes-or-no-p (format "Overwrite `%s'? " dest))))
         (message "Canceled by user")
         (cl-return-from git-walktree-mode-checkout-to))
       (git-walktree-checkout-blob (plist-get info :object) dest)
       (message "%s checked out to %s"
                (plist-get info :file)
                dest))

      ("tree"
       (error "Checking out tree is not supported yet"))
      (_
       (error "Cannot checkout this object")))))

(defgroup git-walktree-faces nil
  "Faces used by git-walktree."
  :group 'git-walktree
  :group 'faces)

(defface git-walktree-tree-face
  ;; Same as dired-directory
  '((t (:inherit font-lock-function-name-face)))
  "Face used for tree objects."
  :group 'git-walktree-faces)
(defface git-walktree-commit-face
  '((t (:inherit font-lock-constant-face)))
  "Face used for commit objects."
  :group 'git-walktree-faces)
(defface git-walktree-symlink-face
  ;; Same as dired-symlink face
  '((t (:inherit font-lock-keyword-face)))
  "Face used for symlink objects."
  :group 'git-walktree-faces)


(defvar git-walktree-mode-font-lock-keywords
  `(
    (,git-walktree-ls-tree-line-regexp
     . (
        (1 'shadow)
        (3 'shadow)
        ))
    (,git-walktree-ls-tree-line-tree-regexp
     . (
        (2 'git-walktree-tree-face)
        (4 'git-walktree-tree-face)
        ))
    (,git-walktree-ls-tree-line-commit-regexp
     . (
        (2 'git-walktree-commit-face)
        (4 'git-walktree-commit-face)
        ))
    (,git-walktree-ls-tree-line-symlink-regexp
     . (
        (2 'git-walktree-symlink-face)
        (4 'git-walktree-symlink-face)
        ))
    )
  "Syntax highlighting for git-walktree mode.")

(defvar git-walktree-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'git-walktree-mode-next-line)
    (define-key map "p" 'git-walktree-mode-previous-line)
    (define-key map (kbd "C-n") 'git-walktree-mode-next-line)
    (define-key map (kbd "C-p") 'git-walktree-mode-previous-line)
    (define-key map "P" 'git-walktree-parent-revision)
    (define-key map "N" 'git-walktree-known-child-revision)
    (define-key map "^" 'git-walktree-up)
    (define-key map "G" 'git-walktree-mode-goto-revision)
    (define-key map (kbd "DEL") 'git-walktree-back)
    (define-key map (kbd "C-m") 'git-walktree-mode-open-this)
    (define-key map "C" 'git-walktree-mode-checkout-to)
    map))

(define-derived-mode git-walktree-mode special-mode "GitWalktree"
  "Major-mode for `git-walktree-open'."
  (setq-local font-lock-defaults
              '(git-walktree-mode-font-lock-keywords
                nil nil nil nil
                ))
  )

;; git-walktree-minor-mode (minor-mode)

(cl-defun git-walktree-minor-mode-checkout-to (dest)
  "Checkout current blob into the working directory DEST."
  ;; TODO: Is it really required?
  (declare (interactive-only git-walktree-checkout-blob))
  (interactive
   (list (let ((current-path (expand-file-name git-walktree-current-path
                                               git-walktree-repository-root)))
           (read-file-name "Checkout to: "
                           current-path
                           current-path))))
  (setq dest
        (expand-file-name dest))
  ;; When DEST is a directory append the name to DEST
  (when (file-directory-p dest)
    (let ((name (file-name-nondirectory git-walktree-current-path)))
      (setq dest (expand-file-name name dest))))
  (let ((obj git-walktree-object-full-sha1))
    (cl-assert obj)
    (when (and (file-exists-p dest)
               (not (yes-or-no-p (format "Overwrite `%s'? " dest))))
      (message "Canceled by user")
      (cl-return-from git-walktree-minor-mode-checkout-to))
    (git-walktree-checkout-blob obj dest)
    (message "%s checked out to %s"
             obj
             dest)))

(defvar git-walktree-minor-mode-map
  (let ((map (make-sparse-keymap)))
    ;; C overwrite view-mode keybind `view-kill-and-leave'
    (define-key map "C" 'git-walktree-minor-mode-checkout-to)
    (define-key map "P" 'git-walktree-parent-revision)
    (define-key map "N" 'git-walktree-known-child-revision)
    (define-key map "^" 'git-walktree-up)
    (define-key map "G" 'git-walktree-mode-goto-revision)
    map)
  "Keymap for `git-walktree-minor-mode'.")

(define-minor-mode git-walktree-minor-mode
  "Minor-mode for git-walktree blob buffer.")




(provide 'git-walktree-mode)

;;; git-walktree-mode.el ends here
