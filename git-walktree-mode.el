;;; git-walktree-mode.el --- Major-mode for git-walktree   -*- lexical-binding: t; -*-

;; Author: 10sr <8.slashes [at] gmail [dot] com>
;; URL: https://github.com/10sr/git-walktree-el
;; Version: 0.0.1
;; Keywords: utility git

;; This file is not part of GNU Emacs.

;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;   http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.

;;; Commentary:

;; Major-mode for git-walktree buffer.


;;; Code:

;; These variables are defined in git-walktree.el
(defconst git-walktree-ls-tree-line-regexp nil)
(defconst git-walktree-ls-tree-line-tree-regexp nil)
(defconst git-walktree-ls-tree-line-commit-regexp nil)
(defvar git-walktree-current-committish)

(declare-function git-walktree--parse-lstree-line
                  "git-walktree")
(declare-function git-walktree--open-noselect
                  "git-walktree")
(declare-function git-walktree--join-path
                  "git-walktree")

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
  "Move cursor vertically down ARG lines and move to file field if found."
  (interactive "^p\np")
  (or arg (setq arg 1))
  (line-move arg nil nil try-vscroll)
  (git-walktree-mode--move-to-file)
  )

(defun git-walktree-mode-previous-line (&optional arg try-vscroll)
  "Move cursor vertically up ARG lines and move to file field if found."
  (interactive "^p\np")
  (or arg (setq arg 1))
  (line-move (- arg) nil nil try-vscroll)
  (git-walktree-mode--move-to-file)
  )

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
  ;; Same as dired-symlink face
  '((t (:inherit font-lock-keyword-face)))
  "Face used for commit objects."
  :group 'git-walktree-faces)


(defvar git-walktree-mode-font-lock-keywords
  nil
  "Syntax highlighting for git-walktree mode.")


(defun git-walktree-mode-open-this ()
  "Open git object of current line."
  (interactive)
  (let ((info (git-walktree--parse-lstree-line (buffer-substring-no-properties (point-at-bol)
                                                                               (point-at-eol)))))
    (if info
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
           (git-walktree--open-noselect git-walktree-current-committish
                                        (git-walktree--join-path (plist-get info
                                                                            :file))
                                        (plist-get info
                                                   :object))))
      (message "No object on current line."))))


(defvar git-walktree-mode-map
  (let ((map (make-sparse-keymap)))
    ;; TODO: Add C to copy to working directory
    (define-key map "n" 'git-walktree-mode-next-line)
    (define-key map "p" 'git-walktree-mode-previous-line)
    (define-key map (kbd "C-n") 'git-walktree-mode-next-line)
    (define-key map (kbd "C-p") 'git-walktree-mode-previous-line)
    ;; TODO: Review keybind
    ;; TODO: Define minor-mode and use also in blob buffer
    (define-key map "P" 'git-walktree-parent-revision)
    (define-key map "N" 'git-walktree-known-child-revision)
    (define-key map "^" 'git-walktree-up)
    ;; TODO: implement
    (define-key map (kbd "DEL") 'git-walktree-back)
    (define-key map (kbd "C-m") 'git-walktree-mode-open-this)
    map))

(define-derived-mode git-walktree-mode special-mode "GitWalktree"
  "Major-mode for `git-walktree-open'."
  ;; This have to be set in major-mode body because regexps are
  ;; defined outside of this file.
  (setq-local git-walktree-mode-font-lock-keywords
              `(
                (,git-walktree-ls-tree-line-regexp . (
                                                      (1 'shadow)
                                                      (3 'shadow)
                                                      ))
                (,git-walktree-ls-tree-line-tree-regexp . (
                                                           (2 'git-walktree-tree-face)
                                                           (4 'git-walktree-tree-face)
                                                           ))
                (,git-walktree-ls-tree-line-commit-regexp . (
                                                             (2 'git-walktree-commit-face)
                                                             (4 'git-walktree-commit-face)
                                                             ))
                ))
  (setq-local font-lock-defaults
              '(git-walktree-mode-font-lock-keywords
                nil nil nil nil
                ))
  )



(provide 'git-walktree-mode)

;;; git-walktree-mode.el ends here
