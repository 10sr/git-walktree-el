;;; git-walktree.el --- Walk through git tree and blob objects   -*- lexical-binding: t; -*-

;; Author: 10sr <8.slashes [at] gmail [dot] com>
;; URL: https://github.com/10sr/git-walktree-el
;; Version: 0.0.1
;; Keywords: utility git

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

;; Walk through git revisions.


;;; Code:

(require 'git-walktree-utils)
(require 'git-walktree-mode)
(require 'git-walktree-read)

(defgroup git-walktree nil
  "Git Walktree."
  :tag "GitWalktree"
  :prefix "git-walktree-"
  :group 'tools)

(defvar-local git-walktree-current-committish nil
  "Committish name of currently browsing.")

(defvar-local git-walktree-current-path nil
  "Path name currently visiting without leading and trailing slash.
This path is always relative to repository root.")

(defvar-local git-walktree-buffer-file-name nil
  "Psudo filename of current buffer.")

(defvar-local git-walktree-object-full-sha1 nil
  "Object name in full sha1 format of current buffer.")

(defvar-local git-walktree-repository-root nil
  "Repository root path of current buffer.")
(put 'git-walktree-repository-root
     'permanent-local
     t)

(defun git-walktree--create-buffer (committish name type)
  "Create and return buffer for COMMITTISH:NAME.
TYPE is target object type."
  (let* ((root (git-walktree--git-plumbing "rev-parse"
                                           "--show-toplevel"))
         (committish-display (git-walktree--committish-fordisplay committish))
         ;; TODO: Fix that buffer name gets too long when name is long
         (name (format "*GitWalkTree<%s:%s>*"
                       (or committish-display "")
                       name)))

    (if (and git-walktree-reuse-tree-buffer
             (string= type "tree"))
        (let ((existing
               (and git-walktree-tree-buffer-for-reuse
                    (buffer-name git-walktree-tree-buffer-for-reuse)
                    git-walktree-tree-buffer-for-reuse)))
          (with-current-buffer (or existing
                                   (setq git-walktree-tree-buffer-for-reuse
                                         (generate-new-buffer "gitwalktreebuf")))
            (setq git-walktree-repository-root root)
            (rename-buffer name t)
            (current-buffer)))
      (with-current-buffer (get-buffer-create name)
        (if git-walktree-repository-root
            (if (string= root
                         git-walktree-repository-root)
                (current-buffer)
              ;; If the buffer is for another repository, create new buffer
              (with-current-buffer (generate-new-buffer name)
                (setq git-walktree-repository-root root)
                (current-buffer)))
          ;; New buffer
          (setq git-walktree-repository-root root)
          (current-buffer))))))

(defun git-walktree--replace-into-buffer (target)
  "Replace TARGET buffer contents with that of current buffer.
It also copy text overlays."
  (let ((src (current-buffer)))
    (with-current-buffer target
      (replace-buffer-contents src)))

  ;; Copy color overlays
  (let ((overlays (overlays-in (point-min) (point-max))))
    (dolist (o overlays)
      (let ((beg (overlay-start o))
            (end (overlay-end o)))
        (move-overlay (copy-overlay o)
                      beg
                      end
                      target)))))

(require 'ansi-color)
(defun git-walktree--open-treeish (committish path treeish)
  "Open git tree buffer of COMMITISH:PATH.

TREEISH should be a tree-ish object full-sha1 of COMMITISH:PATH."
  (cl-assert path)
  (cl-assert treeish)
  (let* (point-tree-start
         (type (git-walktree--git-plumbing "cat-file"
                                           "-t"
                                           treeish))
         (buf (git-walktree--create-buffer committish path type))
         )
    (cl-assert (member type
                       '("commit" "tree")))
    (with-current-buffer buf
      (unless (and (string= treeish
                            git-walktree-object-full-sha1)
                   (or (eq committish
                           git-walktree-current-committish)
                       (string= committish
                                git-walktree-current-committish)))
        (buffer-disable-undo)
        ;; For running git command go back to repository root
        (cd git-walktree-repository-root)
        (save-excursion
          (let ((inhibit-read-only t))
            ;; Remove existing overlays generated by ansi-color-apply-on-region
            (remove-overlays)
            (with-temp-buffer
              (if committish
                  (progn (git-walktree--call-process nil
                                                     "show"
                                                     ;; TODO: Make this args configurable
                                                     ;; "--no-patch"
                                                     "--color=always"
                                                     "--pretty=short"
                                                     "--decorate"
                                                     "--stat"
                                                     committish)
                         (ansi-color-apply-on-region (point-min)
                                                     (point))
                         (insert "\n")
                         (insert (format "Contents of '%s:%s':\n"
                                         (git-walktree--committish-fordisplay committish)
                                         path)))
                (insert (format "Contents of treeish object '%s:\n"
                                treeish)))
              (setq point-tree-start (point))
              (git-walktree--call-process nil
                                          "ls-tree"
                                          ;; "-r"
                                          "--abbrev"

                                          treeish)
              (git-walktree--replace-into-buffer buf))
            ))
        (git-walktree-mode)
        (set-buffer-modified-p nil)

        (setq git-walktree-current-committish committish)
        (setq git-walktree-current-path path)
        (setq git-walktree-object-full-sha1 treeish)
        (let ((dir (expand-file-name path git-walktree-repository-root)))
          (when (and git-walktree-try-cd
                     (file-directory-p dir))
            (cd dir)))
        (when (= (point) (point-min))
          (goto-char point-tree-start)
          (git-walktree-mode--move-to-file)
          )
        ))
    buf))

(defun git-walktree--call-process (&optional infile &rest args)
  "Call git command with input from INFILE and args ARGS.
Result will be inserted into current buffer."
  (let ((status (apply 'call-process
                       git-walktree-git-executable
                       infile
                       t
                       nil
                       args)))
    (unless (eq 0
                status)
      (error "Failed to call git process %S %S"
             infile
             args))))
?w
(defun git-walktree--open-blob (committish path blob)
  "Open blob object of COMMITISH:PATH.
BLOB should be a object full sha1 of COMMITISH:PATH."
  (cl-assert committish)
  (cl-assert path)
  (cl-assert blob)
  (let* ((type (git-walktree--git-plumbing "cat-file"
                                           "-t"
                                           blob))
         (buf (git-walktree--create-buffer committish path type)))
    (cl-assert (string= type "blob"))
    (with-current-buffer buf
      (unless (string= blob
                       git-walktree-object-full-sha1)
        ;; For running git command go back to repository root
        (cd git-walktree-repository-root)
        (let ((inhibit-read-only t))
          (with-temp-buffer
            (git-walktree--call-process nil
                                        "cat-file"
                                        "-p"
                                        blob)
            (git-walktree--replace-into-buffer buf)))
        (setq git-walktree-buffer-file-name
              (concat git-walktree-repository-root "/git@" committish ":" path))
        (setq buffer-file-name
              (concat git-walktree-repository-root "/" path))
        (normal-mode t)
        ;; For asking filename when C-xC-s
        (setq buffer-file-name nil)
        (set-buffer-modified-p t)

        (setq git-walktree-current-committish committish)
        (setq git-walktree-current-path path)
        (setq git-walktree-object-full-sha1 blob)
        (let ((dir (expand-file-name (or (file-name-directory path)
                                         ".")
                                     git-walktree-repository-root)))
          (when (and git-walktree-try-cd
                     (file-directory-p dir))
            (cd dir)))

        (view-mode 1)
        (git-walktree-minor-mode 1)
        ))
    buf))

(defun git-walktree--open-noselect-safe-path (committish &optional path)
  "Open git object of COMMITTISH:PATH.
If PATH not found in COMMITTISH tree, go up path and try again until found.
When PATH is omitted or nil, it is calculated from current file or directory."
  (cl-assert committish)
  (let ((type (git-walktree--git-plumbing "cat-file"
                                          "-t"
                                          committish)))
    (cl-assert (string= type "commit")))

  (setq path
        (or path
            (git-walktree--path-in-repository (or buffer-file-name
                                                  default-directory))))
  ;; PATH must not start with and end with slashes
  (cl-assert (not (string-match-p "\\`/" path)))
  (cl-assert (not (string-match-p "/\\'" path)))

  (let ((obj (git-walktree--resolve-object committish path)))
    (while (not obj)
      (setq path
            (git-walktree--parent-directory path))
      (setq obj
            (git-walktree--resolve-object committish path)))
    (git-walktree--open-noselect committish
                                 path
                                 obj)))

;; TODO: Store view history
;; Or add variable like -previously-opened or -referer?
(defun git-walktree--open-noselect (committish path object)
  "Open buffer to view git object of COMMITTISH:PATH.
When PATH was given and non-nil open that, otherwise open root tree.
When OBJECT was given and non-nil, assume that is the object full sha1  of
COMMITTISH:PATH without checking it."
  ;; TODO: Resolve symbolic-ref commitish here?
  (cl-assert committish)
  (let ((type (git-walktree--git-plumbing "cat-file"
                                          "-t"
                                          committish)))
    (cl-assert (string= type "commit")))

  (setq path (or path
                 "."))
  ;; PATH must not start with and end with slashes
  (cl-assert (not (string-match-p "\\`/" path)))
  (cl-assert (not (string-match-p "/\\'" path)))

  (setq object (or object
                   (git-walktree--resolve-object committish path)))
  (setq object (git-walktree--git-plumbing "rev-parse"
                                           object))
  (cl-assert object)

  (let ((type (git-walktree--git-plumbing "cat-file"
                                          "-t"
                                          object)))
    (pcase type
      ((or "commit" "tree")
       (git-walktree--open-treeish committish path object))
      ("blob"
       (git-walktree--open-blob committish path object))
      (_
       (error "Type cannot handle: %s" type)))))


;;;###autoload
(defun git-walktree-open (committish &optional path)
  "Open git tree buffer of COMMITTISH.
When PATH was given and non-nil open that, otherwise try to open current path.
If target path is not found in COMMITISH tree, go up path and try again until found."
  (interactive (list (git-walktree-read-branch-or-commit "Revision: ")))
  (switch-to-buffer (git-walktree--open-noselect-safe-path committish path)))
;;;###autoload
(defalias 'git-walktree 'git-walktree-open)

(defcustom git-walktree-try-cd t
  "Try to cd if directory exists in current working directory if non-nil.
Otherwise use repository root for gitwalktree buffer's `default-directory'."
  :type 'boolean
  :group 'git-walktree)

(defcustom git-walktree-reuse-tree-buffer t
  "Non-nil to reuse buffer for treeish object."
  :type 'boolean
  :group 'git-walktree)

(defvar git-walktree-tree-buffer-for-reuse nil
  "Buffer to use when `git-walktree-reuse-tree-buffer' is non-nil.")


(defun git-walktree-up (&optional committish path)
  "Open parent directory of COMMITTISH and PATH.
If not given, value of current buffer will be used."
  (interactive)
  (setq committish
        (or committish git-walktree-current-committish))
  (setq path
        (or path git-walktree-current-path))
  (let ((parent (git-walktree--parent-directory path)))
    (if parent
        (switch-to-buffer (git-walktree--open-noselect committish
                                                       parent
                                                       nil))
      (message "Cannot find parent directory for current tree."))))


(defun git-walktree-parent-revision ()
  "Open parent revision of current path.
If current path was not found in the parent revision try to go up path."
  (interactive)
  (cl-assert git-walktree-current-committish)
  (let* ((commit-full-sha1 (git-walktree--git-plumbing "rev-parse"
                                                       git-walktree-current-committish))
         (parents (git-walktree--parent-full-sha1 commit-full-sha1)))
    (dolist (parent parents)
      (git-walktree--put-child parent
                               commit-full-sha1))
    (if (< (length parents)
           1)
        (message "This revision has no parent revision")
      (let* ((parent (git-walktree--choose-committish "This revision has multiple parents. Which to open? (%s) "
                                                      parents))
             (path git-walktree-current-path))
        (cl-assert path)
        (switch-to-buffer (git-walktree--open-noselect-safe-path parent
                                                                 path))))))


(defun git-walktree-known-child-revision ()
  "Open known revision of current path."
  (interactive)
  (let* ((commit-full-sha1 (git-walktree--git-plumbing "rev-parse"
                                                       git-walktree-current-committish))
         (children (git-walktree--get-children commit-full-sha1)))
    (if (< (length children)
           1)
        (message "There are no known child revision")
      (let* ((child (git-walktree--choose-committish "There are multiple known childrens. Which to open? (%s)"
                                                     children))
             (path git-walktree-current-path))
        (cl-assert path)
        (switch-to-buffer (git-walktree--open-noselect-safe-path child
                                                                 path))))))



(provide 'git-walktree)

;;; git-walktree.el ends here
