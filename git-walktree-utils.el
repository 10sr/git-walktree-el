;;; git-walktree-utils.el --- Utilities for git-walktree   -*- lexical-binding: t; -*-

;; Author: 10sr <8.slashes [at] gmail [dot] com>
;; URL: https://github.com/10sr/git-walktree-el
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

;; Utilitiies for git-walktree.


;;; Code:

;; This file includes definitions of Functions that:
;;   - do not depends on git-walktree functions and variables defined outside of this file
;;   - do not create buffers
;;   - do not modify states of existing buffers
;;   - do not modify variables that can be used outside of this file
;; This file also has some variables.

(defcustom git-walktree-git-executable "git"
  "Git executable."
  :type 'string
  :group 'git-walktree)

(defun git-walktree--git-plumbing (&rest args)
  "Run git plubming command with ARGS.
Returns first line of output without newline."
  (with-temp-buffer
    (let ((status (apply 'call-process
                         git-walktree-git-executable
                         nil
                         t
                         nil
                         args)))
      (unless (eq 0
                  status)
        (error "Faild to run git %S:\n%s"
               args
               (buffer-substring-no-properties (point-min)
                                               (point-max))))
      (buffer-substring-no-properties (point-min)
                                      (progn
                                        (goto-char (point-min))
                                        (point-at-eol))))))
;; (git-revision--git-plumbing "cat-file" "-t" "HEAD")


(defun git-walktree--commitish-fordisplay (commitish)
  "Convert COMMITISH and return is a suitable format for displaying."
  (if (and commitish
           (string-match-p "\\`[0-9a-f]+\\'"
                           commitish)
           (>= (length commitish) 32))
      (git-walktree--git-plumbing "rev-parse"
                                  "--short"
                                  commitish)
    commitish))

(defun git-walktree--resolve-object (commitish path)
  "Return object full sha1 name of COMMITISIH:PATH.
If path is equal to \".\" return COMMITISH's root tree object.
PATH will be always treated as relative to repository root."
  (cl-assert commitish)
  (cl-assert path)
  (cl-assert (not (string-match-p "\\`/" path)))
  (cl-assert (not (string-match-p "/\\'" path)))
  (if (string= path ".")
      (git-walktree--git-plumbing "show"
                                  "--no-patch"
                                  "--pretty=format:%T"
                                  commitish)
    (let ((info (git-walktree--parse-lstree-line (git-walktree--git-plumbing "ls-tree"
                                                                             "--full-tree"
                                                                             commitish
                                                                             path))))
      (plist-get info :object))))


(defun git-walktree--parent-directory (path)
  "Return parent directory of PATH without trailing slash.
For root directory return \".\".
If PATH is equal to \".\", return nil."
  (if (string-match-p "/" path)
      (directory-file-name (file-name-directory path))
    (if (string= "." path)
        nil
      ".")))

(defvar git-walktree-known-child-revisions (make-hash-table :test 'equal)
  "Hash of already known pair of commitid -> list of child commitid.
Both values should be object full sha1 names.")

(defun git-walktree--put-child (parent child)
  "Register PARENT and CHILD relationship.
PARENT should be a full sha1 object name."
  ;; Any way to check if PARENT is a full SHA-1 object name?
  (let ((current (gethash parent git-walktree-known-child-revisions)))
    (unless (member child current)
      (puthash parent
               (cons child
                     current)
               git-walktree-known-child-revisions))))

;; TODO: Add aggressive search mode
;; https://stackoverflow.com/a/9870218
;; git log --reverse --pretty=format:%H -n 1 --ancestry-path <PARENT>..HEAD
(defun git-walktree--get-children (parent)
  "Get known children list of PARENT commit.
PARENT should be a full sha1 object name."
  (gethash parent git-walktree-known-child-revisions))

(defun git-walktree--choose-commitish (prompt-format collection)
  "Emit PROMPT-FORMAT and ask user to which commitish of COLLECTION to use.
When collection has just one element, return the first element without asking."
  (cl-assert collection)
  (if (< (length collection) 2)
      (car collection)
    (completing-read (format prompt-format
                             (mapconcat 'git-walktree--commitish-fordisplay
                                        collection
                                        " "))
                     collection
                     nil
                     t)))

(defun git-walktree--join-path (name base)
  "Make path from NAME and BASE."
  (cl-assert base)
  (if (string= base ".")
      name
    (concat base "/" name)))

(defun git-walktree--path-in-repository (path)
  "Convert PATH into relative path to repository root.
Result will not have leading and trailing slashes."
  (with-temp-buffer
    (cd (if (file-directory-p path)
            path
          (file-name-directory path)))
    (let ((root (git-walktree--git-plumbing "rev-parse"
                                            "--show-toplevel")))
      (file-relative-name (directory-file-name path)
                          root))))

(defun git-walktree--parent-full-sha1 (commitish)
  "Return list of parent commits of COMMITISH in sha1 string."
  (let ((type (git-walktree--git-plumbing "cat-file"
                                          "-t"
                                          commitish)))
    (cl-assert (string= type "commit")))
  (let ((parents (git-walktree--git-plumbing "show"
                                             "--no-patch"
                                             "--pretty=format:%P"
                                             commitish)))
    (split-string parents)))

(defconst git-walktree-ls-tree-line-regexp
  "^\\([0-9]\\{6\\}\\) \\(\\w+\\) \\([0-9a-f]+\\)\t\\(.*\\)$"
  "Regexp for one line of output of git ls-tree.")
(defconst git-walktree-ls-tree-line-tree-regexp
  "^\\([0-9]\\{6\\}\\) \\(tree\\) \\([0-9a-f]+\\)\t\\(.*\\)$"
  "Regexp for tree line of output of git ls-tree.")
(defconst git-walktree-ls-tree-line-commit-regexp
  "^\\([0-9]\\{6\\}\\) \\(commit\\) \\([0-9a-f]+\\)\t\\(.*\\)$"
  "Regexp for commit line of output of git ls-tree.")
(defconst git-walktree-ls-tree-line-symlink-regexp
  "^\\(120[0-9]\\{3\\}\\) \\(blob\\) \\([0-9a-f]+\\)\t\\(.*\\)$"
  "Regexp for symlink line of output of git ls-tree.")

(defun git-walktree--parse-lstree-line (str)
  "Extract object info from STR.

STR should be a string like following without newline.:

100644 blob 6fd4d58202d0b46547c6fe43de0f8c878456f966	.editorconfig

Returns property list like (:mode MODE :type TYPE :object OBJECT :file FILE)."
  (save-match-data
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (when (re-search-forward git-walktree-ls-tree-line-regexp
                               nil
                               t)
        (list :mode (match-string 1)
              :type (match-string 2)
              :object (match-string 3)
              :file (match-string 4))))))

(provide 'git-walktree-utils)

;;; git-walktree-utils ends here
