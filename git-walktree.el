;;; git-walktree.el --- Walk through git revisions   -*- lexical-binding: t; -*-

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

;; Walk through git revisions.


;;; Code:

;; TODO: Swtch to GPLv3

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

(defun git-walktree--committish-fordisplay (committish)
  "Convert COMMITTISH and return is a suitable format for displaying."
  (if (and committish
           (string-match-p "\\`[0-9a-f]+\\'"
                           committish)
           (>= (length committish) 32))
      (git-walktree--git-plumbing "rev-parse"
                                  "--short"
                                  committish)
    committish))

(defun git-walktree--create-buffer (committish name type)
  "Create and return buffer for COMMITTISH:NAME.
TYPE is target object type."
  (let* ((root (git-walktree--git-plumbing "rev-parse"
                                           "--show-toplevel"))
         (committish-display (git-walktree--committish-fordisplay committish))
         (name (format "*GitWalkTree<%s:%s>*"
                       (or committish-display "")
                       name)))

    (if (and git-walktree-reuse-tree-buffer
             (string= type "tree"))
        (with-current-buffer (or git-walktree-tree-buffer-for-reuse
                                 (setq git-walktree-tree-buffer-for-reuse
                                       (generate-new-buffer "gitwalktreebuf")))
          (setq git-walktree-repository-root root)
          (rename-buffer name t)
          (current-buffer))
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

(defun git-walktree--resolve-object (committish path)
  "Return object full sha1 name of COMMITISIH:PATH.
If path is equal to \".\" return COMMITTISH's root tree object.
PATH will be always treated as relative to repository root."
  (cl-assert committish)
  (cl-assert path)
  (cl-assert (not (string-match-p "\\`/" path)))
  (cl-assert (not (string-match-p "/\\'" path)))
  (if (string= path ".")
      (git-walktree--git-plumbing "show"
                                  "--no-patch"
                                  "--pretty=format:%T"
                                  committish)
    (let ((info (git-walktree--parse-lstree-line (git-walktree--git-plumbing "ls-tree"
                                                                             "--full-tree"
                                                                             committish
                                                                             path))))
      (plist-get info :object))))

;;;###autoload
(defun git-walktree-open (committish &optional path)
  "Open git tree buffer of COMMITTISH.
When PATH was given and non-nil open that, otherwise try to open current path.
If target path is not found in COMMITISH tree, go up path and try again until found."
  (interactive (list (git-walktree-read-branch-or-commit "Revision: ")))
  (switch-to-buffer (git-walktree--open-noselect-safe-path committish path)))
;;;###autoload
(defalias 'git-walktree 'git-walktree-open)

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

(defcustom git-walktree-git-executable "git"
  "Git executable."
  :type 'string
  :group 'git-walktree)

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
  (let (result mode type object file)
    (save-match-data
      (with-temp-buffer
        (insert str)
        (goto-char (point-min))
        (and (re-search-forward git-walktree-ls-tree-line-regexp
                                nil
                                t)
             (list :mode (match-string 1)
                   :type (match-string 2)
                   :object (match-string 3)
                   :file (match-string 4)))))))

(defun git-walktree--join-path (name &optional base)
  "Make path from NAME and BASE.
  If base is omitted or nil use value of `git-walktree-current-path'."
  (setq base (or base
                 git-walktree-current-path))
  (cl-assert base)
  (if (string= base ".")
      name
    (concat base "/" name)))

(defun git-walktree--parent-directory (path)
  "Return parent directory of PATH without trailing slash.
  For root directory return \".\".
  If PATH is equal to \".\", return nil."
  (if (string-match-p "/" path)
      (directory-file-name (file-name-directory path))
    (if (string= "." path)
        nil
      ".")))

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

(defun git-walktree--choose-committish (prompt-format collection)
  "Emit PROMPT-FORMAT and ask user to which committish of COLLECTION to use.
  When collection has just one element, return the first element without asking."
  (cl-assert collection)
  (if (< (length collection) 2)
      (car collection)
    (completing-read (format prompt-format
                             (mapconcat 'git-walktree--committish-fordisplay
                                        collection
                                        " "))
                     collection
                     nil
                     t)))

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

(defun git-walktree--parent-full-sha1 (committish)
  "Return list of parent commits of COMMITTISH in sha1 string."
  (let ((type (git-walktree--git-plumbing "cat-file"
                                          "-t"
                                          committish)))
    (cl-assert (string= type "commit")))
  (let ((parents (git-walktree--git-plumbing "show"
                                             "--no-patch"
                                             "--pretty=format:%P"
                                             committish)))
    (split-string parents)))

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

;; (git-revision--git-plumbing "cat-file" "-t" "HEAD")


(provide 'git-walktree)

;;; git-walktree.el ends here
