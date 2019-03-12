;;; git-walktree-read.el --- Read commitish from minibuffer   -*- lexical-binding: t; -*-

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

;; Read commitish from minibuffer.


;;; Code:

(require 'git)

(defvar git-walktree--read-history nil
  "History for `git-walktree--read'.")

(defun git-walktree--read (prompt)
  "Read branch, tag or commit with PROMPT.
This function is a fallback used when `magit-read-branch-or-commit' is
 not defined."
  (make-local-variable 'git-repo)
  (setq git-repo default-directory)
  (or (completing-read prompt  ; PROMPT
                       (nconc (git-branches) (git-tags))  ; COLLECTION
                       nil  ; PREDICATE
                       nil  ; REQUIRE-MATCH
                       (or (thing-at-point 'symbol t)  ; INITIAL-INPUT
                           (git-on-branch))
                       'git-walktree--read-history  ; HISTORY
                       )
      (user-error "Nothing selected")))

(if (and (require 'magit nil t)
         (require 'magit-git nil t))
    (fset 'git-walktree-read-branch-or-commit
          'magit-read-branch-or-commit)
  (fset 'git-walktree-read-branch-or-commit
        'git-walktree--read)
  )


(provide 'git-walktree-read)

;;; git-walktree-read.el ends here
