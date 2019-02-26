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

(if (require 'magit nil t)
    (fset 'git-walktree-read-branch-or-commit
          'magit-read-branch-or-commit)
  ;; TODO: Add fallback function
  (error "magit not found"))


;;; git-walktree-read.el ends here
