git-walktree.el
===============

Walk through Git tree and blob objects


What is this?
-------------

`git-walktree` is a Git `ls-tree` browser.
It has following features:

- Browse tree objects of arbitrary commit without checking out
  - Go subtrees
  - Visit blob objects
  - Go Parent and child commits of commit currently browsing
- Checkout (output) tree or blob object to current working directory

Usage and Keybinds
------------------

**<kbd>M-x git-walktree</kbd>** to open git-walktree buffer.
Prompt user for commit to show.

- When current buffer is visiting a file and it exists in the commit,
  open blob buffer for that file
- Otherwise, open tree object buffer for `default-directory`


In git-walktree buffers, following keybinds can be used:

- <kbd>^</kbd> Visit parent tree object
- <kbd>P</kbd> Go parent commit
- <kbd>N</kbd> Go child commit
- <kbd>C</kbd> Checkout current blob or tree object to working directory

Additionally, in tree object buffer:

- <kbd>ENTER</kbd> Visit tree or blob object at point


License
-------


This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

See `LICENSE` for details.
