git-walktree-el
===============

Walk through git tree and blob objects


What is this?
-------------

`git-walktree` is a git repository browser.
It has following features:

- Browse tree object of git commit
  - Go subtree
  - Visit blob object
  - Go Parent and child commits of current commit
- Checkout tree and blob objects to current working directory

Entrypoint
----------

**`M-x git-walktree` ** Open git-walktree tree browser or blob viewer
of commit specified by user.

In buffers, following keybinds can be used:

- **^** Visit parent tree object of current path of current commit
- **P** Go tree object of current path of parent commit
- **N** Go tree object of current path of child commit
- **C** Checkout current blob or tree object

Additionally, in tree object buffer:

- **ENTER** Visit tree or blob object at point


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
