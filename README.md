git-walktree.el
===============

Browse Git tree and blob objects


What is This?
-------------

`git-walktree` is a Git `ls-tree` browser.
It has following features:

- Browse tree objects of arbitrary revisions without checking them out
  - Go subtrees
  - Visit blob objects
  - Go Parent and child revisions
- Checkout (output) tree or blob object to current working directory

Usage and Keybinds
------------------

**<kbd>M-x git-walktree</kbd>** to open git-walktree buffer.
Prompt user for revision to show.

- When current buffer is visiting a file and it exists in the revision,
  open blob buffer for that file
- Otherwise, open tree object buffer for `default-directory`


In git-walktree buffers, following keybinds can be used:

- <kbd>^</kbd> Visit parent tree object
- <kbd>P</kbd> Go parent revision
- <kbd>N</kbd> Go child revision
- <kbd>C</kbd> Checkout current blob or tree object to working directory
- <kbd>G</kbd> Go another revision

Additionally, in tree object buffer:

- <kbd>ENTER</kbd> Visit tree or blob object at point


License
-------

This software is released under GPL version 3 or (at your option)
any later version.

See `LICENSE` for details.
