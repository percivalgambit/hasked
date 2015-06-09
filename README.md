# hasked
A text editor written in Haskell

To run the text editor, type `make run` or `make run FILE=filename` to edit a file.
If filename does not exist, it will be created.  Currently, specifying a file to
save to in the editor is not supported, so it must be specified on the command
line during invocation. When run, the makefile will set up a cabal sandbox
with the correct packages.  `make test` will run all of the tests.

In the text editor, the arrow keys can move around, and typing will insert text
into the buffer.  the delete key will delete text, the enter key will enter a
newline, ctrl + s will save to the current file if one was specified on the
command line, and the escape key or ctrl + d will exit from the text editor and
save if possible.

GitHub repo: https://github.com/percivalgambit/hasked
