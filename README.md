# go-back
An emacs package for revisiting previous positions, undo-style.

## How To
Download go-back.el (or clone the git repo). Then add its directory to the load path if necessary, by putting the following line in your .emacs:

    (add-to-list 'load-path "/path/to/directory/")

Then add the following lines:

	(require 'go-back)
	(global-set-key (kbd "M-r") 'go-back/pop-point-stack)

or using any keybinding you'd like. Then you just use go-back/pop-point-stack repeatedly to jump back through previous positions.

If you do a sequence of pops, then interrupt it (e.g. with C-g), then start popping again, you'll be revisiting the revisits, like emacs' undo.


## Usage
My main use case is, when coding, to jump to a definition somewhere else in the file, and then jump back using this package's 'go-back/pop-point-stack.

In a big file, you might also enjoy winding through the recent history of where you've been reading and editing.


## Possible Improvements
- Make this a minor mode, so it's easier to disable if it goes haywire.
- Add a global history stack as well (currently each stack is buffer-local).
- Make a way to jump back to only commands that resulted in an edit.


## To Do


## Issues
- Fails when going back too far (600 steps on my machine). This is because one of the lookup functions (pop-nonzero-kth) is recursive and hits the recursion limit in elisp (which doesn't have tail-call recursion). Can fix by modifying the lookup slightly.
