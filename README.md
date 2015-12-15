# go-back
An emacs package for revisiting previous positions, undo-style.

## How To
Download go-back.el (or clone the git repo). Then add its directory to the load path if necessary, by putting the following line in your .emacs:

    (add-to-list 'load-path "/path/to/directory/")

Then add the following lines:

	(require 'go-back)
	(global-set-key (kbd "M-r") 'go-back/pop-point-stack)

or using keybinding you'd like. Then you just use go-back/pop-point-stack repeatedly to jump back through previous positions.

If you do a sequence of pops, then interrupt it (e.g. with C-g), then start popping again, you'll be revisiting the revisits, like emacs' undo.


## To Do
- Wrap everything in a minor mode.

