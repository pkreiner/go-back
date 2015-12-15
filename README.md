# go-back
An emacs package for revisiting previous positions, undo-style.

## How to use
Download go-back.el (or clone the git repo). Then add its directory to the load path if necessary, by putting the following line in your .emacs:

    (add-to-list 'load-path "/path/to/directory/")

Then add the following lines:

	(require 'go-back)
	(global-set-key (kbd "M-r") 'go-back/pop-point-stack)


## To Do
- Wrap everything in a minor mode.

