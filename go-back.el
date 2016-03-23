;; A little package that keeps track of all the positions you've
;; visited.

;; Step back through positions with go-back/pop-point-stack, which I have
;; bound to M-r. This works like emacs' undo, so if you start a chain
;; of pops, then interrupt it (e.g. with C-g), and then start popping again, you'll be
;; going back toward the position you started in.


(provide 'go-back)


;; Internal variables

(defvar-local go-back/point-stack nil
  "Internal variable. Stack of markers of point positions. Buffer
  local.")

(setq go-back/point-stack nil)

(defvar-local go-back/point-stack-index 0
  "Internal variable. Index from which we are popping positions
off the point stack. If the n most recent commands were all
go-back/pop-point-stack, this variable will have value n.")

(setq go-back/point-stack-index 0)


;; Customizable variables

(defvar go-back/only-track-motions t
  "Controls whether positions will be stored even if the previous
command didn't result in any movement. Feel free to customize.")
(setq go-back/only-track-motions t)

(defvar go-back/ignored-commands nil
  "List of commands whose use should not push a new position to
  the stack, the idea being that maybe very small motions are not
  worth revisiting. Feel free to customize.")
(setq go-back/ignored-commands
      '(
	forward-char
	backward-char
	forward-word
	backward-word
	self-insert-command
	))


;; Internal functions

(defun go-back/clear-point-stack ()
  "Just for debugging."
  (setq go-back/point-stack nil))

(defun go-back/manage-point-stack ()
  "Pushes the current position to the point stack. Also manages
an index variable that keeps track of how far down we are in a
series of point-stack pops."
  (setq go-back/point-stack
	(go-back/push-to-point-stack go-back/point-stack this-command))
  (if (eq this-command 'go-back/pop-point-stack)
      (setq go-back/point-stack-index (+ 1 go-back/point-stack-index))
    (setq go-back/point-stack-index 0)))
	

(defun go-back/push-to-point-stack (stack command)
  "Push the new position to the stack and return the result. If the
last command was one of the go-back/ignored-commands, or the position
didn't move and go-back/only-track-motions is turned on, then return
the stack unchanged."
  ;; If the stack is null, initialize it.
  (if (null stack) (list (point-marker))
    (if (or (member command go-back/ignored-commands)
	    (and go-back/only-track-motions
		 (equal (car stack) (point-marker))))
	stack
      (cons (point-marker) stack))))


(defun go-back/pop-point-stack ()
  "Pop off the most recent position in go-back/point-stack."
  (interactive)
  (let ((marker
	 (pop-kth go-back/point-stack-index go-back/point-stack)))
    (unless (null marker)
      (goto-char (marker-position marker)))))


;; Hook to be executed before every emacs command

(add-hook 'pre-command-hook 'go-back/manage-point-stack)
;; (remove-hook 'pre-command-hook 'go-back/manage-point-stack)

;; This is for debugging, by displaying the contents of the stack in a
;; buffer that's updated after every command.
;; (add-hook 'post-command-hook 'print-stack)
;; (remove-hook 'post-command-hook 'print-stack)

;; Helper functions

(defmacro pop-kth (k x)
  "Remove and return the k'th element of a (zero-indexed) linked
list. When k >= (length x), or k < 0, does nothing and returns
nil."
  `(cond ((null ,x) nil)
	 ((eq ,k 0)
	  (let ((elt (car ,x)))
	    (progn (setq ,x (cdr ,x)) elt)))
	 (t
	  (pop-nonzero-kth ,k ,x))))

(defun pop-nonzero-kth (k x)
  "Helper function for pop-kth."
  (cond ((null x) nil)
	((eq k 1)
	 (let ((elt (cadr x)))
	   (progn (setcdr x (cddr x)) elt)))
	(t
	 (pop-nonzero-kth (- k 1) (cdr x)))))
