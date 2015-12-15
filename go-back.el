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

(defvar go-back/point-stack-track-non-movement nil
  "Controls whether positions will be stored even if the previous
command didn't result in any movement. Feel free to customize.")
(setq go-back/point-stack-track-non-movement nil)

(defvar go-back/point-stack-ignored-functions nil
  "List of functions whose use should not push a new position to
  the stack, the idea being that maybe very small motions are not
  worth revisiting. Feel free to customize.")
(setq go-back/point-stack-ignored-functions
      '(
	forward-char
	backward-char
	forward-word
	backward-word
      ))


;; Internal functions

(defun go-back/clear-point-stack ()
  "Just for debugging."
  (setq go-back/point-stack nil))

(defun go-back/manage-point-stack ()
  "Pushes the current position to the point stack. Also manages
an index variable that keeps track of how far down we are in a
series of point-stack pops."
  (go-back/push-to-point-stack)
  (if (eq this-command 'go-back/pop-point-stack)
      (setq go-back/point-stack-index (+ 1 go-back/point-stack-index))
    (setq go-back/point-stack-index 0)))
	

(defun go-back/push-to-point-stack ()
  "If the point has moved, and the command wasn't in the list
go-back/point-stack-ignored-functions, then push the new position to
the stack."
  ;; Initialize an empty stack
  (if (null go-back/point-stack) (setq go-back/point-stack (list (point-marker)))
    (unless (or (member this-command go-back/point-stack-ignored-functions)
		(and go-back/point-stack-track-non-movement
		     (equal (car go-back/point-stack) (point-marker))))
      (setq go-back/point-stack (cons (point-marker) go-back/point-stack)))))

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
