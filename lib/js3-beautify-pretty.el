;;; js3-beautify-pretty.el --- JavaScript pretty printer

;;; Code:

(defun js3-beautify-pretty-print ()
  "Pretty print the Javascript code in the ast tree."
  (interactive)
  (let ((ast (js3-beautify-parse)))
    (js3-beautify-pretty-print-line ast)))

(defun js3-beautify-pretty-print-line (node)
  "Pretty print a node, using as few lines as possible."
  (if node
      (let ((line (js3-beautify-print-line node)))
	(if (> (length line) js3-beautify-max-columns)
	    (progn
	      (insert line)
	      nil)
	  (js3-beautify-visit-ast (node #'js3-beautify-pretty-print-line))))
    nil))

(defun js3-beautify-print-line (node)
  "Try to print a node all on one line, returning as a string."
  (let (rv)
    (js3-beautify-print-ast node)
    (setq rv js3-beautify-curstr)
    (setq js3-beautify-curstr "")
    rv))

;;; js3-beautify-pretty.el ends here
