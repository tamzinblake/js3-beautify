;;; js3-beautify-pretty.el --- JavaScript pretty printer

;;; Code:

(defvar temp nil)

(defun js3-beautify-ugly-print ()
  "Try to dump the ast."
  (interactive)
  (insert (js3-beautify-print-line (js3-beautify-parse))))

(defun js3-beautify-pretty-print ()
  "Pretty print the Javascript code in the ast tree."
  (interactive)
  (let ((ast (js3-beautify-parse)))
    (js3-beautify-pretty-print-line ast nil)))

(defun js3-beautify-pretty-print-line (node end-p)
  "Pretty print a node, using as few lines as possible."
  (if node
      (if (or end-p (< (length (js3-beautify-print-line node)) js3-beautify-max-columns))
	  (progn
	    (insert (js3-beautify-print-line node))
	    nil)
	(js3-beautify-visit-ast node #'js3-beautify-pretty-print-line))
    nil))

(defun js3-beautify-print-line (node)
  "Try to print a node all on one line, returning as a string."
  (js3-beautify-print-ast node)
  (setq temp js3-beautify-curstr)
  (setq js3-beautify-curstr "")
  temp)

;;; js3-beautify-pretty.el ends here
