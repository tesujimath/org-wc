;; org-wc.el
;;
;; Count words in org mode trees.
;; Shows word count per heading line, summed over sub-headings.
;; Aims to be fast, so doesn't check carefully what it's counting.  ;-)
;;
;; Simon Guest, 23/4/11
;;
;; Eric Abrahamsen, 8/6/11
;;
;; Implementation based on:
;; - Paul Sexton's word count posted on org-mode mailing list 21/2/11.
;; - clock overlays
;;
;; v2
;; 29/4/11
;; Don't modify buffer, and fixed handling of empty sections.
;;
;; v3
;; 29/4/11
;; Handle narrowing correctly, so partial word count works on narrowed regions.
;;
;; v4
;; 8/6/11
;; Use mapping and property APIs, no more overlays

(setq org-wc-include-tag "count"
      org-wc-prop-name "WORDCOUNT")

(make-variable-buffer-local 'org-wc-include-tag)
(make-variable-buffer-local 'org-wc-prop-name)

(defun org-word-count (beg end)
  (interactive "r")
  (let ((bmp (buffer-modified-p))
	(tag (if (save-excursion
		   (goto-char (point-min))
		   (re-search-forward (concat "^\\*+[ \t].*" ":\\("
					      org-wc-include-tag
					      "\\):" "[^ \t\n]*[ \t]*$")
				      nil t))
		 org-wc-include-tag
	       t)))
    (save-restriction
      (if (org-region-active-p)
	  (narrow-to-region beg end))
      (let* ((funky (if current-prefix-arg
		      'org-wc-count-report
		    'org-wc-count-add-props))
	     (words (apply '+ (org-map-entries funky tag nil))))
       (set-buffer-modified-p bmp)
       (message (format "Counted %d words in %s." words
			(if mark-active "region" "buffer")))))))

(defun org-wc-count-add-props ()
  (org-wc-aux t))

(defun org-wc-count-report ()
  (org-wc-aux nil))

(defun org-wc-aux (props)
  "Report the number of words in the selected region.
Ignores: heading lines,
         blocks,
         comments,
         drawers.
LaTeX macros are counted as 1 word."

  (let ((wc 0)
        (block-begin-re "^#\\\+BEGIN")
        (block-end-re "^#\\+END")
	(latex-macro-regexp "\\\\[A-Za-z]+\\(\\[[^]]*\\]\\|\\){\\([^}]*\\)}"))
    (let ((end (save-excursion
		 (or (outline-next-heading)
		     (point-max)))))
      (save-excursion
	(org-end-of-meta-data-and-drawers)
	(while (< (point) end)
	  (cond
	   ((looking-at block-begin-re)
	    (re-search-forward block-end-re))
	   ;; Ignore comments.
	   ((org-in-commented-line)
	    (forward-line))
	   ;; skip tables
	   ((org-at-table-p)
	    (forward-line))
	   ;; Count latex macros as 1 word, ignoring their arguments.
	   ((save-excursion
	      (backward-char)
	      (looking-at latex-macro-regexp))
	    (goto-char (match-end 0))
	    (setf wc (+ 2 wc)))
	   (t
	    (progn
	      (and (re-search-forward "\\w+\\W*" end 'skip)
		   (incf wc))))))))
    (when props
      (org-entry-put (point) org-wc-prop-name (number-to-string wc)))
    wc))

(defun org-wc-remove-prop ()
  "Delete an existing wordcount property"
  (org-entry-delete (point) org-wc-prop-name))

(defun org-wc-remove-all-props ()
  "Delete wordcount property from all headlines in buffer"
  (interactive)
  (org-map-entries 'org-wc-remove-prop t 'file))

(provide 'org-wc)
