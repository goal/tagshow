
(require 'grizzl)
(require 'dash)

(defvar wscope-output-buffer-name "*Result*"
  "The name of the cscope output buffer.")

(defun wscope-init (dir)
  (interactive "DCscope Initial Directory: ")
  (if (get-process "wscope") (kill-process (get-process "wscope")))
  (if (get-buffer "*wscope*") (kill-buffer (get-buffer "*wscope*")))
  (setq default-directory dir)
  (start-process "wscope" "*wscope*" "cscope" "-ld" "-f" "cscope.out")
  (set-process-filter (get-process "wscope") 'wscope-filter)
  (with-current-buffer "*wscope*"
    (accept-process-output (get-process "wscope") 3)
    (if (looking-at ".*cannot open.*cscope\.out.*")
		(progn
		  (setq buf (get-buffer "*wscope*"))
		  (if buf
			  (kill-buffer buf))
		  (message "wscope: no cscope.out file here"))
      (progn
		(wscope-wait-for-output)
		(message "wscope: load ok"))
      ))
  )

(defun wscope-filter (process string)
  ;; Write the output into the Tramp Process
  (with-current-buffer (process-buffer process)
	(save-excursion
	  (goto-char (point-max))
	  (insert string)
	  ))
  )

(defun wscope-find-global-definition ()
  "Find current symbol's global definition."
  (interactive)
  (setq symbol (current-word))
  (if symbol
	  (progn
		(setq query-command (concat "1" symbol "\n"))
		(setq wscope-action-message (format "Finding global definition: %s" symbol))
		(wscope-query query-command))
	(message "What to find?"))
  )

(defun wscope-query (command)
  (let ((proc (get-process "wscope")) outbuf
		)
	(with-current-buffer (process-buffer proc)

	  (goto-char (point-max))
	  (insert command)

	  (process-send-string "wscope" command)

	  (wscope-wait-for-output )

	  (wscope-process-output)
	  )

	(setq outbuf (get-buffer-create wscope-output-buffer-name))
	(with-current-buffer outbuf
	  (progn
		(let* ((test-tags '("hello" "world" "goodbye" "welcome"))
			   (tagshow-index (grizzl-make-index test-tags))
			   (select-tag (minibuffer-with-setup-hook
							   (lambda () ())
							 (grizzl-completing-read "Show text: TODO" tagshow-index))))
		  (message select-tag))
;;		(pop-to-buffer outbuf)
;;		(shrink-window 5)
;;		(insert wscope-separator-line "\n")
;;		(insert "Search complete.")
;;		(if wscope-first-match
;;			(set-window-point (get-buffer-window outbuf) wscope-first-match-point)
;;		  (insert "\nNothing found!"))
;;		(wscope-list-entry-mode)
		)
	  ))
  )

(defun wscope-make-entry-line (func-name line-number line)
  nil
  )

(defun wscope-process_one_chunk (text-start text-end)
  (with-current-buffer "*wscope*"
	(setq stuff (buffer-substring-no-properties text-start text-end))
	(while (and stuff
				(string-match "\\([^\n]+\n\\)\\(\\(.\\|\n\\)*\\)" stuff))
	  (setq line (substring stuff
							(match-beginning 1) (match-end 1)))

	  (setq stuff (substring stuff
							 (match-beginning 2)
							 (match-end 2)))
	  (if (= (length stuff) 0)
		  (setq stuff nil))

	  (if (string-match
		   "\\([^[:blank:]]*\\)[[:blank:]]+\\([^[:blank:]]*\\)[[:blank:]]+\\([[:digit:]]*\\)[[:blank:]]+\\(.*\\)"
		   line)
		  (progn
			(let (str)
			  (setq file (substring line (match-beginning 1)
									(match-end 1))
					function-name (substring line (match-beginning 2)
											 (match-end 2))
					line-number (substring line
										   (match-beginning 3)
										   (match-end 3))
					line (substring line (match-beginning 4)
									(match-end 4))
					)

			  (wscope-insert-text-with-properites
			   (wscope-make-entry-line function-name
									   line-number
									   line)
			   (expand-file-name file)
			   line-number)
			  ))))
	)
  )

(defun wscope-insert-text-with-properites (text filename &optional line-number)
  nil
  )

(defun wscope-process-output ()
  (setq wscope-first-match nil
		wscope-last-file nil)
  (if (get-buffer wscope-output-buffer-name)
	  (kill-buffer wscope-output-buffer-name)
	)
  (let (text-start text-end text-max)
	(with-current-buffer "*wscope*"
	  (setq text-start (point))
	  (setq text-max (point-max))
	  (if (>= (- text-max text-start) 5000)
		  (setq text-end (+ text-start 5000))
		(setq text-end text-max))
	  )
	(while (and (> (- text-end text-start) 0) (<= text-end text-max))

	  (wscope-process_one_chunk text-start text-end)

	  (setq text-start (+ text-end 1))
	  (if (>= (- text-max text-start) 5000)
		  (setq text-end (+ text-start 5000))
		(setq text-end text-max))))
  )

(defun wscope-wait-for-output (&optional timeout)
  (let ((proc (get-buffer-process (current-buffer)))
		(found nil)
		(start-time (current-time))
		(start-point (point)))
	(save-excursion
	  (while (not found)
		(accept-process-output proc 1)
		(goto-char (point-max)) ;move the last line
		(beginning-of-line) ;move the beginning of last line
		(setq found (looking-at "^>>"))) ;looking for cscope prompt "^>>"
	  )
	)
  )

(provide 'wscope)

;;; wscope.el ends here
