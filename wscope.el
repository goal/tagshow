
(require 'grizzl)
(require 'dash)

(defgroup wscope nil
  "Cscope interface for (X)Emacs.
Using cscope, you can easily search for where symbols are used and defined.
It is designed to answer questions like:

Where is this variable used?
What is the value of this preprocessor symbol?
Where is this function in the source files?
What functions call this function?
What functions are called by this function?
Where does the message \"out of space\" come from?
Where is this source file in the directory structure?
What files include this header file?
"
  :prefix "wscope-"
  :group 'tools)

(defcustom wscope-name-line-width -30
  "*The width of the combined \"function name:line number\" field in the
cscope results buffer. If negative, the field is left-justified."
  :type 'integer
  :group 'wscope)

(defcustom wscope-use-face nil
  "*Whether to use text highlighting (? la font-lock) or not."
  :group 'wscope
  :type '(boolean))

(defvar wscope-output-buffer-name "*Result*"
  "The name of the cscope output buffer.")

(defvar *wscope-result-cache* nil
  "cache find result")

(defface wscope-function-face
  '((((class color) (background dark))
	 (:foreground "cyan"))
	(((class color) (background light))
	 (:foreground "magenta"))
	(t (:bold t)))
  "Face used to highlight function name in the *wscope* buffer."
  :group 'wscope)


(defface wscope-line-number-face
  '((((class color) (background dark))
	 (:foreground "red"))
	(((class color) (background light))
	 (:foreground "red"))
	(t (:bold t)))
  "Face used to highlight line number in the *wscope* buffer."
  :group 'wscope)

(defface wscope-line-face
  '((((class color) (background dark))
	 (:foreground "green"))
	(((class color) (background light))
	 (:foreground "black"))
	(t (:bold nil)))
  "Face used to highlight the rest of line in the *wscope* buffer."
  :group 'wscope)


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
	(setq *wscope-result-cache* nil)
	(with-current-buffer (process-buffer proc)

	  (goto-char (point-max))
	  (insert command)

	  (process-send-string "wscope" command)

	  (wscope-wait-for-output)

	  (wscope-process-output)
	  )

	(let* ((show-tags (-map 'car *wscope-result-cache*))
		   (tagshow-index (grizzl-make-index show-tags))
		   (select-tag (minibuffer-with-setup-hook
						   (lambda () ())
						 (grizzl-completing-read "Show text: TODO" tagshow-index))))
	  (message select-tag))
;;		(if wscope-first-match
;;			(set-window-point (get-buffer-window outbuf) wscope-first-match-point)
;;		  (insert "\nNothing found!"))
;;		(wscope-list-entry-mode)
	  )
  )

(defun wscope-make-entry-line (func-name line-number line)
  ;; The format of entry line:
  ;; func-name[line-number]______line
  ;; <- cscope-name-line-width ->
  ;; `format' of Emacs doesn't have "*s" spec.
  (let* ((fmt (format "%%%ds %%s" wscope-name-line-width))
		 (str (format fmt (format "%s[%s]" func-name line-number) line))
		 beg end)
	(if wscope-use-face
		(progn
		  (setq end (length func-name))
		  (put-text-property 0 end 'face 'wscope-function-face str)
		  (setq beg (1+ end)
				end (+ beg (length line-number)))
		  (put-text-property beg end 'face 'wscope-line-number-face str)
		  (setq end (length str)
				beg (- end (length line)))
		  (put-text-property beg end 'face 'wscope-line-face str)
		  ))
	str))

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
  (let ((newentry '(text filename line-number)))
	(prin1 *wscope-result-cache*)
	(setq *wscope-result-cache* (cons newentry *wscope-result-cache*)))
  )

InOrg

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
