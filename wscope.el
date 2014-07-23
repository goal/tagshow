
;; some source from cscope:
;; 0    {"Find this", "C symbol",           findsymbol},
;; 1    {"Find this", "global definition",      finddef},
;; 2    {"Find", "functions called by this function",   findcalledby},
;; 3    {"Find", "functions calling this function", findcalling},
;; 4    {"Find this", "text string",            findstring},
;; 5    {"Change this", "text string",          findstring},
;; 6    {"Find this", "egrep pattern",          findregexp},
;; 7    {"Find this", "file",               findfile},
;; 8    {"Find", "files #including this file",      findinclude},
;; 9    {"Find", "assignments to this symbol",      findassign},
;; 10    {"Find all", "function definitions",        findallfcns},   /* samuel only */

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

(defvar *wscope-cscope-file-dir* nil
  "the dir where cscope.out is at")

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

(defun wscope-auto-init (dir depth)
  (if (< depth 0)
	  (progn
		(message (concat dir ": no cscope.out file found!"))
		nil)
	(let* ((target-dir (expand-file-name dir))
		   (cur-dir-file-name (concat target-dir "cscope.out")))
	  (if (file-exists-p cur-dir-file-name)
		  (progn
			(wscope-init target-dir)
			1)
		(wscope-auto-init (concat dir "../") (- depth 1)))))
  )

(defun wscope-init (dir)
  (interactive "DCscope Initial Directory: ")
  (if (get-process "wscope") (kill-process (get-process "wscope")))
  (if (get-buffer "*wscope*") (kill-buffer (get-buffer "*wscope*")))
  (setq default-directory dir)
  (setq *wscope-cscope-file-dir* dir)
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

(defun wscope-find-this-symbol ()
  "Locate a symbol in source code."
  (interactive)
  (setq symbol (current-word))
  (if symbol
	  (progn
		(setq query-command (concat "0" symbol "\n"))
		(setq wscope-action-message (format "Find this symbol: %s" symbol))
		(wscope-query query-command))
	(message "What to find?"))
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

(defun wscope-find-called-functions ()
  "Display functions called by a function."
  (interactive)
  (setq symbol (current-word))
  (if symbol
	  (progn
		(setq query-command (concat "2" symbol "\n") )
		(setq wscope-action-message (format "Find functions called by this function: %s" symbol))
		(wscope-query query-command))
	(message "What to find?"))
  )

(defun wscope-find-functions-calling-this-function ()
  "Display functions calling a function."
  (interactive)
  (setq symbol (current-word))
  (if symbol
	  (progn
		(setq query-command (concat "3" symbol "\n") )
		(setq wscope-action-message (format "Find functions calling this function: %s" symbol))
		(wscope-query query-command))
	(message "What to find?"))
  )

(defun wscope-find-this-text-string ()
  "Locate where a text string occurs."
  (interactive)
  (setq symbol (current-word))
  (if symbol
	  (progn
		(setq query-command (concat "4" symbol "\n") )
		(setq wscope-action-message (format "Find this text string: %s" symbol))
		(wscope-query query-command))
	(message "What to find?"))
  )

(defun wscope-interactive (prompt)
  (list
   (let (sym)
	 (setq sym (current-word))
	 (read-string
	  (if sym
		  (format "%s (default %s): "
				  (substring prompt 0 (string-match "[ :]+\\'" prompt))
				  sym)
		prompt)
	  nil nil sym)
	 ))
  )

(defun wscope-find-this-file (symbol)
  "Locate all files by name match"
  (interactive (wscope-interactive "Find files with string in name: "))
  (if symbol
	  (progn
		(setq query-command (concat "7" symbol "\n") )
		(setq wscope-action-message (format "Find files with string in name: %s" symbol))
		(wscope-query query-command))
	(message "What to find?"))
  )

(defun wscope-find-files-including-file (symbol)
  "Locate all files #including a file."
  (interactive (wscope-interactive "Find files #including this file: "))
  (if symbol
	  (progn
		(setq query-command (concat "8" symbol "\n") )
		(setq wscope-action-message (format "Find files #including this file: %s" symbol))
		(wscope-query query-command))
	(message "What to find?"))
  )

(defun wscope-all-symbol-assignments ()
  "Find all the assignments of the symbol. Don't work yet due to cscope bug!!"
  (interactive)
  (setq symbol (current-word))
  (if symbol
	  (progn
		(setq query-command (concat "10" symbol "\n") )
		(setq wscope-action-message (format "Find all assignments of symbol %s" symbol))
		(wscope-query query-command))
	(message "What to find?"))
  )

(defun wscope-query (command)
  (if (get-process "wscope")
	  (-wscope-query command)
	(progn
	  (if (wscope-auto-init default-directory 3)
		  (-wscope-query command)
		nil)))
  )

(defun wscope-report-progress (n total)
  "Show the number of files processed in the message area."
  (when (= 0 (mod n 1000))
	(message (format "Indexing (%d/%d)" n total))))

(defun -wscope-query (command)
  (let ((proc (get-process "wscope")) outbuf
		)
	(setq *wscope-result-cache* nil)
	(with-current-buffer (process-buffer proc)
	  (goto-char (point-max))
	  (insert command)
	  (process-send-string "wscope" command)

	  (wscope-wait-for-output)
	  (wscope-process-output))

	(let* ((show-tags (-map 'car *wscope-result-cache*))
		   (tagshow-index (grizzl-make-index show-tags :progress-fn #'wscope-report-progress))
		   (select-tag (minibuffer-with-setup-hook
						   (lambda () ())
						 (grizzl-completing-read "Show text: TODO" tagshow-index))))
	  (goto-file-and-line select-tag)))
  )

(defun goto-file-and-line (select-tag)
  (let* ((item (car (-select (lambda (x) (equal (car x) select-tag)) *wscope-result-cache*)))
		 (file-name (nth 1 item))
		 (line-number (nth 2 item))
		 )
	(find-file file-name)
	(goto-line (read line-number)))
  )

(defun strip (long-string pre-string)
  (if (string-prefix-p pre-string long-string)
	  (substring long-string (length pre-string) (length long-string))
	long-string)
  )

(defun wscope-make-entry-line (file func-name line-number line)
  ;; The format of entry line:
  ;; func-name[line-number]______line
  ;; <- cscope-name-line-width ->
  ;; `format' of Emacs doesn't have "*s" spec.
  (let* ((short-file (strip file *wscope-cscope-file-dir*))
		 (fmt (format "%%%ds %%s" wscope-name-line-width))
		 (str (format fmt (format "%s[%s]" short-file line-number) line))
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
			   (wscope-make-entry-line file
									   function-name
									   line-number
									   line)
			   (expand-file-name file)
			   line-number)
			  ))))
	)
  )

(defun wscope-insert-text-with-properites (text filename &optional line-number)
  (let ((newentry (list text filename line-number)))
	(setq *wscope-result-cache* (cons newentry *wscope-result-cache*)))
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
