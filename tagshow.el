;; Copyright © 2013 WANG Yanjin
;;
;; Author:   WANG Yanjin <wyj1046#gmail.com>
;; URL:      no yet
;; Version:  0.0.1
;; Keywords: tags

;; This file is NOT part of GNU Emacs.

;;; --- License

;; Licensed under the same terms as Emacs.

(require 'grizzl)
(require 'cl)

(define-minor-mode tagshow-mode
  ""
  )

(defvar *filename-cache* "")

(defvar *tags-cache* "")

(defun run-tags-command (path)
  "run program"
  (let ((stdout-buffer (generate-new-buffer (generate-new-buffer-name " *tagshow stdout*")))
		output
		exit-code)
	(setq exit-code (call-process "exctags" nil stdout-buffer nil "-f -" "--format=2" "--excmd=pattern" "--fields=nksSa" "--extra=" "--sort=yes" path))
    (with-current-buffer stdout-buffer
      (setq output (buffer-string))
      (kill-buffer))
    (list output exit-code)))

(defun get-file-tags (path)
  ""
  (if (eq *filename-cache* path)
	  (cl-map 'list 'car *tags-cache*)
	  (get-target-file-tags path))
  )

(defun get-tag-line-no (line)
  ""
  (if (string-match "line:\\([0-9]+\\)" line)
	  (string-to-number (match-string 1 line))
	nil))

(defun get-single-tag-and-line (line)
  ""
  (let ((tag (car (split-string line)))
		(line-no (get-tag-line-no line))
		)
	(list tag line-no)))

(defun get-target-file-tags (path)
  ""
  (let* ((ret (run-tags-command path))
		(tags-raw-content (car ret))
		(exit-code (last ret))
		(lines (split-string tags-raw-content "\n"))
		(-tags (cl-map 'list 'get-single-tag-and-line lines))
		(filtered-tags (remove-if (lambda (x) (= 0 (length (car x)))) -tags)))
	(setq *filename-cache* path)
	(setq *tags-cache* filtered-tags)
	(cl-map 'list 'car filtered-tags)))

(defun get-current-line-no (select-tag)
  ""
  (car (last (car (remove-if-not (lambda (x) (equal (car x) select-tag)) *tags-cache*)))))

(defun format-tags-cache ()
  ""
  (reduce (lambda (x y) (format "%s %s %d" x (car y) (car (last y)))) *tags-cache* :initial-value ""))

(defun show-tags ()
  ""
  (interactive)
  (let* ((tagshow-index (grizzl-make-index (get-file-tags (buffer-file-name))))
		(select-tag (minibuffer-with-setup-hook
					 (lambda ()
					   (tagshow-mode 1))
					 (grizzl-completing-read "Show text: TODO" tagshow-index))))
;;;	(goto-line (get-current-line-no select-tag))
	(message (format "%s %d %s" select-tag 0 (format-tags-cache)))
	))


(provide 'tagshow)
