;; Copyright © 2013 WANG Yanjin
;;
;; Author:   WANG Yanjin <wyj1046#gmail.com>
;; URL:      http://github.com/goal/tagshow
;; Version:  0.1.0
;; Keywords: tags

;; This file is NOT part of GNU Emacs.

;;; --- License

;; Licensed under the same terms as Emacs.

(require 'grizzl)
(require 'dash)

(define-minor-mode tagshow-mode
  ""
  )

(defvar *ctags-bin-candidates* '("ctags-exuberant" ; Debian
                                 "exuberant-ctags"
                                 "exctags"  ; FreeBSD, NetBSD
                                 "/usr/local/bin/ctags"  ; Homebrew
                                 "/opt/local/bin/ctags"  ; Macports
                                 "ectags"  ; OpenBSD
                                 "ctags"
                                 "ctags.exe"
                                 "tags"
                                 ))

(defvar *filename-cache* "")

(defvar *tags-cache* "")

(defun exists-ctags-program (pstr)
  (ignore-errors
    (let ((ret (run-command pstr "--version")))
      (and (= 0 (nth 1 ret))
           (string-match "Exuberant Ctags" (car ret))))))

(defun get-available-ctags-bin ()
  (or (car (-select (lambda (x) (exists-ctags-program x)) *ctags-bin-candidates*)) (throw 'no-exuberant-ctags-found t)))

(defun run-command (program &rest args)
  "run program"
  (let ((stdout-buffer (generate-new-buffer (generate-new-buffer-name " *tagshow stdout*")))
		output
		exit-code)
	(setq exit-code (apply 'call-process program nil stdout-buffer nil args))
    (with-current-buffer stdout-buffer
      (setq output (buffer-string))
      (kill-buffer))
    (list output exit-code)))

(defun get-file-tags (path)
  ""
  (if (eq *filename-cache* path)
	  (-map 'car *tags-cache*)
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
  (let* ((ret (run-command (get-available-ctags-bin) "-f -" "--format=2" "--excmd=pattern" "--fields=nksSa" "--extra=" "--sort=yes" path))
		(tags-raw-content (car ret))
		(exit-code (last ret))
		(lines (split-string tags-raw-content "\n"))
		(-tags (-map 'get-single-tag-and-line lines))
		(filtered-tags (-remove (lambda (x) (= 0 (length (car x)))) -tags)))
	(setq *filename-cache* path)
	(setq *tags-cache* filtered-tags)
	(-map 'car filtered-tags)))

(defun get-current-line-no (select-tag)
  ""
  (-last-item (car (-select (lambda (x) (equal (car x) select-tag)) *tags-cache*))))

(defun format-tags-cache ()
  "for debug use"
  (-reduce-from (lambda (x y) (format "%s %s %d" x (car y) (car (last y)))) "" *tags-cache*))

(defun show-tags ()
  ""
  (interactive)
  (let* ((tagshow-index (grizzl-make-index (get-file-tags (buffer-file-name))))
		(select-tag (minibuffer-with-setup-hook
					 (lambda ()
					   (tagshow-mode 1))
					 (grizzl-completing-read "Show text: TODO" tagshow-index))))
	(goto-line (get-current-line-no select-tag))
;;;	(message (format "%s %d %s" select-tag (get-current-line-no select-tag) (format-tags-cache)))
	))


(provide 'tagshow)
