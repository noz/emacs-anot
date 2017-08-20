;;; anot.el --- annotations

;; Copyright (C) 2014-2017 NOZAWA Hiromasa

;; Author: NOZAWA Hiromasa
;; Created: 2014-09-24
;; Version: 1.1.1
;; Keywords: annotation

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; - seamless annotations
;;; - per-file annotation files

;;;
;;; Installation:
;;;   (require 'anot)
;;;
;;; Commands:
;;;   anot-region		- turn region into annotation
;;;   anot-insert		- insert annotation fragment at point
;;;   anot-un-annotation	- turn annotation into ordinary text
;;;   anot-toggle-show		- toggle show/hide annotations
;;;   anot-toggle-keep		- toggle keep-in/out annotations in the file
;;;

;;; Code:

(eval-when-compile
  (require 'cl)
  (declare-function cl-delete-if "cl-seq.el")
  (declare-function cl-return "cl-macs.el"))

(defconst anot-file-suffix ".anot")

(defface anot-keep-out-face
  '((((class color) (min-colors 88) (background light))
     :background "darkseagreen2")
    (((class color) (min-colors 88) (background dark))
     :background "#002020")
    (((class color) (min-colors 16))
     :background "blue")
    (((class color) (min-colors 8))
     :background "blue")
    (t :inverse-video t))
  "Face for keep-out annotations.")

(defface anot-keep-in-face
  '((((class color) (min-colors 88) (background light))
     :background "RosyBrown1")
    (((class color) (min-colors 88) (background dark))
     :background "#200000")
    (((class color) (min-colors 16))
     :background "blue")
    (((class color) (min-colors 8))
     :background "blue")
    (t :inverse-video t))
  "Face for keep-in annotations.")

(defvar anot-fragment-string "*anot*")

(defvar anot-list nil)
(make-variable-buffer-local 'anot-list)

(defvar anot-show t)
(make-variable-buffer-local 'anot-show)

(defvar anot-keep nil)
(make-variable-buffer-local 'anot-keep)

(defvar anot-point-save nil)
(make-variable-buffer-local 'anot-point-save)

(defun anot-region (beg end)
  "Turn region into annotation."
  (interactive "r")
  (when (anot-annotation-at beg end)
    (error "Annotaion already exist"))
  (let ((ovl (make-overlay beg end (current-buffer) t)))
    (overlay-put ovl 'face
		 (if anot-keep 'anot-keep-in-face 'anot-keep-out-face))
    (overlay-put ovl 'evaporate t)
    (add-to-list 'anot-list ovl)
    (set-buffer-modified-p t)))

(defun anot-insert (&optional str pos)
  "Insert annotation fragment at point."
  (interactive)
  (let* ((beg (or pos (point)))
	 (s (or str anot-fragment-string))
	 (end (+ beg (length s))))
    (when (anot-annotation-at beg)
      (error "Annotaion already exist"))
    (goto-char beg)
    (insert s)
    (anot-region beg end)))

(defun anot-un-annotation (&optional an)
  "Un-annotation at point."
  (interactive)
  (setq an (or an (anot-annotation-at)))
  (unless an
    (error "No annotation at point"))
  (delete-overlay an)
  (set-buffer-modified-p t))

(defun anot-toggle-show ()
  "Toggle show/hide annotations."
  (interactive)
  (anot-show (not anot-show))
  (anot-status-message))

(defun anot-show (sw)
  "Switch visibility of annotations.
When SW is non-nil, visible. Otherwise invisible."
  (setq anot-show (not (null sw)))
  (dolist (an anot-list) (overlay-put an 'invisible (not anot-show))))

(defun anot-toggle-keep ()
  "Toggle keep in/out annotations in the file after saving."
  (interactive)
  (anot-keep (not anot-keep))
  (anot-status-message))

(defun anot-keep (sw)
  "Switch keep-mode of annotations.
When SW is non-nil, annotations are kept in the file after save.
Otherwise not."
  (setq anot-keep (not (null sw)))
  (let ((face (if anot-keep 'anot-keep-in-face 'anot-keep-out-face)))
    (dolist (an anot-list)
      (overlay-put an 'face face)))
  (set-buffer-modified-p t))

(defun anot-status-message ()
  (message "ANOT (%s,%s)"
	   (if anot-show "show" "hide")
	   (if anot-keep "keep-in" "keep-out")))

(defun anot-annotation-at (&optional pos end)
  "Returns an annotation at current ponit as an overlay object.
Use point POS instead of current point when POS is specified.
If both POS and END are non-nil, list of annotations in the range
POS from END is returned.
Returns NIL when no any annotations are found."
  (if end
      (let (ret)
	(dolist (an anot-list)
	  (when (overlay-buffer an)
	    (let ((os (overlay-start an))
		  (oe (overlay-end an)))
	      (if (or (and (<= pos os) (< os end))
		      (and (< pos oe) (<= oe end))
		      (and (<= os pos) (< end oe)))
		  (push an ret)))))
	ret)
    (let ((p (or pos (point))))
      (dolist (an anot-list)
	(if (and (overlay-buffer an)
		 (<= (overlay-start an) p) (< p (overlay-end an)))
	    (cl-return an))))))

(defun anot-save ()
  (when (and (buffer-file-name) (buffer-modified-p))
    ;; clean up anot-list
    (setq anot-list
	  (sort (cl-delete-if #'(lambda (e)
				  (when (or (null (overlay-buffer e))
					    (= (overlay-start e) (overlay-end e)))
				    (delete-overlay e) t))
			      anot-list)
		(lambda (a b) (< (overlay-start a) (overlay-start b)))))
    (if (= 0 (length anot-list))
	;; no annotations in the buffer
	(when (file-exists-p (anot-file))
	  (delete-file (anot-file)))
      (let (undobackup)
	(setq undobackup buffer-undo-list)
	(buffer-disable-undo)
	(setq anot-point-save (cons (point) (window-start)))
	;; write .anot file
	(let ((curbuf (current-buffer))
	      (anotbuf (find-file-noselect (anot-file)))
	      (al anot-list)
	      (keep (if anot-keep "IN\n" "OUT\n")))
	  (with-current-buffer anotbuf
	    (erase-buffer)
	    (insert (concat "generated by anot.el. DO NOT EDIT.\n"
		     (file-name-nondirectory (buffer-file-name curbuf))
		     "\n" (format-time-string "%Y-%m-%d %H:%M:%S\n")))
	    (insert keep)
	    (dolist (an al)
	      (insert (concat (number-to-string (overlay-start an)) ","
	      		      (number-to-string
			       (- (overlay-end an) (overlay-start an))) "\n"))
	      (insert-buffer-substring-no-properties
	       curbuf (overlay-start an) (overlay-end an))
	      (insert "\n"))
	    (save-buffer))
	  (kill-buffer anotbuf))
	(if anot-keep
	    (dolist (an anot-list) (anot-un-annotation an))
	  (dolist (an anot-list)
	    (delete-region (overlay-start an) (overlay-end an))))
	(setq anot-list nil)
	(buffer-enable-undo)
	(setq buffer-undo-list undobackup)))))

(defun anot-load ()
  (when (and (buffer-file-name) (file-exists-p (anot-file)))
    (let ((undobackup buffer-undo-list))
      (buffer-disable-undo)
      (save-excursion
	(let ((curbuf (current-buffer))
	      (anotbuf (find-file-noselect (anot-file))))
	  (with-current-buffer anotbuf
	    (goto-char (point-min))
	    (forward-line 3)	;; skip header lines
	    (let ((keep (string= (current-word) "IN")) pos len beg end)
	      (while (re-search-forward "^\\([0-9]+\\),\\([0-9]+\\)" nil t)
		(setq pos (string-to-number (match-string 1))
		      len (string-to-number (match-string 2)))
		(forward-line)
		(setq beg (point))
		(forward-char len)
		(setq end (point))
		(with-current-buffer curbuf
		  (setq anot-keep keep)
		  (unless keep
		    (goto-char pos)
		    (insert-buffer-substring-no-properties anotbuf beg end))
		  (anot-region pos (+ pos len)))
		(forward-char))))
	  (kill-buffer anotbuf)))
      (when anot-point-save
  	(goto-char (car anot-point-save))
  	(set-window-start (get-buffer-window) (cdr anot-point-save))
  	(setq anot-point-save nil))
      (unless anot-show (anot-show nil))
      (buffer-enable-undo)
      (setq buffer-undo-list undobackup)
      (set-buffer-modified-p nil)
      (anot-status-message))))

(defun anot-file ()
  (let ((fname (buffer-file-name)))
    (if fname
	(concat fname anot-file-suffix))))

(add-hook 'find-file-hook 'anot-load)
(add-hook 'before-save-hook 'anot-save)
(add-hook 'after-save-hook 'anot-load)
(add-hook 'kill-buffer-hook 'anot-save)

(provide 'anot)

;;; anot.el ends here
