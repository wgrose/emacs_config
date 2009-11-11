;;; paren-glint.el --- unhighlight matching paren after a timeout

;; Copyright (C) 2005  Free Software Foundation, Inc.

;; Author: Karl Landström <kland at comhem dot se>
;; Maintainer: Karl Landström <kland at comhem.se>
;; Version: 1.0
;; Keywords: languages, faces

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
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Installation:
;; 
;; Put this file in a folder where Emacs can find it.  On GNU/Linux
;; it's usually /usr/local/share/emacs/site-lisp/ and on Windows it's
;; something like "C:\Program Files\Emacs<version>\site-lisp".  To
;; make it run slightly faster you can also compile it from Emacs (M-x
;; `emacs-lisp-byte-compile'). Then put the following into your .emacs
;; initialization file (_emacs on Windows):
;; 
;;  (show-paren-mode 1)
;;  (require 'paren-glint)
;;  (paren-glint-mode 1)
;; 
;; It will first highlight according to `show-paren-mode' and then
;; unhighlight after `paren-glint-timeout' seconds.

;; Bugs:
;;
;; On some (rare) occations the unhighlighting does not work for some
;; unknown reason. If point is moved back and forth it works again.

;;; Code:

(require 'advice)

(defcustom paren-glint-timeout 0.6
  "Time in seconds until a matching paren is unhighlighted.")

(defvar paren-glint-mode nil
  "Non-nil if paren glint mode is enabled.
Use the command `paren-glint-mode' to change this variable.")

(defvar paren-glint-timeout-saved-point nil
  "Location used by advice `timeout' of `show-paren-function'.")

(defvar paren-glint-timeout-saved-buffer nil
  "Buffer Lisp object used by advice `timeout' of `show-paren-function'.")

(defun paren-glint-maybe-unhighlight ()
  (when (and (eq (current-buffer) paren-glint-timeout-saved-buffer)
             (= (point) paren-glint-timeout-saved-point))
    (delete-overlay show-paren-overlay)
    (delete-overlay show-paren-overlay-1)))


(defadvice show-paren-function (after timeout)
  "Unhighlight matching paren after `paren-glint-timeout' seconds."
  (if (or (not (eq (current-buffer) paren-glint-timeout-saved-buffer))
          (and (eq (current-buffer) paren-glint-timeout-saved-buffer)
               (eq (point) paren-glint-timeout-saved-point)))
      (progn 
        (setq paren-glint-timeout-saved-buffer (current-buffer))
        (setq paren-glint-timeout-saved-point (point))
        (delete-overlay show-paren-overlay)
        (delete-overlay show-paren-overlay-1))
    (setq paren-glint-timeout-saved-buffer (current-buffer))
    (setq paren-glint-timeout-saved-point (point))
    (run-with-idle-timer paren-glint-timeout nil 
                         'paren-glint-maybe-unhighlight)))


(defun paren-glint-mode (&optional arg)
"Toggle Paren Glint mode in every buffer.
With prefix ARG, turn Paren Glint mode on if and only if ARG is non-nil.
Returns the new status of Paren Glint mode (non-nil means on)."
  (interactive)
  (if (or (and arg (> arg 0)) (null paren-glint-mode))
      (progn (ad-activate 'show-paren-function)
             (setq paren-glint-mode t))
    (progn (ad-deactivate 'show-paren-function)
           (setq paren-glint-mode nil))))


(provide 'paren-glint)
;;; paren-glint.el ends here
