;; Copyright (C) 2012  Lincoln de Sousa <lincoln@comum.org>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'cl)

(defun lock ()
  "Call out a shell command to lock the X11 session"
  (interactive)
  (shell-command "~/bin/lock"))

(defun suspend ()
  "Call out a shell command to lock and suspend the computer"
  (interactive)
  (shell-command "~/bin/suspend"))

(defun nth-format (day)
  "Return proper nth formatting for a given month day"
  (pcase day
    (`"11" "th")
    (`"12" "th")
    (d (pcase (string-to-number (substring d 1 2))
         (`1 "st")
         (`2 "nd")
         (`3 "rd")
         (n "th")))))

(defun nth-day (time)
  "Return the day of the month in the nth format"
  (let ((d (format-time-string "%02d" time)))
    (concat (number-to-string (string-to-number d))
            (nth-format d))))

(defun my-date ()
  "Format current date according to my taste"
  (format-time-string
   (concat "%B " (nth-day (current-time)) " %Y")
   (current-time)))

(defun my-date-time ()
  "Format current date time for interactive sessions"
  (format-time-string
   (concat "%A, %b " (nth-day (current-time)) " %Y -- %H:%M")
   (current-time)))

(defun now ()
  "Message current date & time in the format I like"
  (interactive)
  (message (my-date-time)))

(defun kill-all-buffers-mercilessly ()
  "*DANGEROUS* function that kills all the buffers mercilessly

I suggest you to DO NOT bind it to any keyboard shortcut and
please, be careful, once called, it can't be stopped!"
  (interactive)
  (mapcar '(lambda (b)
             (ignore-errors
               (revert-buffer 1 1))
             (kill-buffer b))
          (buffer-list)))


(defun open-current-file-in-finder ()
  "Opens the current file using the Mac OS `open` utility"
  (interactive)
  (message (shell-command-to-string
            (concat "open " (buffer-file-name)))))


(defun python-find-module (name)
  "Open the file that declares the received python module"
  (interactive "MPython module to open: ")
  (find-file
   (let ((command (concat "python -c 'from __future__ import absolute_import; import sys, " name " as m; sys.stdout.write(m.__file__)'")))
     (replace-regexp-in-string ".py.$" ".py" (shell-command-to-string command)))))


(defun touch (path)
  "Calls the Unix touch command on 'path'"
  (interactive "Fpath to the file to touch: ")
  (shell-command-to-string (concat "touch " path)))


(defun scratch (name)
  "Creates a new buffer pointing to a safe location to save an scratch"
  (interactive "MName: ")
  (find-file
   (concat
    (mapconcat 'file-name-as-directory
               (list (getenv "HOME") "Tmp")
               "")
    name)))


;;;; Tests

;; nth-format
(assert (string= (nth-format "28") "th"))
(assert (string= (nth-format "21") "st"))
(assert (string= (nth-format "22") "nd"))
(assert (string= (nth-format "23") "rd"))
(assert (string= (nth-format "20") "th"))
(assert (string= (nth-format "11") "th"))
(assert (string= (nth-format "12") "th"))

;; nth day
; "2016-07-01"
(assert (string= (nth-day '(22390 18641 631962 984000)) "1st"))

; "2016-07-02"
(assert (string= (nth-day '(22391 18641 631962 984000)) "2nd"))

; "2016-09-15"
(assert (string= (nth-day '(22490 18641 631962 984000)) "15th"))
