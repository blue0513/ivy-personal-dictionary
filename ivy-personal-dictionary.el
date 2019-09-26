;;; ivy-personal-dictionary --- save/insert your favorite words via ivy

;; Copyright (C) 2019- blue0513

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: blue0513
;; URL: https://github.com/blue0513/ivy-personal-dictionary
;; Version: 0.1.0

;;; Commentary:

;; Edit your init.el
;;
;; (require 'ivy-personal-dictionary)
;;

;;; Code:

(defvar personal-dic-saved-file-path
  (locate-user-emacs-file "ivy-personal-dictionary"))

(defun personal-dic--get-list-from (buffer-txt)
  (split-string buffer-txt "\n"))

;; http://ergoemacs.org/emacs/elisp_read_file_content.html
(defun get-string-from-file (filePath)
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun personal-dic--build-candidate-list-from (path)
  (let* ((string (get-string-from-file path)))
    (personal-dic--get-list-from string)))

(defun personal-dic--check-duplicated (str my-list)
  (member str my-list))

;; https://stackoverflow.com/a/17376809/8888451
(defun personal-dic--append-string-to-file (s filename)
  (with-temp-buffer
    (insert (concat "\n" s))
    (write-region (point-min) (point-max) filename t)))

(defun personal-dic--create-file-if-needed (filePath)
  (unless (file-exists-p filePath)
      (write-region "" "" filePath)))

(defun personal-dic--save (str)
  (personal-dic--create-file-if-needed personal-dic-saved-file-path)
  (unless (personal-dic--check-duplicated
           str
           (personal-dic--build-candidate-list-from personal-dic-saved-file-path))
    (personal-dic--append-string-to-file str personal-dic-saved-file-path))
  (message (concat "Added!: " str)))

;; https://stackoverflow.com/a/51445691/8888451
(defun personal-dic--get-selected-text (start end)
  (interactive "r")
  (if (use-region-p)
      (let ((regionp (buffer-substring-no-properties start end)))
        regionp)))

(defun personal-dic--save-input-text ()
  (interactive)
  (let ((s (read-string "String: ")))
    (personal-dic--save s)))

;;; Main Functions

(defun personal-dic-save ()
  (interactive)
  (cond
   ((region-active-p)
    (personal-dic--save
     (personal-dic--get-selected-text (region-beginning) (region-end))))
   ((thing-at-point 'symbol)
    (personal-dic--save (thing-at-point 'symbol)))
   (t
    (personal-dic--save-input-text))))

(defun personal-dic-load ()
  (interactive)
  (let* ((prompt "Select: ")
         (candidates (delete-dups
                      (personal-dic--build-candidate-list-from personal-dic-saved-file-path))))
    (ivy-read prompt
              candidates
              :action (lambda(str) (insert str))
              )))

;; * provide

(provide 'ivy-personal-dictionary)

;;; ivy-personal-dictionary.el ends here
