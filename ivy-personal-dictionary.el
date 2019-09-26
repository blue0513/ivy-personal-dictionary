(setq my-path "")

(defun my-func (str)
  (insert str))

(defun my-get-list (buffer-txt)
  (split-string buffer-txt "\n"))

(defun my-test ()
  (interactive)
  (let* ((prompt "Hoge: ")
         (candidates (delete-dups (build-candidate-list-from my-path))))
    (ivy-read prompt
              candidates
              :action (lambda(x) (my-func x))
              )))

;; http://ergoemacs.org/emacs/elisp_read_file_content.html
(defun get-string-from-file (filePath)
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun build-candidate-list-from (path)
  (let* ((string (get-string-from-file path)))
    (my-get-list string)))

(defun check-is-duplicated (str my-list)
  (member str my-list))

;; https://stackoverflow.com/a/17376809/8888451
(defun my-append-string-to-file (s filename)
  (with-temp-buffer
    (insert (concat "\n" s))
    (write-region (point-min) (point-max) filename t)))

(defun my-paste (str)
  (unless (check-is-duplicated str (build-candidate-list-from my-path))
    (my-append-string-to-file str my-path))
  (message (concat "Added!: " str)))

;; https://stackoverflow.com/a/51445691/8888451
(defun get-selected-text (start end)
  (interactive "r")
  (if (use-region-p)
      (let ((regionp (buffer-substring-no-properties start end)))
        regionp)))

(defun my-input-text ()
  (interactive)
  (let ((s (read-string "String: ")))
    (my-paste s)))

(defun my-save-word ()
  (interactive)
  (cond
   ((region-active-p)
    (my-paste (get-selected-text (region-beginning) (region-end))))
   ((thing-at-point 'symbol)
    (my-paste (thing-at-point 'symbol)))
   (t
    (my-input-text))))
