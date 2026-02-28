;; -*- lexical-binding: t -*-
;; Custom splash screen with random Pokemon ASCII art

(defun mg/pokemon-dir ()
  (expand-file-name "pokemon" user-emacs-directory))

(defun mg/random-pokemon-file ()
  "Return a random Pokemon .txt file path."
  (let* ((files (directory-files (mg/pokemon-dir) t "\\.txt$")))
    (nth (random (length files)) files)))

(defun mg/center-lines (text window-width)
  "Return TEXT with each line padded to center it in WINDOW-WIDTH."
  (let* ((lines (split-string text "\n"))
         (max-w (apply #'max (mapcar #'string-width lines))))
    (mapconcat
     (lambda (line)
       (let ((pad (max 0 (/ (- window-width max-w) 2))))
         (concat (make-string pad ?\s) line)))
     lines "\n")))

(defun mg/splash-buffer ()
  "Create and return the *splash* buffer with a random Pokemon."
  (let* ((buf    (get-buffer-create "*splash*"))
         (file   (mg/random-pokemon-file))
         (base   (file-name-base file))
         (number (car (split-string base "-")))
         (name   (mapconcat #'identity (cdr (split-string base "-")) "-"))
         (art    (with-temp-buffer
                   (insert-file-contents file)
                   (buffer-string)))
         (width  (frame-width)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "\n\n")
        (insert (mg/center-lines art width))
        (insert "\n\n")
        (insert (mg/center-lines (format "[ #%s  %s ]" number name) width))
        (insert "\n"))
      (goto-char (point-min))
      (read-only-mode 1)
      (setq-local cursor-type nil)
      (setq-local mode-line-format nil))
    buf))

(setq initial-buffer-choice #'mg/splash-buffer)

(provide 'mg-splash)
