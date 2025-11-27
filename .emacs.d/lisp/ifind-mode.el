;;; ifind-mode.el -- A minor mode based on isearch, for interactively finding
;;; files in the workspace.

;; (c) 2010 Christian Rovner

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;; Installation:
;;
;; 1. Copy this file into a directory of your choice, 
;;    such as ~/.emacs.d/lisp/
;; 2. Add these lines to your .emacs file:
;;      (defvar workspace-dir "~/YOUR/WORKSPACE/DIR")
;;      (load "~/.emacs.d/lisp/ifind-mode.el")
;;
;; Replace YOUR/WORKSPACE/DIR above by your actual workspace directory

;; Usage:
;;
;; Just type M-x ifind-mode RET
;; Then type part of the filename you're searching for.  A list of matching
;; filenames in your workspace will appear, and it will get smaller as you keep
;; adding characters. Use the up/down arrows to navigate and press RET to visit
;; the file under the cursor.  Any other key will abort the search.

(defvar ifind-min-length 1
  "Minimum length of the search string to trigger the shell command.")

(defvar ifind-string ""
  "The current search string.")

(defvar ifind-ignored-directories '("node_modules" "target" "venv" "build")
  "Folders to be ignored")

(defvar ifind-mode nil
  "Name of the minor mode, if non-nil.")

(setq ifind--go-back-window nil)
(setq ifind--default-scroll-margin nil)

(defconst ifind--buffer-name "*ifind*")

(defun ifind--print-search ()
  (message "Find files matching: %s" ifind-string))

(defvar ifind-mode-map
  (let ((i ?\s)
        (map (make-keymap)))
    ;; Printing characters, search
    (while (< i 256)
      (define-key map (vector i) 'ifind-printing-char)
      (setq i (1+ i)))
    (define-key map [up] 'ifind--previous-line)
    (define-key map [left] 'ifind--previous-line)
    (define-key map [right] 'ifind--next-line)
    (define-key map [down] 'ifind--next-line)
    (define-key map [return] 'ifind-visit-file)
    (define-key map (kbd "C-v") 'ifind-visit-file-split-vertically)
    (define-key map [backspace] 'ifind-del-char)
    ;; All other keys will abort ifind
    (define-key map [t] 'ifind-abort)
    map)
  "Keymap for `ifind-mode'.")

;; Add ifind-mode to minor mode list
(or (assq 'ifind-mode minor-mode-alist)
    (nconc minor-mode-alist
           (list '(ifind-mode ifind-mode))))

(defun ifind--previous-line ()
  (interactive)
  (when (not (eq (line-number-at-pos) 1))
    (previous-line))
  (ifind--print-search))

(defun ifind--next-line ()
  (interactive)
  (let ((last-line (count-lines (point-min) (point-max))))
    (when (not (eq (line-number-at-pos) last-line))
      (next-line)))
  (ifind--print-search))

(defun ifind-mode ()
  "Start Ifind minor mode."
  (interactive)
  (setq ifind-string ""
        ifind-mode " Ifind"
        overriding-terminal-local-map ifind-mode-map
	ifind--go-back-window (selected-window)
	ifind--default-scroll-margin scroll-margin)
  (setq scroll-margin 0)
  (looking-at "")
  (force-mode-line-update)
  ;; Verificar se existe um modo melhor de fazer isso pq não é possível que não exista algo no Emacs que
  ;; crie uma nova janela E um novo buffer
  (generate-new-buffer ifind--buffer-name)
  (display-buffer ifind--buffer-name)
  (select-window (get-buffer-window ifind--buffer-name))
  (ifind-update))

(defun ifind-printing-char ()
  "Add this ordinary printing character to the search string and search."
  (interactive)
  (let ((char last-command-event))
    (setq ifind-string (concat ifind-string (char-to-string char)))
    (ifind-update)))

(defun ifind-abort ()
  "Abort Ifind."
  (interactive)
  (ifind-exit)
  (message "Ifind aborted."))

(defun ifind-visit-file ()
  "Open the file under the cursor in the *ifind* buffer."
  (interactive)
  (set-buffer ifind--buffer-name)
  (let ((filename (buffer-substring-no-properties
		   (line-beginning-position) (line-end-position))))
    (if (> (length filename) 0)
	(progn
	  (select-window ifind--go-back-window)
	  (find-file (concat default-directory filename)))))
  (ifind-exit))

(defun ifind-visit-file-split-vertically ()
  "Open the file under the cursor in the *ifind* buffer."
  (interactive)
  (set-buffer ifind--buffer-name)
  (let ((filename (buffer-substring-no-properties
		   (line-beginning-position) (line-end-position))))
    (if (> (length filename) 0)
	(progn
	  (select-window ifind--go-back-window)
	  (select-window (split-window-right))
	  (find-file (concat default-directory filename)))))
  (ifind-exit))

(defun ifind-del-char ()
  "Delete character from end of search string and search again."
  (interactive)
  (if (> (length ifind-string) 0)
      (progn
	(setq ifind-string (substring ifind-string 0 -1))
	(ifind-update))
    (ifind--print-search)))

(defun ifind-exit ()
  "Exit Ifind minor mode."
  (setq ifind-mode nil
        overriding-terminal-local-map nil
	scroll-margin ifind--default-scroll-margin)
  (force-mode-line-update)
  (setq ifind--go-back-window nil)
  (delete-window)
  (kill-buffer ifind--buffer-name)
)

(defun ifind-format-command (dir excluded-dirs str)
  (format ifind-command dir excluded-dirs str dir))

(defun ifind--list-excluded-dirs ()
  (seq-map 'regexp-quote (append vc-directory-exclusion-list ifind-ignored-directories)))

(setq ifind--escaped-dirs (ifind--list-excluded-dirs))

(defun ifind--should-enter-directory (dir)
  (let ((res))
    (dolist (excl-dir ifind--escaped-dirs res)
      (if (string-match excl-dir dir)
          (setq res t)))
      (not res)))

(defun ifind--get-max-lines-buffer ()
  (- (window-height nil 'floor) 2))

(defun ifind-update ()
  "Display the current search string and search for files."
  (switch-to-buffer ifind--buffer-name)
  (erase-buffer)
  (setq ifind--escaped-dirs (ifind--list-excluded-dirs))
  (let ((max-results (ifind--get-max-lines-buffer)))
    (if (>= (length ifind-string) ifind-min-length)
	(let*
	    ((all-files (directory-files-recursively default-directory ".*" nil 'ifind--should-enter-directory))
	     (relative-files (mapcar '(lambda (d) (file-relative-name d default-directory)) all-files))
	     (matcher (lambda (f) (not (string-match (regexp-quote ifind-string) f))))
	     (matched-files (seq-remove matcher relative-files))
	     (dir-list (take max-results matched-files)))

	  ;; (let ((dir-list (directory-files-recursively default-directory ifind-string nil 'ifind--should-enter-directory)))
	  (insert (mapconcat
		   'identity
		   dir-list
		   "\n"))
	  (beginning-of-buffer)
	  (narrow-to-region (window-start) (point-max))
	  (ifind--print-search))

      (let* ((files-and-dirs-attr (directory-files-and-attributes default-directory nil "[^.]+" t))
	     (files-attr (seq-remove (lambda (d) (seq-elt d 1)) files-and-dirs-attr))
	     (files (mapcar (lambda (d) (seq-elt d 0)) files-attr))
	     (dir-list (take max-results files)))
	(insert (mapconcat 'identity dir-list "\n"))
	(beginning-of-buffer)
	(ifind--print-search))
      )))

(provide 'ifind-mode)
