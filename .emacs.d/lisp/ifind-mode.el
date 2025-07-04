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

;; TODO Fazer uma limpa nos símbolos não utilizados

(defvar ifind-min-length 2
  "Minimum length of the search string to trigger the shell command.")

(defvar ifind-string ""
  "The current search string.")

(defvar ifind-ignored-directories '("node_modules" "target" "venv" "build")
  "Folders to be ignored")

(defvar ifind-mode nil
  "Name of the minor mode, if non-nil.")

(defvar ifind-mode-map
  (let ((i ?\s)
        (map (make-keymap)))
    ;; Printing characters, search
    (while (< i 256)
      (define-key map (vector i) 'ifind-printing-char)
      (setq i (1+ i)))
    (define-key map [up] 'previous-line)
    (define-key map [down] 'next-line)
    (define-key map [return] 'ifind-visit-file)
    (define-key map [backspace] 'ifind-del-char)
    ;; All other keys will abort ifind
    (define-key map [t] 'ifind-abort)
    map)
  "Keymap for `ifind-mode'.")

;; Add ifind-mode to minor mode list
(or (assq 'ifind-mode minor-mode-alist)
    (nconc minor-mode-alist
           (list '(ifind-mode ifind-mode))))

(defun ifind-mode ()
  "Start Ifind minor mode."
  (interactive)
  (setq ifind-string ""
        ifind-mode " Ifind"
        overriding-terminal-local-map ifind-mode-map)
  (looking-at "")
  (force-mode-line-update)
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
  (set-buffer "*ifind*")
  (let ((filename (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position))))
    (if (> (length filename) 0)
        (find-file (concat default-directory filename))))
  (ifind-exit))

(defun ifind-del-char ()
  "Delete character from end of search string and search again."
  (interactive)
  (when (> (length ifind-string) 0)
    (setq ifind-string (substring ifind-string 0 -1))
    (ifind-update)))

(defun ifind-exit ()
  "Exit Ifind minor mode."
  (setq ifind-mode nil
        overriding-terminal-local-map nil)
  (force-mode-line-update)
  (kill-buffer "*ifind*"))

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

(defun ifind-update ()
  "Display the current search string and search for files."
  (switch-to-buffer "*ifind*")
  (erase-buffer)
  (setq ifind--escaped-dirs (ifind--list-excluded-dirs))
  (if (>= (length ifind-string) ifind-min-length)
      (let ((dir-list (directory-files-recursively default-directory ifind-string nil 'ifind--should-enter-directory)))
        (insert (mapconcat
                 '(lambda (dir) (file-relative-name dir default-directory))
                 dir-list
                 "\n"))
        (beginning-of-buffer)
        (message "Find files matching: %s" ifind-string))))

(provide 'ifind-mode)
