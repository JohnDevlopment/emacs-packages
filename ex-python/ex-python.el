;;; ex-python.el --- Minor mode for more Python features

;; Copyright (C) 2023 John Russell
;; Author: John Russell <johndevlopment7@gmail.com>
;; Keywords: languages
;; Package-Version: 1.0-alpha3
;; Version: 1.0-alpha3
;; Package-Commit: 6a8f1829dee82c156c77751ab921654a9e806bb5
;;
;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; Extra functions that are not available in the default Python mode
;; implementation.
;;
;; Commands:
;;
;;     `python-add-docstring': adds a docstring to the current
;;     function.

;;; Code:

(require 'python)

(defgroup ex-python nil
  "Extra functionality for Python mode."
  :group 'python)

(defcustom ex-python-use-lsp nil
  "Non-nil means to use lsp functions."
  :type 'boolean
  :group 'ex-python
  :package-version "1.0"
  :safe 'booleanp)

(defun python-uncomment-region (beg end &optional arg)
  "Uncomment each line in the region."
  (interactive "*r")
  (if (use-region-p)
      (comment-region beg end '(4))))

;;;###autoload
(defun python-add-docstring (initval yn)
  "Add a docstring to the function.

If called interactively, prompts for the
initial line of the docstring, then asks
whether the docstring should be multiline.

If called from Lisp, INITVAL provides the first line,
and YN Is a character that is either 'y' or 'n'."
  (interactive "MFirst line: \ncmultiline? [y/n]")
  (let ((multiline (string yn)) (delim "\"\"\"")
	(standard-output (current-buffer)))
    (beginning-of-defun)
    (end-of-line)
    (newline)
    (indent-for-tab-command)
    (if (string= multiline "y")
	(let (cp)
	  (princ delim)
	  (newline)
	  (indent-for-tab-command)
	  (princ initval)
	  (newline 2)
	  (indent-for-tab-command)
	  (setq cp (point))
	  (princ "...")
	  (newline)
	  (indent-for-tab-command)
	  (princ delim)
	  (goto-char cp))
      (princ (concat delim initval delim)))))

(python-skeleton-define clsinit nil
  "Docstring: "
  "def __init__(self" ("Parameter, %s: "
		       (unless (equal ?\( (char-before)) ", ")
		       str) "):" \n
		       "\"\"\"" str "\"\"\"" \n
		       > _ \n)

(python-skeleton-define runmod nil
  "Main function: "
  "def " str " ():" \n
  "pass" -
  \n \n
  "if __name__ == \"__main\":" \n
  str "()")

;;;###autoload
(defun python-add-class-str ()
  "Utility function to quickly created a __str__ method.
Inserts the text at point with proper indention."
  (interactive)
  (python--add-class-method "str" nil "str" "return \"\""))

;;;###autoload
(defun python-add-class-getitem ()
  "Utility function that adds a __getitem__ method.
Inserts the text at point with proper indention."
  (interactive)
  (python--add-class-method "getitem" "key: str, /" nil))

;;;###autoload
(defun python-add-class-getattr ()
  "Utility function that adds a __getattr__ method.
Inserts the text at point with proper indention."
  (interactive)
  (python--add-class-method "getattr" "key: str" nil))

(defun python--add-class-method (name argstring rettype &rest body)
  ;; (interactive "sName: \nsArguments (minus self): \nsReturn type: ")
  (if (not (python--str-nonempty-p name))
      (error "empty name argument"))
  (let ((standard-output (current-buffer)))
      (princ (format "def __%s__(self%s)%s:"
	     name
	     (python--insert-nonempty-str argstring ", ")
	     (python--insert-nonempty-str rettype " -> ")))
      (newline)
      (indent-for-tab-command)
      (if (not (null body))
	  (dolist (elt body)
	    (princ elt))
	(princ "pass"))))

(defun python--insert-nonempty-str (string &optional prefix suffix)
  (if (python--str-nonempty-p string)
      (concat prefix string suffix)
    ""))

(defun python--str-nonempty-p (string)
  "Returns non-nill if STRING is not empty."
  (> (length string) 0))

;; Default keymap

(defvar ex-python-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c i d") 'python-add-docstring)
    ;; Skeletons
    (define-key map (kbd "C-c i i") 'python-skeleton-clsinit)
    (define-key map (kbd "C-c i r") 'python-skeleton-clsinit)

    (easy-menu-define ex-python-menu map "Ex-Python Mode menu"
      '("Ex-Python"
	:help "More Python-specific Features"
	["Comment Region" comment-region
	 :help "Comment or uncomment each line in the region"
	 :active (use-region-p)]
	["Uncomment Region" python-uncomment-region
	 :help "Uncomment each line in the region"
	 :active (use-region-p)]
	"--"
	["Generate Docstring" python-add-docstring]
	["Add __str__ method" python-add-class-str]
	["Add __getitem__ method" python-add-class-getitem]
	["Add __getattr__ method" python-add-class-getattr]
	"--"
	("Skeletons"
	 :help "A submenu for skeletons"
	 ["Add __init__ method" python-skeleton-clsinit]
	 ["Add __main__ if body" python-skeleton-runmod])
	"--"
	("LSP"
	 :active (ex-python--lsp-enabled-p)
	 :help "A submenu for lsp commands."
	 ["Find definition" lsp-find-definition])
	"---"
	["Disable Ex-Python mode" ex-python-mode]))
    map)
  "Default bindings for ex-python mode.")

;;;###autoload
(define-minor-mode ex-python-mode
  "This minor mode should only be used while in Python mode. It
adds extra commands that are not provided in Python mode.

If `ex-python-use-lsp' is set to a non-nill value, and `lsp'
is installed and Emacs configured to use it, the Ex-Python menu
contains a submenu dedicated to lsp commands.

\\{ex-python-mode-map}"

  :lighter " xpy"
  :interactive '(python-mode)
  :init-value nil
  :keymap ex-python-mode-map
  :group 'ex-python)

;; Internal functions

(defun ex-python--lsp-enabled-p ()
  "Return non-nill if we can use lsp functions."
  (and
   ex-python-use-lsp
   (fboundp 'lsp)))

(provide 'ex-python)

;;; ex-python.el ends here
