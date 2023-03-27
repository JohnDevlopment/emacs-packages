;;; ex-python.el --- Minor mode for more Python features

;; Copyright (C) 2023 John Russell
;; Author: John Russell <johndevlopment7@gmail.com>
;; Keywords: languages
;; Package-Version: 1.0-alpha5
;; Version: 1.0-alpha5
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
;;
;;     `python-add-class-str': adds a __str__ method to a
;;     class.
;;
;;     `python-add-class-repr': adds a __repr__ method to a
;;     class.
;;
;;     `python-add-class-getitem': adds a __getitem method to
;;     a class.
;;
;;     `python-add-class-getattr': adds a __getattr__ method to
;;     a class.
;;
;;     `python-add-class-setattr': adds a __setattr__ method to
;;     a class.
;;
;;     `python-add-class-len': adds a __len__ method to a class.
;;
;;     `python-add-class-eq': adds a __eq__ method to a class.
;;
;;     `python-add-class-ne': adds a __ne__ method to a class.
;;
;;     `python-add-class-lt': adds a __lt__ method to a class.
;;
;;     `python-add-class-le': adds a __le__ method to a class.
;;
;;     `python-add-class-gt': adds a __gt__ method to a class.
;;
;;     `python-add-class-ge': adds a __ge__ method to a class.

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
		       "\"\"\"\"\"\"" \n
		       > _ \n)

(python-skeleton-define runmod nil
  "Main function: "
  "def " str " ():" \n
  "pass" -
  \n \n
  "if __name__ == \"__main__\":" \n
  str "()")

(python-skeleton-define typechecking nil
  "Imports: "
  "from typing import TYPE_CHECKING" \n \n
  "if TYPE_CHECKING:" \n
  > "from typing import " str _)

;;;###autoload
(defun python-add-class-str ()
  "Utility function to quickly created a __str__ method.
Inserts the text at point with proper indention."
  (interactive)
  (python--add-class-method "str" nil "str" "return \"\""))

;;;###autoload
(defun python-add-class-repr ()
  "Utility function to quickly created a __repr__ method.
Inserts the text at point with proper indention."
  (interactive)
  (python--add-class-method "repr" nil "repr" "return \"\""))

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

;;;###autoload
(defun python-add-class-setattr ()
  "Utility function that adds a __setattr__ method.
Inserts the text at point with proper indention."
  (interactive)
  (python--add-class-method "setattr" "key: str, value" "None"))

;;;###autoload
(defun python-add-class-len ()
  "Utility function that adds a __len__ method.
Inserts the text at point with proper indention."
  (interactive)
  (python--add-class-method "len" nil "int"))

;;;###autoload
(defun python-add-class-eq ()
  "Utility function that adds a __eq__ method.
Inserts the text at point with proper indention."
  (interactive)
  (python--add-class-method "eq" "other" "bool" "..."))

;;;###autoload
(defun python-add-class-ne ()
  "Utility function that adds a __ne__ method.
Inserts the text at point with proper indention."
  (interactive)
  (python--add-class-method "ne" "other" "bool" "..."))

;;;###autoload
(defun python-add-class-lt ()
  "Utility function that adds a __lt__ method.
Inserts the text at point with proper indention."
  (interactive)
  (python--add-class-method "lt" "other" "bool" "..."))

;;;###autoload
(defun python-add-class-le ()
  "Utility function that adds a __le__ method.
Inserts the text at point with proper indention."
  (interactive)
  (python--add-class-method "le" "other" "bool" "..."))

;;;###autoload
(defun python-add-class-gt ()
  "Utility function that adds a __gt__ method.
Inserts the text at point with proper indention."
  (interactive)
  (python--add-class-method "gt" "other" "bool" "..."))

;;;###autoload
(defun python-add-class-ge ()
  "Utility function that adds a __ge__ method.
Inserts the text at point with proper indention."
  (interactive)
  (python--add-class-method "ge" "other" "bool" "..."))

(defun python--add-class-method (name argstring rettype &rest body)
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
    (define-key map (kbd "C-c i r") 'python-skeleton-runmod)
    (define-key map (kbd "C-c i t") 'python-skeleton-typechecking)

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
	["Add __repr__ method" python-add-class-repr]
	["Add __getitem__ method" python-add-class-getitem]
	["Add __getattr__ method" python-add-class-getattr]
	["Add __setattr__ method" python-add-class-setattr]
	["Add __len__ method" python-add-class-len]
	("Comparison Methods"
	 ["Add __eq__  method" python-add-class-eq]
	 ["Add __ne__  method" python-add-class-ne]
	 ["Add __lt__  method" python-add-class-lt]
	 ["Add __le__  method" python-add-class-le]
	 ["Add __gt__  method" python-add-class-gt]
	 ["Add __ge__  method" python-add-class-ge])
	"--"
	("Skeletons"
	 :help "A submenu for skeletons"
	 ["Add __init__ method" python-skeleton-clsinit]
	 ["Add __main__ if body" python-skeleton-runmod]
	 ["Add TYPE_CHECKING code" python-skeleton-typechecking])
	;; "--"
	;; ("LSP"
	;;  :active (ex-python--lsp-enabled-p)
	;;  :help "A submenu for lsp commands."
	;;  ["Find definition" lsp-find-definition])
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
