;;; ex-python.el --- Minor mode for more Python features

;; Copyright (C) 2023 John Russell
;; Author: John Russell <johndevlopment7@gmail.com>
;; Keywords: languages
;; Package-Version: 1.0-alpha1
;; Version: 1.0-alpha1
;;
;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; Extra functions that are not available in the default Python
;; mode implementation. These are functions that I wanted for
;; myself

;;; Code:

(require 'python)

(defgroup ex-python nil
  "Extra functionality for Python mode."
  :group 'python)

(defcustom ex-python-lsp-enabled nil
  "Non-nil means that lsp is enabled.
Enables lsp-related functions."
  :type 'boolean
  :safe 'booleanp)

;;;###autoload
(defun python-add-docstring (initval yn)
  "Add a docstring to the function."
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

;; Default keymap

(defvar ex-python-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c i d") 'python-add-docstring)

    (easy-menu-define ex-python-menu map "Ex-Python Mode menu"
      '("Ex-Python"
	:help "More Python-specific Features"
	["Generate Docstring" python-add-docstring]
	"--"
	("LSP"
	 :active (ex-python--lsp-enabled-p)
	 ["Find definition" lsp-find-definition])
	"---"
	["Disable Ex-Python mode" ex-python-mode]))
    map)
  "Default bindings for ex-python mode.")

;;;###autoload
(define-minor-mode ex-python-mode
  "This minor mode should only be used while in Python mode. It
adds extra commands that are not provided in Python mode.

If `ex-python-lsp-enabled' is set to a non-nill value, and `lsp'
is installed and Emacs configured to use it, the Ex-Python menu
contains a submenu dedicated to lsp commands."

  :lighter " xpy"
  :interactive '(python-mode)
  :init-value nil
  :keymap ex-python-mode-map
  :group 'ex-python)

;; Internal functions

(defun ex-python--lsp-enabled-p ()
  (and
   ex-python-lsp-enabled
   (fboundp 'lsp)))

(provide 'ex-python)

;;; ex-python.el ends here
