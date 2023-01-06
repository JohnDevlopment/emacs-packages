;;; ex-python.el --- Minor mode for more Python features

;; Copyright (C) 2023 John Russell
;; Author: John Russell <johndevlopment7@gmail.com>
;; Keywords: languages
;; Package-Version: 1.0-alpha
;; Version: 1.0-alpha
;;
;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; Extra functions that are not available in the default Python
;; mode implementation. These are functions that I wanted for
;; myself

;;; Code:

(require 'python)

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
	["Generate Docstring" python-add-docstring]))
    map)
  "Default bindings for ex-python mode.")

;;;###autoload
(define-minor-mode ex-python-mode
  "This minor mode should only be used while in Python mode. It
adds extra commands that are not provided in Python mode."

  :lighter " xpy"
  :interactive '(python-mode)
  :init-value nil
  :keymap ex-python-mode-map
  :group 'ex-python)

;; (defun ex-python-init ()
;;   (define-prefix-command 'ex-python-map)
;;   (global-set-key (kbd "C-c i") 'ex-python-map)
;;   (define-key ex-python-map "d" 'python-add-docstring))

;; (add-hook 'python-mode-hook 'ex-python-init)

(provide 'ex-python)

;;; ex-python.el ends here
