;;; ex-elisp.el --- Minor mode that adds ELisp utilities

;; Copyright (C) 2023 John Russell
;; Author: John Russell <johndevlopment7@gmail.com>
;; Keywords: languages
;; Package-Version: 1.0-alpha1
;; Version: 1.0-alpha1
;;
;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; This adds functions utility functions for ELisp that are not in the default
;; default package.
;;
;; Commands:
;;
;;     `elisp-uncomment-region': uncomment a region. Does the reverse of

;;; Code:

(defgroup ex-elisp nil
  "Extra functionality for `emacs-lisp-mode'."
  :group 'lisp)

;; Default keymap
(defvar ex-elisp-mode-map
  (let ((map (make-sparse-keymap)))
    (easy-menu-define ex-elisp-menu map "Ex-Elisp Mode menu"
      '("Ex-Elisp"
	["Uncomment Region" elisp-uncomment-region]
	["Disable Ex-Elisp mode" ex-elisp-mode]))
    map)
  "Default bindings for ex-elisp mode.")

;;;###autoload
(defun elisp-uncomment-region (beg end &optional arg)
  "Uncomment each line in the region."
  (interactive "*r")
  (if (use-region-p)
      (comment-region beg end '(4))))

;;;###autoload
(define-minor-mode ex-elisp-mode
  "This minor mode should only be used while in Python mode. It
adds extra commands that are not provided in Python mode.

If `ex-elisp-use-lsp' is set to a non-nill value, and `lsp'
is installed and Emacs configured to use it, the Ex-Elisp menu
contains a submenu dedicated to lsp commands.

\\{ex-elisp-mode-map}"

  :lighter " xlisp"
  :interactive '(emacs-lisp-mode)
  :init-value nil
  :keymap ex-elisp-mode-map
  :group 'ex-elisp)

;; Internal functions

(provide 'ex-elisp)

;;; ex-elisp.el ends here
