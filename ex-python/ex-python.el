;;; ex-python.el --- Minor mode for more Python features

;; Copyright (C) 2023 John Russell
;; Author: John Russell <johndevlopment7@gmail.com>
;; Keywords: languages
;; Package-Version: 1.0-alpha7
;; Version: 1.0-alpha7
;;
;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; Extra functions that are not available in the default Python mode
;; implementation.
;;
;; Commands:
;;
;;     `python-add-docstring': adds a docstring to the current function.
;;
;;     `python-add-class-new': adds a __new__ method to a class.
;;
;;     `python-add-class-str': adds a __str__ method to a class.
;;
;;     `python-add-class-repr': adds a __repr__ method to a class.
;;
;;     `python-add-class-len': adds a __len__ method to a class.
;;
;; -- Attribute access functions --
;;
;;     `python-add-class-setitem': adds a __setitem__ method to a class.
;;
;;     `python-add-class-getitem': adds a __getitem__ method to a class.
;;
;;     `python-add-class-getattr': adds a __getattr__ method to a class.
;;
;;     `python-add-class-setattr': adds a __setattr__ method to a class.
;;
;; -- Descriptor functions --
;;
;;     `python-add-class-get': adds a __get__ method to a class.
;;
;;     `python-add-class-set': adds a __set__ method to a class.
;;
;;     `python-add-class-setname': adds a __set_name__ method to a class.
;;
;; -- Comparison functions --
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
;;
;; -- Math functions --
;;
;;     `python-add-class-add': adds a __add__ method to a class.
;;
;;     `python-add-class-sub': adds a __sub__ method to a class.
;;
;;     `python-add-class-mul': adds a __mul__ method to a class.
;;
;;     `python-add-class-matmul': adds a __matmul__ method to a class.
;;
;;     `python-add-class-truediv': adds a __truediv__ method to a class.
;;
;;     `python-add-class-floordiv': adds a __floordiv__ method to a class.
;;
;;     `python-add-class-mod': adds a __mod__ method to a class.
;;
;;     `python-add-class-pow': adds a __pow__ method to a class.
;;
;;     `python-add-class-lshift': adds a __lshift__ method to a class.
;;
;;     `python-add-class-rshift': adds a __rshift__ method to a class.
;;
;;     `python-add-class-and': adds a __and__ method to a class.
;;
;;     `python-add-class-xor': adds a __xor__ method to a class.
;;
;;     `python-add-class-or': adds a __or__ method to a class.
;;
;;     `python-add-class-radd': adds a __radd__ method to a class.
;;
;;     `python-add-class-rsub': adds a __rsub__ method to a class.
;;
;;     `python-add-class-rmul': adds a __rmul__ method to a class.
;;
;;     `python-add-class-rmatmul': adds a __rmatmul__ method to a class.
;;
;;     `python-add-class-rtruediv': adds a __rtruediv__ method to a class.
;;
;;     `python-add-class-rfloordiv': adds a __rfloordiv__ method to a class.
;;
;;     `python-add-class-rmod': adds a __rmod__ method to a class.
;;
;;     `python-add-class-rpow': adds a __rpow__ method to a class.
;;
;;     `python-add-class-rlshift': adds a __rlshift__ method to a class.
;;
;;     `python-add-class-rrshift': adds a __rrshift__ method to a class.
;;
;;     `python-add-class-rand': adds a __rand__ method to a class.
;;
;;     `python-add-class-rxor': adds a __rxor__ method to a class.
;;
;;     `python-add-class-ror': adds a __ror__ method to a class.
;;

;;; Code:

(require 'python)

(defgroup ex-python nil
  "Extra functionality for Python mode."
  :group 'python)

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
  "def " str "():" \n
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
(defun python-add-class-new ()
  "Add a boilerplate __new__ method, properly indented at point."
  (interactive)
  (let ((standard-output (current-buffer)))
    (princ "def __new__(cls, name: str, bases: tuple[type, ...], namespace: dict[str, Any]):")
    (newline)
    (indent-for-tab-command)
    (princ "pass")))

;;;###autoload
(defun python-add-class-len ()
  "Add a boilerplate __len__ method, properly indented at point."
  (interactive)
  (python--add-class-method "len" nil "int" "return 0"))

;;;###autoload
(defun python-add-class-str ()
  "Add a boilerplate __str__ method, properly indented at point."
  (interactive)
  (python--add-class-method "str" nil "str" "return \"\""))

;;;###autoload
(defun python-add-class-repr ()
  "Add a boilerplate __repr__ method, properly indented at point."
  (interactive)
  (python--add-class-method "repr" nil "repr" "return \"\""))

;;;###autoload
(defun python-add-class-setitem ()
  "Write a boilerplate __setitem__ method, properly indented at point."
  (interactive)
  (python--add-class-method "setitem" "key, value, /" "None"))

;;;###autoload
(defun python-add-class-getitem ()
  "Add a boilerplate __getitem__ method, properly indented at point."
  (interactive)
  (python--add-class-method "getitem" "key, /" nil))

;;;###autoload
(defun python-add-class-getattr ()
  "Add a boilerplate __getitem__ method, properly indented at point."
  (interactive)
  (python--add-class-method "getattr" "key: str" nil))

;;;###autoload
(defun python-add-class-setattr ()
  "Add a boilerplate __setattr__ method, properly indented at point."
  (interactive)
  (python--add-class-method "setattr" "key: str, value" "None"))

;;;###autoload
(defun python-add-class-get ()
  "Write a boilerplate __get__ method, properly indented at point."
  (interactive)
  (python--add-class-method "get" "obj, objtype=None" nil))

;;;###autoload
(defun python-add-class-set ()
  "Write a boilerplate __set__ method, properly indented at point."
  (interactive)
  (python--add-class-method "set" "obj, value" "None"))

;;;###autoload
(defun python-add-class-setname ()
  "Write a boilerplate __set_name__ method, properly indented at point."
  (interactive)
  (python--add-class-method "set_name" "owner, name: str" "None"))

(defmacro python-define-comparison-method (name)
  "Defines a function to make a comparison method. The resulting function will have
a name like python-add-class-NAME."
  (declare (indent 2))
  (let* ((name (symbol-name name))
	 (fn (intern (concat "python-add-class-" name))))
    `(progn
       (defun ,fn ()
	 ,(format "Utility function that adds a __%s__ method. Inserts the text with proper
indentation." name)
	 (interactive)
	 (python--add-class-method ,name "other" "bool" "...")))))

;; (macroexpand '(python-define-comparison-method eq))

(python-define-comparison-method eq)
(python-define-comparison-method ne)
(python-define-comparison-method gt)
(python-define-comparison-method ge)
(python-define-comparison-method lt)
(python-define-comparison-method le)

(defmacro python-define-math-method (name op)
  "Defines a function to make a math method. The resulting function will have a
name like python-add-class-NAME."
  (declare (indent 2))
  (let* ((name (symbol-name name))
	 (fn (intern (concat "python-add-class-" name))))
    `(progn
       (defun ,fn ()
	 ,(format "Adds a __%s__ method to the current class. Inserts the text with proper
indentation." name)
	 (interactive)
	 (python--add-class-method ,name "other" ""
				   ,(format "return self %s other" (symbol-name op)))))))

(python-define-math-method add +)
(python-define-math-method sub -)
(python-define-math-method mul *)
(python-define-math-method matmul @)
(python-define-math-method truediv /)
(python-define-math-method floordiv //)
(python-define-math-method mod %)
(python-define-math-method pow **)
(python-define-math-method lshift <<)
(python-define-math-method rshift >>)
(python-define-math-method and &)
(python-define-math-method xor ^)
(python-define-math-method or |)
(python-define-math-method radd +)
(python-define-math-method rsub -)
(python-define-math-method rmul *)
(python-define-math-method rmatmul @)
(python-define-math-method rtruediv /)
(python-define-math-method rfloordiv //)
(python-define-math-method rmod %)
(python-define-math-method rpow **)
(python-define-math-method rlshift <<)
(python-define-math-method rrshift >>)
(python-define-math-method rand &)
(python-define-math-method rxor ^)
(python-define-math-method ror |)

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
	["Add __len__ method" python-add-class-len]
	("Attribute Access"
	 ["Add __getitem__ method" python-add-class-getitem]
	 ["Add __setitem__ method" python-add-class-setitem]
	 ["Add __getattr__ method" python-add-class-getattr]
	 ["Add __setattr__ method" python-add-class-setattr])
	("Descriptors"
	 ["Add __get__ method" python-add-class-get]
	 ["Add __set__ method" python-add-class-set]
	 ["Add __set_name__ method" python-add-class-setname])
	("Comparison Methods"
	 ["Add __eq__  method" python-add-class-eq]
	 ["Add __ne__  method" python-add-class-ne]
	 ["Add __lt__  method" python-add-class-lt]
	 ["Add __le__  method" python-add-class-le]
	 ["Add __gt__  method" python-add-class-gt]
	 ["Add __ge__  method" python-add-class-ge])
	("Math Methods"
	 ["Add __add__ method" python-add-class-add]
	 ["Add __sub__ method" python-add-class-sub]
	 ["Add __mul__ method" python-add-class-mul]
	 ["Add __matmul__ method" python-add-class-matmul]
	 ["Add __truediv__ method" python-add-class-truediv]
	 ["Add __floordiv__ method" python-add-class-floordiv]
	 ["Add __mod__ method" python-add-class-mod]
	 ["Add __pow__ method" python-add-class-pow]
	 ["Add __lshift__ method" python-add-class-lshift]
	 ["Add __rshift__ method" python-add-class-rshift]
	 ["Add __and__ method" python-add-class-and]
	 ["Add __xor__ method" python-add-class-xor]
	 ["Add __or__ method" python-add-class-or]
	 ["Add __radd__ method" python-add-class-radd]
	 ["Add __rsub__ method" python-add-class-rsub]
	 ["Add __rmul__ method" python-add-class-rmul]
	 ["Add __rmatmul__ method" python-add-class-rmatmul]
	 ["Add __rtruediv__ method" python-add-class-rtruediv]
	 ["Add __rfloordiv__ method" python-add-class-rfloordiv]
	 ["Add __rmod__ method" python-add-class-rmod]
	 ["Add __rpow__ method" python-add-class-rpow]
	 ["Add __rlshift__ method" python-add-class-rlshift]
	 ["Add __rrshift__ method" python-add-class-rrshift]
	 ["Add __rand__ method" python-add-class-rand]
	 ["Add __rxor__ method" python-add-class-rxor]
	 ["Add __ror__ method" python-add-class-ror])
	"--"
	("Skeletons"
	 :help "A submenu for skeletons"
	 ["Add __init__ method" python-skeleton-clsinit]
	 ["Add __main__ if body" python-skeleton-runmod]
	 ["Add TYPE_CHECKING code" python-skeleton-typechecking])
	"---"
	["Disable Ex-Python mode" ex-python-mode]))
    map)
  "Default bindings for ex-python mode.")

;;;###autoload
(define-minor-mode ex-python-mode
  "This minor mode should only be used while in Python mode. It
adds extra commands that are not provided in Python mode.

\\{ex-python-mode-map}"

  :lighter " xpy"
  :interactive '(python-mode)
  :init-value nil
  :keymap ex-python-mode-map
  :group 'ex-python)

;; Internal functions

(provide 'ex-python)

;;; ex-python.el ends here
