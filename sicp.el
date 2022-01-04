;;; sicp.el --- Collections of utils for using sicp -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Enrico Tolotto
;;
;; Author: Enrico Tolotto <https://github.com/enrico>
;; Maintainer: Enrico Tolotto <etolotto@gmail.com>
;; Created: December 27, 2021
;; Modified: December 27, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/enrico/sicp
;; Package-Requires: ((emacs "27.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
;;;
(require 'org)


(defun sicp-org-set-src-name (orig-fun &rest args)
  "Function for naming the CODE used in EDIT-BUFFER-NAME."
  (let* ((name (plist-get (plist-get (org-element-at-point) 'src-block) ':name))
         (new-name (if name
                       (concat name ".rkt")
                     (caddr args))))
    (apply orig-fun (list (car args) new-name))
    (make-local-variable 'org-src-code)
    (setq org-src-code (expand-file-name new-name))))

(defun sicp-local-var-default (var-name default)
  "Get the value of the local variable VAR-NAME or use the DEFAULT value."
  (if (buffer-local-boundp var-name (current-buffer))
      (buffer-local-value var-name (current-buffer))
    default))

(defun sicp-run-racket-repl (orig-fun &rest args)
  "ORIG-FUN ARGS."
  (when (buffer-local-boundp 'org-src-code (current-buffer))
    (let* ((name (buffer-local-value 'org-src-code (current-buffer)))
           (content (buffer-substring-no-properties (point-min) (point-max))))
      (with-temp-file name
          (insert content))
      (setq args (cons (list name) (cdr args)))))
  (apply orig-fun args))

(advice-remove 'org-edit-src-code #'sicp-org-set-src-name)
(advice-remove 'racket--repl-run #'sicp-run-racket-repl)
(advice-add 'org-edit-src-code :around #'sicp-org-set-src-name)
(advice-add 'racket--repl-run :around #'sicp-run-racket-repl)

(defun sicp-add-exercise (number)
  "Function for adding a new exercise NUMBER for sicp."
  (interactive "nInsert exercise number: ")
  (save-excursion
    (save-restriction
      (goto-char (point-max))
      (insert "\n")
      (insert (format "* Exercise %s" number))
      (insert "\n")
      (insert (format "#+NAME: %s" number))
      (org-insert-structure-template "src racket"))))

(defun sicp-add-lang-and-lib ()
  "Function for adding the racket lang and sicp library."
  (interactive)
  (insert "#lang racket")
  (insert "\n\n")
  (insert "(require \"../SicpLibrary/sicp-library.rkt\")"))

(provide 'sicp)
;;; sicp.el ends here
