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
    (setq org-src-code new-name)))



(advice-remove 'org-edit-src-code #'sicp-org-set-src-name)
(advice-add 'org-edit-src-code :around #'sicp-org-set-src-name)

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

(provide 'sicp)
;;; sicp.el ends here
