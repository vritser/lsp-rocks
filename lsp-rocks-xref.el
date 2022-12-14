;;; lsp-rocks-xref.el --- async xref                          -*- lexical-binding: t; -*-

;; Copyright (C) 2021  vritser

;; Author: vritser <vritser@gmail.com>
;; Keywords: LSP

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'xref)

(cl-defgeneric xref-backend-definitions (backend identifier callback)
  "Find definitions of IDENTIFIER.

The result must be a list of xref objects.  If IDENTIFIER
contains sufficient information to determine a unique definition,
return only that definition.  If there are multiple possible
definitions, return all of them.  If no definitions can be found,
return nil.

IDENTIFIER can be any string returned by
`xref-backend-identifier-at-point', or from the table returned by
`xref-backend-identifier-completion-table'.

To create an xref object, call `xref-make'.")

(cl-defgeneric xref-backend-references (_backend identifier callback)
  "Find references of IDENTIFIER.
The result must be a list of xref objects.  If no references can
be found, return nil.

The default implementation uses `semantic-symref-tool-alist' to
find a search tool; by default, this uses \"find | grep\" in the
current project's main and external roots."
  (mapcan
   (lambda (dir)
     (message "Searching %s..." dir)
     (redisplay)
     (prog1
         (xref-references-in-directory identifier dir)
       (message "Searching %s... done" dir)))
   (let ((pr (project-current t)))
     (cons
      (xref--project-root pr)
      (project-external-roots pr)))))

(cl-defgeneric xref-backend-implementations (backend identifier callback)
  "Find implementations of IDENTIFIER.
The result must be a list of xref objects.  If there are multiple possible
implementations, return all of them.  If no implementations can be found,
return nil.

IDENTIFIER can be any string returned by
`xref-backend-identifier-at-point', or from the table returned by
`xref-backend-identifier-completion-table'.

To create an xref object, call `xref-make'.")


(defun xref--show-xref-buffer (xrefs alist)
  (let* ((_xrefs
          (or
           (assoc-default 'fetched-xrefs alist)
           xrefs))
         (xref-alist (xref--analyze _xrefs))
         (dd default-directory)
         buf)
    (with-current-buffer (get-buffer-create xref-buffer-name)
      (setq default-directory dd)
      (xref--xref-buffer-mode)
      (xref--show-common-initialize xref-alist _xrefs alist)
      (pop-to-buffer (current-buffer))
      (setq buf (current-buffer)))
    (xref--auto-jump-first buf (assoc-default 'auto-jump alist))
    buf))

(defun xref-show-definitions-buffer (xrefs alist)
  "Show the definitions list in a regular window.

When only one definition found, jump to it right away instead."
  (let (buf)
    (cond
     ((not (cdr xrefs))
      (xref-pop-to-location (car xrefs)
                            (assoc-default 'display-action alist)))
     (t
      (setq buf
            (xref--show-xref-buffer xrefs
                                    (cons (cons 'fetched-xrefs xrefs)
                                          alist)))
      (xref--auto-jump-first buf (assoc-default 'auto-jump alist))
      buf))))

(defun xref--show-xrefs (xrefs display-action &optional _always-show-list)
  (xref--push-markers)
  (funcall xref-show-xrefs-function xrefs
           `((window . ,(selected-window))
             (display-action . ,display-action)
             (auto-jump . ,xref-auto-jump-to-first-xref))))

(defun xref--show-defs (xrefs display-action)
  (xref--push-markers)
  (funcall xref-show-definitions-function xrefs
           `((window . ,(selected-window))
             (display-action . ,display-action)
             (auto-jump . ,xref-auto-jump-to-first-definition))))

(defun xref--create-fetcher (input kind arg callback)
  "Return an xref list fetcher function.

It revisits the saved position and delegates the finding logic to
the xref backend method indicated by KIND and passes ARG to it."
  (let* ((orig-buffer (current-buffer))
         (orig-position (point))
         (backend (xref-find-backend))
         (method (intern (format "xref-backend-%s" kind))))
    (save-excursion
      ;; Xref methods are generally allowed to depend on the text
      ;; around point, not just on their explicit arguments.
      ;;
      ;; There is only so much we can do, however, to recreate that
      ;; context, given that the user is free to change the buffer
      ;; contents freely in the meantime.
      (when (buffer-live-p orig-buffer)
        (set-buffer orig-buffer)
        (ignore-errors (goto-char orig-position)))
      (if (member (symbol-name backend) '("elisp" "etags"))
          (funcall callback (funcall method backend arg))
        (funcall method backend arg callback)))))

(defun xref--find-xrefs (input kind arg display-action)
  (let ((callback (lambda (display-action xrefs)
                    (xref--show-xrefs xrefs display-action))))
    (xref--create-fetcher input kind arg
                          (apply-partially callback display-action))))

(defun xref--find-definitions (id display-action)
  (let ((callback (lambda (display-action xrefs)
                    (xref--show-defs xrefs display-action))))
    (xref--create-fetcher id 'definitions id
                          (apply-partially callback display-action))))

;;;###autoload
(defun xref-find-implementations (identifier)
  "Find implementations to the identifier at point.
This command might prompt for the identifier as needed, perhaps
offering the symbol at point as the default.
With prefix argument, or if `xref-prompt-for-identifier' is t,
always prompt for the identifier.  If `xref-prompt-for-identifier'
is nil, prompt only if there's no usable symbol at point."
  (interactive (list (xref--read-identifier "Find references of: ")))
  (xref--find-xrefs identifier 'implementations identifier nil))

(provide 'lsp-rocks-xref)

;;; lsp-rocks-xref.el ends here
