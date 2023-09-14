;;; lsp-rocks.el --- LSP Rocks                          -*- lexical-binding: t; -*-

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
(require 'cl-lib)
(require 'json)
(require 'seq)
(require 's)
(require 'subr-x)
(require 'websocket)
(require 'lsp-rocks-xref)
(require 'posframe)
(require 'markdown-mode)
(require 'company)

(defgroup lsp-rocks nil
  "LSP-Rocks group."
  :group 'applications)

(defcustom lsp-rocks-server-bin (concat (substring load-file-name 0 (s-index-of "lsp-rocks.el" load-file-name)) "lib/cli.js")
  "Location of lsp-rocks server."
  :type 'string
  :group 'lsp-rocks)

(defcustom lsp-rocks-name "*lsp-rocks*"
  "LSP-Rocks process buffer."
  :type 'string
  :group 'lsp-rocks)

(defcustom lsp-rocks-server-host "0.0.0.0"
  "LSP-Rocks server host."
  :type 'string
  :group 'lsp-rocks)

(defvar lsp-rocks--server-port nil)

(defvar lsp-rocks--server-process nil)

(defvar lsp-rocks--uri-file-prefix (pcase system-type
                               (`windows-nt "file:///")
                               (_ "file://"))
  "Prefix for a file-uri.")

(defvar-local lsp-rocks-buffer-uri nil
  "If set, return it instead of calculating it using `buffer-file-name'.")

(defcustom lsp-rocks-mark-ring-max-size 16
  "Maximum size of lsp-rocks mark ring.  \
Start discarding off end if gets this big."
  :type 'integer
  :group 'lsp-rocks)

(defcustom lsp-rocks-flash-line-delay .3
  "How many seconds to flash `lsp-rocks-font-lock-flash' after navigation.

Setting this to nil or 0 will turn off the indicator."
  :type 'number
  :group 'lsp-rocks)

(defface lsp-rocks-font-lock-flash
  '((t (:inherit highlight)))
  "Face to flash the current line."
  :group 'lsp-rocks)

(defvar lsp-rocks--mark-ring nil
  "The list of saved lsp-rocks marks, most recent first.")

(defvar lsp-rocks--last-prefix nil)

(defvar lsp-rocks--websocket-clients (make-hash-table :test 'equal :size 16)
  "LSP-Rocks websocket connection.")

(defvar lsp-rocks--recent-requests (make-hash-table :test 'equal :size 32)
  "LSP-Rocks websocket connection.")

(defvar lsp-rocks--xref-callback nil
  "XREF callback.")

(defvar lsp-rocks--company-callback nil
  "Company callback.")

(defvar lsp-rocks-language-server-configuration
  (list (list 'rust-mode (list :name "rust" :command "rust-analyzer" :args (vector)))
        (list 'python-mode (list :name "python" :command "pyright-langserver" :args (vector "--stdio")))
        (list 'java-mode (list :name "java" :command "jdtls" :args (vector)))
        (list 'typescript-mode (list :name "typescript" :command "typescript-language-server" :args (vector "--stdio")))))

(defvar-local lsp-rocks--before-change-begin-pos nil)

(defvar-local lsp-rocks--before-change-end-pos nil)

(defvar-local lsp-rocks--current-file-version 0)

(defconst lsp-rocks--kind->symbol
  '((1 . text)
    (2 . method)
    (3 . function)
    (4 . constructor)
    (5 . field)
    (6 . variable)
    (7 . class)
    (8 . interface)
    (9 . module)
    (10 . property)
    (11 . unit)
    (12 . value)
    (13 . enum)
    (14 . keyword)
    (15 . snippet)
    (16 . color)
    (17 . file)
    (18 . reference)
    (19 . folder)
    (20 . enum-member)
    (21 . constant)
    (22 . struct)
    (23 . event)
    (24 . operator)
    (25 . type-parameter)))

(defun lsp-rocks--suggest-project-root ()
  "Get project root."
  (or
   (when (featurep 'projectile)
     (condition-case nil
         (projectile-project-root)
       (error nil)))
   (when (featurep 'project)
     (when-let ((project (project-current)))
       (if (fboundp 'project-root)
           (project-root project)
         (car (with-no-warnings
                (project-roots project))))))
   default-directory))

(defun lsp-rocks--websocket-client-key ()
  "Make websocket client hashtable key."
  (format "%s:%s" (lsp-rocks--suggest-project-root) (string-replace "-mode" "" (symbol-name major-mode))))

(defun lsp-rocks--save-websocket-client (client)
  "Put the websocket CLIENT to `lsp-rocks--websocket-clients'."
  (puthash
   (lsp-rocks--websocket-client-key)
   client
   lsp-rocks--websocket-clients))

(defun lsp-rocks--get-websocket-client ()
  "Get current websocket client from `lsp-rocks--websocket-clients'."
  (gethash (lsp-rocks--websocket-client-key) lsp-rocks--websocket-clients))

(defun lsp-rocks--buffer-uri ()
  "Return URI of the current buffer."
  (or lsp-rocks-buffer-uri (lsp-rocks--path-to-uri buffer-file-name)))

(defconst lsp-rocks--url-path-allowed-chars
  (url--allowed-chars (append '(?/) url-unreserved-chars))
  "`url-unreserved-chars' with additional delim ?/.
This set of allowed chars is enough for hexifying local file paths.")

(defun lsp-rocks--path-to-uri (path)
  "Convert PATH to a uri."
  (concat lsp-rocks--uri-file-prefix
          (url-hexify-string (file-truename path) lsp-rocks--url-path-allowed-chars)))

(defun lsp-rocks--buffer-language-conf ()
  "Get language corresponding current buffer."
  (cl-some (lambda (it)
             (let ((mode-or-pattern (car it)))
               (cond
                ((and (stringp mode-or-pattern)
                      (s-matches? mode-or-pattern (buffer-file-name))) (cadr it))
                ((eq mode-or-pattern major-mode) (cadr it)))))
           lsp-rocks-language-server-configuration))

(defun lsp-rocks--websocket-on-open-handler (socket)
  (let* ((config (lsp-rocks--buffer-language-conf))
         (language (plist-get config :name))
         (command (plist-get config :command))
         (args (plist-get config :args)))
    (lsp-rocks--request "init"
                        (list :project (lsp-rocks--suggest-project-root)
                              :language language
                              :command command
                              :args args
                              :clientInfo (list :name "Emacs" :version (emacs-version))))))

(defun lsp-rocks--websocket-message-handler (socket frame)
  (let* ((msg (lsp-rocks--json-parse (websocket-frame-payload frame)))
         (id (plist-get msg :id))
         (cmd (plist-get msg :cmd))
         (params (plist-get msg :params))
         (data (plist-get msg :data)))
    (when (string= id (gethash cmd lsp-rocks--recent-requests))
      (pcase cmd
        ("get_var" (lsp-rocks--response id cmd (list :value (symbol-value (intern (plist-get params :name))))))
        ("textDocument/completion" (funcall lsp-rocks--company-callback (lsp-rocks--parse-completion data)))
        ("completionItem/resolve" (lsp-rocks--process-completion-resolve data))
        ("textDocument/definition" (lsp-rocks--process-find-definition data))
        ("textDocument/typeDefinition" (lsp-rocks--process-find-definition data))
        ("textDocument/declaration" (lsp-rocks--process-find-definition data))
        ("textDocument/references" (lsp-rocks--process-find-definition data))
        ("textDocument/implementation" (lsp-rocks--process-find-definition data))
        ("textDocument/hover" (lsp-rocks--process-hover data))
        ("textDocument/signatureHelp" (lsp-rocks--process-signature-help data))
        ("textDocument/prepareRename" (lsp-rocks--process-prepare-rename data))
        ("textDocument/rename" (lsp-rocks--process-rename data))
        ))))

(defun lsp-rocks--create-websocket-client (url)
  "Create a websocket client that connects to URL."
  (websocket-open
   url
   :on-open
   #'lsp-rocks--websocket-on-open-handler
   :on-message
   #'lsp-rocks--websocket-message-handler
   :on-error (lambda (ws type err)
               (message "error occured: %s" err))
   :on-close (lambda (ws)
               (message "connection closed"))))

(defun lsp-rocks--get-free-port ()
  (save-excursion
    (let* ((process-buffer "*lsp-rocks-temp*")
           (process (make-network-process
                     :name process-buffer
                     :buffer process-buffer
                     :family 'ipv4
                     :server t
                     :host "127.0.0.1"
                     :service t))
           process-info)
      (setq process-info (process-contact process))
      (delete-process process)
      (kill-buffer process-buffer)
      (format "%s" (cadr process-info)))))

(defun lsp-rocks-restart ()
  "Restart."
  (lsp-rocks-shutdown)
  (lsp-rocks--start-server)
  (message "[LSP-Rocks] Server restarted."))

(defun lsp-rocks--start-server ()
  "Start the server."
  (unless lsp-rocks--server-port
    (setq lsp-rocks--server-port (lsp-rocks--get-free-port)))
  (setq lsp-rocks--server-process
        (start-process-shell-command
         lsp-rocks-name
         lsp-rocks-name
         (concat lsp-rocks-server-bin " " lsp-rocks--server-port))))

(defun lsp-rocks-shutdown ()
  "Shutdown LSP Rocks Server and reset all variables."
  (interactive)
  (lsp-rocks--kill-server-process)
  (setq lsp-rocks-mode nil))

(defun lsp-rocks--kill-server-process ()
  "Kill LSP-Rocks server process."
  (when (get-buffer lsp-rocks-name)
    (dolist (client (hash-table-values lsp-rocks--websocket-clients))
      (when (eq (websocket-ready-state client) 'open)
        (websocket-close client)))
    (when (process-live-p lsp-rocks--server-process)
      (kill-process lsp-rocks--server-process))
    (kill-buffer lsp-rocks-name)
    (setq lsp-rocks--server-process nil
          lsp-rocks--server-port nil
          lsp-rocks--websocket-clients (clrhash lsp-rocks--websocket-clients)))
  (message "[LSP-Rocks] Server terminated."))

;; (add-hook 'kill-emacs-hook #'lsp-rocks--kill-server-process)

(defconst lsp-rocks--trigger-characters
  '("." "\"" "'" "/" "@" "<"))

(defun lsp-rocks--completion-prefix ()
  "Return the completion prefix.
Return value is compatible with the `prefix' command of a company backend.
Return nil if no completion should be triggered.  Return a string
as the prefix to be completed, or a cons cell of (prefix . t) to bypass
`company-minimum-prefix-length' for trigger characters."
  (or (let* ((max-trigger-len (apply 'max (mapcar (lambda (trigger-char)
                                                    (length trigger-char))
                                                  lsp-rocks--trigger-characters)))
             (trigger-regex (s-join "\\|" (mapcar #'regexp-quote lsp-rocks--trigger-characters)))
             (symbol-cons (company-grab-symbol-cons trigger-regex max-trigger-len)))
        ;; Some major modes define trigger characters as part of the symbol. For
        ;; example "@" is considered a vaild part of symbol in java-mode.
        ;; Company will grab the trigger character as part of the prefix while
        ;; the server doesn't. Remove the leading trigger character to solve
        ;; this issue.
        (let* ((symbol (if (consp symbol-cons)
                           (car symbol-cons)
                         symbol-cons))
               (trigger-char (seq-find (lambda (trigger-char)
                                         (s-starts-with? trigger-char symbol))
                                       lsp-rocks--trigger-characters)))
          (if trigger-char
              (cons (substring symbol (length trigger-char)) t)
            symbol-cons)))
      (company-grab-symbol)))

(defun lsp-rocks--company-post-completion (candidate)
  "Replace a CompletionItem's label with its insertText.  Apply text edits.

CANDIDATE is a string returned by `company-lsp--make-candidate'."
  (let* ((resolved (get-text-property 0 'resolved-item candidate))
         (label (plist-get resolved :label))
         ;; (start (- (point) (length label)))
         (insertText (plist-get resolved :insertText))
         ;; 1 = plaintext, 2 = snippet
         (insertTextFormat (plist-get resolved :insertTextFormat))
         (textEdit (plist-get resolved :textEdit))
         (additionalTextEdits (plist-get resolved :additionalTextEdits))
         (snippet-fn (and (eql insertTextFormat 2)
                          (lsp-rocks--snippet-expansion-fn))))
    (cond (textEdit
           (delete-region (+ (- (point) (length candidate)))
                          (point))
           (insert lsp-rocks--last-prefix)
           (let ((range (plist-get textEdit :range))
                 (newText (plist-get textEdit :newText)))
             (pcase-let ((`(,beg . ,end)
                          (lsp-rocks--range-region range)))
               (delete-region beg end)
               (goto-char beg)
               (funcall (or snippet-fn #'insert) newText))))
          (snippet-fn
           ;; A snippet should be inserted, but using plain
           ;; `insertText'.  This requires us to delete the
           ;; whole completion, since `insertText' is the full
           ;; completion's text.
           (delete-region (- (point) (length candidate)) (point))
           (funcall snippet-fn (or insertText label)))
          (insertText
           (delete-region (- (point) (length candidate)) (point))
           (insert insertText)))
    (when (cl-plusp (length additionalTextEdits))
      (lsp-rocks--apply-text-edits additionalTextEdits))))

(defun company-lsp-rocks (command &optional arg &rest ignored)
  "`company-mode' completion backend existing file names.
Completions works for proper absolute and relative files paths.
File paths with spaces are only supported inside strings."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-yuf))
    (prefix (lsp-rocks--completion-prefix))
    (candidates (cons :async (lambda (callback)
                               (setq lsp-rocks--company-callback callback
                                     lsp-rocks--last-prefix arg)
                               (lsp-rocks--completion arg))))
    (no-cache t)
    (sorted t)
    (annotation (format " (%s)" (lsp-rocks--candidate-kind arg)))
    (kind (lsp-rocks--candidate-kind arg))
    (meta (when lsp-rocks-eldoc-enable-hover (get-text-property 0 'detail arg)))
    (post-completion (lsp-rocks--company-post-completion arg))))

(defun company-box-icons--lsp-rocks (candidate)
  (alist-get (lsp-rocks--candidate-kind-num candidate) company-box-icons--lsp-alist))

(defun lsp-rocks--company-set-selection-advice (&rest args)
  (when-let (label (nth (car args) company-candidates))
    (lsp-rocks--resolve label)))
(advice-add 'company-set-selection :after #'lsp-rocks--company-set-selection-advice)

;;; websocket request functions
(defun lsp-rocks--did-open ()
  (lsp-rocks--request "textDocument/didOpen"
                      (list :textDocument
                            (list :uri (lsp-rocks--buffer-uri)
                                  :languageId (string-replace "-mode" "" (symbol-name major-mode))
                                  :version 0
                                  :text (buffer-substring-no-properties (point-min) (point-max))))))

(defun lsp-rocks--did-close ()
  "Send textDocument/didClose notification."
  (lsp-rocks--request "textDocument/didClose" (lsp-rocks--TextDocumentIdentifier)))

(defun lsp-rocks--did-change (begin end len)
  "Send textDocument/didChange notification."
  (lsp-rocks--request "textDocument/didChange"
                      (list :textDocument
                            (list :uri (lsp-rocks--buffer-uri) :version lsp-rocks--current-file-version)
                            :contentChanges
                            (vector
                             (list :range (list :start lsp-rocks--before-change-begin-pos :end lsp-rocks--before-change-end-pos)
                                   :rangeLength len
                                   :text (buffer-substring-no-properties begin end))))))

(defun lsp-rocks--will-save ()
  "Send textDocument/willSave notification."
  (lsp-rocks--request "textDocument/willSave"
                      ;; 1 Manual, 2 AfterDelay, 3 FocusOut
                      (append '(:reason 1) (lsp-rocks--TextDocumentIdentifier))))

(defun lsp-rocks--did-save ()
  "Send textDocument/didSave notification."
  (lsp-rocks--request "textDocument/didSave"
                      (append `(:text ,(buffer-substring-no-properties (point-min) (point-max)))
                              (lsp-rocks--TextDocumentIdentifier))))

(defun lsp-rocks--completion (prefix)
  (lsp-rocks--request "textDocument/completion" (lsp-rocks--completion-params prefix)))

(defun lsp-rocks--completion-params (prefix)
  "Make textDocument/completion params."
  (append `(:prefix
            ,prefix
            :context ,(if (member prefix lsp-rocks--trigger-characters)
                          `(:triggerKind 2 :triggerCharacter ,prefix)
                        '(:triggerKind 1)))
          (lsp-rocks--TextDocumentPosition)))

(defun lsp-rocks--resolve (label)
  (lsp-rocks--request "completionItem/resolve"
                      (list :label label)))

(defun lsp-rocks-find-definition ()
  "Find definition."
  (interactive)
  (lsp-rocks--request "textDocument/definition" (lsp-rocks--TextDocumentPosition)))

(defun lsp-rocks-find-type-definition ()
  "Find type definition."
  (interactive)
  (lsp-rocks--request "textDocument/typeDefinition" (lsp-rocks--TextDocumentPosition)))

(defun lsp-rocks-find-declaration ()
  "Find declaration."
  (interactive)
  (lsp-rocks--request "textDocument/declaration" (lsp-rocks--TextDocumentPosition)))

;; (list :includeDeclaration t)
(defun lsp-rocks-find-references ()
  "Find references."
  (interactive)
  (lsp-rocks--request "textDocument/references" (lsp-rocks--TextDocumentPosition)))

(defun lsp-rocks-find-implementations ()
  "Find implementations."
  (interactive)
  (lsp-rocks--request "textDocument/implementation" (lsp-rocks--TextDocumentPosition)))

(defun lsp-rocks-describe-thing-at-point ()
  "Display the type signature and documentation of the thing at point."
  (interactive)
  (lsp-rocks--request "textDocument/hover" (lsp-rocks--TextDocumentPosition)))

(defun lsp-rocks--signature-help (isRetrigger kind triggerCharacter)
  "Send signatureHelp request with params."
  (lsp-rocks--request "textDocument/signatureHelp"
                      (list :textDocument
                            (list :uri (lsp-rocks--buffer-uri))
                            :position
                            (lsp-rocks--position)
                            :context
                            (list :triggerKind kind
                                  :triggerCharacter triggerCharacter
                                  :isRetrigger isRetrigger))))

(defun lsp-rocks-signature-help ()
  "Display the type signature and documentation of the thing at point."
  (interactive)
  (lsp-rocks--signature-help :false 1 nil))

(defun lsp-rocks--prepare-rename ()
  "Rename symbols."
  (lsp-rocks--request "textDocument/prepareRename" (lsp-rocks--TextDocumentPosition)))

(defvar-local lsp-rocks--prepare-result nil
  "Result of `lsp-rocks--prepare-rename'.")

(defcustom lsp-rocks-rename-use-prepare t
  "Whether `lsp-rocks-rename' should do a prepareRename first.
For some language servers, textDocument/prepareRename might be
too slow, in which case this variable may be set to nil.
`lsp-rocks-rename' will then use `thing-at-point' `symbol' to determine
the symbol to rename at point."
  :group 'lsp-rocks-mode
  :type 'boolean)

(defface lsp-rocks-face-rename '((t :underline t))
  "Face used to highlight the identifier being renamed.
Renaming can be done using `lsp-rocks-rename'."
  :group 'lsp-rocks-mode)

(defface lsp-rocks-rename-placeholder-face '((t :inherit font-lock-variable-name-face))
  "Face used to display the rename placeholder in.
When calling `lsp-rocks-rename' interactively, this will be the face of
the new name."
  :group 'lsp-rocks-mode)

(defvar lsp-rocks-rename-history '()
  "History for `lsp-rocks--read-rename'.")

(defun lsp-rocks--read-rename (at-point)
  "Read a new name for a `lsp-rocks-rename' at `point' from the user.

Returns a string, which should be the new name for the identifier
at point. If renaming cannot be done at point (as determined from
AT-POINT), throw a `user-error'.

This function is for use in `lsp-rocks-rename' only, and shall not be
relied upon."
  (unless at-point
    (user-error "`lsp-rocks-rename' is invalid here"))

  (let* ((start (caar at-point))
         (end (cdar at-point))
         (placeholder? (cdr at-point))
         (rename-me (buffer-substring start end))
         (placeholder (or placeholder? rename-me))
         (placeholder (propertize placeholder 'face 'lsp-rocks-rename-placeholder-face))
         overlay)

    ;; We need unwind protect, as the user might cancel here, causing the
    ;; overlay to linger.
    (unwind-protect
        (progn
          (setq overlay (make-overlay start end))
          (overlay-put overlay 'face 'lsp-rocks-face-rename)

          (read-string (format "Rename %s to: " rename-me) placeholder
                       'lsp-rocks-rename-history))
      (and overlay (delete-overlay overlay)))))

(defun lsp-rocks--rename-advice ()
  (when lsp-rocks-rename-use-prepare
    (lsp-rocks--prepare-rename)))
(advice-add 'lsp-rocks-rename :before #'lsp-rocks--rename-advice)

(defun lsp-rocks-rename ()
  "Rename symbols."
  (interactive)
  (let ((newName (lsp-rocks--read-rename
                  (or lsp-rocks--prepare-result
                      (when-let ((bounds (bounds-of-thing-at-point 'symbol)))
                        (cons bounds nil))))))
    (lsp-rocks--request "textDocument/rename"
                        (append `(:newName ,newName) (lsp-rocks--TextDocumentPosition)))))

(defun lsp-rocks--candidate-kind (item)
  "Return ITEM's kind symbol."
  (alist-get (get-text-property 0 'kind item)
             lsp-rocks--kind->symbol))

(defun lsp-rocks--candidate-kind-num (item)
  "Return ITEM's kind number."
  (get-text-property 0 'kind item))

(defun lsp-rocks--parse-completion (completions)
  "Parse LPS server returned COMPLETIONS."
  (let* ((head (car completions))
         (tail (cdr completions))
         (head-label (plist-get head :label)))
    (put-text-property 0 1 'kind (plist-get head :kind) head-label)
    (put-text-property 0 1 'detail (plist-get head :detail) head-label)
    (put-text-property 0 1 'resolved-item head head-label)
    (cons head-label
          (cl-mapcar (lambda (it)
                       (let* ((ret (plist-get it :label))
                              (kind (plist-get it :kind))
                              (detail (plist-get it :detail)))
                         (put-text-property 0 1 'kind kind ret)
                         (put-text-property 0 1 'detail detail ret)
                         ret))
                     tail))))

(defun lsp-rocks--lsp-position-to-point (pos-plist &optional marker)
  "Convert LSP position POS-PLIST to Emacs point.
If optional MARKER, return a marker instead"
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (forward-line (min most-positive-fixnum
                         (plist-get pos-plist :line)))
      (unless (eobp) ;; if line was excessive leave point at eob
        (let ((tab-width 1)
              (col (plist-get pos-plist :character)))
          (unless (wholenump col)
            (message
             "Caution: LSP server sent invalid character position %s. Using 0 instead."
             col)
            (setq col 0))
          (goto-char (min (+ (line-beginning-position) col)
                          (line-end-position)))))
      (if marker (copy-marker (point-marker)) (point)))))

(defun lsp-rocks--range-region (range &optional markers)
  "Return region (BEG . END) that represents LSP RANGE.
If optional MARKERS, make markers."
  (let ((beg (lsp-rocks--lsp-position-to-point (plist-get range :start) markers))
        (end (lsp-rocks--lsp-position-to-point (plist-get range :end) markers)))
    (cons beg end)))

(defun lsp-rocks--snippet-expansion-fn ()
  "Compute a function to expand snippets.
Doubles as an indicator of snippet support."
  (and (boundp 'yas-minor-mode)
       (symbol-value 'yas-minor-mode)
       'yas-expand-snippet))

(defun lsp-rocks--process-completion-resolve (item)
  "Process LSP resolved completion ITEM."
  (let ((candidate (nth company-selection company-candidates)))
    (put-text-property 0 1 'resolved-item item candidate)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; xref integration ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lsp-rocks--xref-backend () "lsp-rocks xref backend." 'xref-lsp-rocks)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql xref-lsp-rocks)))
  (propertize (or (thing-at-point 'symbol) "")
              'identifier-at-point t))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql xref-lsp-rocks)))
  (list (propertize (or (thing-at-point 'symbol) "")
                    'identifier-at-point t)))

(cl-defmethod xref-backend-definitions ((_backend (eql xref-lsp-rocks)) identifier callback)
  (save-excursion
    (setq lsp-rocks--xref-callback callback)
    (lsp-rocks-find-definition)))

(cl-defmethod xref-backend-references ((_backend (eql xref-lsp-rocks)) identifier callback)
  (save-excursion
    (setq lsp-rocks--xref-callback callback)
    (lsp-rocks-find-references)))

(cl-defmethod xref-backend-implementations ((_backend (eql xref-lsp-rocks)) identifier callback)
  (save-excursion
    (setq lsp-rocks--xref-callback callback)
    (lsp-rocks-find-implementations)))

(cl-defmethod xref-backend-type-definitions ((_backend (eql xref-lsp-rocks)) identifier callback)
  (save-excursion
    (setq lsp-rocks--xref-callback callback)
    (lsp-rocks-find-type-definition)))

(defun lsp-rocks--process-find-definition (locations)
  ""
  (funcall lsp-rocks--xref-callback
           (cl-mapcar (lambda (it)
                        (let* ((filepath (plist-get it :uri))
                               (range (plist-get it :range))
                               (start (plist-get range :start))
                               (end (plist-get range :end))
                               (start-line (plist-get start :line))
                               (start-column (plist-get start :character))
                               (end-line (plist-get end :line))
                               (end-column (plist-get end :character)))
                          (save-excursion
                            (save-restriction
                              (widen)
                              (let* ((beg (lsp-rocks--lsp-position-to-point start))
                                     (end (lsp-rocks--lsp-position-to-point end))
                                     (bol (progn (goto-char beg) (point-at-bol)))
                                     (summary (buffer-substring bol (point-at-eol)))
                                     (hi-beg (- beg bol))
                                     (hi-end (- (min (point-at-eol) end) bol)))
                                (when summary
                                  (add-face-text-property hi-beg hi-end 'xref-match t summary))
                                (xref-make summary
                                           (xref-make-file-location filepath (1+ start-line) start-column)))))))
                      locations)))

(defface lsp-rocks-hover-posframe
  '((t :inherit tooltip))
  "Background and foreground for `lsp-rocks-hover-posframe'."
  :group 'lsp-mode)

(defcustom lsp-rocks-hover-buffer " *lsp-rocks-help*"
  "Buffer for display hover info."
  :type 'string
  :group 'lsp-rocks)

(defcustom lsp-rocks-eldoc-enable-hover  nil
  "If non-nil, `eldoc' will display hover info when it is present."
  :type 'boolean
  :group 'lsp-rocks)

(defcustom lsp-rocks-enable-signature nil
  "Display signature documentation in `posframe'."
  :type 'boolean
  :group 'lsp-rocks)

(defcustom lsp-rocks-signature-buffer " *lsp-rocks-signature*"
  "Buffer for display signature documentation."
  :type 'string
  :group 'lsp-rocks)

(defun lsp-rocks--markdown-render ()
  (when (fboundp 'gfm-view-mode)
    (let ((inhibit-message t))
      (setq-local markdown-fontify-code-blocks-natively t)
      (set-face-background 'markdown-code-face (face-attribute 'lsp-rocks-hover-posframe :background nil t))
      (set-face-attribute 'markdown-code-face nil :height 130)
      (gfm-view-mode)))
  (read-only-mode 0)
  (prettify-symbols-mode 1)
  (display-line-numbers-mode -1)
  (font-lock-ensure)

  (setq-local mode-line-format nil))

(defun lsp-rocks--process-hover (hover-help)
  "Use posframe to show the HOVER-HELP string."
  (with-current-buffer (get-buffer-create lsp-rocks-hover-buffer)
    (erase-buffer)
    (insert hover-help)
    (lsp-rocks--markdown-render))

  (when (posframe-workable-p)
    (posframe-show lsp-rocks-hover-buffer
                   :max-width 60
                   :position (point)
                   :accept-focus nil
                   :lines-truncate t
                   :vertical-scroll-bars t
                   :internal-border-width 10
                   :poshandler #'posframe-poshandler-point-bottom-left-corner-upward
                   :background-color (face-attribute 'lsp-rocks-hover-posframe :background nil t)
                   :foreground-color (face-attribute 'lsp-rocks-hover-posframe :foreground nil t))))

(defun lsp-rocks--process-signature-help (signature-help)
  "Use posframe to show the SIGNATURE-HELP string."
  (let* ((signatures (plist-get signature-help :signatures))
         (activeSignature (plist-get signature-help :activeSignature))
         (info (mapcar (lambda (it)
                         (let ((label (plist-get it :label))
                               (doc (plist-get it :documentation)))
                           (if doc
                               (format "%s\n___\n%s" label doc)
                             label)))
                       signatures)))
    (with-current-buffer (get-buffer-create lsp-rocks-signature-buffer)
      (erase-buffer)
      (insert (nth activeSignature info))
      (lsp-rocks--markdown-render))

    (when (posframe-workable-p)
      (posframe-show lsp-rocks-signature-buffer
                     :max-width 60
                     :max-height 20
                     :lines-truncate t
                     :horizontal-scroll-bars t
                     :vertical-scroll-bars t
                     :position (point)
                     :accept-focus nil
                     :internal-border-width 10
                     :poshandler #'posframe-poshandler-point-bottom-left-corner-upward
                     :background-color (face-attribute 'lsp-rocks-hover-posframe :background nil t)
                     :foreground-color (face-attribute 'lsp-rocks-hover-posframe :foreground nil t)))))

(defun lsp-rocks--process-prepare-rename (data)
  (let* ((range (plist-get data :range))
         (start (plist-get range :start))
         (end (plist-get range :end))
         (placeholder (plist-get data :placeholder))
         (start-point (lsp-rocks--lsp-position-to-point start))
         (end-point (lsp-rocks--lsp-position-to-point end)))
    (setq-local lsp-rocks--prepare-result
                (cons (cons start-point end-point)
                      (if (string-empty-p placeholder) nil placeholder)))
    (with-current-buffer (current-buffer)
      (require 'pulse)
      (let ((pulse-iterations 1)
            (pulse-delay lsp-rocks-flash-line-delay))
        (pulse-momentary-highlight-region start-point end-point 'lsp-rocks-font-lock-flash)))))

(defun lsp-rocks--uri-to-path (uri)
  "Convert URI to file path."
  (when (keywordp uri) (setq uri (substring (symbol-name uri) 1)))
  (let* ((retval (url-unhex-string (url-filename (url-generic-parse-url uri))))
         ;; Remove the leading "/" for local MS Windows-style paths.
         (normalized (if (and (eq system-type 'windows-nt)
                              (cl-plusp (length retval)))
                         (substring retval 1)
                       retval)))
    normalized))

(defun lsp-rocks--process-rename (data)
  (when lsp-rocks--prepare-result
    (setq-local lsp-rocks--prepare-result nil))

  (let ((changes (plist-get data :documentChanges)))
    (dolist (item changes)
      (let* ((textDocument (plist-get item :textDocument))
             (filepath (lsp-rocks--uri-to-path (plist-get textDocument :uri)))
             (edits (plist-get item :edits)))
        (find-file-noselect filepath)
        (save-excursion
          (find-file filepath)
          (dolist (edit edits)
            (let ((region (lsp-rocks--range-region (plist-get edit :range)))
                  (newText (plist-get edit :newText)))
              (delete-region (car region) (cdr region))
              (goto-char (car region))
              (insert newText))))))))

(defun lsp-rocks--TextDocumentIdentifier ()
  "Make a TextDocumentIdentifier object."
  `(:textDocument
    (:uri ,(lsp-rocks--buffer-uri))))

(defun lsp-rocks--TextDocumentPosition ()
  "Make a TextDocumentPosition object."
  (append `(:position ,(lsp-rocks--position))
          (lsp-rocks--TextDocumentIdentifier)))

(defun lsp-rocks--json-parse (json)
  "Parse JSON data to `plist'."
  (json-parse-string json :object-type 'plist :array-type 'list))

(defun lsp-rocks--json-stringify (object)
  "Stringify OBJECT data to JSON."
  (json-serialize object :null-object nil))

(defun lsp-rocks--request (cmd &optional params)
  "Send a websocket message with given CMD and PARAMS."
  (when-let ((client (lsp-rocks--get-websocket-client))
             (id (lsp-rocks--request-id)))
    (when (equal (websocket-ready-state client) 'open)
      ;; save the last request for the cmd
      (puthash cmd id lsp-rocks--recent-requests)
      (websocket-send-text
       client
       (lsp-rocks--json-stringify
        (list :id id :cmd cmd :params params))))))

(defun lsp-rocks--response (id cmd data)
  "Send response to server."
  (websocket-send-text
   (lsp-rocks--get-websocket-client)
   (lsp-rocks--json-stringify
    (list :id id :cmd cmd :data data))))

(defun lsp-rocks--point-position (pos)
  "Get position of POS."
  (save-excursion
    (goto-char pos)
    (lsp-rocks--position)))

(defun lsp-rocks--calculate-column ()
  "Calculate character offset of cursor in current line."
  (/ (- (length
         (encode-coding-region
          (line-beginning-position)
          (min (point) (point-max)) 'utf-16 t))
        2)
     2))

(defun lsp-rocks--position ()
  (list :line (1- (line-number-at-pos)) :character (lsp-rocks--calculate-column)))

(defun lsp-rocks--request-id ()
  (lsp-rocks--random-string 8))

(defun lsp-rocks--random-alnum ()
  (let* ((alnum "abcdefghijklmnopqrstuvwxyz0123456789")
         (i (% (abs (random)) (length alnum))))
    (substring alnum i (1+ i))))

(defun lsp-rocks--random-string (n)
  "Generate a slug of n random alphanumeric characters."
  (if (= 0 n) ""
    (concat (lsp-rocks--random-alnum) (lsp-rocks--random-string (1- n)))))

(defun lsp-rocks--before-change (begin end)
  (setq-local lsp-rocks--before-change-begin-pos (lsp-rocks--point-position begin))
  (setq-local lsp-rocks--before-change-end-pos (lsp-rocks--point-position end)))

(defun lsp-rocks--after-change (begin end len)
  (setq lsp-rocks--current-file-version (1+ lsp-rocks--current-file-version))
  (lsp-rocks--did-change begin end len)
  (when lsp-rocks-enable-signature
    (lsp-rocks--signature-help t 3 nil)))

(defun lsp-rocks--before-revert-hook ()
  (lsp-rocks--did-close))

(defun lsp-rocks--after-revert-hook ()
  (lsp-rocks--did-open))

(defun lsp-rocks--before-save-hook ()
  (lsp-rocks--will-save))

(defun lsp-rocks--after-save-hook ()
  (lsp-rocks--did-save))

(defun lsp-rocks--kill-buffer-hook ()
  (setq lsp-rocks-mode nil)
  (lsp-rocks--did-close))

(defun lsp-rocks--post-command-hook ()
  (let ((this-command-string (format "%s" this-command)))

    (unless (member this-command-string '("self-insert-command" "company-complete-selection" "yas-next-field-or-maybe-expand"))
      (posframe-hide lsp-rocks-signature-buffer))

    (when lsp-rocks-mode
      (posframe-hide lsp-rocks-hover-buffer))

    (when (and lsp-rocks-mode
               lsp-rocks-enable-signature
               (or (string-equal this-command-string "company-complete-selection")
                   (string-equal this-command-string "yas-next-field-or-maybe-expand")))
      (lsp-rocks--signature-help t 3 nil))))

(defconst lsp-rocks--internal-hooks
  '((before-change-functions . lsp-rocks--before-change)
    (after-change-functions . lsp-rocks--after-change)
    (before-revert-hook . lsp-rocks--before-revert-hook)
    (after-revert-hook . lsp-rocks--after-revert-hook)
    (kill-buffer-hook . lsp-rocks--kill-buffer-hook)
    (xref-backend-functions . lsp-rocks--xref-backend)
    (before-save-hook . lsp-rocks--before-save-hook)
    (after-save-hook . lsp-rocks--after-save-hook)
    (post-command-hook . lsp-rocks--post-command-hook)))

(defun lsp-rocks--enable ()
  (unless lsp-rocks--server-process
    (lsp-rocks--start-server))

  (unless (lsp-rocks--get-websocket-client)
    (while (null (lsp-rocks--get-websocket-client))
      (ignore-errors
        (lsp-rocks--save-websocket-client
         (lsp-rocks--create-websocket-client
          (format "ws://%s:%s" lsp-rocks-server-host lsp-rocks--server-port))))
      (sleep-for 0 20)))

  (setq lsp-rocks-buffer-uri (lsp-rocks--buffer-uri))
  (lsp-rocks--did-open)
  (add-to-list 'company-backends 'company-lsp-rocks)
  (when (featurep 'company-box)
    (add-to-list 'company-box-icons-functions 'company-box-icons--lsp-rocks))
  (dolist (hook lsp-rocks--internal-hooks)
    (add-hook (car hook) (cdr hook) nil t)))

(defun lsp-rocks--disable ()
  (dolist (hook lsp-rocks--internal-hooks)
    (remove-hook (car hook) (cdr hook) t)))

(defvar lsp-rocks-mode-map (make-sparse-keymap))

;;;###autoload
(define-minor-mode lsp-rocks-mode
  "LSP Rocks mode."
  :init-value nil
  :lighter " LSP/R"
  :keymap lsp-rocks-mode-map
  (if lsp-rocks-mode
      (lsp-rocks--enable)
    (lsp-rocks--disable)))

(provide 'lsp-rocks)

;;; lsp-rocks.el ends here
