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
(require 'map)
(require 'seq)
(require 's)
(require 'subr-x)
(require 'websocket)

(defgroup lsp-rocks nil
  "LSP-Rocks group."
  :group 'applications)

(defcustom lsp-rocks-server-bin "/path/to/cli.js"
  "Location of lsp-rocks server."
  :group 'lsp-rocks)

(defcustom lsp-rocks-name "*lsp-rocks*"
  ""
  :group 'lsp-rocks)

(defcustom lsp-rocks-server-host "0.0.0.0"
  ""
  :group 'lsp-rocks)

(defvar lsp-rocks--server-port nil)

(defvar lsp-rocks--server-process nil)

(defcustom lsp-rocks-use-ssl nil
  ""
  :group 'lsp-rocks)

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

(defvar lsp-rocks--websocket-clients (make-hash-table :test 'equal :size 16)
  "LSP-Rocks websocket connection.")

(defvar lsp-rocks--recent-requests (make-hash-table :test 'equal :size 32)
  "LSP-Rocks websocket connection.")

(defvar lsp-rocks--company-callback nil
  "Company callback.")

(defvar lsp-rocks-language-server-configuration
  (list (list 'rust-mode (list :name "rust" :command "/Users/vritser/Downloads/rust-analyzer/server/rust-analyzer" :args (vector)))
        (list 'python-mode (list :name "python" :command "pyright-langserver" :args (vector "--stdio")))
        (list 'typescript-mode (list :name "typescript" :command "/Users/vritser/.emacs.d/.cache/lsp/npm/typescript-language-server/bin/typescript-language-server" :args (vector "--stdio")))))

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
        ("textDocument/definition" (lsp-rocks--process-find-definition (plist-get data :uri) (plist-get data :position)))))))

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
  ""
  (lsp-rocks-shutdown)
  (lsp-rocks--start-server)
  (message "[LSP-Rocks] Server restarted."))

(defun lsp-rocks--start-server ()
  ""
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
  (lsp-rocks--kill-server-process))

(defun lsp-rocks--kill-server-process ()
  "Kill LSP-Rocks server process."
  (when (get-buffer lsp-rocks-name)
    (dolist (client (hash-table-values lsp-rocks--websocket-clients))
      (websocket-close client))
    (kill-process lsp-rocks--server-process)
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
Return nil if no completion should be triggered. Return a string
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

(defun company-lsp-rocks (command &optional arg &rest ignored)
  "`company-mode' completion backend existing file names.
Completions works for proper absolute and relative files paths.
File paths with spaces are only supported inside strings."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-yuf))
    (prefix (lsp-rocks--completion-prefix))
    (candidates (cons :async (lambda (callback)
                               (setq lsp-rocks--company-callback callback)
                               (lsp-rocks--completion arg))))
    (no-cache t)
    (sorted t)
    (annotation (format " (%s)" (lsp-rocks--candidate-kind arg)))
    (meta (get-text-property 0 'detail arg))))

;;; websocket request functions
(defun lsp-rocks--did-open ()
  (lsp-rocks--request "textDocument/didOpen"
                      (list :uri (lsp-rocks--current-file-uri)
                            :languageId (string-replace "-mode" "" (symbol-name major-mode))
                            :version 0
                            :text (buffer-substring-no-properties (point-min) (point-max)))))

(defun lsp-rocks--did-change (begin end len)
  (lsp-rocks--request "textDocument/didChange"
                      (list :textDocument
                            (list :uri (lsp-rocks--current-file-uri) :version lsp-rocks--current-file-version)
                            :contentChanges
                            (vector
                             (list :range (list :start lsp-rocks--before-change-begin-pos :end lsp-rocks--before-change-end-pos)
                                   :rangeLength len
                                   :text (buffer-substring-no-properties begin end))))))

(defun lsp-rocks--completion (prefix)
  (lsp-rocks--request "textDocument/completion"
                      (list :prefix prefix
                            :textDocument
                            (list :uri (lsp-rocks--current-file-uri))
                            :position
                            (lsp-rocks--position)
                            :context
                            (if (member prefix lsp-rocks--trigger-characters)
                                (list :triggerKind 2 :triggerCharacter prefix)
                              (list :triggerKind 1)))))

(defun lsp-rocks-find-definition ()
  "Find definition."
  (interactive)
  (lsp-rocks--request "textDocument/definition"
                      (list :textDocument
                            (list :uri (lsp-rocks--current-file-uri))
                            :position
                            (lsp-rocks--position))))

(defun lsp-rocks-find-definition-return ()
  "Pop off lsp-rocks--mark-ring and jump to the top location."
  (interactive)
  ;; Pop entries that refer to non-existent buffers.
  (while (and lsp-rocks--mark-ring (not (marker-buffer (car lsp-rocks--mark-ring))))
    (setq lsp-rocks--mark-ring (cdr lsp-rocks--mark-ring)))
  (or lsp-rocks--mark-ring
      (error "[LSP-Rocks] No lsp-rocks mark set"))
  (let* ((this-buffer (current-buffer))
         (marker (pop lsp-rocks--mark-ring))
         (buffer (marker-buffer marker))
         (position (marker-position marker)))
    (set-buffer buffer)
    (or (and (>= position (point-min))
             (<= position (point-max)))
        (if widen-automatically
            (widen)
          (error "[LSP-Rocks] mark position is outside accessible part of buffer %s"
                 (buffer-name buffer))))
    (goto-char position)
    (unless (equal buffer this-buffer)
      (switch-to-buffer buffer))))

(defun lsp-rocks--candidate-kind (item)
  "Return ITEM's kind."
  (alist-get (get-text-property 0 'kind item)
             lsp-rocks--kind->symbol))

(defun lsp-rocks--parse-completion (data)
  (cl-mapcar (lambda (it)
               (let* ((ret (plist-get it :newText))
                     (kind (plist-get it :kind))
                     (detail (plist-get it :detail)))
                 (put-text-property 0 1 'kind kind ret)
                 (put-text-property 0 1 'detail detail ret)
                 ret))
             data))

(defun lsp-rocks--point-from (position &optional marker)
  "Convert POSITION to point.
If optional MARKER, return a marker instead"
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (forward-line (min most-positive-fixnum
                         (plist-get position :line)))
      (unless (eobp) ;; if line was excessive leave point at eob
        (let ((tab-width 1)
              (character (plist-get position :character)))
          (unless (wholenump character)
            (message
             "[LSP Rocks] Caution: LSP server sent invalid character position %s. Using 0 instead."
             character)
            (setq character 0))
          ;; We cannot use `move-to-column' here, because it moves to *visual*
          ;; columns, which can be different from LSP columns in case of
          ;; `whitespace-mode', `prettify-symbols-mode', etc.
          (goto-char (min (+ (line-beginning-position) character)
                          (line-end-position)))))
      (if marker (copy-marker (point-marker)) (point)))))

(defun lsp-rocks--process-find-definition (filepath position)
  ;; Record postion.
  (set-marker (mark-marker) (point) (current-buffer))
  (add-to-history 'lsp-rocks--mark-ring (copy-marker (mark-marker)) lsp-rocks-mark-ring-max-size t)

  ;; Jump to define.
  (find-file filepath)

  (goto-char (lsp-rocks--point-from position))
  (recenter)

  ;; Flash define line.
  (require 'pulse)
  (let ((pulse-iterations 1)
        (pulse-delay lsp-rocks-flash-line-delay))
    (pulse-momentary-highlight-one-line (point) 'lsp-rocks-font-lock-flash)))

(defun lsp-rocks--json-parse (json)
  (json-parse-string json :object-type 'plist :array-type 'list))

(defun lsp-rocks--json-stringify (object)
  (json-serialize object :null-object nil))

(defun lsp-rocks--request (cmd &optional params)
  (let ((id (lsp-rocks--request-id)))
    ;; save the last request for the CMD
    (puthash cmd id lsp-rocks--recent-requests)
    (websocket-send-text
     (lsp-rocks--get-websocket-client)
     (lsp-rocks--json-stringify
      (list :id id  :cmd cmd :params params)))))

(defun lsp-rocks--response (id cmd data)
  (websocket-send-text
   (lsp-rocks--get-websocket-client)
   (lsp-rocks--json-stringify
    (list :id id :cmd cmd :data data))))

(defun lsp-rocks--current-file-uri ()
  (concat "file://" (buffer-file-name)))

(defun lsp-rocks--monitor-before-change (begin end)
  (setq-local lsp-rocks--before-change-begin-pos (lsp-rocks--point-position begin))
  (setq-local lsp-rocks--before-change-end-pos (lsp-rocks--point-position end)))

(defun lsp-rocks--monitor-after-change (begin end len)
  (setq lsp-rocks--current-file-version (1+ lsp-rocks--current-file-version))
  (lsp-rocks--did-change begin end len))

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

;; (defconst lsp-rocks--internal-hooks
;;   '((before-change-functions . lsp-rocks-monitor-before-change)
;;     (after-change-functions . lsp-rocks-monitor-after-change)
;;     (post-command-hook . lsp-rocks-monitor-post-command)
;;     (after-save-hook . lsp-rocks-monitor-after-save)
;;     (kill-buffer-hook . lsp-rocks-close-buffer-file)
;;     (find-file-hook . lsp-rocks-search-words-open-file)
;;     (before-revert-hook . lsp-rocks-close-buffer-file)
;;     ))

(defconst lsp-rocks--internal-hooks
  '((before-change-functions . lsp-rocks--monitor-before-change)
    (after-change-functions . lsp-rocks--monitor-after-change)))

(defun lsp-rocks--enable ()
  (unless lsp-rocks--server-process
    (lsp-rocks--start-server))

  (sleep-for 1)
  (unless (lsp-rocks--get-websocket-client)
    (lsp-rocks--save-websocket-client (lsp-rocks--create-websocket-client
                                       (concat (if lsp-rocks-use-ssl "wss://" "ws://")
                                               lsp-rocks-server-host ":" lsp-rocks--server-port))))
  (sleep-for 1)
  (lsp-rocks--did-open)
  (add-to-list 'company-backends 'company-lsp-rocks)
  (dolist (hook lsp-rocks--internal-hooks)
    (add-hook (car hook) (cdr hook) nil t)))

(defun lsp-rocks--disable ()
  (dolist (hook lsp-rocks--internal-hooks)
    (remove-hook (car hook) (cdr hook) t)))

(defvar lsp-rocks-mode-map (make-sparse-keymap))

;;;###autoload
(define-minor-mode lsp-rocks-mode
  "LSP Rocks mode."
  :keymap lsp-rocks-mode-map
  :lighter "LSP/R"
  :init-value nil
  (if lsp-rocks-mode
      (lsp-rocks--enable)
    (lsp-rocks--disable)))

(provide 'lsp-rocks)

;;; lsp-rocks.el ends here
