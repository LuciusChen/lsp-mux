;;; lsp-mux.el --- LSP multiplexer for Emacs  -*- lexical-binding: t; -*-

;; Author: Lucius Chen
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (compat "30.1"))
;; Keywords: tools, languages

;;; Commentary:

;; lsp-mux provides a pure Elisp foundation for multiplexing multiple
;; language servers behind one client-facing endpoint.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'jsonrpc nil t)
(require 'subr-x)
(require 'url-parse)
(require 'url-util)


(defgroup lsp-mux nil
  "LSP multiplexer."
  :group 'tools
  :prefix "lsp-mux-")

(defcustom lsp-mux-route-rules
  '(("textDocument/definition" . first-success)
    ("textDocument/references" . merge)
    ("textDocument/codeAction" . merge)
    ("textDocument/completion" . merge)
    ("textDocument/diagnostic" . merge)
    ("textDocument/hover" . first-success))
  "Method routing rules.

Values are symbols:
- `broadcast' send to all backends.
- `first-success' pick first non-nil/non-empty response.
- `merge' merge all responses."
  :type '(alist :key-type string
                :value-type (choice (const broadcast)
                                    (const first-success)
                                    (const merge)))
  :group 'lsp-mux)

(defcustom lsp-mux-method-backends nil
  "Optional method-specific backend allowlist.

Each element is (METHOD . BACKEND-NAMES), where METHOD is an LSP method
string and BACKEND-NAMES is a list of backend names to target.  If a
method has no entry here, all backends are considered."
  :type '(alist :key-type string
                :value-type (repeat string))
  :group 'lsp-mux)

(defcustom lsp-mux-request-timeout-seconds 1.5
  "Timeout in seconds for aggregated requests."
  :type 'number
  :group 'lsp-mux)

(defcustom lsp-mux-enable-trace nil
  "Non-nil means lsp-mux writes runtime traces into `lsp-mux-trace-buffer'."
  :type 'boolean
  :group 'lsp-mux)

(defcustom lsp-mux-trace-buffer "*lsp-mux-trace*"
  "Buffer name used for lsp-mux traces."
  :type 'string
  :group 'lsp-mux)

(defcustom lsp-mux-listen-host "127.0.0.1"
  "Host interface used by `lsp-mux-server-start'."
  :type 'string
  :group 'lsp-mux)

(defcustom lsp-mux-listen-port 9257
  "TCP port used by `lsp-mux-server-start'."
  :type 'natnum
  :group 'lsp-mux)

(defcustom lsp-mux-auto-select-port t
  "Non-nil means `lsp-mux-start' picks an available local TCP port."
  :type 'boolean
  :group 'lsp-mux)

(defcustom lsp-mux-backend-commands nil
  "Backend command specs used by `lsp-mux-start'.

Each element is a list: (NAME PROGRAM ARG...)."
  :type '(repeat (list :tag "Backend"
                       (string :tag "Name")
                       (string :tag "Program")
                       (repeat :tag "Args" string)))
  :group 'lsp-mux)

(defcustom lsp-mux-eslint-fallback-configuration
  '(:validate "on"
    :run "onType"
    :problems (:shortenToSingleLine :json-false)
    :rulesCustomizations []
    :codeAction (:disableRuleComment (:enable t :location "separateLine")
                 :showDocumentation (:enable t))
    :codeActionOnSave (:mode "all")
    :experimental (:useFlatConfig t)
    :useFlatConfig t
    :nodePath :null
    :workingDirectory (:mode "location"))
  "Fallback workspace/configuration object for eslint backend.

Used when the client returns nil for eslint `workspace/configuration`
requests. This prevents eslint-lsp from failing with missing file path
metadata in pull diagnostics/code action flows."
  :type 'sexp
  :group 'lsp-mux)

(cl-defstruct (lsp-mux-parser
               (:constructor lsp-mux-parser-create))
  "Incremental parser state for LSP Content-Length frames."
  (carry (unibyte-string) :type string))

(cl-defstruct (lsp-mux-backend
               (:constructor lsp-mux-backend-create))
  "A backend endpoint in the multiplexer."
  name
  send-fn
  process
  (parser (lsp-mux-parser-create)))

(cl-defstruct (lsp-mux-pending
               (:constructor lsp-mux-pending-create))
  "Pending aggregated request state."
  client-id
  method
  strategy
  backend-order
  backend-id-map
  (expected 0 :type integer)
  (done 0 :type integer)
  (finalized nil)
  timer
  results
  errors)

(cl-defstruct (lsp-mux-session
               (:constructor lsp-mux-session-create))
  "One client-facing multiplexing session."
  (backends nil)
  client-send-fn
  client-process
  (client-parser (lsp-mux-parser-create))
  (diagnostics-by-uri-backend (make-hash-table :test #'equal))
  (backend-capabilities (make-hash-table :test #'equal))
  (backend-capabilities-known (make-hash-table :test #'equal))
  (backend-request-map (make-hash-table :test #'eql))
  (pending-by-client-id (make-hash-table :test #'equal))
  (next-client-id 1000000 :type integer)
  (next-backend-id 1 :type integer)
  (pending-by-backend-id (make-hash-table :test #'eql)))

(cl-defstruct (lsp-mux-server
               (:constructor lsp-mux-server-create))
  "Top-level server state."
  process
  backend-specs
  (sessions (make-hash-table :test #'eq)))

(defun lsp-mux-route-for-method (method)
  "Return route strategy for METHOD."
  (or (cdr (assoc method lsp-mux-route-rules))
      'broadcast))

(defun lsp-mux--backend-hint-from-params (params)
  "Extract backend hint name from request PARAMS."
  (or (and (listp params)
           (plist-get params :lsp-mux-backend))
      (and (listp params)
           (listp (plist-get params :item))
           (plist-get (plist-get params :item) :lsp-mux-backend))))

(defun lsp-mux--document-sync-notification-p (method)
  "Return non-nil if METHOD is a text document sync notification."
  (member method
          '("textDocument/didOpen"
            "textDocument/didChange"
            "textDocument/didSave"
            "textDocument/didClose")))

(defun lsp-mux--targets-for-method (session method &optional params)
  "Return backend targets in SESSION for METHOD and PARAMS."
  (let* ((hint-name (lsp-mux--backend-hint-from-params params))
         (hint-backend (and hint-name
                            (lsp-mux--session-backend session hint-name))))
    (if hint-backend
        (if (lsp-mux--backend-supports-method-p session hint-name method)
            (list hint-backend)
          nil)
      (let ((allowed (cdr (assoc method lsp-mux-method-backends)))
            (backends (lsp-mux-session-backends session)))
        (if (null allowed)
            (cl-loop for backend in backends
                     for backend-name = (lsp-mux-backend-name backend)
                     when (lsp-mux--backend-supports-method-p session backend-name method)
                     collect backend)
          (let ((seen (make-hash-table :test #'equal)))
            (cl-loop for backend-name in allowed
                     for backend = (lsp-mux--session-backend session backend-name)
                     when (and backend
                               (not (gethash backend-name seen))
                               (lsp-mux--backend-supports-method-p session backend-name method))
                     do (puthash backend-name t seen)
                     and collect backend)))))))

(defun lsp-mux--method-capability-key (method)
  "Return capability plist key that gates METHOD, or nil if unknown."
  (cond
   ((string= method "textDocument/hover") :hoverProvider)
   ((string= method "textDocument/signatureHelp") :signatureHelpProvider)
   ((string= method "textDocument/completion") :completionProvider)
   ((string= method "textDocument/codeAction") :codeActionProvider)
   ((string= method "textDocument/codeLens") :codeLensProvider)
   ((string= method "textDocument/documentLink") :documentLinkProvider)
   ((string= method "textDocument/documentSymbol") :documentSymbolProvider)
   ((string= method "textDocument/documentHighlight") :documentHighlightProvider)
   ((string= method "textDocument/definition") :definitionProvider)
   ((string= method "textDocument/declaration") :declarationProvider)
   ((string= method "textDocument/typeDefinition") :typeDefinitionProvider)
   ((string= method "textDocument/implementation") :implementationProvider)
   ((string= method "textDocument/references") :referencesProvider)
   ((string= method "textDocument/rename") :renameProvider)
   ((string= method "textDocument/formatting") :documentFormattingProvider)
   ((string= method "textDocument/rangeFormatting") :documentRangeFormattingProvider)
   ((string= method "textDocument/onTypeFormatting") :documentOnTypeFormattingProvider)
   ((or (string= method "textDocument/semanticTokens/full")
        (string= method "textDocument/semanticTokens/range"))
    :semanticTokensProvider)
   ((string= method "textDocument/inlayHint") :inlayHintProvider)
   ((string= method "textDocument/diagnostic") :diagnosticProvider)
   ((string= method "workspace/symbol") :workspaceSymbolProvider)
   (t nil)))

(defun lsp-mux--backend-supports-method-p (session backend-name method)
  "Return non-nil if BACKEND-NAME in SESSION supports METHOD."
  (let* ((cap-key (lsp-mux--method-capability-key method))
         (known-p (gethash backend-name
                           (lsp-mux-session-backend-capabilities-known session)))
         (caps (gethash backend-name (lsp-mux-session-backend-capabilities session))))
    (if (or (null cap-key) (not known-p))
        (let ((completion (plist-get caps :completionProvider))
              (code-action (plist-get caps :codeActionProvider)))
          (cond
           ((or (string= method "completionItem/resolve")
                (string= method "textDocument/completionItem/resolve"))
            (cond
             ((not known-p) t)
             ((null completion) nil)
             ((eq completion :json-false) nil)
             ((and (listp completion) (keywordp (car completion)))
              (let ((flag (plist-get completion :resolveProvider)))
                (and flag (not (eq flag :json-false)))))
             (t t)))
           ((or (string= method "codeAction/resolve")
                (string= method "textDocument/codeAction/resolve"))
            (cond
             ((not known-p) t)
             ((null code-action) nil)
             ((eq code-action :json-false) nil)
             ((and (listp code-action) (keywordp (car code-action)))
              (let ((flag (plist-get code-action :resolveProvider)))
                (and flag (not (eq flag :json-false)))))
             (t t)))
           (t t)))
      (let ((value (plist-get caps cap-key)))
        (cond
         ((null value) nil)
         ((eq value :json-false) nil)
         (t t))))))

(defun lsp-mux--maybe-record-backend-capabilities (session backend-name pending object)
  "Record initialize capabilities for BACKEND-NAME in SESSION from OBJECT."
  (when (and (string= (lsp-mux-pending-method pending) "initialize")
             (or (plist-member object :result)
                 (plist-member object :error)))
    (puthash backend-name t (lsp-mux-session-backend-capabilities-known session))
    (let* ((result (plist-get object :result))
           (caps (and (listp result)
                      (plist-get result :capabilities))))
      (puthash backend-name caps (lsp-mux-session-backend-capabilities session)))))

(defun lsp-mux--backend-request-record (backend-name backend-id method params)
  "Create backend request record."
  (list :backend-name backend-name
        :backend-id backend-id
        :method method
        :params params))

(defun lsp-mux--put-backend-capability (session backend-name cap-key cap-value)
  "Set CAP-KEY for BACKEND-NAME in SESSION to CAP-VALUE."
  (puthash backend-name t (lsp-mux-session-backend-capabilities-known session))
  (let* ((old-caps (gethash backend-name (lsp-mux-session-backend-capabilities session)))
         (caps (if old-caps (copy-sequence old-caps) nil)))
    (setq caps (plist-put caps cap-key cap-value))
    (puthash backend-name caps (lsp-mux-session-backend-capabilities session))))

(defun lsp-mux--apply-register-capabilities (session backend-name params)
  "Apply dynamic register capability PARAMS for BACKEND-NAME in SESSION."
  (dolist (reg (plist-get params :registrations))
    (when-let* ((method (plist-get reg :method))
                (cap-key (lsp-mux--method-capability-key method)))
      (let ((opts (plist-get reg :registerOptions)))
        (lsp-mux--put-backend-capability
         session backend-name cap-key (if (null opts) t opts))))))

(defun lsp-mux--apply-unregister-capabilities (session backend-name params)
  "Apply dynamic unregister capability PARAMS for BACKEND-NAME in SESSION."
  (dolist (unreg (or (plist-get params :unregisterations)
                     (plist-get params :unregistrations)))
    (when-let* ((method (plist-get unreg :method))
                (cap-key (lsp-mux--method-capability-key method)))
      (lsp-mux--put-backend-capability session backend-name cap-key nil))))

(defun lsp-mux--maybe-apply-dynamic-capabilities-from-client-response (session record object)
  "Apply dynamic capability side effects for successful backend REQUEST RECORD."
  (when (plist-member object :result)
    (let ((backend-name (plist-get record :backend-name))
          (method (plist-get record :method))
          (params (plist-get record :params)))
      (cond
       ((string= method "client/registerCapability")
        (lsp-mux--apply-register-capabilities session backend-name params))
       ((string= method "client/unregisterCapability")
        (lsp-mux--apply-unregister-capabilities session backend-name params))))))

(defun lsp-mux-merge-capabilities (caps-list)
  "Merge capability plists from CAPS-LIST.

Booleans are OR-ed. Plists are merged recursively. Other values pick
last non-nil value."
  (cl-labels ((merge-two (a b)
                (cond
                 ((null a) b)
                 ((null b) a)
                 ((and (listp a) (keywordp (car a))
                       (listp b) (keywordp (car b)))
                  (let ((keys (delete-dups
                               (append (cl-loop for (k _) on a by #'cddr collect k)
                                       (cl-loop for (k _) on b by #'cddr collect k))))
                        (out nil))
                    (dolist (k keys (nreverse out))
                      (let ((v (merge-two (plist-get a k) (plist-get b k))))
                        (setq out (cons v (cons k out)))))))
                 ((and (booleanp a) (booleanp b))
                  (or a b))
                 (t b))))
    (cl-reduce #'merge-two caps-list :initial-value nil)))

(defun lsp-mux--parse-headers (header-text)
  "Parse LSP HEADER-TEXT and return an alist.

Keys are lower-cased strings."
  (let ((lines (split-string header-text "\r\n" t))
        (out nil))
    (dolist (line lines (nreverse out))
      (when (string-match "^\\([^:]+\\):[ \t]*\\(.*\\)$" line)
        (push (cons (downcase (match-string 1 line))
                    (match-string 2 line))
              out)))))

(defun lsp-mux-parser-feed (parser chunk)
  "Feed CHUNK to PARSER.

Return a cons: (MESSAGES . UPDATED-PARSER), where MESSAGES is a list of
raw JSON payload strings decoded from complete LSP frames."
  (let ((data (concat (lsp-mux-parser-carry parser) chunk))
        (messages nil)
        (pos 0)
        (continue t))
    (while continue
      (let ((hdr-end (string-match "\r\n\r\n" data pos)))
        (if (not hdr-end)
            (setq continue nil)
          (let* ((header-text (substring data pos hdr-end))
                 (headers (lsp-mux--parse-headers header-text))
                 (len-str (cdr (assoc "content-length" headers))))
            (unless (and len-str (string-match-p "\\`[0-9]+\\'" len-str))
              ;; Some servers may occasionally emit non-LSP text chunks.
              ;; Slide one byte and continue scanning to re-sync with the
              ;; next valid `Content-Length` frame.
              (setq pos (1+ pos))
              (setq len-str nil))
            (when len-str
              (let* ((body-start (+ hdr-end 4))
                     (len (string-to-number len-str))
                     (body-end (+ body-start len)))
                (if (> body-end (length data))
                    (setq continue nil)
                  (push (substring data body-start body-end) messages)
                  (setq pos body-end))))))))
    (setf (lsp-mux-parser-carry parser)
          (if (< pos (length data))
              (substring data pos)
            (unibyte-string)))
    (cons (nreverse messages) parser))
)

(defun lsp-mux--json-normalize-plist (plist)
  "Normalize PLIST values recursively."
  (if (null plist)
      nil
    (let ((k (car plist))
          (v (cadr plist)))
      (cons k
            (cons (lsp-mux--json-normalize v)
                  (lsp-mux--json-normalize-plist (cddr plist)))))))

(defun lsp-mux--json-normalize (object)
  "Normalize OBJECT for `json-serialize'."
  (cond
   ((eq object :json-false) :false)
   ((or (eq object t) (null object)) object)
   ((symbolp object)
    (if (keywordp object)
        object
      (symbol-name object)))
   ((and (listp object)
         (keywordp (car-safe object)))
    (lsp-mux--json-normalize-plist object))
   ;; `json-serialize' treats Lisp lists as JSON objects (plist/alist).
   ;; LSP payloads frequently contain arrays represented as plain lists
   ;; (e.g. ("documentation" "details")), so convert those to vectors.
   ((listp object)
    (vconcat (mapcar #'lsp-mux--json-normalize object)))
   ((consp object)
    (cons (lsp-mux--json-normalize (car object))
          (lsp-mux--json-normalize (cdr object))))
   ((vectorp object)
    (vconcat (mapcar #'lsp-mux--json-normalize object)))
   (t object)))

(defun lsp-mux-json-encode (object)
  "Encode OBJECT to an LSP frame string."
  (let* ((json (json-serialize (lsp-mux--json-normalize object)))
         (len (string-bytes json)))
    (format "Content-Length: %d\r\n\r\n%s" len json)))

(defun lsp-mux-json-decode (json)
  "Decode JSON string to plist structure."
  (with-temp-buffer
    (insert json)
    (goto-char (point-min))
    ;; `json-parse-buffer' tolerates trailing bytes after the first JSON value.
    (json-parse-buffer
     :object-type 'plist
     :array-type 'list
     :null-object nil
     :false-object :json-false)))

(defun lsp-mux-session-add-backend (session name send-fn &optional process)
  "Attach a backend to SESSION.
NAME is backend identifier, SEND-FN accepts one decoded JSON object.
Optional PROCESS keeps runtime process handle."
  (setf (lsp-mux-session-backends session)
        (append (lsp-mux-session-backends session)
                (list (lsp-mux-backend-create :name name
                                              :send-fn send-fn
                                              :process process))))
  session)

(defun lsp-mux--session-backend (session backend-name)
  "Return backend named BACKEND-NAME from SESSION."
  (cl-find backend-name (lsp-mux-session-backends session)
           :key #'lsp-mux-backend-name
           :test #'equal))

(defun lsp-mux-session-remove-backend (session backend-name &optional reason)
  "Detach BACKEND-NAME from SESSION and clean related runtime state.

REASON is optional text used for synthetic backend errors on pending requests."
  (when (lsp-mux--session-backend session backend-name)
    (setf (lsp-mux-session-backends session)
          (cl-remove backend-name (lsp-mux-session-backends session)
                     :key #'lsp-mux-backend-name
                     :test #'equal))
    ;; Remove diagnostics and capabilities for this backend.
    (let (affected-uris)
      (maphash
       (lambda (key _diagnostics)
         (when (equal (cdr key) backend-name)
           (push (car key) affected-uris)
           (remhash key (lsp-mux-session-diagnostics-by-uri-backend session))))
       (lsp-mux-session-diagnostics-by-uri-backend session))
      ;; Re-publish aggregated diagnostics for affected URIs.
      (dolist (uri (delete-dups affected-uris))
        (lsp-mux--send-to-client
         session
         (list :jsonrpc "2.0"
               :method "textDocument/publishDiagnostics"
               :params (list :uri uri
                             :diagnostics (lsp-mux--collect-uri-diagnostics session uri))))))
    (remhash backend-name (lsp-mux-session-backend-capabilities session))
    (remhash backend-name (lsp-mux-session-backend-capabilities-known session))
    ;; Remove backend-initiated request mappings for this backend.
    (maphash
     (lambda (client-id record)
       (when (equal (plist-get record :backend-name) backend-name)
         (remhash client-id (lsp-mux-session-backend-request-map session))))
     (lsp-mux-session-backend-request-map session))
    ;; Mark pending requests for this backend as completed with backend-closed error.
    (let (ids)
      (maphash
       (lambda (backend-id pending)
         (when (and (not (lsp-mux-pending-finalized pending))
                    (equal (cdr (assoc backend-name
                                       (lsp-mux-pending-backend-id-map pending)))
                           backend-id))
           (push backend-id ids)
           (setf (lsp-mux-pending-done pending)
                 (1+ (lsp-mux-pending-done pending)))
           (push (cons backend-name
                       (list :code -32004
                             :message (or reason "backend closed")))
                 (lsp-mux-pending-errors pending))
           (when (= (lsp-mux-pending-done pending)
                    (lsp-mux-pending-expected pending))
             (lsp-mux--pending-finalize-and-send session pending))))
       (lsp-mux-session-pending-by-backend-id session))
      (dolist (backend-id ids)
        (remhash backend-id (lsp-mux-session-pending-by-backend-id session)))))
  session)

(defun lsp-mux--result-empty-p (result)
  "Return non-nil if RESULT should be treated as empty."
  (null result))

(defun lsp-mux--merge-generic-results (results)
  "Merge generic RESULTS for merge route."
  (cond
   ((null results) nil)
   ((cl-every #'listp results)
    (cl-loop for r in results append r))
   (t
    (car (last results)))))

(defun lsp-mux--tag-item-with-backend (item backend-name)
  "Attach BACKEND-NAME provenance tag to ITEM."
  (if (and (listp item) (keywordp (car item)))
      (let ((copy (copy-sequence item)))
        (plist-put copy :lsp-mux-backend backend-name))
    item))

(defun lsp-mux--merge-code-action-results (named-results)
  "Merge NAMED-RESULTS for codeAction-like responses."
  (cl-loop for (backend . result) in named-results
           append (cl-loop for item in (or result '())
                           collect (lsp-mux--tag-item-with-backend item backend))))

(defun lsp-mux--completion-items (result)
  "Extract completion items list from RESULT."
  (cond
   ((null result) nil)
   ((and (listp result) (keywordp (car result)))
    (or (plist-get result :items) '()))
   ((listp result) result)
   (t nil)))

(defun lsp-mux--completion-incomplete-p (result)
  "Return non-nil if RESULT indicates incomplete completion."
  (and (listp result)
       (keywordp (car result))
       (plist-get result :isIncomplete)))

(defun lsp-mux--diagnostic-items (result)
  "Extract a diagnostics items list from RESULT."
  (cond
   ((and (listp result) (keywordp (car result)))
    (let ((items (plist-get result :items)))
      (cond
       ((vectorp items) (append items nil))
       ((listp items) items)
       (t nil))))
   ((listp result) result)
   (t nil)))

(defun lsp-mux--merge-diagnostic-results (results)
  "Merge textDocument/diagnostic RESULTS into one response object."
  (let ((items
         (cl-loop for r in results
                  append (lsp-mux--diagnostic-items r))))
    (list :kind "full"
          :items items)))

(defun lsp-mux--merge-completion-results (named-results)
  "Merge NAMED-RESULTS for completion responses into CompletionList."
  (let ((items
         (cl-loop for (backend . result) in named-results
                  append (cl-loop for item in (lsp-mux--completion-items result)
                                  collect (lsp-mux--tag-item-with-backend item backend))))
        (incomplete-p
         (cl-some (lambda (named)
                    (lsp-mux--completion-incomplete-p (cdr named)))
                  named-results)))
    (list :isIncomplete (and incomplete-p t)
          :items items)))

(defun lsp-mux--merge-initialize-results (results)
  "Merge initialize RESULTS according to capabilities semantics."
  (let* ((first (car results))
         (caps (cl-loop for r in results
                        for c = (plist-get r :capabilities)
                        when c collect c))
         (merged-caps (lsp-mux-merge-capabilities caps)))
    ;; Keep diagnosticProvider so clients can use pull diagnostics (e.g. eslint).
    (when merged-caps
      (setq merged-caps (copy-sequence merged-caps)))
    (if first
        (plist-put (copy-sequence first) :capabilities merged-caps)
      nil)))

(defun lsp-mux--finalize-pending (pending)
  "Build final client response payload from PENDING."
  (let* ((ordered-named-results
          (cl-loop for bname in (lsp-mux-pending-backend-order pending)
                   for pair = (assoc bname (lsp-mux-pending-results pending))
                   when pair collect pair))
         (ordered-results (mapcar #'cdr ordered-named-results))
         (method (lsp-mux-pending-method pending))
         (strategy (lsp-mux-pending-strategy pending))
         (result
          (cond
           ((string= method "initialize")
            (lsp-mux--merge-initialize-results ordered-results))
           ((string= method "shutdown")
            nil)
           ((string= method "textDocument/completion")
            (lsp-mux--merge-completion-results ordered-named-results))
           ((string= method "textDocument/codeAction")
            (lsp-mux--merge-code-action-results ordered-named-results))
           ((string= method "textDocument/diagnostic")
            (lsp-mux--merge-diagnostic-results ordered-results))
           ((eq strategy 'first-success)
            (or (cl-find-if-not #'lsp-mux--result-empty-p ordered-results)
                (car (last ordered-results))))
           ((eq strategy 'merge)
            (lsp-mux--merge-generic-results ordered-results))
           (t
            (car ordered-results)))))
    (if result
        (list :jsonrpc "2.0"
              :id (lsp-mux-pending-client-id pending)
              :result result)
      (if-let* ((err (cdar (lsp-mux-pending-errors pending))))
          (list :jsonrpc "2.0"
                :id (lsp-mux-pending-client-id pending)
                :error err)
        (if (< (lsp-mux-pending-done pending)
               (lsp-mux-pending-expected pending))
            (list :jsonrpc "2.0"
                  :id (lsp-mux-pending-client-id pending)
                  :error (list :code -32001 :message "lsp-mux request timeout"))
          (list :jsonrpc "2.0"
                :id (lsp-mux-pending-client-id pending)
                :result nil))))))

(defun lsp-mux--send-to-client (session object)
  "Send OBJECT to the client side of SESSION."
  (when-let* ((send-fn (lsp-mux-session-client-send-fn session)))
    (funcall send-fn object)))

(defun lsp-mux--send-to-backend-safe (backend object trace-tag &rest trace-args)
  "Send OBJECT to BACKEND and trace failures.

TRACE-TAG and TRACE-ARGS are used with `lsp-mux--trace' when send fails."
  (condition-case err
      (progn
        (funcall (lsp-mux-backend-send-fn backend) object)
        t)
    (error
     (when (and (listp object)
                (string= (or (plist-get object :method) "") "initialize"))
       (lsp-mux--trace "initialize-payload=%S" object))
     (apply #'lsp-mux--trace (concat trace-tag " err=%s")
            (append trace-args (list err)))
     nil)))

(defun lsp-mux--trace (fmt &rest args)
  "Append formatted trace line using FMT and ARGS."
  (when lsp-mux-enable-trace
    (with-current-buffer (get-buffer-create lsp-mux-trace-buffer)
      (goto-char (point-max))
      (insert (format-time-string "[%Y-%m-%d %H:%M:%S] "))
      (insert (apply #'format fmt args))
      (insert "\n"))))

(defun lsp-mux--pending-detach-ids (session pending)
  "Remove all pending id mappings in SESSION that point to PENDING."
  (remhash (lsp-mux-pending-client-id pending)
           (lsp-mux-session-pending-by-client-id session))
  (maphash
   (lambda (id value)
     (when (eq value pending)
       (remhash id (lsp-mux-session-pending-by-backend-id session))))
   (lsp-mux-session-pending-by-backend-id session)))

(defun lsp-mux--pending-finalize-and-send (session pending)
  "Finalize PENDING once and send the response via SESSION."
  (unless (lsp-mux-pending-finalized pending)
    (setf (lsp-mux-pending-finalized pending) t)
    (when-let* ((tm (lsp-mux-pending-timer pending)))
      (cancel-timer tm)
      (setf (lsp-mux-pending-timer pending) nil))
    (lsp-mux--pending-detach-ids session pending)
    (lsp-mux--send-to-client session
                             (lsp-mux--finalize-pending pending))))

(defun lsp-mux--pending-timeout (session pending)
  "Handle timeout for PENDING in SESSION."
  (lsp-mux--trace
   "timeout: method=%s client-id=%s done=%d expected=%d"
   (lsp-mux-pending-method pending)
   (lsp-mux-pending-client-id pending)
   (lsp-mux-pending-done pending)
  (lsp-mux-pending-expected pending))
  (lsp-mux--pending-finalize-and-send session pending))

(defun lsp-mux--first-success-can-finalize-early-p (pending backend-name result)
  "Return non-nil if PENDING can finalize early with BACKEND-NAME RESULT."
  (and (not (lsp-mux--result-empty-p result))
       (let ((order (lsp-mux-pending-backend-order pending))
             (results (lsp-mux-pending-results pending))
             (errors (lsp-mux-pending-errors pending))
             (allowed t))
         (while (and order allowed (not (equal (car order) backend-name)))
           (let* ((name (car order))
                  (done-result (assoc name results))
                  (done-error (assoc name errors)))
             (cond
              ((and done-result
                    (not (lsp-mux--result-empty-p (cdr done-result))))
               (setq allowed nil))
              ((or done-result done-error)
               nil)
              (t
               (setq allowed nil))))
           (setq order (cdr order)))
         allowed)))

(defun lsp-mux--diag-key (diag)
  "Return dedup key for diagnostic DIAG."
  (list (plist-get diag :range)
        (plist-get diag :message)
        (plist-get diag :source)
        (plist-get diag :code)))

(defun lsp-mux--normalize-diagnostic (diag backend-name)
  "Normalize DIAG and add attribution for BACKEND-NAME."
  (let ((copy (copy-sequence diag)))
    (unless (plist-get copy :source)
      (setq copy (plist-put copy :source backend-name)))
    (setq copy (plist-put copy :lsp-mux-backends (list backend-name)))
    copy))

(defun lsp-mux--merge-diagnostic-attribution (existing incoming backend-name)
  "Merge attribution of INCOMING into EXISTING for BACKEND-NAME."
  (let* ((old-bes (or (plist-get existing :lsp-mux-backends) '()))
         (new-bes (delete-dups (append old-bes
                                       (or (plist-get incoming :lsp-mux-backends)
                                           (list backend-name))))))
    (plist-put existing :lsp-mux-backends new-bes)))

(defun lsp-mux--collect-uri-diagnostics (session uri)
  "Collect and deduplicate diagnostics for URI from all backends in SESSION."
  (let ((diag-map (make-hash-table :test #'equal)))
    (maphash
     (lambda (key diagnostics)
       (when (equal (car key) uri)
         (let ((backend (cdr key)))
           (dolist (diag diagnostics)
             (let* ((normalized (lsp-mux--normalize-diagnostic diag backend))
                    (dkey (lsp-mux--diag-key normalized))
                    (existing (gethash dkey diag-map)))
               (puthash dkey
                        (if existing
                            (lsp-mux--merge-diagnostic-attribution
                             existing normalized backend)
                          normalized)
                        diag-map))))))
     (lsp-mux-session-diagnostics-by-uri-backend session))
    (let (out)
      (maphash (lambda (_k diag) (push diag out)) diag-map)
      (nreverse out))))

(defun lsp-mux--handle-backend-diagnostics (session backend-name params)
  "Handle publish diagnostics PARAMS from BACKEND-NAME in SESSION."
  (let* ((uri (plist-get params :uri))
         (diagnostics (or (plist-get params :diagnostics) '()))
         (store-key (cons uri backend-name)))
    (lsp-mux--trace "diagnostics: backend=%s uri=%s count=%d"
                    backend-name uri (length diagnostics))
    (puthash store-key diagnostics
             (lsp-mux-session-diagnostics-by-uri-backend session))
    (lsp-mux--send-to-client
     session
     (list :jsonrpc "2.0"
           :method "textDocument/publishDiagnostics"
           :params (list :uri uri
                         :diagnostics (lsp-mux--collect-uri-diagnostics session uri))))))

(defun lsp-mux--clear-uri-diagnostics (session uri)
  "Clear all cached diagnostics for URI in SESSION."
  (let (to-remove)
    (maphash
     (lambda (key _diagnostics)
       (when (equal (car key) uri)
         (push key to-remove)))
     (lsp-mux-session-diagnostics-by-uri-backend session))
    (dolist (key to-remove)
      (remhash key (lsp-mux-session-diagnostics-by-uri-backend session)))
    (when to-remove
      (lsp-mux--trace "diagnostics-clear: uri=%s removed=%d" uri (length to-remove))
      (lsp-mux--send-to-client
       session
       (list :jsonrpc "2.0"
             :method "textDocument/publishDiagnostics"
             :params (list :uri uri :diagnostics '()))))))

(defun lsp-mux--dispatch-request (session method params client-id strategy)
  "Dispatch request from client to backends in SESSION."
  (when (and (string= method "textDocument/codeAction")
             (listp params))
    (let* ((ctx (plist-get params :context))
           (diags (and (listp ctx) (plist-get ctx :diagnostics))))
      (when (null diags)
        (setq ctx (plist-put (or ctx '()) :diagnostics []))
        (setq params (plist-put params :context ctx)))))
  (let ((targets (lsp-mux--targets-for-method session method params)))
    (when (string= method "initialize")
      ;; Start a fresh capability view for a new client initialize cycle.
      (clrhash (lsp-mux-session-backend-capabilities session))
      (clrhash (lsp-mux-session-backend-capabilities-known session)))
    (if (null targets)
        (lsp-mux--send-to-client
         session
         (list :jsonrpc "2.0"
               :id client-id
               :error (list :code -32003
                            :message (format "no backend for method: %s" method))))
      (let ((pending (lsp-mux-pending-create :client-id client-id
                                             :method method
                                             :strategy strategy
                                             :backend-order (mapcar #'lsp-mux-backend-name targets)
                                             :backend-id-map nil
                                             :expected (length targets))))
        (lsp-mux--trace "dispatch: method=%s client-id=%s strategy=%s backends=%s"
                        method client-id strategy
                        (mapcar #'lsp-mux-backend-name targets))
        (puthash client-id pending (lsp-mux-session-pending-by-client-id session))
        (setf (lsp-mux-pending-timer pending)
              (run-at-time lsp-mux-request-timeout-seconds nil
                           #'lsp-mux--pending-timeout session pending))
        (dolist (backend targets)
          (let ((backend-id (lsp-mux-session-next-backend-id session)))
            (setf (lsp-mux-session-next-backend-id session) (1+ backend-id))
            (push (cons (lsp-mux-backend-name backend) backend-id)
                  (lsp-mux-pending-backend-id-map pending))
            (puthash backend-id pending (lsp-mux-session-pending-by-backend-id session))
            (unless (lsp-mux--send-to-backend-safe
                     backend
                     (list :jsonrpc "2.0"
                           :id backend-id
                           :method method
                           :params params)
                     "backend-send-failed: backend=%s method=%s client-id=%s"
                     (lsp-mux-backend-name backend) method client-id)
              (setf (lsp-mux-pending-done pending) (1+ (lsp-mux-pending-done pending)))
              (push (cons (lsp-mux-backend-name backend)
                          (list :code -32002
                                :message "backend send failed"))
                    (lsp-mux-pending-errors pending)))))
        (when (= (lsp-mux-pending-done pending)
                 (lsp-mux-pending-expected pending))
          (lsp-mux--pending-finalize-and-send session pending)))))
  session)

(defun lsp-mux--dispatch-cancel-request (session params)
  "Dispatch client-side $/cancelRequest PARAMS to backend request ids."
  (let* ((client-id (plist-get params :id))
         (pending (and client-id
                       (gethash client-id (lsp-mux-session-pending-by-client-id session)))))
    (if pending
        (progn
          (lsp-mux--trace "cancel: client-id=%s backend-ids=%s"
                          client-id (lsp-mux-pending-backend-id-map pending))
          (dolist (pair (lsp-mux-pending-backend-id-map pending))
            (pcase-let ((`(,backend-name . ,backend-id) pair))
              (when-let* ((backend (lsp-mux--session-backend session backend-name)))
                (lsp-mux--send-to-backend-safe
                 backend
                 (list :jsonrpc "2.0"
                       :method "$/cancelRequest"
                       :params (list :id backend-id))
                 "cancel-dispatch-failed: backend=%s client-id=%s"
                 backend-name client-id)))))
      ;; If no mapping was found, keep current behavior.
      (dolist (backend (lsp-mux-session-backends session))
        (lsp-mux--send-to-backend-safe
         backend
         (list :jsonrpc "2.0"
               :method "$/cancelRequest"
               :params params)
         "cancel-broadcast-failed: backend=%s"
         (lsp-mux-backend-name backend)))))
  session)

(defun lsp-mux--file-uri-to-path (uri)
  "Convert file URI string URI to local path, or nil."
  (when (stringp uri)
    (condition-case nil
        (let ((parsed (url-generic-parse-url uri)))
          (when (and (string= (url-type parsed) "file")
                     (let ((host (url-host parsed)))
                       (or (null host)
                           (string-empty-p host)
                           (string= host "localhost"))))
            (url-unhex-string (or (url-filename parsed) ""))))
      (error nil))))

(defun lsp-mux--path-to-file-uri (path)
  "Convert local PATH to an encoded file URI string."
  (when (stringp path)
    (url-encode-url (concat "file://" path))))

(defun lsp-mux--find-workspace-folder-from-scope-uri (scope-uri)
  "Find workspace folder plist from SCOPE-URI by locating package.json."
  (when-let* ((path (lsp-mux--file-uri-to-path scope-uri)))
    (let ((dir (if (file-directory-p path)
                   (directory-file-name path)
                 (file-name-directory path)))
          (found nil))
      (while (and dir (not found))
        (let ((pkg (expand-file-name "package.json" dir))
              (parent (file-name-directory (directory-file-name dir))))
          (if (file-exists-p pkg)
              (setq found (list :uri (lsp-mux--path-to-file-uri dir)
                                :name (file-name-nondirectory dir)))
            (setq dir (unless (or (null parent) (equal parent dir)) parent)))))
      found)))

(defun lsp-mux--eslint-configuration-for-request-item (item fallback)
  "Build ESLint workspace/configuration response for request ITEM.

FALLBACK is the base configuration plist used when folder lookup fails."
  (let* ((defaults
          '(:validate "on"
            :run "onType"
            :problems (:shortenToSingleLine :json-false)
            :rulesCustomizations []
            :codeAction (:disableRuleComment (:enable t :location "separateLine")
                         :showDocumentation (:enable t))
            :codeActionOnSave (:mode "all")
            :experimental (:useFlatConfig t)
            :useFlatConfig t
            :nodePath :null
            :workingDirectory (:mode "location")))
         (cfg (copy-sequence (or fallback '()))))
    ;; Ensure required settings are always present. Some clients return nil for
    ;; workspace/configuration and user overrides may omit nested keys that
    ;; vscode-eslint-language-server dereferences without guards.
    (dolist (key '(:validate :run :problems :rulesCustomizations
                             :codeAction :codeActionOnSave :experimental
                             :useFlatConfig :nodePath :workingDirectory))
      (unless (plist-member cfg key)
        (setq cfg (plist-put cfg key (plist-get defaults key)))))
    ;; json-serialize encodes plist nil values as {} rather than null.
    ;; Force explicit JSON null for nodePath to keep eslint path handling safe.
    (when (null (plist-get cfg :nodePath))
      (setq cfg (plist-put cfg :nodePath :null)))
    (let ((experimental (plist-get cfg :experimental)))
      (unless (and (listp experimental)
                   (plist-member experimental :useFlatConfig))
        (setq cfg (plist-put cfg :experimental
                             (list :useFlatConfig t)))))
    (when-let* ((scope-uri (plist-get item :scopeUri))
                (workspace-folder
                 (lsp-mux--find-workspace-folder-from-scope-uri scope-uri))
                (workspace-uri (plist-get workspace-folder :uri))
                (workspace-path (lsp-mux--file-uri-to-path workspace-uri)))
      (setq cfg (plist-put cfg :workspaceFolder workspace-folder))
      (setq cfg (plist-put cfg :workingDirectory
                           (list :directory workspace-path))))
    cfg))

(defun lsp-mux--plist-deep-get (plist dotted-key)
  "Get value from PLIST using DOTTED-KEY path like \"a.b.c\"."
  (let ((cur plist)
        (parts (and (stringp dotted-key)
                    (split-string dotted-key "\\." t))))
    (while (and parts (listp cur))
      (setq cur (plist-get cur (intern (concat ":" (car parts)))))
      (setq parts (cdr parts)))
    cur))

(defun lsp-mux--dispatch-client-response-to-backend (session object)
  "Dispatch client response OBJECT to matching backend in SESSION."
  (let* ((client-id (plist-get object :id))
         (record (and client-id
                      (gethash client-id (lsp-mux-session-backend-request-map session)))))
    (when record
      (remhash client-id (lsp-mux-session-backend-request-map session))
      (let* ((backend-name (plist-get record :backend-name))
             (backend-id (plist-get record :backend-id))
             (method (plist-get record :method))
             (result (plist-get object :result)))
        ;; eslint expects strict workspace/configuration payload shapes.
        ;; Do not trust client-returned values here; synthesize deterministic
        ;; section-specific objects (similar to rassumfrassum tslint preset).
        (when (and (string= backend-name "eslint")
                   (string= method "workspace/configuration"))
          (let* ((items (or (plist-get (plist-get record :params) :items) []))
                 (items-list (if (vectorp items) (append items nil) items))
                 (fallback lsp-mux-eslint-fallback-configuration)
                 (normalize-item
                  (lambda (req)
                    (let ((section (plist-get req :section)))
                      (cond
                       ;; section=="" is the one eslint-ls commonly uses.
                       ((or (null section) (string= section ""))
                        (lsp-mux--eslint-configuration-for-request-item
                         req fallback))
                       ;; section=="eslint" should receive plain eslint config object.
                       ((string= section "eslint")
                        fallback)
                       ;; Other sections: return empty object.
                       (t
                        (make-hash-table :test #'equal)))))))
            (lsp-mux--trace "eslint workspace/configuration request items=%S"
                            items-list)
            (setq result
                  (let* ((count (max 1 (length items-list)))
                         (reqs (if (> (length items-list) 0)
                                   items-list
                                 (make-list count nil)))
                         (normalized
                          (cl-loop for req in reqs
                                   collect
                                   (let* ((eslint-cfg
                                           (funcall normalize-item req))
                                          (section (plist-get req :section)))
                                     (cond
                                      ((or (null section) (string= section ""))
                                       eslint-cfg)
                                      ((string= section "eslint")
                                       eslint-cfg)
                                      ((string-prefix-p "eslint." section)
                                       (lsp-mux--plist-deep-get
                                        eslint-cfg
                                        (string-remove-prefix "eslint." section)))
                                      (t
                                       (make-hash-table :test #'equal)))))))
                    (if (vectorp result)
                        (vconcat normalized)
                      normalized)))
          (lsp-mux--trace "eslint workspace/configuration normalized result=%S"
                          result)))
        (when-let* ((backend (lsp-mux--session-backend session backend-name)))
          (lsp-mux--trace "client-response->backend: backend=%s client-id=%s backend-id=%s"
                          backend-name client-id backend-id)
          (lsp-mux--send-to-backend-safe
           backend
           (append (list :jsonrpc "2.0" :id backend-id)
                   (if (plist-member object :result)
                       (list :result result)
                     (list :error (plist-get object :error))))
           "client-response->backend-failed: backend=%s client-id=%s"
           backend-name client-id))
        (lsp-mux--maybe-apply-dynamic-capabilities-from-client-response
         session record object)))
    (and record t)))

(defun lsp-mux--backend-cancel-to-client-id (session backend-name backend-id)
  "Resolve BACKEND-ID from BACKEND-NAME to client id in SESSION."
  (let ((found nil))
    (maphash
     (lambda (client-id record)
       (when (and (equal (plist-get record :backend-name) backend-name)
                  (equal (plist-get record :backend-id) backend-id)
                  (null found))
         (setq found client-id)))
     (lsp-mux-session-backend-request-map session))
    found))

(defun lsp-mux-session-handle-client-object (session object)
  "Handle one decoded client OBJECT for SESSION."
  (let ((method (plist-get object :method))
        (id (plist-get object :id))
        (params (plist-get object :params)))
    (when method
      (lsp-mux--trace "client->mux: method=%s id=%s" method id))
    (cond
     ((and method id)
      (let ((strategy (if (member method '("initialize" "shutdown"))
                          'merge
                        (lsp-mux-route-for-method method))))
        (lsp-mux--dispatch-request session method params id strategy)))
     ((and id (or (plist-member object :result)
                  (plist-member object :error)))
     (or (lsp-mux--dispatch-client-response-to-backend session object)
          session))
     ((and method (string= method "$/cancelRequest"))
      (lsp-mux--dispatch-cancel-request session params))
     (method
      (when (string= method "textDocument/didClose")
        (let ((uri (plist-get (plist-get params :textDocument) :uri)))
          (when uri
            (lsp-mux--clear-uri-diagnostics session uri))))
      (let ((targets (if (lsp-mux--document-sync-notification-p method)
                         (lsp-mux-session-backends session)
                       (lsp-mux--targets-for-method session method params))))
        (if (null targets)
            (lsp-mux--trace "client-notification-dropped: method=%s (no backend target)"
                            method)
          (dolist (backend targets)
            (lsp-mux--send-to-backend-safe
             backend
             (list :jsonrpc "2.0"
                   :method method
                   :params params)
             "client-notification->backend-failed: backend=%s method=%s"
             (lsp-mux-backend-name backend) method)))))
     (t session))))

(defun lsp-mux-session-handle-backend-object (session backend-name object)
  "Handle one decoded backend OBJECT for SESSION from BACKEND-NAME."
  (let ((method (plist-get object :method))
        (id (plist-get object :id)))
    (lsp-mux--trace "backend->mux: backend=%s method=%s id=%s has-result=%s has-error=%s"
                    backend-name method id
                    (plist-member object :result)
                    (plist-member object :error))
    (cond
     ((and id (or (plist-member object :result)
                  (plist-member object :error)))
      (when-let* ((pending (gethash id (lsp-mux-session-pending-by-backend-id session))))
        (unless (lsp-mux-pending-finalized pending)
          (lsp-mux--maybe-record-backend-capabilities session backend-name pending object)
          (remhash id (lsp-mux-session-pending-by-backend-id session))
          (setf (lsp-mux-pending-done pending)
                (1+ (lsp-mux-pending-done pending)))
          (if (plist-member object :error)
              (push (cons backend-name (plist-get object :error))
                    (lsp-mux-pending-errors pending))
            (push (cons backend-name (plist-get object :result))
                  (lsp-mux-pending-results pending)))
          (if (and (eq (lsp-mux-pending-strategy pending) 'first-success)
                   (plist-member object :result)
                   (lsp-mux--first-success-can-finalize-early-p
                    pending backend-name (plist-get object :result)))
              (progn
                (lsp-mux--trace "finalize-early-first-success: method=%s client-id=%s backend=%s"
                                (lsp-mux-pending-method pending)
                                (lsp-mux-pending-client-id pending)
                                backend-name)
                (lsp-mux--pending-finalize-and-send session pending))
            (when (= (lsp-mux-pending-done pending)
                     (lsp-mux-pending-expected pending))
              (lsp-mux--trace "finalize: method=%s client-id=%s"
                              (lsp-mux-pending-method pending)
                              (lsp-mux-pending-client-id pending))
              (lsp-mux--pending-finalize-and-send session pending))))))
     ((and method (null id))
      (cond
       ((string= method "textDocument/publishDiagnostics")
        (lsp-mux--handle-backend-diagnostics
         session backend-name (plist-get object :params)))
       ((string= method "$/cancelRequest")
        (let* ((params (plist-get object :params))
               (backend-cancel-id (plist-get params :id))
               (client-id (and backend-cancel-id
                               (lsp-mux--backend-cancel-to-client-id
                                session backend-name backend-cancel-id))))
          (if client-id
              (progn
                (lsp-mux--trace "backend-cancel->client: backend=%s backend-id=%s client-id=%s"
                                backend-name backend-cancel-id client-id)
                (lsp-mux--send-to-client
                 session
                 (list :jsonrpc "2.0"
                       :method "$/cancelRequest"
                       :params (list :id client-id))))
            ;; If mapping is unavailable, preserve current behavior.
            (lsp-mux--send-to-client session object))))
       (t
        ;; Forward other backend notifications.
        (lsp-mux--send-to-client session object))))
     ((and method id)
      ;; Backend-initiated request (e.g. client/registerCapability, workspace/applyEdit).
      (let ((client-id (lsp-mux-session-next-client-id session)))
        (setf (lsp-mux-session-next-client-id session) (1+ client-id))
        (puthash client-id (lsp-mux--backend-request-record
                            backend-name id method (plist-get object :params))
                 (lsp-mux-session-backend-request-map session))
        (lsp-mux--trace "backend-request->client: backend=%s backend-id=%s client-id=%s method=%s"
                        backend-name id client-id method)
        (lsp-mux--send-to-client
         session
         (list :jsonrpc "2.0"
               :id client-id
               :method method
               :params (plist-get object :params)))))))
  session)

(defun lsp-mux-session-start-backend-process (session name program args)
  "Start backend process PROGRAM with ARGS and add it to SESSION."
  (let* ((proc-name (format "lsp-mux-%s" name))
         (proc (make-process :name proc-name
                             :buffer (generate-new-buffer (format " *%s*" proc-name))
                             :command (cons program args)
                             :coding 'binary
                             :connection-type 'pipe))
         (backend (lsp-mux-backend-create
                   :name name
                   :process proc
                   :send-fn (lambda (object)
                              (process-send-string
                               proc
                               (encode-coding-string
                                (lsp-mux-json-encode object) 'utf-8)))
                   :parser (lsp-mux-parser-create))))
    (set-process-filter
     proc
     (lambda (_proc chunk)
       (let* ((ret (lsp-mux-parser-feed (lsp-mux-backend-parser backend) chunk))
              (payloads (car ret)))
         (setf (lsp-mux-backend-parser backend) (cdr ret))
         (dolist (payload payloads)
           (lsp-mux-session-handle-backend-object
            session name
            (lsp-mux-json-decode
             (decode-coding-string payload 'utf-8 t)))))))
    (set-process-sentinel
     proc
     (lambda (_proc _event)
       (unless (process-live-p proc)
         (lsp-mux-session-remove-backend
          session name (format "backend process exited: %s" name)))))
    (setf (lsp-mux-session-backends session)
          (append (lsp-mux-session-backends session) (list backend)))
    session))

(defun lsp-mux-stdio-open (backend-specs write-fn)
  "Create a stdio-oriented multiplexing session.

BACKEND-SPECS uses the same format as `lsp-mux-backend-commands'.
WRITE-FN is called with framed LSP strings destined for the client
transport (typically stdout)."
  (unless backend-specs
    (user-error "No backend specs provided"))
  (unless (functionp write-fn)
    (user-error "WRITE-FN must be callable"))
  (let ((session
         (lsp-mux-session-create
          :client-send-fn (lambda (obj)
                            (funcall write-fn (lsp-mux-json-encode obj)))
          :client-parser (lsp-mux-parser-create))))
    (dolist (spec backend-specs)
      (pcase-let ((`(,name ,program . ,args) spec))
        (lsp-mux-session-start-backend-process session name program args)))
    session))

(defun lsp-mux-stdio-feed (session chunk)
  "Feed client transport CHUNK into stdio SESSION."
  (let* ((ret (lsp-mux-parser-feed (lsp-mux-session-client-parser session) chunk))
         (payloads (car ret)))
    (setf (lsp-mux-session-client-parser session) (cdr ret))
    (dolist (payload payloads)
      (lsp-mux-session-handle-client-object
       session (lsp-mux-json-decode payload))))
  session)

(defun lsp-mux-stdio-close (session)
  "Close stdio SESSION and all backend processes."
  (lsp-mux-session-stop session))

(defun lsp-mux--session-clear-runtime-state (session)
  "Clear transient runtime state for SESSION."
  (let ((seen (make-hash-table :test #'eq)))
    (maphash
     (lambda (_backend-id pending)
       (unless (gethash pending seen)
         (puthash pending t seen)
         (when-let* ((tm (lsp-mux-pending-timer pending)))
           (cancel-timer tm)
           (setf (lsp-mux-pending-timer pending) nil))))
     (lsp-mux-session-pending-by-backend-id session)))
  (clrhash (lsp-mux-session-pending-by-backend-id session))
  (clrhash (lsp-mux-session-pending-by-client-id session))
  (clrhash (lsp-mux-session-backend-request-map session))
  (clrhash (lsp-mux-session-diagnostics-by-uri-backend session))
  (clrhash (lsp-mux-session-backend-capabilities session))
  (clrhash (lsp-mux-session-backend-capabilities-known session))
  session)

(defun lsp-mux-session-stop (session)
  "Stop SESSION and all backend processes."
  (dolist (backend (lsp-mux-session-backends session))
    (when-let* ((proc (lsp-mux-backend-process backend)))
      (when (process-live-p proc)
        (delete-process proc))
      (when-let* ((buf (process-buffer proc)))
        (kill-buffer buf))))
  (lsp-mux--session-clear-runtime-state session)
  (setf (lsp-mux-session-backends session) nil)
  session)

(defun lsp-mux--server-open-session (server client-proc)
  "Create and register a new session in SERVER for CLIENT-PROC."
  (let ((session
         (lsp-mux-session-create
          :client-process client-proc
          :client-send-fn (lambda (obj)
                            (when (process-live-p client-proc)
                              (process-send-string client-proc
                                                   (lsp-mux-json-encode obj))))
          :client-parser (lsp-mux-parser-create))))
    (dolist (spec (lsp-mux-server-backend-specs server))
      (pcase-let ((`(,name ,program . ,args) spec))
        (lsp-mux-session-start-backend-process session name program args)))
    (puthash client-proc session (lsp-mux-server-sessions server))
    (set-process-filter
     client-proc
     (lambda (_proc chunk)
       (when-let* ((sess (gethash client-proc (lsp-mux-server-sessions server))))
         (let* ((ret (lsp-mux-parser-feed (lsp-mux-session-client-parser sess) chunk))
                (payloads (car ret)))
           (setf (lsp-mux-session-client-parser sess) (cdr ret))
           (dolist (payload payloads)
             (lsp-mux-session-handle-client-object
              sess (lsp-mux-json-decode payload)))))))
    (set-process-sentinel
     client-proc
     (lambda (_proc _event)
       (unless (process-live-p client-proc)
         (when-let* ((sess (gethash client-proc (lsp-mux-server-sessions server))))
           (remhash client-proc (lsp-mux-server-sessions server))
           (lsp-mux-session-stop sess)))))
    session))

(defun lsp-mux-server-start (backend-specs &optional host port)
  "Start lsp-mux TCP server for BACKEND-SPECS on HOST and PORT."
  (let* ((host (or host lsp-mux-listen-host))
         (port (or port lsp-mux-listen-port))
         (server (lsp-mux-server-create :backend-specs backend-specs)))
    (setf (lsp-mux-server-process server)
          (make-network-process
           :name (format "lsp-mux:%s:%s" host port)
           :family 'ipv4
           :host host
           :service port
           :server t
           :noquery t
           :log (lambda (_server client message)
                  (when (or (string-prefix-p "open" message)
                            (string-prefix-p "accept" message))
                    (lsp-mux--server-open-session server client)))))
    server))

(defun lsp-mux-server-stop (server)
  "Stop SERVER and all active client sessions."
  (maphash
   (lambda (client-proc session)
     (ignore-errors
       (lsp-mux-session-stop session))
     (when (process-live-p client-proc)
       (delete-process client-proc)))
   (lsp-mux-server-sessions server))
  (clrhash (lsp-mux-server-sessions server))
  (when-let* ((proc (lsp-mux-server-process server)))
    (when (process-live-p proc)
      (delete-process proc)))
  server)

(defvar lsp-mux--server nil
  "Current running lsp-mux TCP server instance.")

(defvar lsp-mux--active-host nil
  "Host for the currently running lsp-mux server.")

(defvar lsp-mux--active-port nil
  "Port for the currently running lsp-mux server.")

(defun lsp-mux--free-local-port ()
  "Return an available local TCP port."
  (let* ((probe (make-network-process :name "lsp-mux-port-probe"
                                      :host "127.0.0.1"
                                      :service 0
                                      :server t
                                      :noquery t))
         (service (process-contact probe :service))
         (port (cond
                ((integerp service) service)
                ((and (listp service) (integerp (cadr service))) (cadr service))
                (t (user-error "Unexpected process-contact :service value: %S" service)))))
    (delete-process probe)
    port))

(defun lsp-mux-start ()
  "Start lsp-mux using `lsp-mux-backend-commands'."
  (interactive)
  (unless lsp-mux-backend-commands
    (user-error "No backends configured in lsp-mux-backend-commands"))
  (when lsp-mux--server
    (lsp-mux-stop))
  (let ((host lsp-mux-listen-host)
        (port (if lsp-mux-auto-select-port
                  (lsp-mux--free-local-port)
                lsp-mux-listen-port)))
    (setq lsp-mux--server
          (lsp-mux-server-start lsp-mux-backend-commands host port)
          lsp-mux--active-host host
          lsp-mux--active-port port)
    (lsp-mux--trace "server-start: host=%s port=%d backends=%s"
                    host port (mapcar #'car lsp-mux-backend-commands))
    (message "lsp-mux listening on %s:%d" host port)))

(defun lsp-mux-stop ()
  "Stop the active lsp-mux server."
  (interactive)
  (unless lsp-mux--server
    (user-error "lsp-mux is not running"))
  (lsp-mux-server-stop lsp-mux--server)
  (lsp-mux--trace "server-stop")
  (setq lsp-mux--server nil
        lsp-mux--active-host nil
        lsp-mux--active-port nil)
  (message "lsp-mux stopped"))

(defun lsp-mux-eglot-contact ()
  "Return an Eglot server-program contact for currently running lsp-mux."
  (unless (and lsp-mux--active-host lsp-mux--active-port)
    (user-error "lsp-mux is not running"))
  (list "nc" lsp-mux--active-host (number-to-string lsp-mux--active-port)))

(provide 'lsp-mux)
;;; lsp-mux.el ends here
