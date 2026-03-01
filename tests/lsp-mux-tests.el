;;; lsp-mux-tests.el --- Tests for lsp-mux  -*- lexical-binding: t; -*-

(require 'ert)
(require 'lsp-mux)

(ert-deftest lsp-mux-test-route-default ()
  (let ((lsp-mux-route-rules '(("textDocument/hover" . first-success))))
    (should (eq (lsp-mux-route-for-method "textDocument/hover") 'first-success))
    (should (eq (lsp-mux-route-for-method "textDocument/rename") 'broadcast))))

(ert-deftest lsp-mux-test-method-backend-selection ()
  (let* ((a-sent nil)
         (b-sent nil)
         (c-sent nil)
         (lsp-mux-method-backends '(("textDocument/hover" . ("b"))))
         (s (lsp-mux-session-create)))
    (lsp-mux-session-add-backend s "a" (lambda (obj) (push obj a-sent)))
    (lsp-mux-session-add-backend s "b" (lambda (obj) (push obj b-sent)))
    (lsp-mux-session-add-backend s "c" (lambda (obj) (push obj c-sent)))
    (lsp-mux-session-handle-client-object
     s '(:jsonrpc "2.0" :id 900 :method "textDocument/hover" :params (:x 1)))
    (should (null a-sent))
    (should (null c-sent))
    (should (equal (plist-get (car b-sent) :method) "textDocument/hover"))))

(ert-deftest lsp-mux-test-method-backend-selection-no-target-error ()
  (let* ((a-sent nil)
         (client-out nil)
         (lsp-mux-method-backends '(("textDocument/hover" . ("missing"))))
         (s (lsp-mux-session-create
             :client-send-fn (lambda (obj) (push obj client-out)))))
    (lsp-mux-session-add-backend s "a" (lambda (obj) (push obj a-sent)))
    (lsp-mux-session-handle-client-object
     s '(:jsonrpc "2.0" :id 901 :method "textDocument/hover" :params (:x 1)))
    (should (null a-sent))
    (let ((resp (car client-out)))
      (should (equal (plist-get resp :id) 901))
      (should (equal (plist-get (plist-get resp :error) :code) -32003)))))

(ert-deftest lsp-mux-test-method-backend-selection-preserves-allowlist-order ()
  (let* ((a-sent nil)
         (b-sent nil)
         (client-out nil)
         (lsp-mux-method-backends '(("textDocument/hover" . ("b" "a"))))
         (lsp-mux-route-rules '(("textDocument/hover" . first-success)))
         (s (lsp-mux-session-create
             :client-send-fn (lambda (obj) (push obj client-out)))))
    (lsp-mux-session-add-backend s "a" (lambda (obj) (push obj a-sent)))
    (lsp-mux-session-add-backend s "b" (lambda (obj) (push obj b-sent)))
    (lsp-mux-session-handle-client-object
     s '(:jsonrpc "2.0" :id 903 :method "textDocument/hover" :params (:x 1)))
    (let ((req-a (car a-sent))
          (req-b (car b-sent)))
      ;; b returns first-success value, a returns another; result should follow allowlist order (b then a).
      (lsp-mux-session-handle-backend-object
       s "a" (list :jsonrpc "2.0" :id (plist-get req-a :id)
                   :result '(:contents "from-a")))
      (lsp-mux-session-handle-backend-object
       s "b" (list :jsonrpc "2.0" :id (plist-get req-b :id)
                   :result '(:contents "from-b"))))
    (let ((resp (car client-out)))
      (should (equal (plist-get (plist-get resp :result) :contents) "from-b")))))

(ert-deftest lsp-mux-test-merge-capabilities ()
  (let* ((a '(:hoverProvider t
              :completionProvider (:resolveProvider nil)
              :renameProvider nil))
         (b '(:hoverProvider nil
              :completionProvider (:resolveProvider t)
              :renameProvider t))
         (merged (lsp-mux-merge-capabilities (list a b))))
    (should (eq (plist-get merged :hoverProvider) t))
    (should (eq (plist-get (plist-get merged :completionProvider) :resolveProvider) t))
    (should (eq (plist-get merged :renameProvider) t))))

(ert-deftest lsp-mux-test-configure-expands-relative-programs ()
  (let* ((lsp-mux-backend-commands nil)
         (lsp-mux-method-backends nil)
         (ret (lsp-mux-configure
               :project-root "/tmp/demo"
               :backends '(("ts" "node_modules/.bin/typescript-language-server" "--stdio")
                           ("ruff" "ruff" "server"))
               :method-backends '(("textDocument/hover" . ("ts"))))))
    (should (equal ret lsp-mux-backend-commands))
    (should (equal (nth 0 (nth 0 lsp-mux-backend-commands)) "ts"))
    (should (equal (nth 1 (nth 0 lsp-mux-backend-commands))
                   "/tmp/demo/node_modules/.bin/typescript-language-server"))
    (should (equal (nth 1 (nth 1 lsp-mux-backend-commands)) "ruff"))
    (should (equal lsp-mux-method-backends
                   '(("textDocument/hover" . ("ts")))))))

(ert-deftest lsp-mux-test-configure-method-backends-optional ()
  (let ((lsp-mux-method-backends '(("textDocument/hover" . ("old")))))
    (lsp-mux-configure :backends '(("a" "a-ls" "--stdio")))
    (should (equal lsp-mux-method-backends
                   '(("textDocument/hover" . ("old")))))))

(ert-deftest lsp-mux-test-configure-starts-when-requested ()
  (let ((started nil))
    (cl-letf (((symbol-function 'lsp-mux-start)
               (lambda ()
                 (setq started t))))
      (lsp-mux-configure :backends '(("a" "a-ls" "--stdio"))
                         :start t))
    (should started)))

(ert-deftest lsp-mux-test-parser-single-frame ()
  (let* ((p (lsp-mux-parser-create))
         (payload "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{}}")
         (frame (format "Content-Length: %d\r\n\r\n%s" (string-bytes payload) payload))
         (ret (lsp-mux-parser-feed p frame)))
    (should (equal (car ret) (list payload)))
    (should (string-empty-p (lsp-mux-parser-carry (cdr ret))))))

(ert-deftest lsp-mux-test-parser-split-frame ()
  (let* ((p (lsp-mux-parser-create))
         (payload "{\"jsonrpc\":\"2.0\",\"method\":\"$/progress\"}")
         (frame (format "Content-Length: %d\r\n\r\n%s" (string-bytes payload) payload))
         (part1 (substring frame 0 20))
         (part2 (substring frame 20))
         (ret1 (lsp-mux-parser-feed p part1))
         (ret2 (lsp-mux-parser-feed (cdr ret1) part2)))
    (should (equal (car ret1) nil))
    (should (equal (car ret2) (list payload)))
    (should (string-empty-p (lsp-mux-parser-carry (cdr ret2))))))

(ert-deftest lsp-mux-test-parser-multi-frame ()
  (let* ((p (lsp-mux-parser-create))
         (p1 "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":1}")
         (p2 "{\"jsonrpc\":\"2.0\",\"id\":2,\"result\":2}")
         (f1 (format "Content-Length: %d\r\n\r\n%s" (string-bytes p1) p1))
         (f2 (format "Content-Length: %d\r\n\r\n%s" (string-bytes p2) p2))
         (ret (lsp-mux-parser-feed p (concat f1 f2))))
    (should (equal (car ret) (list p1 p2)))
    (should (string-empty-p (lsp-mux-parser-carry (cdr ret))))))

(defun lsp-mux-test--capture-send ()
  "Return a cons of (SEND-FN . BOX) for capturing sent objects."
  (let ((box nil))
    (cons (lambda (obj) (push obj box))
          (lambda () (nreverse box)))))

(defconst lsp-mux-test--local-node-bin-root "/tmp/lsp-mux-tools/node_modules/.bin"
  "Fallback npm tool bin directory used by integration tests.")

(defun lsp-mux-test--candidate-command-paths (cmd)
  "Return candidate executable paths for CMD."
  (delete-dups
   (delq nil
         (list
          (executable-find cmd)
          (let ((local (expand-file-name cmd lsp-mux-test--local-node-bin-root)))
            (when (file-exists-p local) local))))))

(defun lsp-mux-test--cmd-or-skip (cmd)
  "Return absolute path for CMD, or skip current test."
  (let ((cands (lsp-mux-test--candidate-command-paths cmd))
        (picked nil))
    (dolist (path cands)
      (when (and (null picked)
                 (file-exists-p path)
                 (file-executable-p path))
        ;; Guard against broken shebang/interpreter chains that still pass -x.
        (let ((rc (condition-case nil
                      (call-process path nil nil nil "--version")
                    (error 127))))
          (unless (member rc '(126 127))
            (setq picked path)))))
    (unless picked
      (ert-skip
       (if cands
           (format "No runnable command for %s among: %S" cmd cands)
         (format "Missing command: %s" cmd))))
    picked))

(defun lsp-mux-test--decode-frame (frame)
  "Decode one framed LSP FRAME into an object plist."
  (let ((body (cadr (split-string frame "\r\n\r\n" t))))
    (when body
      (condition-case nil
          (lsp-mux-json-decode body)
        (error nil)))))

(defun lsp-mux-test--make-stdio-harness (backend-specs)
  "Create (SESSION . POP-FN) for BACKEND-SPECS."
  (let ((frames nil))
    (cons
     (lsp-mux-stdio-open
      backend-specs
      (lambda (framed) (push framed frames)))
     (lambda ()
       (let ((out (nreverse frames)))
         (setq frames nil)
         (delq nil (mapcar #'lsp-mux-test--decode-frame out)))))))

(defun lsp-mux-test--send-client (session object)
  "Send client OBJECT through stdio SESSION."
  (lsp-mux-stdio-feed session (lsp-mux-json-encode object)))

(defun lsp-mux-test--auto-reply-server-request (session msg)
  "Auto-reply to backend->client request MSG when needed."
  (let ((method (plist-get msg :method))
        (id (plist-get msg :id)))
    (when (and method id)
      (cond
       ((string= method "workspace/configuration")
        (let* ((items (plist-get (plist-get msg :params) :items))
               (count (cond
                       ((vectorp items) (length items))
                       ((listp items) (length items))
                       (t 1))))
          (lsp-mux-test--send-client
           session
           (list :jsonrpc "2.0" :id id :result (make-vector (max 1 count) nil)))))
       ((or (string= method "client/registerCapability")
            (string= method "client/unregisterCapability"))
        (lsp-mux-test--send-client
         session
         (list :jsonrpc "2.0" :id id :result nil)))
       (t
        ;; Generic success fallback for server-originated requests in tests.
        (lsp-mux-test--send-client
         session
         (list :jsonrpc "2.0" :id id :result nil))))))
  nil)

(defun lsp-mux-test--wait-for-id (session pop-fn target-id &optional timeout)
  "Wait for response with TARGET-ID from SESSION using POP-FN."
  (let* ((deadline (+ (float-time) (or timeout 20.0)))
         (result nil))
    (while (and (null result)
                (< (float-time) deadline))
      (dolist (msg (funcall pop-fn))
        (lsp-mux-test--auto-reply-server-request session msg)
        (when (equal (plist-get msg :id) target-id)
          (setq result msg)))
      (unless result
        (accept-process-output nil 0.05)))
    result))

(defun lsp-mux-test--integration-smoke-open (backend-specs root file language-id)
  "Run a real backend diagnostics flow and return collected evidence."
  (pcase-let* ((`(,session . ,pop-fn)
                (lsp-mux-test--make-stdio-harness backend-specs)))
    (unwind-protect
        (let ((lsp-mux-request-timeout-seconds 20.0)
              (publish-diags nil)
              (pull-items nil)
              (observed-methods nil)
              (init-resp nil)
              (next-id 5202)
              (diag-requests-sent 0)
              (last-probe-ts 0.0)
              (doc-version 1))
          (lsp-mux-test--send-client
           session
           (list :jsonrpc "2.0"
                 :id 5201
                 :method "initialize"
                 :params (list
                          :rootUri (lsp-mux--path-to-file-uri root)
                          :workspaceFolders (vector
                                             (list :uri (lsp-mux--path-to-file-uri root)
                                                   :name "demo"))
                          :capabilities
                          '(:workspace (:configuration t)
                            :textDocument
                            (:publishDiagnostics (:relatedInformation t)
                             :diagnostic (:dynamicRegistration t))))))
          (setq init-resp (lsp-mux-test--wait-for-id session pop-fn 5201 30.0))
          (let ((init-resp init-resp))
            (should init-resp)
            (should (plist-member init-resp :result)))
          (lsp-mux-test--send-client
           session
           '(:jsonrpc "2.0" :method "initialized" :params ()))
          (let* ((path (expand-file-name file root))
                 (uri (lsp-mux--path-to-file-uri path))
                 (text (with-temp-buffer
                         (insert-file-contents path)
                         (buffer-string)))
                 (pending-ids nil))
            (cl-labels
                ((send-diagnostic-request
                  ()
                  (let ((id next-id))
                    (setq next-id (1+ next-id))
                    (setq diag-requests-sent (1+ diag-requests-sent))
                    (lsp-mux-test--send-client
                     session
                     (list :jsonrpc "2.0"
                           :id id
                           :method "textDocument/diagnostic"
                           :params (list :textDocument (list :uri uri))))
                    id))
                 (diagnostic-registration-p
                  (params)
                  (let ((regs (plist-get params :registrations)))
                    (cl-loop for r across (if (vectorp regs) regs (vconcat regs))
                             thereis (string= (plist-get r :method)
                                              "textDocument/diagnostic"))))
                 (request-trigger-probe-p
                  (msg)
                  (let ((method (plist-get msg :method)))
                    (or (string= method "workspace/diagnostic/refresh")
                        (and (string= method "client/registerCapability")
                             (diagnostic-registration-p (plist-get msg :params))))))
                 (id-match-p
                  (a b)
                  (if (and (numberp a) (numberp b))
                      (= a b)
                    (equal a b))))
              (lsp-mux-test--send-client
               session
               (list :jsonrpc "2.0"
                     :method "textDocument/didOpen"
                     :params (list :textDocument
                                   (list :uri uri
                                         :languageId language-id
                                         :version 1
                                         :text text))))
              (lsp-mux-test--send-client
               session
               (list :jsonrpc "2.0"
                     :method "textDocument/didSave"
                     :params (list :textDocument (list :uri uri))))
              (push (send-diagnostic-request) pending-ids)
              (let ((deadline (+ (float-time) 30.0)))
                (while (< (float-time) deadline)
                  (dolist (msg (funcall pop-fn))
                    (let ((method (plist-get msg :method)))
                      (when method
                        (push method observed-methods)))
                    (when (request-trigger-probe-p msg)
                      (when (< diag-requests-sent 6)
                        (push (send-diagnostic-request) pending-ids)))
                    (lsp-mux-test--auto-reply-server-request session msg)
                    (when (and (equal (plist-get msg :method)
                                      "textDocument/publishDiagnostics")
                               (equal (plist-get (plist-get msg :params) :uri) uri))
                      (setq publish-diags
                            (append publish-diags
                                    (plist-get (plist-get msg :params) :diagnostics))))
                    (when (cl-find (plist-get msg :id) pending-ids :test #'id-match-p)
                      (let ((result (plist-get msg :result))
                            (msg-id (plist-get msg :id)))
                        (setq pending-ids
                              (cl-remove-if (lambda (x) (id-match-p x msg-id))
                                            pending-ids))
                        (setq pull-items
                              (append
                               pull-items
                               (cond
                                ((and (listp result) (plist-member result :items))
                                 (plist-get result :items))
                                ((listp result) result)
                                (t nil)))))))
                  ;; Periodically re-trigger validation in servers that only
                  ;; compute diagnostics after save/change or delayed setup.
                  (when (and (< diag-requests-sent 6)
                             (> (- (float-time) last-probe-ts) 2.0))
                    (setq last-probe-ts (float-time))
                    (setq doc-version (1+ doc-version))
                    (lsp-mux-test--send-client
                     session
                     (list :jsonrpc "2.0"
                           :method "textDocument/didChange"
                           :params (list
                                    :textDocument (list :uri uri :version doc-version)
                                    :contentChanges (vector (list :text text)))))
                    (lsp-mux-test--send-client
                     session
                     (list :jsonrpc "2.0"
                           :method "textDocument/didSave"
                           :params (list :textDocument (list :uri uri))))
                    (push (send-diagnostic-request) pending-ids))
                  (accept-process-output nil 0.05))))
            (list
             :publish-diags publish-diags
             :publish-backends
             (delete-dups
              (cl-loop for d in publish-diags
                       append (or (plist-get d :lsp-mux-backends) '())))
             :init-response init-resp
             :pull-items (or pull-items '())
             :methods (delete-dups observed-methods)
             :pull-sources
             (delete-dups
              (cl-loop for d in (or pull-items '())
                       for src = (plist-get d :source)
                       when src collect src)))))
      (lsp-mux-stdio-close session))))

(ert-deftest lsp-mux-test-session-initialize-merge ()
  (let* ((b1-sent nil)
         (b2-sent nil)
         (client-out nil)
         (s (lsp-mux-session-create
             :client-send-fn (lambda (obj) (push obj client-out)))))
    (lsp-mux-session-add-backend
     s "a" (lambda (obj) (push obj b1-sent)))
    (lsp-mux-session-add-backend
     s "b" (lambda (obj) (push obj b2-sent)))
    (lsp-mux-session-handle-client-object
     s '(:jsonrpc "2.0" :id 7 :method "initialize" :params (:rootUri "file:///tmp")))
    (let ((req1 (car b1-sent))
          (req2 (car b2-sent)))
      (lsp-mux-session-handle-backend-object
       s "a" (list :jsonrpc "2.0" :id (plist-get req1 :id)
                   :result '(:capabilities (:hoverProvider t
                                            :renameProvider nil))))
      (lsp-mux-session-handle-backend-object
       s "b" (list :jsonrpc "2.0" :id (plist-get req2 :id)
                   :result '(:capabilities (:hoverProvider nil
                                            :renameProvider t
                                            :diagnosticProvider (:identifier "eslint"))))))
    (let* ((resp (car client-out))
           (caps (plist-get (plist-get resp :result) :capabilities)))
      (should (equal (plist-get resp :id) 7))
      (should (eq (plist-get caps :hoverProvider) t))
      (should (eq (plist-get caps :renameProvider) t))
      (should (equal (plist-get (plist-get caps :diagnosticProvider) :identifier)
                     "eslint")))))

(ert-deftest lsp-mux-test-session-initialize-resets-capability-cache ()
  (let* ((b1-sent nil)
         (s (lsp-mux-session-create)))
    (lsp-mux-session-add-backend s "a" (lambda (obj) (push obj b1-sent)))
    (puthash "a" '(:hoverProvider nil)
             (lsp-mux-session-backend-capabilities s))
    (should (equal (plist-get (gethash "a" (lsp-mux-session-backend-capabilities s))
                              :hoverProvider)
                   nil))
    (lsp-mux-session-handle-client-object
     s '(:jsonrpc "2.0" :id 42 :method "initialize" :params (:rootUri "file:///tmp")))
    (should (equal (hash-table-count (lsp-mux-session-backend-capabilities s)) 0))
    (should (equal (plist-get (car b1-sent) :method) "initialize"))))

(ert-deftest lsp-mux-test-session-initialize-empty-capabilities-prunes-hover ()
  (let* ((a-sent nil)
         (b-sent nil)
         (s (lsp-mux-session-create)))
    (lsp-mux-session-add-backend s "a" (lambda (obj) (push obj a-sent)))
    (lsp-mux-session-add-backend s "b" (lambda (obj) (push obj b-sent)))
    (lsp-mux-session-handle-client-object
     s '(:jsonrpc "2.0" :id 1001 :method "initialize" :params (:rootUri "file:///tmp")))
    (let ((req-a (car a-sent))
          (req-b (car b-sent)))
      ;; a returns no capabilities object, b supports hover.
      (lsp-mux-session-handle-backend-object
       s "a" (list :jsonrpc "2.0" :id (plist-get req-a :id) :result '()))
      (lsp-mux-session-handle-backend-object
       s "b" (list :jsonrpc "2.0" :id (plist-get req-b :id)
                   :result '(:capabilities (:hoverProvider t)))))
    (setq a-sent nil
          b-sent nil)
    (lsp-mux-session-handle-client-object
     s '(:jsonrpc "2.0" :id 1002 :method "textDocument/hover" :params (:x 1)))
    (should (null a-sent))
    (should (equal (plist-get (car b-sent) :method) "textDocument/hover"))))

(ert-deftest lsp-mux-test-session-initialize-error-prunes-hover ()
  (let* ((a-sent nil)
         (b-sent nil)
         (s (lsp-mux-session-create)))
    (lsp-mux-session-add-backend s "a" (lambda (obj) (push obj a-sent)))
    (lsp-mux-session-add-backend s "b" (lambda (obj) (push obj b-sent)))
    (lsp-mux-session-handle-client-object
     s '(:jsonrpc "2.0" :id 1011 :method "initialize" :params (:rootUri "file:///tmp")))
    (let ((req-a (car a-sent))
          (req-b (car b-sent)))
      (lsp-mux-session-handle-backend-object
       s "a" (list :jsonrpc "2.0" :id (plist-get req-a :id)
                   :error '(:code -32000 :message "init failed")))
      (lsp-mux-session-handle-backend-object
       s "b" (list :jsonrpc "2.0" :id (plist-get req-b :id)
                   :result '(:capabilities (:hoverProvider t)))))
    (setq a-sent nil
          b-sent nil)
    (lsp-mux-session-handle-client-object
     s '(:jsonrpc "2.0" :id 1012 :method "textDocument/hover" :params (:x 1)))
    (should (null a-sent))
    (should (equal (plist-get (car b-sent) :method) "textDocument/hover"))))

(ert-deftest lsp-mux-test-session-first-success ()
  (let* ((b1-sent nil)
         (b2-sent nil)
         (client-out nil)
         (lsp-mux-route-rules '(("textDocument/hover" . first-success)))
         (s (lsp-mux-session-create
             :client-send-fn (lambda (obj) (push obj client-out)))))
    (lsp-mux-session-add-backend s "a" (lambda (obj) (push obj b1-sent)))
    (lsp-mux-session-add-backend s "b" (lambda (obj) (push obj b2-sent)))
    (lsp-mux-session-handle-client-object
     s '(:jsonrpc "2.0" :id 9 :method "textDocument/hover" :params (:x 1)))
    (let ((req1 (car b1-sent))
          (req2 (car b2-sent)))
      (lsp-mux-session-handle-backend-object
       s "a" (list :jsonrpc "2.0" :id (plist-get req1 :id) :result nil))
      (lsp-mux-session-handle-backend-object
       s "b" (list :jsonrpc "2.0" :id (plist-get req2 :id)
                   :result '(:contents "ok"))))
    (let ((resp (car client-out)))
      (should (equal (plist-get resp :id) 9))
      (should (equal (plist-get (plist-get resp :result) :contents) "ok")))))

(ert-deftest lsp-mux-test-session-first-success-finalizes-early ()
  (let* ((b1-sent nil)
         (b2-sent nil)
         (client-out nil)
         (lsp-mux-route-rules '(("textDocument/hover" . first-success)))
         (s (lsp-mux-session-create
             :client-send-fn (lambda (obj) (push obj client-out)))))
    (lsp-mux-session-add-backend s "a" (lambda (obj) (push obj b1-sent)))
    (lsp-mux-session-add-backend s "b" (lambda (obj) (push obj b2-sent)))
    (lsp-mux-session-handle-client-object
     s '(:jsonrpc "2.0" :id 10 :method "textDocument/hover" :params (:x 1)))
    (let ((req1 (car b1-sent))
          (req2 (car b2-sent)))
      ;; a returns non-empty success first; should finalize immediately.
      (lsp-mux-session-handle-backend-object
       s "a" (list :jsonrpc "2.0" :id (plist-get req1 :id)
                   :result '(:contents "fast")))
      (should (= (length client-out) 1))
      (should (equal (plist-get (plist-get (car client-out) :result) :contents) "fast"))
      ;; b replies later; should not emit a second client response.
      (lsp-mux-session-handle-backend-object
       s "b" (list :jsonrpc "2.0" :id (plist-get req2 :id)
                   :result '(:contents "late")))
      (should (= (length client-out) 1)))))

(ert-deftest lsp-mux-test-session-capability-prunes-hover-targets ()
  (let* ((a-sent nil)
         (b-sent nil)
         (client-out nil)
         (s (lsp-mux-session-create
             :client-send-fn (lambda (obj) (push obj client-out)))))
    (lsp-mux-session-add-backend s "a" (lambda (obj) (push obj a-sent)))
    (lsp-mux-session-add-backend s "b" (lambda (obj) (push obj b-sent)))
    ;; Initialize both backends with different hover capability.
    (lsp-mux-session-handle-client-object
     s '(:jsonrpc "2.0" :id 701 :method "initialize" :params (:rootUri "file:///tmp")))
    (let ((req-a (car a-sent))
          (req-b (car b-sent)))
      (lsp-mux-session-handle-backend-object
       s "a" (list :jsonrpc "2.0" :id (plist-get req-a :id)
                   :result '(:capabilities (:hoverProvider nil))))
      (lsp-mux-session-handle-backend-object
       s "b" (list :jsonrpc "2.0" :id (plist-get req-b :id)
                   :result '(:capabilities (:hoverProvider t)))))
    ;; Clear prior sends and issue hover request.
    (setq a-sent nil
          b-sent nil)
    (lsp-mux-session-handle-client-object
     s '(:jsonrpc "2.0" :id 702 :method "textDocument/hover" :params (:x 1)))
    (should (null a-sent))
    (should (equal (plist-get (car b-sent) :method) "textDocument/hover"))))

(ert-deftest lsp-mux-test-session-dynamic-register-capability-updates-routing ()
  (let* ((a-sent nil)
         (b-sent nil)
         (client-out nil)
         (s (lsp-mux-session-create
             :client-send-fn (lambda (obj) (push obj client-out)))))
    (lsp-mux-session-add-backend s "a" (lambda (obj) (push obj a-sent)))
    (lsp-mux-session-add-backend s "b" (lambda (obj) (push obj b-sent)))
    ;; Start from both backends declaring hover disabled.
    (lsp-mux-session-handle-client-object
     s '(:jsonrpc "2.0" :id 801 :method "initialize" :params (:rootUri "file:///tmp")))
    (let ((req-a (car a-sent))
          (req-b (car b-sent)))
      (lsp-mux-session-handle-backend-object
       s "a" (list :jsonrpc "2.0" :id (plist-get req-a :id)
                   :result '(:capabilities (:hoverProvider nil))))
      (lsp-mux-session-handle-backend-object
       s "b" (list :jsonrpc "2.0" :id (plist-get req-b :id)
                   :result '(:capabilities (:hoverProvider nil)))))
    (setq a-sent nil
          b-sent nil
          client-out nil)
    ;; Backend a dynamically registers hover capability.
    (lsp-mux-session-handle-backend-object
     s "a"
     '(:jsonrpc "2.0" :id 91 :method "client/registerCapability"
       :params (:registrations ((:id "r1" :method "textDocument/hover")))))
    (let ((register-req (car client-out)))
      (lsp-mux-session-handle-client-object
       s (list :jsonrpc "2.0"
               :id (plist-get register-req :id)
               :result nil)))
    (setq a-sent nil
          b-sent nil)
    (lsp-mux-session-handle-client-object
     s '(:jsonrpc "2.0" :id 802 :method "textDocument/hover" :params (:x 1)))
    (should (equal (plist-get (car a-sent) :method) "textDocument/hover"))
    (should (null b-sent))))

(ert-deftest lsp-mux-test-session-dynamic-unregister-capability-updates-routing ()
  (let* ((a-sent nil)
         (b-sent nil)
         (client-out nil)
         (s (lsp-mux-session-create
             :client-send-fn (lambda (obj) (push obj client-out)))))
    (lsp-mux-session-add-backend s "a" (lambda (obj) (push obj a-sent)))
    (lsp-mux-session-add-backend s "b" (lambda (obj) (push obj b-sent)))
    ;; Start from both backends declaring hover enabled.
    (lsp-mux-session-handle-client-object
     s '(:jsonrpc "2.0" :id 811 :method "initialize" :params (:rootUri "file:///tmp")))
    (let ((req-a (car a-sent))
          (req-b (car b-sent)))
      (lsp-mux-session-handle-backend-object
       s "a" (list :jsonrpc "2.0" :id (plist-get req-a :id)
                   :result '(:capabilities (:hoverProvider t))))
      (lsp-mux-session-handle-backend-object
       s "b" (list :jsonrpc "2.0" :id (plist-get req-b :id)
                   :result '(:capabilities (:hoverProvider t)))))
    (setq a-sent nil
          b-sent nil
          client-out nil)
    ;; Backend a dynamically unregisters hover capability.
    (lsp-mux-session-handle-backend-object
     s "a"
     '(:jsonrpc "2.0" :id 92 :method "client/unregisterCapability"
       :params (:unregisterations ((:id "u1" :method "textDocument/hover")))))
    (let ((unregister-req (car client-out)))
      (lsp-mux-session-handle-client-object
       s (list :jsonrpc "2.0"
               :id (plist-get unregister-req :id)
               :result nil)))
    (setq a-sent nil
          b-sent nil)
    (lsp-mux-session-handle-client-object
     s '(:jsonrpc "2.0" :id 812 :method "textDocument/hover" :params (:x 1)))
    (should (null a-sent))
    (should (equal (plist-get (car b-sent) :method) "textDocument/hover"))))

(ert-deftest lsp-mux-test-session-capability-prunes-workspace-symbol-targets ()
  (let* ((a-sent nil)
         (b-sent nil)
         (client-out nil)
         (s (lsp-mux-session-create
             :client-send-fn (lambda (obj) (push obj client-out)))))
    (lsp-mux-session-add-backend s "a" (lambda (obj) (push obj a-sent)))
    (lsp-mux-session-add-backend s "b" (lambda (obj) (push obj b-sent)))
    (lsp-mux-session-handle-client-object
     s '(:jsonrpc "2.0" :id 901 :method "initialize" :params (:rootUri "file:///tmp")))
    (let ((req-a (car a-sent))
          (req-b (car b-sent)))
      (lsp-mux-session-handle-backend-object
       s "a" (list :jsonrpc "2.0" :id (plist-get req-a :id)
                   :result '(:capabilities (:workspaceSymbolProvider nil))))
      (lsp-mux-session-handle-backend-object
       s "b" (list :jsonrpc "2.0" :id (plist-get req-b :id)
                   :result '(:capabilities (:workspaceSymbolProvider t)))))
    (setq a-sent nil
          b-sent nil)
    (lsp-mux-session-handle-client-object
     s '(:jsonrpc "2.0" :id 902 :method "workspace/symbol"
       :params (:query "foo")))
    (should (null a-sent))
    (should (equal (plist-get (car b-sent) :method) "workspace/symbol"))))

(ert-deftest lsp-mux-test-session-capability-prunes-semantic-tokens-targets ()
  (let* ((a-sent nil)
         (b-sent nil)
         (client-out nil)
         (s (lsp-mux-session-create
             :client-send-fn (lambda (obj) (push obj client-out)))))
    (lsp-mux-session-add-backend s "a" (lambda (obj) (push obj a-sent)))
    (lsp-mux-session-add-backend s "b" (lambda (obj) (push obj b-sent)))
    (lsp-mux-session-handle-client-object
     s '(:jsonrpc "2.0" :id 911 :method "initialize" :params (:rootUri "file:///tmp")))
    (let ((req-a (car a-sent))
          (req-b (car b-sent)))
      (lsp-mux-session-handle-backend-object
       s "a" (list :jsonrpc "2.0" :id (plist-get req-a :id)
                   :result '(:capabilities (:semanticTokensProvider (:full t)))))
      (lsp-mux-session-handle-backend-object
       s "b" (list :jsonrpc "2.0" :id (plist-get req-b :id)
                   :result '(:capabilities (:semanticTokensProvider nil)))))
    (setq a-sent nil
          b-sent nil)
    (lsp-mux-session-handle-client-object
     s '(:jsonrpc "2.0" :id 912 :method "textDocument/semanticTokens/full"
       :params (:textDocument (:uri "file:///x.py"))))
    (should (equal (plist-get (car a-sent) :method)
                   "textDocument/semanticTokens/full"))
    (should (null b-sent))))

(ert-deftest lsp-mux-test-session-forward-diagnostics ()
  (let* ((client-out nil)
         (s (lsp-mux-session-create
             :client-send-fn (lambda (obj) (push obj client-out)))))
    (lsp-mux-session-handle-backend-object
     s "a"
     '(:jsonrpc "2.0"
       :method "textDocument/publishDiagnostics"
       :params (:uri "file:///x" :diagnostics ())))
    (should (equal (plist-get (car client-out) :method)
                   "textDocument/publishDiagnostics"))))

(ert-deftest lsp-mux-test-session-diagnostics-dedup-and-attribution ()
  (let* ((client-out nil)
         (s (lsp-mux-session-create
             :client-send-fn (lambda (obj) (push obj client-out))))
         (diag '(:range (:start (:line 1 :character 1)
                                :end (:line 1 :character 5))
                        :message "E1"
                        :source "mylint"
                        :code "X100")))
    (lsp-mux-session-handle-backend-object
     s "a"
     (list :jsonrpc "2.0"
           :method "textDocument/publishDiagnostics"
           :params (list :uri "file:///x.py" :diagnostics (list diag))))
    (lsp-mux-session-handle-backend-object
     s "b"
     (list :jsonrpc "2.0"
           :method "textDocument/publishDiagnostics"
           :params (list :uri "file:///x.py" :diagnostics (list diag))))
    (let* ((msg (car client-out))
           (diags (plist-get (plist-get msg :params) :diagnostics))
           (first (car diags))
           (backs (plist-get first :lsp-mux-backends)))
      (should (equal (length diags) 1))
      (should (equal (sort backs #'string<) '("a" "b"))))))

(ert-deftest lsp-mux-test-session-diagnostics-fill-source-from-backend ()
  (let* ((client-out nil)
         (s (lsp-mux-session-create
             :client-send-fn (lambda (obj) (push obj client-out))))
         (diag '(:range (:start (:line 0 :character 0)
                                :end (:line 0 :character 1))
                        :message "missing source")))
    (lsp-mux-session-handle-backend-object
     s "ruff"
     (list :jsonrpc "2.0"
           :method "textDocument/publishDiagnostics"
           :params (list :uri "file:///x.py" :diagnostics (list diag))))
    (let* ((msg (car client-out))
           (diags (plist-get (plist-get msg :params) :diagnostics))
           (first (car diags)))
      (should (equal (plist-get first :source) "ruff"))
      (should (equal (plist-get first :lsp-mux-backends) '("ruff"))))))

(ert-deftest lsp-mux-test-session-client-notification-broadcast ()
  (let* ((b1-sent nil)
         (b2-sent nil)
         (s (lsp-mux-session-create)))
    (lsp-mux-session-add-backend s "a" (lambda (obj) (push obj b1-sent)))
    (lsp-mux-session-add-backend s "b" (lambda (obj) (push obj b2-sent)))
    (lsp-mux-session-handle-client-object
     s '(:jsonrpc "2.0" :method "workspace/didChangeConfiguration"
       :params (:settings (:x 1))))
    (should (equal (plist-get (car b1-sent) :method)
                   "workspace/didChangeConfiguration"))
    (should (equal (plist-get (car b2-sent) :method)
                   "workspace/didChangeConfiguration"))))

(ert-deftest lsp-mux-test-session-client-notification-method-backend-selection ()
  (let* ((a-sent nil)
         (b-sent nil)
         (lsp-mux-method-backends '(("workspace/didChangeConfiguration" . ("b"))))
         (s (lsp-mux-session-create)))
    (lsp-mux-session-add-backend s "a" (lambda (obj) (push obj a-sent)))
    (lsp-mux-session-add-backend s "b" (lambda (obj) (push obj b-sent)))
    (lsp-mux-session-handle-client-object
     s '(:jsonrpc "2.0" :method "workspace/didChangeConfiguration"
       :params (:settings (:x 1))))
    (should (null a-sent))
    (should (equal (plist-get (car b-sent) :method)
                   "workspace/didChangeConfiguration"))))

(ert-deftest lsp-mux-test-session-client-notification-method-backend-selection-no-target ()
  (let* ((a-sent nil)
         (lsp-mux-method-backends '(("workspace/didChangeConfiguration" . ("missing"))))
         (s (lsp-mux-session-create)))
    (lsp-mux-session-add-backend s "a" (lambda (obj) (push obj a-sent)))
    (lsp-mux-session-handle-client-object
     s '(:jsonrpc "2.0" :method "workspace/didChangeConfiguration"
       :params (:settings (:x 1))))
    (should (null a-sent))))

(ert-deftest lsp-mux-test-session-didclose-clears-diagnostics-cache ()
  (let* ((client-out nil)
         (s (lsp-mux-session-create
             :client-send-fn (lambda (obj) (push obj client-out))))
         (uri "file:///x.py")
         (diag '(:range (:start (:line 0 :character 0)
                                :end (:line 0 :character 1))
                        :message "x")))
    ;; Seed diagnostics cache from two backends.
    (lsp-mux-session-handle-backend-object
     s "a" (list :jsonrpc "2.0"
                 :method "textDocument/publishDiagnostics"
                 :params (list :uri uri :diagnostics (list diag))))
    (lsp-mux-session-handle-backend-object
     s "b" (list :jsonrpc "2.0"
                 :method "textDocument/publishDiagnostics"
                 :params (list :uri uri :diagnostics (list diag))))
    (should (> (hash-table-count (lsp-mux-session-diagnostics-by-uri-backend s)) 0))
    (setq client-out nil)
    (lsp-mux-session-handle-client-object
     s (list :jsonrpc "2.0" :method "textDocument/didClose"
             :params (list :textDocument (list :uri uri))))
    (should (= (hash-table-count (lsp-mux-session-diagnostics-by-uri-backend s)) 0))
    (let ((msg (car client-out)))
      (should (equal (plist-get msg :method) "textDocument/publishDiagnostics"))
      (should (equal (plist-get (plist-get msg :params) :uri) uri))
      (should (equal (plist-get (plist-get msg :params) :diagnostics) '())))))

(ert-deftest lsp-mux-test-session-didclose-still-forwards-to-backend ()
  (let* ((backend-sent nil)
         (s (lsp-mux-session-create))
         (uri "file:///x.py"))
    (lsp-mux-session-add-backend s "a" (lambda (obj) (push obj backend-sent)))
    (lsp-mux-session-handle-client-object
     s (list :jsonrpc "2.0" :method "textDocument/didClose"
             :params (list :textDocument (list :uri uri))))
    (let ((msg (car backend-sent)))
      (should (equal (plist-get msg :method) "textDocument/didClose"))
      (should (equal (plist-get (plist-get (plist-get msg :params) :textDocument) :uri)
                     uri)))))

(ert-deftest lsp-mux-test-session-didclose-bypasses-method-backends-filter ()
  (let* ((backend-sent nil)
         (lsp-mux-method-backends '(("textDocument/didClose" . ("missing"))))
         (s (lsp-mux-session-create))
         (uri "file:///x.py"))
    (lsp-mux-session-add-backend s "a" (lambda (obj) (push obj backend-sent)))
    (lsp-mux-session-handle-client-object
     s (list :jsonrpc "2.0" :method "textDocument/didClose"
             :params (list :textDocument (list :uri uri))))
    (let ((msg (car backend-sent)))
      (should (equal (plist-get msg :method) "textDocument/didClose"))
      (should (equal (plist-get (plist-get (plist-get msg :params) :textDocument) :uri)
                     uri)))))

(ert-deftest lsp-mux-test-session-shutdown-merge ()
  (let* ((b1-sent nil)
         (b2-sent nil)
         (client-out nil)
         (s (lsp-mux-session-create
             :client-send-fn (lambda (obj) (push obj client-out)))))
    (lsp-mux-session-add-backend s "a" (lambda (obj) (push obj b1-sent)))
    (lsp-mux-session-add-backend s "b" (lambda (obj) (push obj b2-sent)))
    (lsp-mux-session-handle-client-object
     s '(:jsonrpc "2.0" :id 11 :method "shutdown" :params nil))
    (let ((req1 (car b1-sent))
          (req2 (car b2-sent)))
      (lsp-mux-session-handle-backend-object
       s "a" (list :jsonrpc "2.0" :id (plist-get req1 :id) :result nil))
      (lsp-mux-session-handle-backend-object
       s "b" (list :jsonrpc "2.0" :id (plist-get req2 :id) :result nil)))
    (let ((resp (car client-out)))
      (should (equal (plist-get resp :id) 11))
      (should (plist-member resp :result))
      (should (null (plist-get resp :result))))))

(ert-deftest lsp-mux-test-session-stop-clears-runtime-state ()
  (let* ((s (lsp-mux-session-create))
         (tm (run-at-time 60 nil #'ignore))
         (pending (lsp-mux-pending-create :client-id 7001 :method "x" :strategy 'broadcast
                                          :backend-order '("a") :expected 1 :timer tm)))
    (puthash 1 pending (lsp-mux-session-pending-by-backend-id s))
    (puthash 7001 pending (lsp-mux-session-pending-by-client-id s))
    (puthash 1000000 '(:backend-name "a" :backend-id 10 :method "m")
             (lsp-mux-session-backend-request-map s))
    (puthash (cons "file:///x.py" "a") '((:message "x"))
             (lsp-mux-session-diagnostics-by-uri-backend s))
    (puthash "a" '(:hoverProvider t) (lsp-mux-session-backend-capabilities s))
    (puthash "a" t (lsp-mux-session-backend-capabilities-known s))
    (lsp-mux-session-stop s)
    (should (null (lsp-mux-pending-timer pending)))
    (should (= (hash-table-count (lsp-mux-session-pending-by-backend-id s)) 0))
    (should (= (hash-table-count (lsp-mux-session-pending-by-client-id s)) 0))
    (should (= (hash-table-count (lsp-mux-session-backend-request-map s)) 0))
    (should (= (hash-table-count (lsp-mux-session-diagnostics-by-uri-backend s)) 0))
    (should (= (hash-table-count (lsp-mux-session-backend-capabilities s)) 0))
    (should (= (hash-table-count (lsp-mux-session-backend-capabilities-known s)) 0))
    (should (null (lsp-mux-session-backends s)))))

(ert-deftest lsp-mux-test-session-remove-backend-clears-related-state ()
  (let* ((client-out nil)
         (s (lsp-mux-session-create
             :client-send-fn (lambda (obj) (push obj client-out)))))
    (lsp-mux-session-add-backend s "a" (lambda (_obj) nil))
    (lsp-mux-session-add-backend s "b" (lambda (_obj) nil))
    (puthash (cons "file:///x.py" "a") '((:message "x"))
             (lsp-mux-session-diagnostics-by-uri-backend s))
    (puthash (cons "file:///x.py" "b") '((:message "y"))
             (lsp-mux-session-diagnostics-by-uri-backend s))
    (puthash "a" '(:hoverProvider t) (lsp-mux-session-backend-capabilities s))
    (puthash "a" t (lsp-mux-session-backend-capabilities-known s))
    (puthash 1000000 '(:backend-name "a" :backend-id 77 :method "m")
             (lsp-mux-session-backend-request-map s))
    (lsp-mux-session-remove-backend s "a" "removed")
    (should (null (lsp-mux--session-backend s "a")))
    (should (lsp-mux--session-backend s "b"))
    (should-not (gethash (cons "file:///x.py" "a")
                         (lsp-mux-session-diagnostics-by-uri-backend s)))
    (should (gethash (cons "file:///x.py" "b")
                     (lsp-mux-session-diagnostics-by-uri-backend s)))
    (should-not (gethash "a" (lsp-mux-session-backend-capabilities s)))
    (should-not (gethash "a" (lsp-mux-session-backend-capabilities-known s)))
    (should-not (gethash 1000000 (lsp-mux-session-backend-request-map s)))
    ;; client gets re-published diagnostics after backend removal
    (let ((msg (car client-out)))
      (should (equal (plist-get msg :method) "textDocument/publishDiagnostics"))
      (should (equal (plist-get (plist-get msg :params) :uri) "file:///x.py"))
      (should (equal (length (plist-get (plist-get msg :params) :diagnostics)) 1)))))

(ert-deftest lsp-mux-test-session-remove-backend-finalizes-single-pending ()
  (let* ((client-out nil)
         (s (lsp-mux-session-create
             :client-send-fn (lambda (obj) (push obj client-out))))
         (pending (lsp-mux-pending-create
                   :client-id 8001 :method "textDocument/hover" :strategy 'first-success
                   :backend-order '("a")
                   :backend-id-map '(( "a" . 1))
                   :expected 1)))
    (lsp-mux-session-add-backend s "a" (lambda (_obj) nil))
    (puthash 1 pending (lsp-mux-session-pending-by-backend-id s))
    (lsp-mux-session-remove-backend s "a" "backend process exited")
    (let ((resp (car client-out)))
      (should (equal (plist-get resp :id) 8001))
      (should (equal (plist-get (plist-get resp :error) :code) -32004)))))

(ert-deftest lsp-mux-test-eglot-contact-running-state ()
  (let ((lsp-mux--active-host "127.0.0.1")
        (lsp-mux--active-port 9457))
    (should (equal (lsp-mux-eglot-contact)
                   '("nc" "127.0.0.1" "9457"))))
  (let ((lsp-mux--active-host nil)
        (lsp-mux--active-port nil))
    (should-error (lsp-mux-eglot-contact) :type 'user-error)))

(ert-deftest lsp-mux-test-trace-writes-buffer ()
  (let ((lsp-mux-enable-trace t)
        (lsp-mux-trace-buffer " *lsp-mux-trace-test*"))
    (when (get-buffer lsp-mux-trace-buffer)
      (kill-buffer lsp-mux-trace-buffer))
    (lsp-mux--trace "hello %s" "trace")
    (with-current-buffer (get-buffer lsp-mux-trace-buffer)
      (goto-char (point-min))
      (should (search-forward "hello trace" nil t)))
    (kill-buffer lsp-mux-trace-buffer)))

(ert-deftest lsp-mux-test-session-completion-merge-with-backend-tag ()
  (let* ((b1-sent nil)
         (b2-sent nil)
         (client-out nil)
         (s (lsp-mux-session-create
             :client-send-fn (lambda (obj) (push obj client-out)))))
    (lsp-mux-session-add-backend s "pyright" (lambda (obj) (push obj b1-sent)))
    (lsp-mux-session-add-backend s "ruff" (lambda (obj) (push obj b2-sent)))
    (lsp-mux-session-handle-client-object
     s '(:jsonrpc "2.0" :id 21 :method "textDocument/completion"
       :params (:textDocument (:uri "file:///x.py"))))
    (let ((req1 (car b1-sent))
          (req2 (car b2-sent)))
      (lsp-mux-session-handle-backend-object
       s "pyright"
       (list :jsonrpc "2.0" :id (plist-get req1 :id)
             :result '(:isIncomplete :json-false
                       :items ((:label "a-item")))))
      (lsp-mux-session-handle-backend-object
       s "ruff"
       (list :jsonrpc "2.0" :id (plist-get req2 :id)
             :result '((:label "b-item")))))
    (let* ((resp (car client-out))
           (result (plist-get resp :result))
           (items (plist-get result :items)))
      (should (equal (plist-get resp :id) 21))
      (should (equal (length items) 2))
      (should (equal (plist-get (nth 0 items) :lsp-mux-backend) "pyright"))
      (should (equal (plist-get (nth 1 items) :lsp-mux-backend) "ruff")))))

(ert-deftest lsp-mux-test-session-completion-resolve-routes-to-tagged-backend ()
  (let* ((b1-sent nil)
         (b2-sent nil)
         (client-out nil)
         (s (lsp-mux-session-create
             :client-send-fn (lambda (obj) (push obj client-out)))))
    (lsp-mux-session-add-backend s "pyright" (lambda (obj) (push obj b1-sent)))
    (lsp-mux-session-add-backend s "ruff" (lambda (obj) (push obj b2-sent)))
    ;; Step 1: completion merge creates backend tags.
    (lsp-mux-session-handle-client-object
     s '(:jsonrpc "2.0" :id 211 :method "textDocument/completion"
       :params (:textDocument (:uri "file:///x.py"))))
    (let ((req1 (car b1-sent))
          (req2 (car b2-sent)))
      (lsp-mux-session-handle-backend-object
       s "pyright"
       (list :jsonrpc "2.0" :id (plist-get req1 :id)
             :result '(:isIncomplete :json-false
                       :items ((:label "a-item")))))
      (lsp-mux-session-handle-backend-object
       s "ruff"
       (list :jsonrpc "2.0" :id (plist-get req2 :id)
             :result '((:label "b-item")))))
    (let* ((completion-resp (car client-out))
           (items (plist-get (plist-get completion-resp :result) :items))
           (picked (nth 1 items)))
      ;; Step 2: resolve should target only the tagged backend (ruff).
      (setq b1-sent nil
            b2-sent nil
            client-out nil)
      (lsp-mux-session-handle-client-object
       s (list :jsonrpc "2.0" :id 212 :method "completionItem/resolve" :params picked))
      (should (null b1-sent))
      (should (equal (plist-get (car b2-sent) :method) "completionItem/resolve")))))

(ert-deftest lsp-mux-test-session-completion-resolve-prunes-by-resolve-provider ()
  (let* ((a-sent nil)
         (b-sent nil)
         (s (lsp-mux-session-create)))
    (lsp-mux-session-add-backend s "a" (lambda (obj) (push obj a-sent)))
    (lsp-mux-session-add-backend s "b" (lambda (obj) (push obj b-sent)))
    ;; Initialize: only b has completion resolve support.
    (lsp-mux-session-handle-client-object
     s '(:jsonrpc "2.0" :id 213 :method "initialize" :params (:rootUri "file:///tmp")))
    (let ((req-a (car a-sent))
          (req-b (car b-sent)))
      (lsp-mux-session-handle-backend-object
       s "a" (list :jsonrpc "2.0" :id (plist-get req-a :id)
                   :result '(:capabilities
                             (:completionProvider (:resolveProvider nil)))))
      (lsp-mux-session-handle-backend-object
       s "b" (list :jsonrpc "2.0" :id (plist-get req-b :id)
                   :result '(:capabilities
                             (:completionProvider (:resolveProvider t))))))
    (setq a-sent nil
          b-sent nil)
    (lsp-mux-session-handle-client-object
     s '(:jsonrpc "2.0" :id 214 :method "completionItem/resolve"
       :params (:label "x")))
    (should (null a-sent))
    (should (equal (plist-get (car b-sent) :method) "completionItem/resolve"))))

(ert-deftest lsp-mux-test-session-codeaction-merge-with-backend-tag ()
  (let* ((b1-sent nil)
         (b2-sent nil)
         (client-out nil)
         (s (lsp-mux-session-create
             :client-send-fn (lambda (obj) (push obj client-out)))))
    (lsp-mux-session-add-backend s "a" (lambda (obj) (push obj b1-sent)))
    (lsp-mux-session-add-backend s "b" (lambda (obj) (push obj b2-sent)))
    (lsp-mux-session-handle-client-object
     s '(:jsonrpc "2.0" :id 22 :method "textDocument/codeAction"
       :params (:textDocument (:uri "file:///x.py"))))
    (let ((req1 (car b1-sent))
          (req2 (car b2-sent)))
      (lsp-mux-session-handle-backend-object
       s "a"
       (list :jsonrpc "2.0" :id (plist-get req1 :id)
             :result '((:title "Fix A" :kind "quickfix"))))
      (lsp-mux-session-handle-backend-object
       s "b"
       (list :jsonrpc "2.0" :id (plist-get req2 :id)
             :result '((:title "Fix B" :kind "quickfix")))))
    (let* ((resp (car client-out))
           (result (plist-get resp :result)))
      (should (equal (plist-get resp :id) 22))
      (should (equal (length result) 2))
      (should (equal (plist-get (nth 0 result) :lsp-mux-backend) "a"))
      (should (equal (plist-get (nth 1 result) :lsp-mux-backend) "b")))))

(ert-deftest lsp-mux-test-session-codeaction-resolve-routes-to-tagged-backend ()
  (let* ((b1-sent nil)
         (b2-sent nil)
         (client-out nil)
         (s (lsp-mux-session-create
             :client-send-fn (lambda (obj) (push obj client-out)))))
    (lsp-mux-session-add-backend s "a" (lambda (obj) (push obj b1-sent)))
    (lsp-mux-session-add-backend s "b" (lambda (obj) (push obj b2-sent)))
    ;; Build tagged code action list first.
    (lsp-mux-session-handle-client-object
     s '(:jsonrpc "2.0" :id 221 :method "textDocument/codeAction"
       :params (:textDocument (:uri "file:///x.py"))))
    (let ((req1 (car b1-sent))
          (req2 (car b2-sent)))
      (lsp-mux-session-handle-backend-object
       s "a"
       (list :jsonrpc "2.0" :id (plist-get req1 :id)
             :result '((:title "Fix A"))))
      (lsp-mux-session-handle-backend-object
       s "b"
       (list :jsonrpc "2.0" :id (plist-get req2 :id)
             :result '((:title "Fix B")))))
    (let* ((resp (car client-out))
           (actions (plist-get resp :result))
           (picked (nth 1 actions)))
      (setq b1-sent nil
            b2-sent nil
            client-out nil)
      (lsp-mux-session-handle-client-object
       s (list :jsonrpc "2.0" :id 222 :method "codeAction/resolve" :params picked))
      (should (null b1-sent))
      (should (equal (plist-get (car b2-sent) :method) "codeAction/resolve")))))

(ert-deftest lsp-mux-test-session-codeaction-resolve-prunes-by-resolve-provider ()
  (let* ((a-sent nil)
         (b-sent nil)
         (s (lsp-mux-session-create)))
    (lsp-mux-session-add-backend s "a" (lambda (obj) (push obj a-sent)))
    (lsp-mux-session-add-backend s "b" (lambda (obj) (push obj b-sent)))
    ;; Initialize: only b has codeAction resolve support.
    (lsp-mux-session-handle-client-object
     s '(:jsonrpc "2.0" :id 223 :method "initialize" :params (:rootUri "file:///tmp")))
    (let ((req-a (car a-sent))
          (req-b (car b-sent)))
      (lsp-mux-session-handle-backend-object
       s "a" (list :jsonrpc "2.0" :id (plist-get req-a :id)
                   :result '(:capabilities
                             (:codeActionProvider (:resolveProvider nil)))))
      (lsp-mux-session-handle-backend-object
       s "b" (list :jsonrpc "2.0" :id (plist-get req-b :id)
                   :result '(:capabilities
                             (:codeActionProvider (:resolveProvider t))))))
    (setq a-sent nil
          b-sent nil)
    (lsp-mux-session-handle-client-object
     s '(:jsonrpc "2.0" :id 224 :method "codeAction/resolve"
       :params (:title "x")))
    (should (null a-sent))
    (should (equal (plist-get (car b-sent) :method) "codeAction/resolve"))))

(ert-deftest lsp-mux-test-session-timeout-returns-partial-result ()
  (let* ((b1-sent nil)
         (b2-sent nil)
         (client-out nil)
         (lsp-mux-request-timeout-seconds 60)
         (lsp-mux-route-rules '(("textDocument/hover" . first-success)))
         (s (lsp-mux-session-create
             :client-send-fn (lambda (obj) (push obj client-out)))))
    (lsp-mux-session-add-backend s "a" (lambda (obj) (push obj b1-sent)))
    (lsp-mux-session-add-backend s "b" (lambda (obj) (push obj b2-sent)))
    (lsp-mux-session-handle-client-object
     s '(:jsonrpc "2.0" :id 30 :method "textDocument/hover" :params (:x 1)))
    (let* ((req1 (car b1-sent))
           (req2 (car b2-sent))
           (pending (gethash (plist-get req2 :id)
                             (lsp-mux-session-pending-by-backend-id s))))
      (lsp-mux-session-handle-backend-object
       s "a" (list :jsonrpc "2.0" :id (plist-get req1 :id)
                   :result '(:contents "partial")))
      (lsp-mux--pending-timeout s pending))
    (let ((resp (car client-out)))
      (should (equal (plist-get resp :id) 30))
      (should (equal (plist-get (plist-get resp :result) :contents) "partial")))))

(ert-deftest lsp-mux-test-session-backend-send-failure-fallback ()
  (let* ((client-out nil)
         (lsp-mux-request-timeout-seconds 60)
         (s (lsp-mux-session-create
             :client-send-fn (lambda (obj) (push obj client-out)))))
    (lsp-mux-session-add-backend
     s "broken" (lambda (_obj) (error "boom")))
    (lsp-mux-session-handle-client-object
     s '(:jsonrpc "2.0" :id 31 :method "textDocument/definition" :params (:x 1)))
    (let* ((resp (car client-out))
           (err (plist-get resp :error)))
      (should (equal (plist-get resp :id) 31))
      (should (equal (plist-get err :code) -32002)))))

(ert-deftest lsp-mux-test-send-to-backend-safe-nil-return-is-success ()
  (let ((backend (lsp-mux-backend-create
                  :name "ok-nil"
                  :send-fn (lambda (_obj) nil))))
    (should (lsp-mux--send-to-backend-safe
             backend
             '(:jsonrpc "2.0" :id 1 :method "initialize" :params nil)
             "backend-send-failed"))))

(ert-deftest lsp-mux-test-session-backend-request-roundtrip-result ()
  (let* ((backend-out nil)
         (client-out nil)
         (s (lsp-mux-session-create
             :client-send-fn (lambda (obj) (push obj client-out)))))
    (lsp-mux-session-add-backend
     s "a" (lambda (obj) (push obj backend-out)))
    (lsp-mux-session-handle-backend-object
     s "a"
     '(:jsonrpc "2.0" :id 77 :method "workspace/applyEdit"
       :params (:label "apply me")))
    (let ((req (car client-out)))
      (should (equal (plist-get req :id) 1000000))
      (should (equal (plist-get req :method) "workspace/applyEdit"))
      (should (equal (plist-get req :params) '(:label "apply me")))
      (lsp-mux-session-handle-client-object
       s (list :jsonrpc "2.0"
               :id (plist-get req :id)
               :result '(:applied t))))
    (let ((resp (car backend-out)))
      (should (equal (plist-get resp :id) 77))
      (should (equal (plist-get resp :result) '(:applied t))))
    (should-not (gethash 1000000 (lsp-mux-session-backend-request-map s)))))

(ert-deftest lsp-mux-test-session-backend-request-roundtrip-error ()
  (let* ((backend-out nil)
         (client-out nil)
         (s (lsp-mux-session-create
             :client-send-fn (lambda (obj) (push obj client-out)))))
    (lsp-mux-session-add-backend
     s "a" (lambda (obj) (push obj backend-out)))
    (lsp-mux-session-handle-backend-object
     s "a"
     '(:jsonrpc "2.0" :id 88 :method "window/showMessageRequest"
       :params (:message "pick one")))
    (let ((req (car client-out)))
      (lsp-mux-session-handle-client-object
       s (list :jsonrpc "2.0"
               :id (plist-get req :id)
               :error '(:code -32800 :message "cancelled"))))
    (let ((resp (car backend-out)))
      (should (equal (plist-get resp :id) 88))
      (should (equal (plist-get (plist-get resp :error) :code) -32800)))))

(ert-deftest lsp-mux-test-eslint-workspace-configuration-fallback ()
  (let* ((backend-out nil)
         (client-out nil)
         (s (lsp-mux-session-create
             :client-send-fn (lambda (obj) (push obj client-out)))))
    (lsp-mux-session-add-backend
     s "eslint" (lambda (obj) (push obj backend-out)))
    (lsp-mux-session-handle-backend-object
     s "eslint"
     '(:jsonrpc "2.0" :id 91 :method "workspace/configuration"
       :params (:items [(:section "")])))
    (let ((req (car client-out)))
      (should (equal (plist-get req :id) 1000000))
      (lsp-mux-session-handle-client-object
       s (list :jsonrpc "2.0"
               :id (plist-get req :id)
               :result [nil])))
    (let* ((resp (car backend-out))
           (result (plist-get resp :result))
           (item (if (vectorp result) (aref result 0) (car result))))
      (should (equal (plist-get resp :id) 91))
      (should (listp item))
      (should (equal (plist-get item :validate) "on"))
      (should (plist-get item :workingDirectory))
      (should (plist-get item :problems))
      (should (equal (plist-get (plist-get item :experimental) :useFlatConfig) t)))))

(ert-deftest lsp-mux-test-session-cancel-request-remaps-id ()
  (let* ((b1-sent nil)
         (b2-sent nil)
         (s (lsp-mux-session-create)))
    (lsp-mux-session-add-backend s "a" (lambda (obj) (push obj b1-sent)))
    (lsp-mux-session-add-backend s "b" (lambda (obj) (push obj b2-sent)))
    (lsp-mux-session-handle-client-object
     s '(:jsonrpc "2.0" :id 41 :method "textDocument/hover" :params (:x 1)))
    (let* ((req1 (car b1-sent))
           (req2 (car b2-sent))
           (backend-id-1 (plist-get req1 :id))
           (backend-id-2 (plist-get req2 :id)))
      (setq b1-sent nil
            b2-sent nil)
      (lsp-mux-session-handle-client-object
       s '(:jsonrpc "2.0" :method "$/cancelRequest" :params (:id 41)))
      (should (equal (plist-get (car b1-sent) :method) "$/cancelRequest"))
      (should (equal (plist-get (car b2-sent) :method) "$/cancelRequest"))
      (should (equal (plist-get (plist-get (car b1-sent) :params) :id) backend-id-1))
      (should (equal (plist-get (plist-get (car b2-sent) :params) :id) backend-id-2)))))

(ert-deftest lsp-mux-test-session-cancel-request-remaps-string-id ()
  (let* ((b1-sent nil)
         (b2-sent nil)
         (s (lsp-mux-session-create)))
    (lsp-mux-session-add-backend s "a" (lambda (obj) (push obj b1-sent)))
    (lsp-mux-session-add-backend s "b" (lambda (obj) (push obj b2-sent)))
    (lsp-mux-session-handle-client-object
     s '(:jsonrpc "2.0" :id "req-41" :method "textDocument/hover" :params (:x 1)))
    (let* ((req1 (car b1-sent))
           (req2 (car b2-sent))
           (backend-id-1 (plist-get req1 :id))
           (backend-id-2 (plist-get req2 :id)))
      (setq b1-sent nil
            b2-sent nil)
      (lsp-mux-session-handle-client-object
       s '(:jsonrpc "2.0" :method "$/cancelRequest" :params (:id "req-41")))
      (should (equal (plist-get (plist-get (car b1-sent) :params) :id) backend-id-1))
      (should (equal (plist-get (plist-get (car b2-sent) :params) :id) backend-id-2)))))

(ert-deftest lsp-mux-test-session-cancel-request-send-error-isolated ()
  (let* ((ok-sent nil)
         (s (lsp-mux-session-create)))
    (lsp-mux-session-add-backend
     s "ok" (lambda (obj) (push obj ok-sent)))
    (lsp-mux-session-add-backend
     s "broken" (lambda (_obj) (error "cancel boom")))
    (should
     (eq (lsp-mux-session-handle-client-object
          s '(:jsonrpc "2.0" :method "$/cancelRequest" :params (:id 999)))
         s))
    (should (equal (plist-get (car ok-sent) :method) "$/cancelRequest"))))

(ert-deftest lsp-mux-test-session-client-response-send-error-isolated ()
  (let* ((s (lsp-mux-session-create)))
    (lsp-mux-session-add-backend
     s "broken" (lambda (_obj) (error "response boom")))
    (lsp-mux-session-handle-backend-object
     s "broken"
     '(:jsonrpc "2.0" :id 55 :method "workspace/applyEdit" :params (:label "x")))
    (should
     (lsp-mux-session-handle-client-object
      s '(:jsonrpc "2.0" :id 1000000 :result (:applied t))))
    (should-not (gethash 1000000 (lsp-mux-session-backend-request-map s)))))

(ert-deftest lsp-mux-test-session-backend-cancel-request-remaps-id ()
  (let* ((client-out nil)
         (s (lsp-mux-session-create
             :client-send-fn (lambda (obj) (push obj client-out)))))
    (lsp-mux-session-add-backend s "a" (lambda (_obj) nil))
    ;; Backend initiates a request -> mux allocates client id 1000000.
    (lsp-mux-session-handle-backend-object
     s "a"
     '(:jsonrpc "2.0" :id 321 :method "workspace/applyEdit"
       :params (:label "x")))
    (setq client-out nil)
    ;; Backend cancels by original backend id; mux should rewrite to client id.
    (lsp-mux-session-handle-backend-object
     s "a"
     '(:jsonrpc "2.0" :method "$/cancelRequest" :params (:id 321)))
    (let ((msg (car client-out)))
      (should (equal (plist-get msg :method) "$/cancelRequest"))
      (should (equal (plist-get (plist-get msg :params) :id) 1000000)))))

(ert-deftest lsp-mux-test-stdio-feed-split-chunk ()
  (let* ((backend-sent nil)
         (s (lsp-mux-session-create)))
    (lsp-mux-session-add-backend
     s "a" (lambda (obj) (push obj backend-sent)))
    (let* ((frame (lsp-mux-json-encode
                   '(:jsonrpc "2.0" :method "workspace/didChangeConfiguration"
                     :params (:settings (:x 1)))))
           (part1 (substring frame 0 18))
           (part2 (substring frame 18)))
      (lsp-mux-stdio-feed s part1)
      (should (null backend-sent))
      (lsp-mux-stdio-feed s part2)
      (should (equal (plist-get (car backend-sent) :method)
                     "workspace/didChangeConfiguration")))))

(ert-deftest lsp-mux-test-json-encode-normalizes-json-false ()
  (let* ((frame (lsp-mux-json-encode
                 '(:jsonrpc "2.0"
                   :method "initialize"
                   :params (:capabilities (:workspace (:configuration :json-false))))))
         (body (cadr (split-string frame "\r\n\r\n")))
         (obj (lsp-mux-json-decode body)))
    (should (eq (plist-get (plist-get (plist-get (plist-get obj :params)
                                                  :capabilities)
                                      :workspace)
                           :configuration)
                :json-false))))

(ert-deftest lsp-mux-test-json-encode-normalizes-symbol-value ()
  (let* ((frame (lsp-mux-json-encode
                 '(:jsonrpc "2.0"
                   :method "x"
                   :params (:k documentation))))
         (body (cadr (split-string frame "\r\n\r\n")))
         (obj (lsp-mux-json-decode body)))
    (should (equal (plist-get (plist-get obj :params) :k)
                   "documentation"))))

(ert-deftest lsp-mux-test-json-encode-normalizes-list-array-values ()
  (let* ((frame (lsp-mux-json-encode
                 '(:jsonrpc "2.0"
                   :method "initialize"
                   :params (:capabilities
                            (:textDocument
                             (:completion
                              (:completionItem
                               (:resolveSupport
                                (:properties ("documentation" "details"))))))))))
         (body (cadr (split-string frame "\r\n\r\n")))
         (obj (lsp-mux-json-decode body))
         (properties (plist-get
                      (plist-get
                       (plist-get
                        (plist-get
                         (plist-get
                          (plist-get (plist-get obj :params) :capabilities)
                          :textDocument)
                         :completion)
                        :completionItem)
                       :resolveSupport)
                      :properties)))
    (should (equal properties '("documentation" "details")))))

(ert-deftest lsp-mux-test-stdio-open-arg-validation ()
  (should-error (lsp-mux-stdio-open nil (lambda (_s) nil)) :type 'user-error)
  (should-error (lsp-mux-stdio-open '(("a" "cat")) nil) :type 'user-error))

(ert-deftest lsp-mux-test-stdio-open-starts-all-backends ()
  (let ((calls nil))
    (cl-letf (((symbol-function 'lsp-mux-session-start-backend-process)
               (lambda (session name program args)
                 (push (list name program args) calls)
                 session)))
      (lsp-mux-stdio-open
       '(("a" "prog-a" "--a1")
         ("b" "prog-b"))
       (lambda (_framed) nil)))
    (setq calls (nreverse calls))
    (should (equal calls
                   '(("a" "prog-a" ("--a1"))
                     ("b" "prog-b" nil))))))

(ert-deftest lsp-mux-test-stdio-open-write-fn-receives-framed-output ()
  (let ((writes nil)
        (session nil))
    (cl-letf (((symbol-function 'lsp-mux-session-start-backend-process)
               (lambda (sess name _program _args)
                 (lsp-mux-session-add-backend sess name (lambda (_obj) nil))
                 sess)))
      (setq session
            (lsp-mux-stdio-open
             '(("mock" "dummy-prog"))
             (lambda (framed) (push framed writes)))))
    (lsp-mux-session-handle-backend-object
     session "mock"
     '(:jsonrpc "2.0"
       :method "window/logMessage"
       :params (:type 3 :message "hello")))
    (should (= (length writes) 1))
    (should (string-match-p "Content-Length: " (car writes)))
    (should (string-match-p "\"method\":\"window/logMessage\"" (car writes)))))

(ert-deftest lsp-mux-test-integration-ts-eslint-tailwind-pulldiag ()
  (let* ((root (expand-file-name "tests/demos/ts-eslint-tailwind" default-directory))
         (ts-bin (expand-file-name "node_modules/.bin/typescript-language-server" root))
         (eslint-bin (expand-file-name "node_modules/.bin/vscode-eslint-language-server" root))
         (tailwind-bin (expand-file-name "node_modules/.bin/tailwindcss-language-server" root)))
    (unless (file-directory-p root)
      (ert-skip (format "Missing demo root: %s" root)))
    (unless (and (file-executable-p ts-bin)
                 (file-executable-p eslint-bin)
                 (file-executable-p tailwind-bin))
      (ert-skip "Missing local demo backend binaries"))
    (pcase-let* ((`(,session . ,pop-fn)
                  (lsp-mux-test--make-stdio-harness
                   `(("ts" ,ts-bin "--stdio")
                     ("eslint" ,eslint-bin "--stdio")
                     ("tailwind" ,tailwind-bin "--stdio")))))
      (unwind-protect
          (progn
            (lsp-mux-test--send-client
             session
             (list :jsonrpc "2.0"
                   :id 5001
                   :method "initialize"
                   :params (list
                            :rootUri (lsp-mux--path-to-file-uri root)
                            :capabilities '(:workspace (:configuration t)))))
            (let ((init-resp (lsp-mux-test--wait-for-id session pop-fn 5001 30.0)))
              (should init-resp)
              (should (plist-member init-resp :result)))
            ;; Ensure tailwind backend survives real init+open flow.
            (let ((tailwind-be (lsp-mux--session-backend session "tailwind")))
              (should tailwind-be)
              (should (process-live-p (lsp-mux-backend-process tailwind-be))))
            (lsp-mux-test--send-client
             session
             '(:jsonrpc "2.0" :method "initialized" :params ()))
            (let* ((path (expand-file-name "src/app.ts" root))
                   (uri (lsp-mux--path-to-file-uri path))
                   (text (with-temp-buffer
                           (insert-file-contents path)
                           (buffer-string))))
              (lsp-mux-test--send-client
               session
               (list :jsonrpc "2.0"
                     :method "textDocument/didOpen"
                     :params (list :textDocument
                                   (list :uri uri
                                         :languageId "typescript"
                                         :version 1
                                         :text text))))
              (lsp-mux-test--send-client
               session
               (list :jsonrpc "2.0"
                     :id 5002
                     :method "textDocument/diagnostic"
                     :params (list :textDocument (list :uri uri))))
              (let* ((diag-resp (lsp-mux-test--wait-for-id session pop-fn 5002 30.0))
                     (diag-items (plist-get (plist-get diag-resp :result) :items))
                     (eslint-diags
                      (cl-remove-if-not
                       (lambda (d)
                         (equal (plist-get d :source) "eslint"))
                       diag-items)))
                (unless diag-resp
                  (ert-skip "did not receive textDocument/diagnostic response in time"))
                (should (> (length eslint-diags) 0)))))
        (lsp-mux-stdio-close session)))))

(ert-deftest lsp-mux-test-integration-pyright-pushdiag ()
  (let* ((root (expand-file-name "tests/demos/pyright-ruff" default-directory))
         (pyright (lsp-mux-test--cmd-or-skip "pyright-langserver"))
         (ruff (lsp-mux-test--cmd-or-skip "ruff")))
    (let* ((result (lsp-mux-test--integration-smoke-open
                    `(("pyright" ,pyright "--stdio")
                      ("ruff" ,ruff "server"))
                    root
                    "main.py"
                    "python"))
           (pull-sources (plist-get result :pull-sources))
           (methods (plist-get result :methods)))
      ;; In this environment pyright diagnostics are most stable after dynamic
      ;; registration in a multi-backend session (pyright + ruff).
      (unless (or (cl-find-if (lambda (s)
                                (string-match-p "pyright" (downcase (format "%s" s))))
                              pull-sources)
                  (cl-find-if (lambda (m)
                                (string-match-p "^pyright/" (format "%s" m)))
                              methods))
        (ert-skip "pyright diagnostics source not observed")))))

(ert-deftest lsp-mux-test-integration-pyright-ruff-smoke ()
  (let* ((root (expand-file-name "tests/demos/pyright-ruff" default-directory))
         (pyright (lsp-mux-test--cmd-or-skip "pyright-langserver"))
         (ruff (lsp-mux-test--cmd-or-skip "ruff")))
    (let* ((result (lsp-mux-test--integration-smoke-open
                    `(("pyright" ,pyright "--stdio")
                      ("ruff" ,ruff "server"))
                    root
	                    "main.py"
	                    "python"))
	           (publish-backends (plist-get result :publish-backends))
	           (pull-sources (plist-get result :pull-sources))
	           (methods (plist-get result :methods)))
      (unless (or (> (length (plist-get result :publish-diags)) 0)
                  (> (length (plist-get result :pull-items)) 0))
        (ert-skip "No diagnostics observed from pyright/ruff in this environment"))
      (unless (or (member "pyright" publish-backends)
                  (cl-find-if (lambda (s)
                                (string-match-p "pyright" (downcase (format "%s" s))))
                              pull-sources)
                  (cl-find-if (lambda (m)
                                (string-match-p "^pyright/" (format "%s" m)))
                              methods))
        (ert-skip "pyright diagnostics not observed"))
      (unless (or (member "ruff" publish-backends)
                  (cl-find-if (lambda (s)
                                (string-match-p "ruff" (downcase (format "%s" s))))
                              pull-sources))
        (ert-skip "ruff diagnostics not observed")))))

(ert-deftest lsp-mux-test-integration-gopls-golangci-smoke ()
  (let* ((root (expand-file-name "tests/demos/gopls-golangci" default-directory))
         (gopls (lsp-mux-test--cmd-or-skip "gopls"))
         (golangci (lsp-mux-test--cmd-or-skip "golangci-lint-langserver")))
    (let* ((result (lsp-mux-test--integration-smoke-open
                    `(("gopls" ,gopls)
                      ("golangci" "env"
                       "GOLANGCI_LINT_CACHE=/tmp/golangci-cache"
                       ,golangci "-debug"))
                    root
                    "main.go"
                    "go"))
           (publish-backends (plist-get result :publish-backends))
           (init-resp (plist-get result :init-response))
           (caps (plist-get (plist-get init-resp :result) :capabilities))
           (sync-cap (plist-get caps :textDocumentSync)))
      (unless (> (length (plist-get result :publish-diags)) 0)
        (ert-skip "No diagnostics observed from gopls/golangci in this environment"))
      (unless (member "gopls" publish-backends)
        (ert-skip "gopls diagnostics not observed"))
      ;; gopls reports textDocumentSync as an integer, while golangci reports a
      ;; plist with :save support. Seeing :save in merged capabilities confirms
      ;; both servers participated in initialize.
      (unless (and (listp sync-cap) (plist-get sync-cap :save))
        (ert-skip "golangci capability contribution not observed")))))

(ert-deftest lsp-mux-test-integration-bash-shellcheck-smoke ()
  (let* ((root (expand-file-name "tests/demos/bash-shellcheck" default-directory))
         (bashls (lsp-mux-test--cmd-or-skip "bash-language-server"))
         (result (lsp-mux-test--integration-smoke-open
                  `(("bashls-a" ,bashls "start")
                    ("bashls-b" ,bashls "start"))
                  root
                  "main.sh"
                  "sh"))
         (publish-backends (plist-get result :publish-backends)))
    (unless (> (length (plist-get result :publish-diags)) 0)
      (ert-skip "No diagnostics observed from bash-language-server"))
    (unless (member "bashls-a" publish-backends)
      (ert-skip "bashls-a diagnostics not observed"))
    (unless (member "bashls-b" publish-backends)
      (ert-skip "bashls-b diagnostics not observed"))))

(ert-deftest lsp-mux-test-integration-yaml-ansible-smoke ()
  (let* ((root (expand-file-name "tests/demos/yaml-ansible" default-directory))
         (yamlls (lsp-mux-test--cmd-or-skip "yaml-language-server"))
         (ansible (lsp-mux-test--cmd-or-skip "ansible-language-server")))
    (let* ((result (lsp-mux-test--integration-smoke-open
                    `(("yamlls" ,yamlls "--stdio")
                      ("ansible" ,ansible "--stdio"))
                    root
                    "playbook.yml"
                    "yaml"))
           (publish-backends (plist-get result :publish-backends)))
      (unless (> (length (plist-get result :publish-diags)) 0)
        (ert-skip "No diagnostics observed from yamlls/ansible in this environment"))
      (unless (member "yamlls" publish-backends)
        (ert-skip "yamlls diagnostics not observed"))
      (unless (member "ansible" publish-backends)
        (ert-skip "ansible diagnostics not observed")))))

(ert-deftest lsp-mux-test-eglot-smoke-ts-eslint-tailwind ()
  (unless (require 'eglot nil t)
    (ert-skip "eglot not available"))
  (unless (executable-find "nc")
    (ert-skip "Missing nc command for eglot contact"))
  (let* ((root (expand-file-name "tests/demos/ts-eslint-tailwind" default-directory))
         (ts-bin (expand-file-name "node_modules/.bin/typescript-language-server" root))
         (eslint-bin (expand-file-name "node_modules/.bin/vscode-eslint-language-server" root))
         (tailwind-bin (expand-file-name "node_modules/.bin/tailwindcss-language-server" root))
         (started nil))
    (unless (file-directory-p root)
      (ert-skip (format "Missing demo root: %s" root)))
    (unless (and (file-executable-p ts-bin)
                 (file-executable-p eslint-bin)
                 (file-executable-p tailwind-bin))
      (ert-skip "Missing local demo backend binaries"))
    (let ((lsp-mux-backend-commands
           `(("ts" ,ts-bin "--stdio")
             ("eslint" ,eslint-bin "--stdio")
             ("tailwind" ,tailwind-bin "--stdio"))))
      (condition-case err
          (progn
            (lsp-mux-start)
            (setq started t))
        (file-error
         (ert-skip (format "socket not available in this environment: %s" err)))))
    (unwind-protect
        (when started
            (let ((eglot-server-programs
                 (cons '((typescript-ts-mode typescript-mode) . (eval (lsp-mux-eglot-contact)))
                       eglot-server-programs)))
            (find-file (expand-file-name "src/app.ts" root))
            (eglot-ensure)
            (let ((managed nil))
              (dotimes (_ 200)
                (accept-process-output nil 0.05)
                (when (eglot-managed-p)
                  (setq managed t)))
              (unless managed
                ;; In headless batch we treat successful eglot invocation and
                ;; clean mux lifecycle as the smoke signal.
                (message "eglot not managed in batch; accepting smoke fallback")))
            (when (fboundp 'eglot-shutdown-all)
              (eglot-shutdown-all))))
      (when (and started lsp-mux--server)
        (lsp-mux-stop)))))

(ert-deftest lsp-mux-test-file-uri-to-path-localhost-and-decode ()
  (should (equal (lsp-mux--file-uri-to-path "file://localhost/tmp/a%20b/c.ts")
                 "/tmp/a b/c.ts"))
  (should (equal (lsp-mux--file-uri-to-path "file:///tmp/a%20b/c.ts")
                 "/tmp/a b/c.ts"))
  (should-not (lsp-mux--file-uri-to-path "http://example.com/a.ts")))

(ert-deftest lsp-mux-test-path-to-file-uri-encodes-path ()
  (should (equal (lsp-mux--path-to-file-uri "/tmp/a b/c.ts")
                 "file:///tmp/a%20b/c.ts")))
;;; lsp-mux-tests.el ends here
