;;; eglot-semtok.el --- Semantic tokens -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 emacs-lsp maintainers
;; Copyright (C) 2025 Lua Reis <me@lua.blog.br>
;;
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
;;
;;; Commentary:
;;
;;  Semantic tokens
;;  https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_semanticTokens
;;
;;; Code:

(require 'eglot)

(defgroup eglot-semtok nil
  "LSP support for semantic-tokens."
  :prefix "eglot-semtok-"
  :group 'eglot
  :tag "Eglot Semantic tokens")

(defcustom eglot-semtok-warn-on-missing-face nil
  "Warning on missing face for token type/modifier.
When non-nil, this option will emit a warning any time a token
or modifier type returned by a language server has no face associated with it."
  :group 'eglot-semtok
  :type 'boolean)

(defcustom eglot-semtok-apply-modifiers t
  "Whether semantic tokens should take token modifiers into account."
  :group 'eglot-semtok
  :type 'boolean)

(defcustom eglot-semtok-allow-ranged-requests t
  "Whether to use ranged semantic token requests when available.

Note that even when this is set to t, delta requests will
be preferred whenever possible, unless
`eglot-semtok-allow-delta-requests' is false."
  :group 'eglot-semtok
  :type 'boolean)

(defcustom eglot-semtok-allow-delta-requests t
  "Whether to use semantic token delta requests when available.

When supported by the language server, delta requests are always
preferred over both full and ranged token requests."
  :group 'eglot-semtok
  :type 'boolean)

(defcustom eglot-semtok-honor-refresh-requests nil
  "Whether to honor semanticTokens/refresh requests.

When set to nil, refresh requests will be silently discarded.
When set to t, semantic tokens will be re-requested for all buffers
associated with the requesting language server."
  :group 'eglot-semtok
  :type 'boolean)

(defcustom eglot-semtok-enable-multiline-token-support t
  "When set to nil, tokens will be truncated after end-of-line."
  :group 'eglot-semtok
  :type 'boolean)

(defface eglot-semtok-constant-face
  '((t :inherit font-lock-constant-face))
  "Face used for semantic highlighting scopes matching constant scopes."
  :group 'eglot-semtok)

(defface eglot-semtok-variable-face
  '((t :inherit font-lock-variable-name-face))
  "Face used for semantic highlighting scopes matching variable.*.
Unless overridden by a more specific face association."
  :group 'eglot-semtok)

(defface eglot-semtok-function-face
  '((t :inherit font-lock-function-name-face))
  "Face used for semantic highlighting scopes matching entity.name.function.*.
Unless overridden by a more specific face association."
  :group 'eglot-semtok)

(defface eglot-semtok-method-face
  '((t :inherit eglot-semtok-function-face))
  "Face used for semantic highlighting scopes matching entity.name.method.*.
Unless overridden by a more specific face association."
  :group 'eglot-semtok)

(defface eglot-semtok-namespace-face
  '((t :inherit font-lock-type-face :weight bold))
  "Face used for semantic highlighting scopes matching entity.name.namespace.*.
Unless overridden by a more specific face association."
  :group 'eglot-semtok)

(defface eglot-semtok-comment-face
  '((t (:inherit font-lock-comment-face)))
  "Face used for comments."
  :group 'eglot-semtok)

(defface eglot-semtok-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "Face used for keywords."
  :group 'eglot-semtok)

(defface eglot-semtok-string-face
  '((t (:inherit font-lock-string-face)))
  "Face used for keywords."
  :group 'eglot-semtok)

(defface eglot-semtok-number-face
  '((t (:inherit font-lock-constant-face)))
  "Face used for numbers."
  :group 'eglot-semtok)

(defface eglot-semtok-regexp-face
  '((t (:inherit font-lock-string-face :slant italic)))
  "Face used for regexps."
  :group 'eglot-semtok)

(defface eglot-semtok-operator-face
  '((t (:inherit font-lock-function-name-face)))
  "Face used for operators."
  :group 'eglot-semtok)

(defface eglot-semtok-namespace-face
  '((t (:inherit font-lock-keyword-face)))
  "Face used for namespaces."
  :group 'eglot-semtok)

(defface eglot-semtok-type-face
  '((t (:inherit font-lock-type-face)))
  "Face used for types."
  :group 'eglot-semtok)

(defface eglot-semtok-struct-face
  '((t (:inherit font-lock-type-face)))
  "Face used for structs."
  :group 'eglot-semtok)

(defface eglot-semtok-class-face
  '((t (:inherit font-lock-type-face)))
  "Face used for classes."
  :group 'eglot-semtok)

(defface eglot-semtok-interface-face
  '((t (:inherit font-lock-type-face)))
  "Face used for interfaces."
  :group 'eglot-semtok)

(defface eglot-semtok-enum-face
  '((t (:inherit font-lock-type-face)))
  "Face used for enums."
  :group 'eglot-semtok)

(defface eglot-semtok-type-parameter-face
  '((t (:inherit font-lock-type-face)))
  "Face used for type parameters."
  :group 'eglot-semtok)

;; function face already defined, move here when support
;; for theia highlighting gets removed
(defface eglot-semtok-member-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face used for members."
  :group 'eglot-semtok)

(defface eglot-semtok-property-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face used for properties."
  :group 'eglot-semtok)

(defface eglot-semtok-event-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face used for event properties."
  :group 'eglot-semtok)

(defface eglot-semtok-macro-face
  '((t (:inherit font-lock-preprocessor-face)))
  "Face used for macros."
  :group 'eglot-semtok)

(defface eglot-semtok-variable-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face used for variables."
  :group 'eglot-semtok)

(defface eglot-semtok-parameter-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face used for parameters."
  :group 'eglot-semtok)

(defface eglot-semtok-label-face
  '((t (:inherit font-lock-comment-face)))
  "Face used for labels."
  :group 'eglot-semtok)

(defface eglot-semtok-deprecated-face
  '((t :strike-through t))
  "Face used for semantic highlighting scopes matching constant scopes."
  :group 'eglot-semtok)

(defface eglot-semtok-definition-face
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face used for definition modifier."
  :group 'eglot-semtok)

(defface eglot-semtok-implementation-face
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face used for implementation modifier."
  :group 'eglot-semtok)

(defface eglot-semtok-default-library-face
  '((t :inherit font-lock-builtin-face))
  "Face used for defaultLibrary modifier."
  :group 'eglot-semtok)

(defface eglot-semtok-static-face
  '((t :inherit font-lock-keyword-face))
  "Face used for static modifier."
  :group 'eglot-semtok)

(defvar-local eglot-semtok-faces
  '(("comment" . eglot-semtok-comment-face)
    ("keyword" . eglot-semtok-keyword-face)
    ("string" . eglot-semtok-string-face)
    ("number" . eglot-semtok-number-face)
    ("regexp" . eglot-semtok-regexp-face)
    ("operator" . eglot-semtok-operator-face)
    ("namespace" . eglot-semtok-namespace-face)
    ("type" . eglot-semtok-type-face)
    ("struct" . eglot-semtok-struct-face)
    ("class" . eglot-semtok-class-face)
    ("interface" . eglot-semtok-interface-face)
    ("enum" . eglot-semtok-enum-face)
    ("typeParameter" . eglot-semtok-type-parameter-face)
    ("function" . eglot-semtok-function-face)
    ("method" . eglot-semtok-method-face)
    ("member" . eglot-semtok-member-face)
    ("property" . eglot-semtok-property-face)
    ("event" . eglot-semtok-event-face)
    ("macro" . eglot-semtok-macro-face)
    ("variable" . eglot-semtok-variable-face)
    ("parameter" . eglot-semtok-parameter-face)
    ("label" . eglot-semtok-label-face)
    ("enumConstant" . eglot-semtok-constant-face)
    ("enumMember" . eglot-semtok-constant-face)
    ("dependent" . eglot-semtok-type-face)
    ("concept" . eglot-semtok-interface-face))
  "Faces to use for semantic tokens.")

(defvar-local eglot-semtok-modifier-faces
  '(("declaration" . eglot-semtok-interface-face)
    ("definition" . eglot-semtok-definition-face)
    ("implementation" . eglot-semtok-implementation-face)
    ("readonly" . eglot-semtok-constant-face)
    ("static" . eglot-semtok-static-face)
    ("deprecated" . eglot-semtok-deprecated-face)
    ("abstract" . eglot-semtok-keyword-face)
    ("async" . eglot-semtok-macro-face)
    ("modification" . eglot-semtok-operator-face)
    ("documentation" . eglot-semtok-comment-face)
    ("defaultLibrary" . eglot-semtok-default-library-face))
  "Semantic tokens modifier faces.
Faces to use for semantic token modifiers if
`eglot-semtok-apply-modifiers' is non-nil.")

(defclass eglot-semtok-server (eglot-lsp-server)
  ((semtok-faces
    :initform nil
    :documentation "Semantic tokens faces.")
   (semtok-modifier-faces
    :initform nil
    :documentation "Semantic tokens modifier faces.")
   (semtok-face-overrides
    :initform nil
    :documentation "Overrides for semantic tokens faces."))
  :documentation
  "Represents an Eglot server with semantic tokens support.")

(cl-defmethod eglot-client-capabilities ((server eglot-semtok-server))
  (append
   (cl-call-next-method server)
   (list :semanticTokens
         (list :dynamicRegistration :json-false
               :requests '(:range t :full t)
               :tokenModifiers
               (when eglot-semtok-apply-modifiers
                 (vconcat
                  (mapcar #'car (eglot-semtok--modifier-faces-for server))))
               :overlappingTokenSupport t
               :multilineTokenSupport
               (if eglot-semtok-enable-multiline-token-support
                   t :json-false)
               :tokenTypes
               (vconcat
                (mapcar #'car (eglot-semtok--type-faces-for server)))
               :formats '("relative")))))

(defvar eglot-semtok--pending-full-token-requests '()
  "Buffers which should have their semantic tokens refreshed on idle.

This is an alist of the form ((buffer_i . fontify_immediately_i) ...); entries
with fontify_immediately set to t will immediately refontify once their
token request is answered.")

;; NOTE: doesn't keep track of outstanding requests, so might still produce large latency outliers
;; if the language server doesn't process all outstanding token requests within one lsp-idle-delay
(defcustom eglot-semtok-max-concurrent-idle-requests 1
  "Maximum number of on-idle token requests to be dispatched simultaneously."
  :group 'eglot-semtok
  :type 'integer)

(defvar eglot-semtok--idle-timer nil)

(defun eglot-semtok--process-pending-requests ()
  (let ((fuel eglot-semtok-max-concurrent-idle-requests))
    (while (and eglot-semtok--pending-full-token-requests (> fuel 0))
      (cl-destructuring-bind (buffer . fontify-immediately)
          (pop eglot-semtok--pending-full-token-requests)
        (when (buffer-live-p buffer)
          (setq fuel (1- fuel))
          (with-current-buffer buffer
            (eglot-semtok--request nil fontify-immediately))))))
  (unless eglot-semtok--pending-full-token-requests
    (cancel-timer eglot-semtok--idle-timer)
    (setq eglot-semtok--idle-timer nil)))

(defun eglot-semtok--sort-pending-requests (pending-requests)
  ;; service currently visible buffers first, otherwise prefer immediate-fontification requests
  (sort (seq-filter (lambda (it) (buffer-live-p (car it))) pending-requests)
   :lessp (lambda (entry-a entry-b)
            (let ((a-hidden (eq nil (get-buffer-window (car entry-a))))
                  (b-hidden (eq nil (get-buffer-window (car entry-b)))))
              (cond ((and b-hidden (not a-hidden)) t)   ; sort a before b
                    ((and a-hidden (not b-hidden)) nil) ; sort b before a
                    ((and (not (cdr entry-a)) (cdr entry-b)) nil) ; otherwise sort b before a only if b is immediate and a is not
                    (t t))))))
        
(defun eglot-semtok--request-full-token-set-when-idle (buffer fontify-immediately)
  "Request full token set after an idle timeout of `lsp-idle-delay'.

If FONTIFY-IMMEDIATELY is non-nil, fontification will be performed immediately
 once the corresponding response is received."
  (let ((do-fontify-immediately (or fontify-immediately
                                    (cdr (assoc buffer eglot-semtok--pending-full-token-requests)))))
    (setq eglot-semtok--pending-full-token-requests
          (eglot-semtok--sort-pending-requests
           (cons (cons buffer do-fontify-immediately)
                 (cl-remove buffer eglot-semtok--pending-full-token-requests :key #'car)))))
  (unless eglot-semtok--idle-timer
    (setq eglot-semtok--idle-timer
          (run-with-idle-timer eglot-send-changes-idle-time t #'eglot-semtok--process-pending-requests))))

(defun eglot-semtok--refresh-if-enabled (buffer)
  (when (buffer-local-value 'eglot-semtok-mode buffer)
    (eglot-semtok--request-full-token-set-when-idle buffer t)))

(defvar-local eglot-semtok--cache nil
  "Previously returned token set.

When non-nil, `eglot-semtok--cache' should adhere to the
following lsp-interface:
`(_SemanticTokensCache
  (:_documentVersion)
  (:response :_region :_truncated))'.")

(defsubst eglot-semtok--putcache (k v)
  "Set key K of `eglot-semtok--cache' to V."
  (setq eglot-semtok--cache
        (plist-put eglot-semtok--cache k v)))

(defvar-local eglot-semtok--teardown nil)

(defun eglot-semtok--ingest-range-response (response)
  "Handle RESPONSE to semanticTokens/range request."
  (eglot-semtok--putcache :response response)
  (cl-assert (plist-get eglot-semtok--cache :_region))
  (eglot-semtok--request-full-token-set-when-idle (current-buffer) nil))

(defun eglot-semtok--ingest-full-response (response)
  "Handle RESPONSE to semanticTokens/full request."
  (eglot-semtok--putcache :response response)
  (cl-assert (not (plist-get eglot-semtok--cache :_region))))

(defsubst eglot-semtok--apply-delta-edits (old-data edits)
  "Apply EDITS obtained from full/delta request to OLD-DATA."
  (let* ((old-token-count (length old-data))
         (old-token-index 0)
         (substrings))
    (cl-loop
     for edit across edits
     do
     (when (< old-token-index (plist-get edit :start))
       (push (substring old-data old-token-index (plist-get edit :start)) substrings))
     (push (plist-get edit :data) substrings)
     (setq old-token-index (+ (plist-get edit :start) (plist-get edit :deleteCount)))
     finally do (push (substring old-data old-token-index old-token-count) substrings))
    (vconcat (nreverse substrings))))

(defun eglot-semtok--ingest-full/delta-response (response)
  "Handle RESPONSE to semanticTokens/full/delta request."
  (if (plist-get response :edits)
      (let ((old-data (plist-get (plist-get eglot-semtok--cache :response) :data)))
        (cl-assert (not (plist-get eglot-semtok--cache :_region)))
        (when old-data
          (eglot-semtok--putcache
           :response (plist-put response
                                :data (eglot-semtok--apply-delta-edits
                                       old-data (plist-get response :edits))))))
    ;; server decided to send full response instead
    (eglot-semtok--ingest-full-response response)))

(defun eglot-region-range (beg end)
  "Return a LSP range representing region BEG to END."
  (list :start (eglot--pos-to-lsp-position beg)
        :end (eglot--pos-to-lsp-position end)))

(defun eglot-semtok--request (region fontify-immediately)
  "Send semantic tokens request to the language server.

A full/delta request will be sent if delta requests are supported by
the language server, allowed via `eglot-semtok-allow-delta-requests',
and if a full set of tokens had previously been received.
Otherwise, a ranged request will be dispatched if REGION is non-nil,
ranged requests are supported by the language server, and allowed via
`eglot-semtok-allow-delta-requests'. In all other cases, a full
tokens request will be dispatched.

If FONTIFY-IMMEDIATELY is non-nil, fontification will be performed immediately
 upon receiving the response."
  (let ((request-type :textDocument/semanticTokens/full)
        (request (list :textDocument (eglot--TextDocumentIdentifier)))
        (response-handler nil)
        (final-region nil))
    (cond
     ((and eglot-semtok-allow-delta-requests
           (eglot-server-capable :textDocument/semanticTokensFull/Delta)
           (let ((response (plist-get eglot-semtok--cache :response)))
             (and (plist-get response :resultId) (plist-get response :data)
                  (not (plist-get eglot-semtok--cache :_region)))))
      (setq request-type :textDocument/semanticTokens/full/delta)
      (setq response-handler #'eglot-semtok--ingest-full/delta-response)
      (setq request
            (plist-put request :previousResultId
                       (plist-get (plist-get eglot-semtok--cache :response) :resultId))))
     ((and eglot-semtok-allow-ranged-requests region
           (eglot-server-capable :textDocument/semanticTokensRangeProvider))
      (setq request-type :textDocument/semanticTokens/range)
      (setq final-region region)
      (setq request
            (plist-put request :range (eglot-region-range (car final-region) (cdr final-region))))
      (setq response-handler #'eglot-semtok--ingest-range-response))
     (t (setq response-handler #'eglot-semtok--ingest-full-response)))
    (eglot--async-request
     (eglot--current-server-or-lose) request-type request
     :success-fn
     (lambda (response)
       (eglot-semtok--putcache :_documentVersion eglot--versioned-identifier)
       (eglot-semtok--putcache :_region final-region)
       (funcall response-handler response)
       (when (or fontify-immediately (plist-get eglot-semtok--cache :_truncated))
         (font-lock-flush)))
     :error-fn ;; buffer is not captured in `error-handler', it is in `callback'
     (let ((buf (current-buffer)))
       (lambda (&rest _)
         (when (buffer-live-p buf)
           (eglot-semtok--request-full-token-set-when-idle buf t)))))))


;;;###autoload
(defvar-local semantic-token-modifier-cache (make-hash-table)
  "A cache of modifier values to the selected fonts.
This allows whole-bitmap lookup instead of checking each bit. The
expectation is that usage of modifiers will tend to cluster, so
we will not have the full range of possible usages, hence a
tractable hash map.

This is set as buffer-local. It should probably be shared in a
given workspace/language-server combination.

This cache should be flushed every time any modifier
configuration changes.")

(defun eglot-semtok--fontify (old-fontify-region beg-orig end-orig &optional loudly)
  "Apply fonts to retrieved semantic tokens.
OLD-FONTIFY-REGION is the underlying region fontification function,
e.g., `font-lock-fontify-region'.
BEG-ORIG and END-ORIG deliminate the requested fontification region and maybe
modified by OLD-FONTIFY-REGION.
LOUDLY will be forwarded to OLD-FONTIFY-REGION as-is."
  (let* ((server (eglot-current-server))
         (faces (oref server semtok-faces))
         (modifier-faces
          (when eglot-semtok-apply-modifiers
            (oref server semtok-modifier-faces)))
         old-bounds
         beg end)
    (cond
     ((or (eq nil faces)
          (eq nil eglot-semtok--cache)
          (eq nil (plist-get eglot-semtok--cache :response)))
      ;; default to non-semantic highlighting until first response has arrived
      (funcall old-fontify-region beg-orig end-orig loudly))
     ((not (= eglot--versioned-identifier
              (plist-get eglot-semtok--cache :_documentVersion)))
      ;; delay fontification until we have fresh tokens
      '(jit-lock-bounds 0 . 0))
     (t
      (setq old-bounds (funcall old-fontify-region beg-orig end-orig loudly))
      ;; this is to prevent flickering when semantic token highlighting
      ;; is layered on top of, e.g., tree-sitter-hl, or clojure-mode's syntax highlighting.
      (setq beg (min beg-orig (cadr old-bounds))
            end (max end-orig (cddr old-bounds)))
      ;; if we're using the response to a ranged request, we'll only be able to fontify within
      ;; that range (and hence shouldn't clear any highlights outside of that range)
      (let ((token-region (plist-get eglot-semtok--cache :_region)))
        (if token-region
            (progn
              (eglot-semtok--putcache :_truncated (or (< beg (car token-region))
                                                      (> end (cdr token-region))))
              (setq beg (max beg (car token-region)))
              (setq end (min end (cdr token-region))))
          (eglot-semtok--putcache :_truncated nil)))
      (let* ((inhibit-field-text-motion t)
             (data (plist-get (plist-get eglot-semtok--cache :response) :data))
             (i0 0)
             (i-max (1- (length data)))
             (current-line 1)
             (line-delta)
             (column 0)
             (face)
             (line-start-pos)
             (line-min)
             (line-max-inclusive)
             (text-property-beg)
             (text-property-end))
        (save-mark-and-excursion
          (save-restriction
            (widen)
            (goto-char beg)
            (goto-char (line-beginning-position))
            (setq line-min (line-number-at-pos))
            (with-silent-modifications
              (goto-char end)
              (goto-char (line-end-position))
              (setq line-max-inclusive (line-number-at-pos))
              (forward-line (- line-min line-max-inclusive))
              (let ((skip-lines (- line-min current-line)))
                (while (and (<= i0 i-max) (< (aref data i0) skip-lines))
                  (setq skip-lines (- skip-lines (aref data i0)))
                  (setq i0 (+ i0 5)))
                (setq current-line (- line-min skip-lines)))
              (forward-line (- current-line line-min))
              (setq line-start-pos (point))
              (cl-loop
               for i from i0 to i-max by 5 do
               (setq line-delta (aref data i))
               (unless (= line-delta 0)
                 (forward-line line-delta)
                 (setq line-start-pos (point))
                 (setq column 0)
                 (setq current-line (+ current-line line-delta)))
               (setq column (+ column (aref data (1+ i))))
               (setq face (aref faces (aref data (+ i 3))))
               (setq text-property-beg (+ line-start-pos column))
               (setq text-property-end
                     (min (if eglot-semtok-enable-multiline-token-support
                              (point-max) (line-end-position))
                      (+ text-property-beg (aref data (+ i 2)))))
               (when face
                 (put-text-property text-property-beg text-property-end 'face face))
               ;; Deal with modifiers. We cache common combinations of
               ;; modifiers, storing the faces they resolve to.
               (let* ((modifier-code (aref data (+ i 4)))
                      (faces-to-apply (gethash modifier-code semantic-token-modifier-cache 'not-found)))
                 (when (eq 'not-found faces-to-apply)
                   (setq faces-to-apply nil)
                   (cl-loop for j from 0 to (1- (length modifier-faces)) do
                            (when (and (aref modifier-faces j)
                                       (> (logand modifier-code (ash 1 j)) 0))
                              (push (aref modifier-faces j) faces-to-apply)))
                   (puthash modifier-code faces-to-apply semantic-token-modifier-cache))
                 (dolist (face faces-to-apply)
                   (add-face-text-property text-property-beg text-property-end face)))
               when (> current-line line-max-inclusive) return nil)))))
      `(jit-lock-bounds ,beg . ,end)))))

(defun eglot-semtok--request-update ()
  "Request semantic-tokens update."
  ;; when dispatching ranged requests, we'll over-request by several chunks in both directions,
  ;; which should minimize those occasions where font-lock region extension extends beyond the
  ;; region covered by our freshly requested tokens (see lsp-mode issue #3154), while still limiting
  ;; requests to fairly small regions even if the underlying buffer is large
  (when (eglot-server-capable :textDocument/semanticTokensFull)
    (eglot-semtok--request
     (cons (max (point-min) (- (window-start) (* 5 jit-lock-chunk-size)))
           (min (point-max) (+ (window-end) (* 5 jit-lock-chunk-size)))) t)))

(defun eglot-semtok--as-defined-by-server (server)
  "Return plist of token-types and token-modifiers defined by SERVER or nil."
  (cl-destructuring-bind (&key tokenModifiers tokenTypes &allow-other-keys)
      (plist-get (plist-get (eglot--capabilities server)
                            :semanticTokensProvider)
                 :legend)
    (when (and tokenTypes tokenModifiers)
      (list :token-types tokenTypes
            :token-modifiers tokenModifiers))))

;;;###autoload
(defun eglot-semtok--initialize-buffer ()
  "Initialize the buffer for semantic tokens.
IS-RANGE-PROVIDER is non-nil when server supports range requests."
  (let* ((old-extend-region-functions font-lock-extend-region-functions)
         ;; make sure font-lock always fontifies entire lines (TODO: do we also have
         ;; to change some jit-lock-...-region functions/variables?)
         (new-extend-region-functions
          (if (memq 'font-lock-extend-region-wholelines old-extend-region-functions)
              old-extend-region-functions
            (cons 'font-lock-extend-region-wholelines old-extend-region-functions)))
         (buffer (current-buffer)))
    (setq eglot-semtok--cache nil)
    (setq font-lock-extend-region-functions new-extend-region-functions)
    (add-function :around (local 'font-lock-fontify-region-function) #'eglot-semtok--fontify)
    (add-hook 'lsp-on-change-hook #'eglot-semtok--request-update nil t)
    (eglot-semtok--request-update)
    (setq eglot-semtok--teardown
          (lambda ()
            (setq eglot-semtok--pending-full-token-requests
                  (cl-remove buffer eglot-semtok--pending-full-token-requests :key #'car))
            (setq font-lock-extend-region-functions old-extend-region-functions)
            (setq eglot-semtok--cache nil)
            (remove-function (local 'font-lock-fontify-region-function)
                             #'eglot-semtok--fontify)
            (remove-hook 'lsp-on-change-hook #'eglot-semtok--request-update t)))))

(defun eglot-semtok--build-face-map (identifiers faces category varname)
  "Build map of FACES for IDENTIFIERS using CATEGORY and VARNAME."
  (vconcat
   (mapcar (lambda (id)
             (let ((maybe-face (cdr (assoc id faces))))
               (when (and eglot-semtok-warn-on-missing-face (not maybe-face))
                 (display-warning
                  'eglot-semtok
                  (format-message "No face has been associated to the %s `%s': consider adding a corresponding definition to %s"
                                  category id varname)))
               maybe-face))
           identifiers)))

(defun eglot-semtok--apply-alist-overrides (base overrides discard-defaults)
  "Merge or replace BASE with OVERRIDES, depending on DISCARD-DEFAULTS.
For keys present in both alists, the assignments made by
OVERRIDES will take precedence."
  (if discard-defaults
      overrides
    (let* ((copy-base (copy-alist base)))
      (mapc (pcase-lambda (`(,key . ,value))
              (setf (alist-get key copy-base nil nil #'string=) value))
            overrides)
      copy-base)))

(defun eglot-semtok--type-faces-for (server)
  "Return the semantic token type faces for SERVER."
  (eglot-semtok--apply-alist-overrides
   eglot-semtok-faces
   (plist-get (oref server semtok-face-overrides) :types)
   (plist-get (oref server semtok-face-overrides) :discard-default-types)))

(defun eglot-semtok--modifier-faces-for (server)
  "Return the semantic token type faces for SERVER."
  (eglot-semtok--apply-alist-overrides
   eglot-semtok-modifier-faces
   (plist-get (oref server semtok-face-overrides) :modifiers)
   (plist-get (oref server semtok-face-overrides) :discard-default-modifiers)))

(defun eglot-semtok--on-refresh (server)
  "Clear semantic tokens within all buffers of SERVER."
  (cl-assert (not (eq nil server)))
  (when eglot-semtok-honor-refresh-requests
    (cl-loop
     for ws-buffer in (eglot--managed-buffers server) do
     (let ((fontify-immediately (equal (current-buffer) ws-buffer)))
       (with-current-buffer ws-buffer (eglot-semtok--request nil fontify-immediately))))))

;;;###autoload
(defun eglot-semtok--initialize-workspace (server)
  "Initialize semantic tokens for SERVER."
  (cl-assert server)
  (cl-destructuring-bind (&key token-types token-modifiers)
      (eglot-semtok--as-defined-by-server server)
    (oset server semtok-faces
          (eglot-semtok--build-face-map
           token-types (eglot-semtok--type-faces-for server)
           "semantic token" "eglot-semtok-faces"))
    (oset server semtok-modifier-faces
          (eglot-semtok--build-face-map
           token-modifiers (eglot-semtok--modifier-faces-for server)
           "semantic token modifier" "eglot-semtok-modifier-faces"))))

;;;###autoload
(define-minor-mode eglot-semtok-mode
  "Toggle semantic-tokens support."
  :group 'eglot-semtok
  :global nil
  (cond
   ((and eglot-semtok-mode (eglot-server-capable :textDocument/semanticTokensFull))
    (add-hook 'lsp-configure-hook #'eglot-semtok--enable nil t)
    (add-hook 'lsp-unconfigure-hook #'eglot-semtok--disable nil t)
    (mapc #'eglot-semtok--initialize-workspace
          (lsp--find-workspaces-for "textDocument/semanticTokensFull"))
    (eglot-semtok--initialize-buffer))
   (t
    (remove-hook 'lsp-configure-hook #'eglot-semtok--enable t)
    (remove-hook 'lsp-unconfigure-hook #'eglot-semtok--disable t)
    (when eglot-semtok--teardown
      (funcall eglot-semtok--teardown))
    (eglot-semtok--request-update)
    (setq eglot-semtok--cache nil
          eglot-semtok--teardown nil))))

(provide 'eglot-semtok)
;;; eglot-semtok.el ends here
