;;; eglot-semtok.el --- Semantic tokens -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Lua Reis <me@lua.blog.br>
;; Copyright (C) 2020 emacs-lsp maintainers
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
;;  This implementation works by advicing around `font-lock-fontify-region-function' so that it
;;  includes fontification information from `eglot-semtok--cache' when it's available for the
;;  requested range. If the desired fontification range is not contained in the
;;  `eglot-semtok--cache' data, and if a request for a bigger range is not pending, it requests
;;  tokens in a slightly larger range for the LSP server. This data is also requested and updated in
;;  `eglot--document-changed-hook'. If the server supports `full/delta' requests, then these are
;;  preferred to ranged requests.
;;
;;  TODO: perhaps request the full document on idle, like lsp-mode does? does performance really
;;  improve when doing this?
;;
;;; Code:

(require 'eglot)

(defclass eglot-semtok-server (eglot-lsp-server)
  ((semtok-faces
    :initform nil
    :documentation "Semantic tokens faces.")
   (semtok-modifier-faces
    :initform nil
    :documentation "Semantic tokens modifier faces.")
   (semtok-modifier-cache
    :initform (make-hash-table)
    :documentation "A hashmap of modifier values to the selected faces."))
  "Represents an Eglot server with semantic tokens support.")

(defgroup eglot-semtok nil
  "Eglot support for semantic-tokens."
  :prefix "eglot-semtok-"
  :group 'eglot
  :tag "Eglot Semantic tokens")

(defcustom eglot-semtok-honor-refresh-requests nil
  "Whether to honor semanticTokens/refresh requests.

When set to nil, refresh requests will be silently discarded.
When set to t, semantic tokens will be re-requested for all buffers
associated with the requesting language server."
  :group 'eglot-semtok
  :type 'boolean)

(defcustom eglot-semtok-faces
  '(("comment" . font-lock-comment-face)
    ("keyword" . font-lock-keyword-face)
    ("string" . font-lock-string-face)
    ("number" . font-lock-constant-face)
    ("regexp" . font-lock-string-face)
    ("operator" . font-lock-function-name-face)
    ("namespace" . font-lock-keyword-face)
    ("type" . font-lock-type-face)
    ("struct" . font-lock-type-face)
    ("class" . font-lock-type-face)
    ("interface" . font-lock-type-face)
    ("enum" . font-lock-type-face)
    ("typeParameter" . font-lock-type-face)
    ("function" . font-lock-function-name-face)
    ("method" . font-lock-function-name-face)
    ("member" . font-lock-variable-name-face)
    ("field" . font-lock-variable-name-face)
    ("property" . font-lock-variable-name-face)
    ("event" . font-lock-variable-name-face)
    ("macro" . font-lock-preprocessor-face)
    ("variable" . font-lock-variable-name-face)
    ("parameter" . font-lock-variable-name-face)
    ("label" . font-lock-comment-face)
    ("enumConstant" . font-lock-constant-face)
    ("enumMember" . font-lock-constant-face)
    ("dependent" . font-lock-type-face)
    ("concept" . font-lock-type-face))
  "Alist of faces to use to highlight semantic tokens.
Each element is a cons cell whose car is a token type name and cdr is
the face to use."
  :group 'eglot-semtok
  :type `(alist :key-type (string :tag "Token name")
                :value-type (choice (face :tag "Face")
                                    (plist :tag "Face Attributes"
                                           :key-type
                                           (choice
                                            ,@(mapcar
                                               (lambda (cell)
                                                 `(const :tag ,(capitalize
                                                                (cdr cell))
                                                         ,(car cell)))
                                               face-attribute-name-alist))))))

(defcustom eglot-semtok-modifier-faces
  '(("declaration" . font-lock-type-face)
    ("definition" . font-lock-function-name-face)
    ("implementation" . font-lock-function-name-face)
    ("readonly" . font-lock-constant-face)
    ("static" . font-lock-keyword-face)
    ("abstract" . font-lock-keyword-face)
    ("async" . font-lock-preprocessor-face)
    ("modification" . font-lock-function-name-face)
    ("deprecated" . eglot-diagnostic-tag-deprecated-face)
    ("documentation" . font-lock-doc-face)
    ("defaultLibrary" . font-lock-builtin-face))
  "List of face to use to highlight tokens with modifiers.
Each element is a list of the following form: (MODIFIER FACE
[PRIORITY]).  MODIFIER is a token modifiers name.  FACE is the face to
use to highlight.  Option PRIORITY is a number whose value should
be between -100 to 100 (inclusive).  It's default value is 0.
Set to nil to disable special treatment of modifiers."
  :group 'eglot-semtok
  :type `(alist :key-type (string :tag "Token name")
                :value-type (list (choice (face :tag "Face")
                                          (plist :tag "Face Attributes"
                                                 :key-type
                                                 (choice
                                                  ,@(mapcar
                                                     (lambda (cell)
                                                       `(const :tag ,(capitalize
                                                                      (cdr cell))
                                                               ,(car cell)))
                                                     face-attribute-name-alist))))
                                  (radio (const :inline t :tag "Use default priority" nil)
                                         (number :tag "Priority")))))

(cl-defmethod eglot-client-capabilities ((_ eglot-semtok-server))
  (let* ((cap (cl-call-next-method))
         (ws (plist-get cap :workspace))
         (td (plist-get cap :textDocument))
         (ws (plist-put ws :semanticTokens '(:refreshSupport :json-false)))
         (td (plist-put td :semanticTokens
                        (list :dynamicRegistration :json-false
                              :requests '(:range t :full t)
                              :tokenModifiers (vconcat (mapcar #'car eglot-semtok-modifier-faces))
                              :overlappingTokenSupport t
                              :multilineTokenSupport t
                              :tokenTypes (vconcat (mapcar #'car eglot-semtok-faces))
                              :formats ["relative"]))))
    (plist-put (plist-put cap :workspace ws) :textDocument td)))

(defvar-local eglot-semtok--cache nil)

(defsubst eglot-semtok--put-cache (k v)
  "Set key K of `eglot-semtok--cache' to V."
  (setq eglot-semtok--cache
        (plist-put eglot-semtok--cache k v)))

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
  (let ((method :textDocument/semanticTokens/full)
        (params (list :textDocument (eglot--TextDocumentIdentifier)))
        (response-handler #'eglot-semtok--ingest-full-response)
        (final-region nil)
        (buf (current-buffer)))
    (cond
     ((and (eglot-server-capable :semanticTokensProvider :full :delta)
           (let ((response (plist-get eglot-semtok--cache :response)))
             (and (plist-get response :resultId) (plist-get response :data)
                  (not (plist-get eglot-semtok--cache :region)))))
      (setq method :textDocument/semanticTokens/full/delta)
      (setq response-handler #'eglot-semtok--ingest-full/delta-response)
      (setq params
            (plist-put params :previousResultId
                       (plist-get (plist-get eglot-semtok--cache :response) :resultId))))
     ((and region (eglot-server-capable :semanticTokensProvider :range))
      (setq method :textDocument/semanticTokens/range)
      (setq final-region region)
      (setq params
            (plist-put params :range (eglot-region-range (car final-region) (cdr final-region))))
      (setq response-handler #'eglot-semtok--ingest-range-response)))
    (eglot--async-request
     (eglot--current-server-or-lose) method params
     :success-fn
     (lambda (response)
       (eglot--when-live-buffer buf
         (eglot-semtok--put-cache :documentVersion eglot--versioned-identifier)
         (eglot-semtok--put-cache :region final-region)
         (funcall response-handler response)
         (when (or fontify-immediately (plist-get eglot-semtok--cache :truncated))
           (font-lock-flush (car-safe region) (cdr-safe region)))))
     :hint #'eglot-semtok--request)))

(defun eglot-semtok--fontify (orig-fontify beg-orig end-orig &optional loudly)
  "Apply fonts to retrieved semantic tokens.
OLD-FONTIFY-REGION is the underlying region fontification function,
e.g., `font-lock-fontify-region'.
BEG-ORIG and END-ORIG deliminate the requested fontification region and maybe
modified by OLD-FONTIFY-REGION.
LOUDLY will be forwarded to OLD-FONTIFY-REGION as-is."
  (with-slots ((modifier-cache semtok-modifier-cache)
               (faces semtok-faces)
               (modifier-faces semtok-modifier-faces))
      (eglot-current-server)
    (let (old-bounds beg end)
      (cond
       ((or (eq nil faces)
            (eq nil eglot-semtok--cache)
            (eq nil (plist-get eglot-semtok--cache :response)))
        ;; default to non-semantic highlighting until first response has arrived
        (funcall orig-fontify beg-orig end-orig loudly))
       ((not (= eglot--versioned-identifier
                (plist-get eglot-semtok--cache :documentVersion)))
        ;; delay fontification until we have fresh tokens
        '(jit-lock-bounds 0 . 0))
       (t
        (setq old-bounds (funcall orig-fontify beg-orig end-orig loudly))
        ;; this is to prevent flickering when semantic token highlighting
        ;; is layered on top of, e.g., tree-sitter-hl, or clojure-mode's syntax highlighting.
        (setq beg (min beg-orig (cadr old-bounds))
              end (max end-orig (cddr old-bounds)))
        ;; if we're using the response to a ranged request, we'll only be able to fontify within
        ;; that range (and hence shouldn't clear any highlights outside of that range)
        (if-let* ((token-region (plist-get eglot-semtok--cache :region)))
            (progn
              (eglot-semtok--put-cache :truncated (or (< beg (car token-region))
                                                      (> end (cdr token-region))))
              (setq beg (max beg (car token-region)))
              (setq end (min end (cdr token-region))))
          (eglot-semtok--put-cache :truncated nil))
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
                       (min (point-max) (+ text-property-beg (aref data (+ i 2)))))
                 (when face
                   (put-text-property text-property-beg text-property-end 'face face))
                 ;; Deal with modifiers. We cache common combinations of
                 ;; modifiers, storing the faces they resolve to.
                 (let* ((modifier-code (aref data (+ i 4)))
                        (faces-to-apply (gethash modifier-code modifier-cache 'not-found)))
                   (when (eq 'not-found faces-to-apply)
                     (setq faces-to-apply nil)
                     (cl-loop for j from 0 to (1- (length modifier-faces)) do
                              (when (and (aref modifier-faces j)
                                         (> (logand modifier-code (ash 1 j)) 0))
                                (push (aref modifier-faces j) faces-to-apply)))
                     (puthash modifier-code faces-to-apply modifier-cache))
                   (dolist (face faces-to-apply)
                     (add-face-text-property text-property-beg text-property-end face)))
                 when (> current-line line-max-inclusive) return nil)))))
        `(jit-lock-bounds ,beg . ,end))))))

(defun eglot-semtok--request-update (&optional beg end)
  "Request semantic-tokens update."
  (when (eglot-server-capable :semanticTokensProvider)
    (eglot-semtok--request
     (cons (max (point-min) (- (or beg (window-start)) (* 5 jit-lock-chunk-size)))
           (min (point-max) (+ (or end (window-end)) (* 5 jit-lock-chunk-size))))
     t)))

(defun eglot-semtok--on-refresh (server)
  "Clear semantic tokens within all buffers of SERVER."
  (cl-assert (not (eq nil server)))
  (when eglot-semtok-honor-refresh-requests
    (cl-loop
     for ws-buffer in (eglot--managed-buffers server) do
     (let ((fontify-immediately (equal (current-buffer) ws-buffer)))
       (with-current-buffer ws-buffer (eglot-semtok--request nil fontify-immediately))))))


;;; Process response

(defun eglot-semtok--ingest-range-response (response)
  "Handle RESPONSE to semanticTokens/range request."
  (eglot-semtok--put-cache :response response)
  (cl-assert (plist-get eglot-semtok--cache :region)))

(defun eglot-semtok--ingest-full-response (response)
  "Handle RESPONSE to semanticTokens/full request."
  (eglot-semtok--put-cache :response response)
  (cl-assert (not (plist-get eglot-semtok--cache :region))))

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
  (if-let* ((edits (plist-get response :edits)))
      (let ((old-data (plist-get (plist-get eglot-semtok--cache :response) :data)))
        (cl-assert (not (plist-get eglot-semtok--cache :region)))
        (when old-data
          (eglot-semtok--put-cache
           :response
           (plist-put response :data
                      (eglot-semtok--apply-delta-edits old-data edits)))))
    ;; server decided to send full response instead
    (eglot-semtok--ingest-full-response response)))

;;; Initialization

(defun eglot-semtok--build-face-map (identifiers faces category varname)
  "Build map of FACES for IDENTIFIERS using CATEGORY and VARNAME."
  (vconcat
   (mapcar (lambda (id)
             (let ((maybe-face (cdr (assoc id faces))))
               (when (not maybe-face)
                 (display-warning
                  'eglot-semtok
                  (format-message "No face has been associated to the %s `%s': consider adding a corresponding definition to %s"
                                  category id varname)))
               maybe-face))
           identifiers)))

(defun eglot-semtok--initialize-server (server)
  "Initialize semantic tokens for SERVER."
  (when (cl-typep server 'eglot-semtok-server)
    (cl-destructuring-bind (&key tokenTypes tokenModifiers &allow-other-keys)
        (plist-get (plist-get (eglot--capabilities server)
                              :semanticTokensProvider)
                   :legend)
      (oset server semtok-faces
            (eglot-semtok--build-face-map
             tokenTypes eglot-semtok-faces
             "semantic token" "eglot-semtok-faces"))
      (oset server semtok-modifier-faces
            (eglot-semtok--build-face-map
             tokenModifiers eglot-semtok-modifier-faces
             "semantic token modifier" "eglot-semtok-modifier-faces")))))

(add-hook 'eglot-connect-hook 'eglot-semtok--initialize-server)

;;; Minor mode

(defun eglot-semtok--destroy ()
  (unless (eglot-managed-p)
    (eglot-semtok-mode -1)))

(define-minor-mode eglot-semtok-mode
  "Enable semantic tokens support for Eglot."
  :global nil
  (if eglot-semtok-mode
      (if (and (cl-typep (eglot-current-server) 'eglot-semtok-server)
               (eglot-server-capable :semanticTokensProvider))
          (progn
            (add-hook 'eglot-managed-mode-hook #'eglot-semtok--destroy nil t)
            (add-hook 'eglot--document-changed-hook #'eglot-semtok--request-update nil t)
            (add-function :around (local 'font-lock-fontify-region-function)
                  #'eglot-semtok--fontify)
            (eglot-semtok--request-update))
        (eglot-semtok-mode -1))
    (remove-hook 'eglot-managed-mode-hook #'eglot-semtok--destroy t)
    (remove-hook 'eglot--document-changed-hook #'eglot-semtok--request-update t)
    (remove-function (local 'font-lock-fontify-region-function) #'eglot-semtok--fontify)))

(add-hook 'eglot-managed-mode-hook #'eglot-semtok-mode)

(provide 'eglot-semtok)
;;; eglot-semtok.el ends here
