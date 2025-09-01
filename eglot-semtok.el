;;; eglot-semtok.el --- Semantic tokens support for Eglot -*- lexical-binding: t; -*-
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
;;; Code:

(require 'eglot)

;;;###autoload
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

(defcustom eglot-semtok-faces
  '(("namespace" . font-lock-keyword-face)
    ("type" . font-lock-type-face)
    ("class" . font-lock-type-face)
    ("enum" . font-lock-type-face)
    ("interface" . font-lock-type-face)
    ("struct" . font-lock-type-face)
    ("typeParameter" . font-lock-type-face)
    ("parameter" . font-lock-variable-name-face)
    ("variable" . font-lock-variable-name-face)
    ("property" . font-lock-property-use-face)
    ("enumMember" . font-lock-constant-face)
    ("event" . font-lock-variable-name-face)
    ("function" . font-lock-function-name-face)
    ("method" . font-lock-function-name-face)
    ("macro" . font-lock-preprocessor-face)
    ("keyword" . font-lock-keyword-face)
    ("modifier" . font-lock-function-name-face)
    ("comment" . font-lock-comment-face)
    ("string" . font-lock-string-face)
    ("number" . font-lock-constant-face)
    ("regexp" . font-lock-string-face)
    ("operator" . font-lock-function-name-face)
    ("decorator" . font-lock-type-face))
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
  '(("declaration" . font-lock-function-name-face)
    ("definition" . font-lock-function-name-face)
    ("readonly" . font-lock-constant-face)
    ("static" . font-lock-keyword-face)
    ("deprecated" . eglot-diagnostic-tag-deprecated-face)
    ("abstract" . font-lock-keyword-face)
    ("async" . font-lock-preprocessor-face)
    ("modification" . font-lock-function-name-face)
    ("documentation" . font-lock-doc-face)
    ("defaultLibrary" . font-lock-builtin-face))
  "List of face to use to highlight tokens with modifiers.
Each element is a cons cell whose car is a modifier name and cdr is
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

(cl-defmethod eglot-client-capabilities ((_ eglot-semtok-server))
  "Return the client capabilities of an `eglot-semtok-server' instance."
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

(defvar-local eglot-semtok--idle-timer nil
  "Idle timer to request full semantic tokens.")

(defvar-local eglot-semtok--last-request-hash nil
  "Hash of last request parameters and document version.")

(defvar-local eglot-semtok--cache nil)

(defsubst eglot-semtok--put-cache (k v)
  "Set key K of `eglot-semtok--cache' to V."
  (setq eglot-semtok--cache
        (plist-put eglot-semtok--cache k v)))

(defun eglot-semtok--region-range (beg end)
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
  (let* ((method :textDocument/semanticTokens/full)
         (params (list :textDocument (eglot--TextDocumentIdentifier)))
         (response-handler #'eglot-semtok--ingest-full-response)
         (final-region nil)
         (buf (current-buffer))
         (doc-version eglot--versioned-identifier)
         (hash (sxhash (cons doc-version params))))
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
            (plist-put params :range (eglot-semtok--region-range
                                      (car final-region) (cdr final-region))))
      (setq response-handler #'eglot-semtok--ingest-range-response)))
    (unless (eq hash eglot-semtok--last-request-hash)
      (setq eglot-semtok--last-request-hash hash)
      (eglot--async-request
       (eglot--current-server-or-lose) method params
       :success-fn
       (lambda (response)
         (eglot--when-live-buffer buf
           (eglot-semtok--put-cache :documentVersion doc-version)
           (eglot-semtok--put-cache :region final-region)
           (funcall response-handler response)
           (when (or fontify-immediately (plist-get eglot-semtok--cache :truncated))
             (jit-lock-refontify (car-safe region) (cdr-safe region)))
           (when final-region (eglot-semtok--request-full-on-idle))))
       :timeout 30
       :hint #'eglot-semtok--request))))

(defun eglot-semtok--fontify (beg end)
  "Apply the cached semantic tokens from BEG to END."
  (with-slots ((modifier-cache semtok-modifier-cache)
               (faces semtok-faces)
               (modifier-faces semtok-modifier-faces))
      (eglot-current-server)
    (cond
     ((not faces)) ;; not initialized yet; just skip
     ((not (and eglot-semtok--cache
                (plist-get eglot-semtok--cache :response)
                (eq eglot--versioned-identifier (plist-get eglot-semtok--cache :documentVersion))))
      (eglot-semtok--request-update beg end))
     (t
      ;; if we're using the response to a ranged request, we'll only be able to fontify within
      ;; that range (and hence shouldn't clear any highlights outside of that range)
      (if-let* ((token-region (plist-get eglot-semtok--cache :region)))
          (progn
            (let ((truncated (or (< beg (car token-region))
                                 (> end (cdr token-region)))))
              (eglot-semtok--put-cache :truncated truncated)
              (when truncated
                (eglot-semtok--request-update beg end)))
            (setq beg (max beg (car token-region)))
            (setq end (min end (cdr token-region))))
        (eglot-semtok--put-cache :truncated nil))
      (remove-list-of-text-properties beg end '(font-lock-face))
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
                 (put-text-property text-property-beg text-property-end 'font-lock-face face))
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
                   (put-text-property text-property-beg text-property-end 'font-lock-face face)))
               when (> current-line line-max-inclusive) return nil)))))
      `(jit-lock-bounds ,beg . ,end)))))

(defun eglot-semtok--request-update (&optional beg end)
  "Request semantic tokens update from BEG to END."
  (when (eglot-server-capable :semanticTokensProvider)
    (when-let* ((range (or (and beg end (cons beg end))
                           (when-let* ((win (get-buffer-window nil 'visible)))
                             (cons (window-start win) (window-end win)))))
                (margin (* 3 jit-lock-chunk-size)))
      (eglot-semtok--request (cons (max (point-min) (- (car range) margin))
                                   (min (point-max) (+ (cdr range) margin)))
                             t))))

(defun eglot-semtok--request-full-on-idle ()
  "Make a full semantic tokens request after an idle timer."
  (when (eglot-server-capable :semanticTokensProvider)
    (let* ((buf (current-buffer))
           (fun (lambda ()
                  (eglot--when-live-buffer buf
                    (eglot-semtok--request nil t)))))
      (when eglot-semtok--idle-timer (cancel-timer eglot-semtok--idle-timer))
      (setq eglot-semtok--idle-timer (run-with-idle-timer (* 3 eglot-send-changes-idle-time) nil fun)))))

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
    (apply #'vconcat (nreverse substrings))))

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
  "Initialize SERVER for semantic tokens."
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
  "Disable `eglot-semtok-mode' when Eglot is disabled."
  (unless (eglot-managed-p)
    (eglot-semtok-mode -1)))

;;;###autoload
(define-minor-mode eglot-semtok-mode
  "Enable semantic tokens support for Eglot."
  :global nil
  (if eglot-semtok-mode
      (if (and (eglot-managed-p)
               (cl-typep (eglot-current-server) 'eglot-semtok-server)
               (eglot-server-capable :semanticTokensProvider))
          (progn
            (jit-lock-register #'eglot-semtok--fontify 'contextual)
            (add-hook 'eglot-managed-mode-hook #'eglot-semtok--destroy nil t)
            (add-hook 'eglot--document-changed-hook #'eglot-semtok--request-update nil t)
            (jit-lock-refontify))
        (eglot-semtok-mode -1))
    (jit-lock-unregister #'eglot-semtok--fontify)
    (save-restriction
      (widen)
      (remove-list-of-text-properties (point-min) (point-max) '(font-lock-face)))
    (remove-hook 'eglot-managed-mode-hook #'eglot-semtok--destroy t)
    (remove-hook 'eglot--document-changed-hook #'eglot-semtok--request-update t)
    (jit-lock-refontify)))

;;;###autoload
(add-hook 'eglot-managed-mode-hook #'eglot-semtok-mode)

(provide 'eglot-semtok)
;;; eglot-semtok.el ends here
