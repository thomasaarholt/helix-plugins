(require "helix/components.scm")
(require "helix/editor.scm")
(require "helix/misc.scm")
(require "helix/static.scm")
(require-builtin helix/core/text as text.)

(require "core/model.scm")
(require "ui/window.scm")

(provide merge-conflicts
         merge-conflicts-refresh
         merge-conflicts-next
         merge-conflicts-prev
         merge-conflicts-keep-ours
         merge-conflicts-keep-theirs
         merge-conflicts-keep-base
         merge-conflicts-keep-both
         merge-conflicts-strip-markers)

(define *merge-conflicts-window-name* "merge-conflicts-window")
(define *merge-conflicts-session* #f)
(define *merge-conflicts-hooks-installed* #f)
(define *merge-conflicts-undo-doc-id* #f)
(define *merge-conflicts-undo-stack* '())
(define *merge-conflicts-redo-stack* '())

(struct MergeConflictUndoEntry (start-char resolved-text original-text action) #:transparent)

(define (default-preview-action)
  'ours)

(define (pluralize count singular plural)
  (if (= count 1) singular plural))

(define (key-matches-char? event char)
  (and (key-event-char event) (equal? (key-event-char event) char)))

(define (current-doc-id)
  (editor->doc-id (editor-focus)))

(define (current-doc-path)
  (editor-document->path (current-doc-id)))

(define (current-doc-text)
  (text.rope->string (editor->text (current-doc-id))))

(define (current-doc-label)
  (let ([path (current-doc-path)])
    (if path path "<unnamed buffer>")))

(define (scan-current-document)
  (scan-conflicts (current-doc-text)))

(define (same-session-document? session doc-id)
  (and session (equal? (MergeConflictSession-doc-id session) doc-id)))

(define (hide-merge-conflicts-window!)
  (pop-last-component-by-name! *merge-conflicts-window-name*))

(define (announce-empty-document! document)
  (cond
    [(and (ConflictDocument-contains-markers document) (ConflictDocument-malformed document))
     (set-warning! "Merge-conflict markers are present, but the current buffer no longer parses cleanly.")]
    [else (set-status! "No merge conflicts found in the current buffer.")]))

(define (reset-session-preview-state! session)
  (session-set-selected-action! session (default-preview-action))
  (session-set-preview-active! session #f)
  (session-set-preview-length! session 0))

(define (reset-merge-conflicts-undo-state! doc-id)
  (set! *merge-conflicts-undo-doc-id* doc-id)
  (set! *merge-conflicts-undo-stack* '())
  (set! *merge-conflicts-redo-stack* '()))

(define (ensure-merge-conflicts-undo-state! doc-id)
  (unless (equal? *merge-conflicts-undo-doc-id* doc-id)
    (reset-merge-conflicts-undo-state! doc-id)))

(define (shift-merge-conflicts-entry-stack stack start-char delta)
  (if (= delta 0)
      stack
      (map (lambda (entry)
             (if (> (MergeConflictUndoEntry-start-char entry) start-char)
                 (MergeConflictUndoEntry (+ (MergeConflictUndoEntry-start-char entry) delta)
                                         (MergeConflictUndoEntry-resolved-text entry)
                                         (MergeConflictUndoEntry-original-text entry)
                                         (MergeConflictUndoEntry-action entry))
                 entry))
           stack)))

(define (shift-merge-conflicts-history-stacks! start-char delta)
  (set! *merge-conflicts-undo-stack*
        (shift-merge-conflicts-entry-stack *merge-conflicts-undo-stack* start-char delta))
  (set! *merge-conflicts-redo-stack*
        (shift-merge-conflicts-entry-stack *merge-conflicts-redo-stack* start-char delta)))

(define (record-merge-conflict-undo! start-char resolved-text original-text action)
  (set! *merge-conflicts-undo-stack*
        (cons (MergeConflictUndoEntry start-char resolved-text original-text action)
              *merge-conflicts-undo-stack*))
  (set! *merge-conflicts-redo-stack* '()))

(define (build-session doc-id doc-path conflicts)
  (MergeConflictSession doc-id
                        doc-path
                        (box conflicts)
                        (box 0)
                        (box (default-preview-action))
                        (box #f)
                        (box 0)))

(define (prepare-session-from-current-buffer announce-empty?)
  (let* ([doc-id (current-doc-id)]
         [doc-path (current-doc-path)]
         [document (scan-current-document)]
         [conflicts (ConflictDocument-conflicts document)])
    (ensure-merge-conflicts-undo-state! doc-id)
    (cond
      [(> (length conflicts) 0)
       (if (same-session-document? *merge-conflicts-session* doc-id)
           (begin
              (set-box! (MergeConflictSession-conflicts-box *merge-conflicts-session*) conflicts)
              (session-clamp-current-index! *merge-conflicts-session*)
              (reset-session-preview-state! *merge-conflicts-session*)
              *merge-conflicts-session*)
           (begin
              (set! *merge-conflicts-session* (build-session doc-id doc-path conflicts))
              *merge-conflicts-session*))]
      [else
       (when (same-session-document? *merge-conflicts-session* doc-id)
         (set! *merge-conflicts-session* #f))
        (when announce-empty? (announce-empty-document! document))
        #f])))

(define (active-session announce-empty?)
  (if (and *merge-conflicts-session*
           (same-session-document? *merge-conflicts-session* (current-doc-id))
           (session-preview-active? *merge-conflicts-session*))
      *merge-conflicts-session*
      (prepare-session-from-current-buffer announce-empty?)))

(define (announce-current-conflict! session)
  (let ([conflict (session-current-conflict session)])
    (when conflict
      (set-status!
       (to-string "Merge conflict"
                  (ConflictBlock-index conflict)
                  "of"
                  (session-conflict-count session)
                  "at lines"
                  (string-append (int->string (ConflictBlock-start-line conflict))
                                 "-"
                                 (int->string (ConflictBlock-end-line conflict))))))))

(define (focus-current-conflict! session)
  (let ([conflict (session-current-conflict session)])
    (when conflict
      (set-current-selection-object!
       (range->selection
         (range (ConflictBlock-start-char conflict) (ConflictBlock-end-char conflict))))
      (align_view_center))))

(define (select-span! start length)
  (set-current-selection-object!
   (range->selection (range start (+ start length)))))

(define (replace-span-with! start length text)
  (select-span! start length)
  (if (> length 0)
      (replace-selection-with text)
      (when (> (string-length text) 0)
        (insert_string text))))

(define (show-merge-conflicts-window! session)
  (hide-merge-conflicts-window!)
  (push-component!
   (new-component!
    *merge-conflicts-window-name*
    session
    merge-conflicts-render
    (hash "handle_event" merge-conflicts-window-event))))

(define (refresh-session-for-open-document! doc-id)
  (when (and (same-session-document? *merge-conflicts-session* doc-id)
             (equal? doc-id (current-doc-id))
             (not (session-preview-active? *merge-conflicts-session*)))
    (let* ([document (scan-current-document)]
           [conflicts (ConflictDocument-conflicts document)])
      (if (> (length conflicts) 0)
          (begin
            (set-box! (MergeConflictSession-conflicts-box *merge-conflicts-session*) conflicts)
            (session-clamp-current-index! *merge-conflicts-session*)
            (reset-session-preview-state! *merge-conflicts-session*))
          (begin
            (set! *merge-conflicts-session* #f)
            (hide-merge-conflicts-window!)
            (if (and (ConflictDocument-contains-markers document)
                     (ConflictDocument-malformed document))
                (set-warning! "Merge-conflict markers are present, but the current buffer no longer parses cleanly.")
                (set-status! "All merge conflicts resolved.")))))))

(define (announce-open-document-conflicts! doc-id)
  (when (equal? doc-id (current-doc-id))
    (let* ([document (scan-current-document)]
           [conflicts (ConflictDocument-conflicts document)]
           [count (length conflicts)])
      (cond
        [(> count 0)
         (set-status! (to-string "Found"
                                 count
                                 (pluralize count "merge conflict." "merge conflicts.")
                                 "Run :merge-conflicts."))]
        [(and (ConflictDocument-contains-markers document) (ConflictDocument-malformed document))
         (set-warning! "Found merge-conflict markers, but the current buffer does not parse cleanly.")]
        [else void]))))

;; Hooks cannot be unregistered, so only install them once per runtime.
(define (install-merge-conflict-hooks!)
  (unless *merge-conflicts-hooks-installed*
    (set! *merge-conflicts-hooks-installed* #t)
    (register-hook 'document-opened announce-open-document-conflicts!)
    (register-hook
     'document-changed
     (lambda (doc-id _old-text) (refresh-session-for-open-document! doc-id)))))

(define (preview-action-options conflict)
  (if (ConflictBlock-has-base conflict)
      '(ours theirs base both)
      '(ours theirs both)))

(define (list-index-of lst value)
  (let loop ([remaining lst] [index 0])
    (cond
      [(null? remaining) #f]
      [(equal? (car remaining) value) index]
      [else (loop (cdr remaining) (+ index 1))])))

(define (restore-current-preview! session)
  (let ([conflict (session-current-conflict session)])
    (when (and conflict (session-preview-active? session))
      (replace-span-with! (ConflictBlock-start-char conflict)
                          (session-preview-length session)
                          (ConflictBlock-full-text conflict))
      (session-set-preview-active! session #f)
      (session-set-preview-length! session 0)
      (focus-current-conflict! session))))

(define (preview-status-message action)
  (to-string "Previewing"
             (action-name action)
             "Press Tab to cycle, Enter to apply, Esc to restore."))

(define (preview-current-resolution! action)
  (let ([session (active-session #t)])
    (if (not session)
        'no-session
        (let ([conflict (session-current-conflict session)])
          (cond
            [(not conflict) 'no-session]
            [(and (equal? action 'base) (not (ConflictBlock-has-base conflict)))
             (set-warning! "This conflict does not include a base section.")
             'unsupported]
            [else
             (when (session-preview-active? session)
               (restore-current-preview! session))
             (let* ([preview-text (resolution-text conflict action)]
                    [preview-length (string-length preview-text)])
               (replace-span-with! (ConflictBlock-start-char conflict)
                                   (string-length (ConflictBlock-full-text conflict))
                                   preview-text)
               (session-set-selected-action! session action)
               (session-set-preview-active! session #t)
               (session-set-preview-length! session preview-length)
               (select-span! (ConflictBlock-start-char conflict) preview-length)
               (align_view_center)
               (set-status! (preview-status-message action))
                'previewed)])))))

(define (cycle-preview-action! delta)
  (let ([session (active-session #t)])
    (if (not session)
        'no-session
        (let* ([conflict (session-current-conflict session)]
               [options (and conflict (preview-action-options conflict))])
          (if (not conflict)
              'no-session
              (let* ([current-action (session-selected-action session)]
                     [current-index (or (list-index-of options current-action) 0)])
                (if (not (session-preview-active? session))
                    (preview-current-resolution! current-action)
                    (let ([next-action
                           (list-ref options (modulo (+ current-index delta) (length options)))])
                      (preview-current-resolution! next-action)))))))))

(define (navigate-current-session! delta)
  (let ([session (active-session #t)])
    (when session
      (when (session-preview-active? session)
        (restore-current-preview! session))
      (session-cycle-current-index! session delta)
      (reset-session-preview-state! session)
      (focus-current-conflict! session)
      (announce-current-conflict! session))
    session))

(define (find-conflict-index-containing-char conflicts char-index)
  (let loop ([remaining conflicts] [index 0])
    (cond
      [(null? remaining) #f]
      [else
       (let ([conflict (car remaining)])
         (if (and (<= (ConflictBlock-start-char conflict) char-index)
                  (<= char-index (ConflictBlock-end-char conflict)))
             index
             (loop (cdr remaining) (+ index 1))))])))

(define (focus-conflict-containing-char! session char-index)
  (let ([index (find-conflict-index-containing-char (session-conflicts session) char-index)])
    (when index
      (session-set-current-index! session index)))
  (focus-current-conflict! session)
  (announce-current-conflict! session))

(define (action-label action)
  (cond
    [(equal? action 'ours) "Kept ours."]
    [(equal? action 'theirs) "Kept theirs."]
    [(equal? action 'base) "Kept base."]
    [(equal? action 'both) "Kept both."]
    [(equal? action 'strip) "Stripped conflict markers."]
    [else "Updated conflict."]))

(define (action-name action)
  (cond
    [(equal? action 'ours) "ours"]
    [(equal? action 'theirs) "theirs"]
    [(equal? action 'base) "base"]
    [(equal? action 'both) "both"]
    [(equal? action 'strip) "strip"]
    [else "ours"]))

(define (any-of? pred lst)
  (cond
    [(null? lst) #f]
    [(pred (car lst)) #t]
    [else (any-of? pred (cdr lst))]))

(define (range-touches-conflict? r-from r-to start-char end-char)
  (if (= r-from r-to)
      ;; Treat a zero-width selection (cursor) as inside the conflict if it
      ;; lies anywhere within [start, end].
      (and (>= r-from start-char) (<= r-from end-char))
      ;; Non-empty ranges count as overlap when they share any extent at all.
      (and (< r-from end-char) (> r-to start-char))))

(define (current-selection-ranges)
  (let ([selection (current-selection-object)])
    (if selection
        (map (lambda (r) (cons (range->from r) (range->to r)))
             (selection->ranges selection))
        '())))

(define (conflicts-overlapping-ranges conflicts ranges)
  (if (null? ranges)
      '()
      (filter
       (lambda (c)
         (let ([cs (ConflictBlock-start-char c)]
               [ce (ConflictBlock-end-char c)])
           (any-of? (lambda (r) (range-touches-conflict? (car r) (cdr r) cs ce))
                    ranges)))
       conflicts)))

(define (action-applicable? action conflict)
  (if (equal? action 'base)
      (ConflictBlock-has-base conflict)
      #t))

(define (target-conflicts-for-action session)
  ;; Conflicts whose extent intersects the user's current selection take
  ;; priority. This lets `%` (or any range selection) drive a batch apply.
  ;; If nothing in the selection overlaps a conflict, fall back to the
  ;; session's current conflict (preserving the original single-conflict
  ;; behavior used by the modal window).
  (let* ([conflicts (session-conflicts session)]
         [matched (conflicts-overlapping-ranges conflicts (current-selection-ranges))])
    (cond
      [(not (null? matched)) matched]
      [else
       (let ([current (session-current-conflict session)])
         (if current (list current) '()))])))

(define (apply-resolution-to-conflict! action conflict)
  (let* ([start-char (ConflictBlock-start-char conflict)]
         [original-text (ConflictBlock-full-text conflict)]
         [original-length (string-length original-text)]
         [resolved-text (resolution-text conflict action)]
         [resolved-length (string-length resolved-text)])
    (replace-span-with! start-char original-length resolved-text)
    (shift-merge-conflicts-history-stacks! start-char (- resolved-length original-length))
    (record-merge-conflict-undo! start-char resolved-text original-text action)))

(define (batch-apply-status-message action applied-count skipped-count remaining-count)
  (let ([skipped-msg
         (if (> skipped-count 0)
             (string-append " ("
                            (int->string skipped-count)
                            " "
                            (pluralize skipped-count "conflict" "conflicts")
                            " without base skipped.)")
             "")])
    (cond
      [(= remaining-count 0)
       (string-append (action-label action)
                      " Resolved "
                      (int->string applied-count)
                      " "
                      (pluralize applied-count "conflict." "conflicts.")
                      " All merge conflicts resolved."
                      skipped-msg)]
      [else
       (string-append (action-label action)
                      " Resolved "
                      (int->string applied-count)
                      " "
                      (pluralize applied-count "conflict." "conflicts.")
                      " "
                      (int->string remaining-count)
                      " "
                      (pluralize remaining-count "conflict remains." "conflicts remain.")
                      skipped-msg)])))

(define (apply-current-resolution! action)
  (let ([session (active-session #t)])
    (if (not session)
        'no-session
        (begin
          (when (session-preview-active? session)
            (restore-current-preview! session))
          (let ([targets (target-conflicts-for-action session)])
            (cond
              [(null? targets) 'no-session]
              [else
               (let* ([applicable (filter (lambda (c) (action-applicable? action c)) targets)]
                      [skipped-count (- (length targets) (length applicable))])
                 (cond
                   [(null? applicable)
                    (if (equal? action 'base)
                        (set-warning! "No conflicts in the current selection include a base section.")
                        (set-warning! "No conflicts in the current selection support this action."))
                    'unsupported]
                   [else
                    ;; Apply in reverse document order so that earlier conflicts
                    ;; keep their original char positions while we edit later
                    ;; ones first. `conflicts-overlapping-ranges` preserves the
                    ;; ascending order of `session-conflicts`.
                    (for-each (lambda (c) (apply-resolution-to-conflict! action c))
                              (reverse applicable))
                    (let* ([applied-count (length applicable)]
                           [updated-session (prepare-session-from-current-buffer #f)]
                           [remaining-count (if updated-session
                                                (session-conflict-count updated-session)
                                                0)])
                      (cond
                        [updated-session
                         (focus-current-conflict! updated-session)
                         (set-status! (batch-apply-status-message action
                                                                  applied-count
                                                                  skipped-count
                                                                  remaining-count))
                         'updated]
                        [else
                         (hide-merge-conflicts-window!)
                         (set-status! (batch-apply-status-message action
                                                                  applied-count
                                                                  skipped-count
                                                                   0))
                         'resolved-all]))]))]))))))

(define (undo-merge-conflict-change!)
  (when (and *merge-conflicts-session* (session-preview-active? *merge-conflicts-session*))
    (restore-current-preview! *merge-conflicts-session*))
  (if (null? *merge-conflicts-undo-stack*)
      (begin
        (set-status! "No accepted merge-conflict resolution to undo.")
        'noop)
      (let* ([entry (car *merge-conflicts-undo-stack*)]
             [start-char (MergeConflictUndoEntry-start-char entry)]
             [resolved-text (MergeConflictUndoEntry-resolved-text entry)]
             [resolved-length (string-length resolved-text)]
             [original-text (MergeConflictUndoEntry-original-text entry)])
        (set! *merge-conflicts-undo-stack* (cdr *merge-conflicts-undo-stack*))
        (set! *merge-conflicts-redo-stack* (cons entry *merge-conflicts-redo-stack*))
        (replace-span-with! start-char resolved-length original-text)
        (shift-merge-conflicts-history-stacks! start-char (- (string-length original-text) resolved-length))
        (let ([updated-session (prepare-session-from-current-buffer #t)])
          (if updated-session
              (begin
                (reset-session-preview-state! updated-session)
                (focus-conflict-containing-char! updated-session start-char)
                'updated)
              (begin
                (hide-merge-conflicts-window!)
                'closed))))))

(define (redo-merge-conflict-change!)
  (when (and *merge-conflicts-session* (session-preview-active? *merge-conflicts-session*))
    (restore-current-preview! *merge-conflicts-session*))
  (if (null? *merge-conflicts-redo-stack*)
      (begin
        (set-status! "No merge-conflict resolution to redo.")
        'noop)
      (let* ([entry (car *merge-conflicts-redo-stack*)]
             [start-char (MergeConflictUndoEntry-start-char entry)]
             [resolved-text (MergeConflictUndoEntry-resolved-text entry)]
             [original-text (MergeConflictUndoEntry-original-text entry)])
        (set! *merge-conflicts-redo-stack* (cdr *merge-conflicts-redo-stack*))
        (set! *merge-conflicts-undo-stack* (cons entry *merge-conflicts-undo-stack*))
        (replace-span-with! start-char (string-length original-text) resolved-text)
        (shift-merge-conflicts-history-stacks! start-char (- (string-length resolved-text)
                                                             (string-length original-text)))
        (let ([updated-session (prepare-session-from-current-buffer #t)])
          (if updated-session
              (begin
                (reset-session-preview-state! updated-session)
                (focus-current-conflict! updated-session)
                (announce-current-conflict! updated-session)
                'updated)
              (begin
                (hide-merge-conflicts-window!)
                'closed))))))

(define (merge-conflicts-window-event _state event)
  (cond
    [(or (key-event-escape? event) (key-matches-char? event #\q))
     (when *merge-conflicts-session*
       (restore-current-preview! *merge-conflicts-session*))
     event-result/close]
    [(key-event-tab? event)
     (cycle-preview-action! (if (equal? (key-event-modifier event) key-modifier-shift) -1 1))
     event-result/consume]
    [(or (key-event-up? event) (key-matches-char? event #\k))
     (navigate-current-session! -1)
     event-result/consume]
    [(or (key-event-down? event) (key-matches-char? event #\j))
     (navigate-current-session! 1)
     event-result/consume]
    [(key-event-enter? event)
     (if (equal? (apply-current-resolution! (session-selected-action *merge-conflicts-session*)) 'resolved-all)
         event-result/close
         event-result/consume)]
    [(key-matches-char? event #\r)
     (when (and *merge-conflicts-session* (session-preview-active? *merge-conflicts-session*))
       (restore-current-preview! *merge-conflicts-session*))
     (if (prepare-session-from-current-buffer #t)
          (begin
             (focus-current-conflict! *merge-conflicts-session*)
             (announce-current-conflict! *merge-conflicts-session*)
            event-result/consume)
          event-result/close)]
    [(key-matches-char? event #\u)
     (if (equal? (undo-merge-conflict-change!) 'closed)
         event-result/close
         event-result/consume)]
    [(key-matches-char? event #\U)
     (if (equal? (redo-merge-conflict-change!) 'closed)
         event-result/close
         event-result/consume)]
    [(key-matches-char? event #\o)
      (if (equal? (apply-current-resolution! 'ours) 'resolved-all)
           event-result/close
           event-result/consume)]
    [(key-matches-char? event #\t)
     (if (equal? (apply-current-resolution! 'theirs) 'resolved-all)
         event-result/close
         event-result/consume)]
    [(key-matches-char? event #\b)
     (if (equal? (apply-current-resolution! 'base) 'resolved-all)
         event-result/close
         event-result/consume)]
    [(key-matches-char? event #\a)
     (if (equal? (apply-current-resolution! 'both) 'resolved-all)
         event-result/close
         event-result/consume)]
    [(key-matches-char? event #\s)
     (if (equal? (apply-current-resolution! 'strip) 'resolved-all)
         event-result/close
         event-result/consume)]
    [else event-result/consume]))

;;@doc
;; Open or refresh the merge-conflict window for the current buffer.
(define (merge-conflicts)
  (let ([session (prepare-session-from-current-buffer #t)])
    (when session
      (focus-current-conflict! session)
      (show-merge-conflicts-window! session)
      (announce-current-conflict! session))))

;;@doc
;; Rescan the current buffer and reopen the merge-conflict window if conflicts remain.
(define (merge-conflicts-refresh)
  (when (and *merge-conflicts-session* (session-preview-active? *merge-conflicts-session*))
    (restore-current-preview! *merge-conflicts-session*))
  (merge-conflicts))

;;@doc
;; Jump to the next merge conflict in the current buffer.
(define (merge-conflicts-next)
  (navigate-current-session! 1))

;;@doc
;; Jump to the previous merge conflict in the current buffer.
(define (merge-conflicts-prev)
  (navigate-current-session! -1))

;;@doc
;; Keep the current branch's side. Applies to every conflict overlapping the
;; current selection, or to the current conflict when nothing is selected.
(define (merge-conflicts-keep-ours)
  (apply-current-resolution! 'ours))

;;@doc
;; Keep the incoming branch's side. Applies to every conflict overlapping the
;; current selection, or to the current conflict when nothing is selected.
(define (merge-conflicts-keep-theirs)
  (apply-current-resolution! 'theirs))

;;@doc
;; Keep the base section when present. Applies to every conflict overlapping
;; the current selection, or to the current conflict when nothing is selected.
;; Conflicts without a base section are skipped.
(define (merge-conflicts-keep-base)
  (apply-current-resolution! 'base))

;;@doc
;; Concatenate ours followed by theirs. Applies to every conflict overlapping
;; the current selection, or to the current conflict when nothing is selected.
(define (merge-conflicts-keep-both)
  (apply-current-resolution! 'both))

;;@doc
;; Remove conflict marker lines and keep the section bodies in order. Applies
;; to every conflict overlapping the current selection, or to the current
;; conflict when nothing is selected.
(define (merge-conflicts-strip-markers)
  (apply-current-resolution! 'strip))

(install-merge-conflict-hooks!)
