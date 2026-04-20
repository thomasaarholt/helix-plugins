(require "helix/components.scm")
(require "helix/editor.scm")
(require "helix/misc.scm")
(require "helix/static.scm")
(require (prefix-in helix. "helix/commands.scm"))

(require "core/fs.scm")
(require "core/model.scm")
(require "ui/window.scm")

(provide dired
         dired-refresh
         dired-next
         dired-prev
         dired-open
         dired-up-directory
         dired-create-file
         dired-create-directory
         dired-rename
         dired-delete)

(define *dired-window-name* "dired-window")
(define *dired-session* #f)

(define (key-matches-char? event char)
  (and (key-event-char event) (equal? (key-event-char event) char)))

(define (hide-dired-window!)
  (pop-last-component-by-name! *dired-window-name*))

(define (show-dired-window! session)
  (hide-dired-window!)
  (push-component!
   (new-component!
    *dired-window-name*
    session
    dired-render
    (hash "handle_event" dired-window-event
          "cursor" dired-cursor))))

(define (current-doc-id)
  (editor->doc-id (editor-focus)))

(define (current-doc-path)
  (editor-document->path (current-doc-id)))

(define (default-dired-directory)
  (let ([path (current-doc-path)])
    (cond
      [(and path (path-exists? path) (is-dir? path))
       (normalize-directory-path path)]
      [(and path (path-exists? path) (is-file? path))
       (normalize-directory-path path)]
      [else (normalize-directory-path (get-helix-cwd))])))

(define (active-session)
  (if *dired-session*
      *dired-session*
      (begin
        (set! *dired-session* (build-session (default-dired-directory)))
        *dired-session*)))

(define (announce-session! session)
  (set-status!
   (string-append "Dired "
                  (session-directory session)
                  " ("
                  (int->string (session-directory-count session))
                  " dirs, "
                  (int->string (session-file-count session))
                  " files).")))

(define (refresh-session! [preferred-path #f])
  (let ([session (active-session)])
    (reload-session! session preferred-path)
    (announce-session! session)
    session))

(define (open-session-for-directory! directory [preferred-path #f])
  (let ([session (active-session)])
    (open-session-directory! session directory preferred-path)
    (announce-session! session)
    session))

(define (current-entry)
  (session-current-entry (active-session)))

(define (current-entry-or-warn)
  (let ([entry (current-entry)])
    (if entry
        entry
        (begin
          (set-warning! "The current directory is empty.")
          #f))))

(define (blank-input? text)
  (= (string-length text) 0))

(define (target-parent-exists? path)
  (let ([parent (path-parent path)])
    (and parent (path-exists? parent) (is-dir? parent))))

(define (delete-selection-path session)
  (let* ([entries (session-entries session)]
         [index (session-current-index session)]
         [count (length entries)])
    (cond
      [(<= count 1) #f]
      [(< (+ index 1) count) (DiredEntry-path (list-ref entries (+ index 1)))]
      [(> index 0) (DiredEntry-path (list-ref entries (- index 1)))]
      [else #f])))

(define (open-entry! entry)
  (cond
    [(entry-parent? entry)
     (let ([current-directory (session-directory (active-session))])
       (open-session-for-directory! (DiredEntry-path entry) current-directory))
     'updated]
    [(entry-directory? entry)
     (open-session-for-directory! (DiredEntry-path entry))
     'updated]
    [else
     (let ([path (DiredEntry-path entry)])
       (enqueue-thread-local-callback (lambda () (helix.open path)))
       'close)]))

(define (visit-parent-directory!)
  (let* ([session (active-session)]
         [directory (session-directory session)]
         [parent (path-parent directory)])
    (if parent
        (begin
          (open-session-for-directory! parent directory)
          'updated)
        (begin
          (set-warning! "Already at the filesystem root.")
          'noop))))

(define (create-file-at-input! session input)
  (let ([target (resolve-path (session-directory session) input)])
    (cond
      [(blank-input? input)
       (set-warning! "File name cannot be empty.")]
      [(path-exists? target)
       (set-warning! "That path already exists.")]
      [(not (target-parent-exists? target))
       (set-warning! "The parent directory does not exist.")]
      [else
       (create-empty-file! target)
       (refresh-session! target)
       (set-status! (string-append "Created " target "."))])))

(define (create-directory-at-input! session input)
  (let ([target (resolve-path (session-directory session) input)])
    (cond
      [(blank-input? input)
       (set-warning! "Directory name cannot be empty.")]
      [(path-exists? target)
       (set-warning! "That path already exists.")]
      [(not (target-parent-exists? target))
       (set-warning! "The parent directory does not exist.")]
      [else
       (create-directory! target)
       (refresh-session! target)
       (set-status! (string-append "Created directory " target "."))])))

(define (rename-entry-to-input! session entry input)
  (let* ([source (DiredEntry-path entry)]
         [target (resolve-path (session-directory session) input)])
    (cond
      [(blank-input? input)
       (set-warning! "Target path cannot be empty.")]
      [(path-exists? target)
       (set-warning! "The target path already exists.")]
      [(equal? source target)
       (set-warning! "Source and target are the same path.")]
      [(not (target-parent-exists? target))
       (set-warning! "The target parent directory does not exist.")]
      [(and (entry-directory? entry) (path-descendant? source target))
       (set-warning! "Cannot move a directory inside itself.")]
      [else
       (move-path! source target)
       (refresh-session! target)
       (set-status! (string-append "Moved " source " to " target "."))])))

(define (delete-entry-with-confirmation! session entry confirmation)
  (cond
    [(not (equal? confirmation "yes"))
     (set-warning! "Delete cancelled. Type yes to confirm deletion.")]
    [else
     (let ([fallback (delete-selection-path session)]
           [path (DiredEntry-path entry)])
       (delete-path! path)
       (refresh-session! fallback)
       (set-status! (string-append "Deleted " path ".")))]))

(define (prompt-for-new-file!)
  (let ([session (active-session)])
    (push-component!
     (prompt "New file (relative or absolute): "
             (lambda (input) (create-file-at-input! session input))))))

(define (prompt-for-new-directory!)
  (let ([session (active-session)])
    (push-component!
     (prompt "New directory (relative or absolute): "
             (lambda (input) (create-directory-at-input! session input))))))

(define (prompt-for-rename!)
  (let* ([session (active-session)]
         [entry (current-entry-or-warn)])
    (cond
      [(not entry) void]
      [(entry-parent? entry)
       (set-warning! "Cannot rename the parent directory entry.")]
      [else
       (push-component!
        (prompt (string-append "Rename/move " (DiredEntry-name entry) " to: ")
                (lambda (input) (rename-entry-to-input! session entry input))))])))

(define (prompt-for-delete!)
  (let* ([session (active-session)]
         [entry (current-entry-or-warn)])
    (cond
      [(not entry) void]
      [(entry-parent? entry)
       (set-warning! "Cannot delete the parent directory entry.")]
      [else
       (push-component!
        (prompt (string-append "Delete " (DiredEntry-name entry) "? Type yes: ")
                (lambda (input) (delete-entry-with-confirmation! session entry input))))])))

(define (dired-window-event state event)
  (cond
    [(or (key-event-escape? event) (key-matches-char? event #\q))
     event-result/close]
    [(or (key-event-up? event) (key-matches-char? event #\k))
     (session-move! state -1)
     event-result/consume]
    [(or (key-event-down? event) (key-matches-char? event #\j))
     (session-move! state 1)
     event-result/consume]
    [(key-matches-char? event #\g)
     (session-goto-top! state)
     event-result/consume]
    [(key-matches-char? event #\G)
     (session-goto-bottom! state)
     event-result/consume]
    [(or (key-event-enter? event) (key-matches-char? event #\l) (key-matches-char? event #\o))
     (let ([entry (current-entry-or-warn)])
       (if entry
           (if (equal? (open-entry! entry) 'close)
               event-result/close
               event-result/consume)
           event-result/consume))]
    [(or (key-matches-char? event #\h) (key-matches-char? event #\-))
     (visit-parent-directory!)
     event-result/consume]
    [(key-matches-char? event #\r)
     (refresh-session!)
     event-result/consume]
    [(key-matches-char? event #\n)
     (prompt-for-new-file!)
     event-result/consume]
    [(key-matches-char? event #\+)
     (prompt-for-new-directory!)
     event-result/consume]
    [(key-matches-char? event #\R)
     (prompt-for-rename!)
     event-result/consume]
    [(key-matches-char? event #\D)
     (prompt-for-delete!)
     event-result/consume]
    [else event-result/consume-without-rerender]))

;;@doc
;; Open the Dired browser for the current file's directory, or Helix's cwd.
(define (dired)
  (let* ([target-directory (if *dired-session*
                               (session-directory *dired-session*)
                               (default-dired-directory))]
         [session (open-session-for-directory! target-directory)])
    (show-dired-window! session)))

;;@doc
;; Refresh the current Dired directory listing.
(define (dired-refresh)
  (let ([session (refresh-session!)])
    (show-dired-window! session)))

;;@doc
;; Move the Dired selection to the next entry.
(define (dired-next)
  (let ([session (active-session)])
    (session-move! session 1)
    (show-dired-window! session)))

;;@doc
;; Move the Dired selection to the previous entry.
(define (dired-prev)
  (let ([session (active-session)])
    (session-move! session -1)
    (show-dired-window! session)))

;;@doc
;; Open the selected file, or descend into the selected directory.
(define (dired-open)
  (let ([entry (current-entry-or-warn)])
    (when entry
      (if (equal? (open-entry! entry) 'updated)
          (show-dired-window! (active-session))
          (hide-dired-window!)))))

;;@doc
;; Visit the parent directory of the current Dired listing.
(define (dired-up-directory)
  (when (equal? (visit-parent-directory!) 'updated)
    (show-dired-window! (active-session))))

;;@doc
;; Prompt for a new file path and create an empty file from Dired.
(define (dired-create-file)
  (show-dired-window! (active-session))
  (prompt-for-new-file!))

;;@doc
;; Prompt for a new directory path and create it from Dired.
(define (dired-create-directory)
  (show-dired-window! (active-session))
  (prompt-for-new-directory!))

;;@doc
;; Prompt for a new path and rename or move the selected Dired entry.
(define (dired-rename)
  (show-dired-window! (active-session))
  (prompt-for-rename!))

;;@doc
;; Prompt for confirmation and delete the selected Dired entry.
(define (dired-delete)
  (show-dired-window! (active-session))
  (prompt-for-delete!))
