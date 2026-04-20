(require "fs.scm")

(provide DiredEntry
         DiredEntry?
         DiredEntry-kind
         DiredEntry-path
         DiredEntry-name
         entry-parent?
         entry-directory?
         entry-file?
         DiredSession
         DiredSession?
         DiredSession-directory-box
         DiredSession-entries-box
         DiredSession-current-index-box
         DiredSession-scroll-offset-box
         DiredSession-visible-lines-box
         build-session
         session-directory
         session-entries
         session-entry-count
         session-current-index
         session-set-current-index!
         session-clamp-current-index!
         session-scroll-offset
         session-set-scroll-offset!
         session-visible-lines
         session-set-visible-lines!
         session-current-entry
         session-directory-count
         session-file-count
         session-ensure-selection-visible!
         session-move!
         session-goto-top!
         session-goto-bottom!
         reload-session!
         open-session-directory!)

(struct DiredEntry (kind path name) #:transparent)

(struct DiredSession
        (directory-box
         entries-box
         current-index-box
         scroll-offset-box
         visible-lines-box)
        #:transparent)

(define (entry-parent? entry)
  (equal? (DiredEntry-kind entry) 'parent))

(define (entry-directory? entry)
  (equal? (DiredEntry-kind entry) 'directory))

(define (entry-file? entry)
  (equal? (DiredEntry-kind entry) 'file))

(define (merge-lists left right comparator)
  (cond
    [(null? left) right]
    [(null? right) left]
    [(comparator (car left) (car right))
     (cons (car left) (merge-lists (cdr left) right comparator))]
    [else
     (cons (car right) (merge-lists left (cdr right) comparator))]))

(define (odd-items lst)
  (cond
    [(null? lst) '()]
    [(null? (cdr lst)) (list (car lst))]
    [else (cons (car lst) (odd-items (cdr (cdr lst))))]))

(define (even-items lst)
  (cond
    [(null? lst) '()]
    [(null? (cdr lst)) '()]
    [else (cons (car (cdr lst)) (even-items (cdr (cdr lst))))]))

(define (merge-sort lst comparator)
  (cond
    [(null? lst) '()]
    [(null? (cdr lst)) lst]
    [else
     (merge-lists (merge-sort (odd-items lst) comparator)
                  (merge-sort (even-items lst) comparator)
                  comparator)]))

(define (entry-rank entry)
  (cond
    [(entry-directory? entry) 0]
    [(entry-file? entry) 1]
    [else 2]))

(define (entry<? left right)
  (let ([left-rank (entry-rank left)]
        [right-rank (entry-rank right)])
    (if (= left-rank right-rank)
        (string<? (DiredEntry-name left) (DiredEntry-name right))
        (< left-rank right-rank))))

(define (path->entry path)
  (DiredEntry (if (is-dir? path) 'directory 'file)
              path
              (path-display-name path)))

(define (parent-entry directory)
  (let ([parent (path-parent directory)])
    (if parent
        (DiredEntry 'parent parent "..")
        #f)))

(define (scan-directory directory)
  (let* ([normalized-directory (normalize-directory-path directory)]
         [entries (merge-sort (map path->entry (read-dir normalized-directory)) entry<?)]
         [parent (parent-entry normalized-directory)])
    (if parent
        (cons parent entries)
        entries)))

(define (build-session directory)
  (let* ([normalized-directory (normalize-directory-path directory)]
         [entries (scan-directory normalized-directory)])
    (DiredSession (box normalized-directory)
                  (box entries)
                  (box 0)
                  (box 0)
                  (box 1))))

(define (session-directory session)
  (unbox (DiredSession-directory-box session)))

(define (session-entries session)
  (unbox (DiredSession-entries-box session)))

(define (session-entry-count session)
  (length (session-entries session)))

(define (session-current-index session)
  (unbox (DiredSession-current-index-box session)))

(define (session-set-current-index! session index)
  (let ([count (session-entry-count session)])
    (set-box! (DiredSession-current-index-box session)
              (cond
                [(<= count 0) 0]
                [else (max 0 (min index (- count 1)))]))))

(define (session-clamp-current-index! session)
  (session-set-current-index! session (session-current-index session)))

(define (session-scroll-offset session)
  (unbox (DiredSession-scroll-offset-box session)))

(define (session-set-scroll-offset! session offset)
  (set-box! (DiredSession-scroll-offset-box session) (max 0 offset)))

(define (session-visible-lines session)
  (unbox (DiredSession-visible-lines-box session)))

(define (session-set-visible-lines! session visible-lines)
  (set-box! (DiredSession-visible-lines-box session) (max 1 visible-lines)))

(define (try-list-ref lst index)
  (if (and (>= index 0) (< index (length lst)))
      (list-ref lst index)
      #f))

(define (session-current-entry session)
  (try-list-ref (session-entries session) (session-current-index session)))

(define (count-entries-of-kind entries kind)
  (length (filter (lambda (entry) (equal? (DiredEntry-kind entry) kind)) entries)))

(define (session-directory-count session)
  (count-entries-of-kind (session-entries session) 'directory))

(define (session-file-count session)
  (count-entries-of-kind (session-entries session) 'file))

(define (session-ensure-selection-visible! session)
  (let* ([visible-lines (max 1 (session-visible-lines session))]
         [count (session-entry-count session)]
         [current-index (session-current-index session)]
         [current-scroll (session-scroll-offset session)]
         [max-scroll (max 0 (- count visible-lines))]
         [new-scroll current-scroll])
    (when (< current-index current-scroll)
      (set! new-scroll current-index))
    (when (>= current-index (+ new-scroll visible-lines))
      (set! new-scroll (- (+ current-index 1) visible-lines)))
    (session-set-scroll-offset! session (max 0 (min new-scroll max-scroll)))))

(define (session-move! session delta)
  (session-set-current-index! session (+ (session-current-index session) delta))
  (session-ensure-selection-visible! session)
  session)

(define (session-goto-top! session)
  (session-set-current-index! session 0)
  (session-set-scroll-offset! session 0)
  session)

(define (session-goto-bottom! session)
  (let ([count (session-entry-count session)])
    (when (> count 0)
      (session-set-current-index! session (- count 1))
      (session-ensure-selection-visible! session)))
  session)

(define (find-entry-index entries path)
  (let loop ([remaining entries] [index 0])
    (cond
      [(null? remaining) #f]
      [(and path (equal? (DiredEntry-path (car remaining)) path)) index]
      [else (loop (cdr remaining) (+ index 1))])))

(define (reload-session! session [preferred-path #f])
  (let* ([directory (normalize-directory-path (session-directory session))]
         [current-entry (session-current-entry session)]
         [selected-path (if preferred-path
                            preferred-path
                            (and current-entry (DiredEntry-path current-entry)))]
         [entries (scan-directory directory)]
         [matched-index (find-entry-index entries selected-path)])
    (set-box! (DiredSession-directory-box session) directory)
    (set-box! (DiredSession-entries-box session) entries)
    (session-set-current-index! session (if matched-index matched-index 0))
    (session-ensure-selection-visible! session)
    session))

(define (open-session-directory! session directory [preferred-path #f])
  (set-box! (DiredSession-directory-box session) (normalize-directory-path directory))
  (reload-session! session preferred-path))
