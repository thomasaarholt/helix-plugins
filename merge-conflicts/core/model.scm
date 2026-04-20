(provide ConflictBlock
         ConflictBlock?
         ConflictBlock-index
         ConflictBlock-start-line
         ConflictBlock-end-line
         ConflictBlock-start-char
         ConflictBlock-end-char
         ConflictBlock-full-text
         ConflictBlock-ours-text
         ConflictBlock-ours-preview
         ConflictBlock-ours-line-count
         ConflictBlock-base-text
         ConflictBlock-base-preview
         ConflictBlock-base-line-count
         ConflictBlock-theirs-text
         ConflictBlock-theirs-preview
         ConflictBlock-theirs-line-count
         ConflictBlock-has-base
         ConflictDocument
         ConflictDocument?
         ConflictDocument-conflicts
         ConflictDocument-contains-markers
         ConflictDocument-malformed
         MergeConflictSession
         MergeConflictSession?
         MergeConflictSession-doc-id
         MergeConflictSession-doc-path
         MergeConflictSession-conflicts-box
         MergeConflictSession-current-index-box
         MergeConflictSession-selected-action-box
         MergeConflictSession-preview-active-box
         MergeConflictSession-preview-length-box
         session-conflicts
         session-conflict-count
         session-current-index
         session-current-conflict
         session-set-current-index!
         session-cycle-current-index!
         session-clamp-current-index!
         session-selected-action
         session-set-selected-action!
         session-preview-active?
         session-set-preview-active!
         session-preview-length
         session-set-preview-length!
         scan-conflicts
         resolution-text)

(struct LineInfo (index start-char end-char raw-text preview-text) #:transparent)

(struct ConflictBlock
        (index
         start-line
         end-line
         start-char
         end-char
         full-text
         ours-text
         ours-preview
         ours-line-count
         base-text
         base-preview
         base-line-count
         theirs-text
         theirs-preview
         theirs-line-count
         has-base)
        #:transparent)

(struct ConflictDocument (conflicts contains-markers malformed) #:transparent)

(struct MergeConflictSession
        (doc-id
         doc-path
         conflicts-box
         current-index-box
         selected-action-box
         preview-active-box
         preview-length-box)
        #:transparent)

(define *preview-limit* 3)

(define (drop-string-suffix text suffix)
  (if (ends-with? text suffix)
      (substring text 0 (- (string-length text) (string-length suffix)))
      text))

(define (line->preview-text text)
  (drop-string-suffix (drop-string-suffix text "\n") "\r"))

(define (scan-lines text)
  (define len (string-length text))

  (let loop ([index 0] [line-start 0] [line-index 0] [lines '()])
    (cond
      [(>= index len)
       (reverse
        (if (< line-start len)
            (let ([raw (substring text line-start len)])
              (cons (LineInfo line-index line-start len raw (line->preview-text raw)) lines))
            lines))]
      [(equal? (string-ref text index) #\newline)
       (let* ([line-end (+ index 1)]
              [raw (substring text line-start line-end)])
         (loop line-end
               line-end
               (+ line-index 1)
               (cons (LineInfo line-index line-start line-end raw (line->preview-text raw)) lines)))]
      [else (loop (+ index 1) line-start line-index lines)])))

(define (start-marker-line? line)
  (starts-with? (LineInfo-raw-text line) "<<<<<<<"))

(define (base-marker-line? line)
  (starts-with? (LineInfo-raw-text line) "|||||||"))

(define (divider-marker-line? line)
  (starts-with? (LineInfo-raw-text line) "======="))

(define (end-marker-line? line)
  (starts-with? (LineInfo-raw-text line) ">>>>>>>"))

(define (take-preview-lines lines remaining)
  (cond
    [(or (null? lines) (<= remaining 0)) '()]
    [else
     (cons (LineInfo-preview-text (car lines))
           (take-preview-lines (cdr lines) (- remaining 1)))]))

(define (lines->text lines)
  (if (null? lines) "" (apply string-append (map LineInfo-raw-text lines))))

(define (build-conflict-block conflict-index start-marker base-marker divider-marker end-marker ours-lines base-lines theirs-lines has-base)
  (define ordered-ours (reverse ours-lines))
  (define ordered-base (reverse base-lines))
  (define ordered-theirs (reverse theirs-lines))
  (define ours-text (lines->text ordered-ours))
  (define base-text (if has-base (lines->text ordered-base) #f))
  (define theirs-text (lines->text ordered-theirs))

  (ConflictBlock conflict-index
                 (+ 1 (LineInfo-index start-marker))
                 (+ 1 (LineInfo-index end-marker))
                 (LineInfo-start-char start-marker)
                 (LineInfo-end-char end-marker)
                 (string-append (LineInfo-raw-text start-marker)
                                ours-text
                                (if has-base
                                    (string-append (LineInfo-raw-text base-marker) base-text)
                                    "")
                                (LineInfo-raw-text divider-marker)
                                theirs-text
                                (LineInfo-raw-text end-marker))
                 ours-text
                 (take-preview-lines ordered-ours *preview-limit*)
                 (length ordered-ours)
                 base-text
                 (if has-base (take-preview-lines ordered-base *preview-limit*) '())
                 (length ordered-base)
                 theirs-text
                 (take-preview-lines ordered-theirs *preview-limit*)
                 (length ordered-theirs)
                 has-base))

(define (parse-conflict-block lines conflict-index)
  (define start-marker (car lines))

  (define (gather-theirs remaining ours-lines base-lines has-base base-marker divider-marker)
    (cond
      [(null? remaining) #f]
      [else
       (let ([line (car remaining)])
         (if (end-marker-line? line)
              (cons (cdr remaining)
                    (build-conflict-block conflict-index
                                          start-marker
                                          base-marker
                                          divider-marker
                                          line
                                          ours-lines
                                          base-lines
                                         '()
                                         has-base))
             (let loop ([rest (cdr remaining)] [theirs-lines (list line)])
               (cond
                 [(null? rest) #f]
                 [else
                  (let ([next-line (car rest)])
                    (if (end-marker-line? next-line)
                        (cons (cdr rest)
                              (build-conflict-block conflict-index
                                                    start-marker
                                                    base-marker
                                                    divider-marker
                                                    next-line
                                                    ours-lines
                                                    base-lines
                                                    theirs-lines
                                                    has-base))
                        (loop (cdr rest) (cons next-line theirs-lines))))]))))]))

  (define (gather-base remaining ours-lines base-marker)
    (cond
      [(null? remaining) #f]
      [else
       (let ([line (car remaining)])
         (cond
           [(divider-marker-line? line)
            (gather-theirs (cdr remaining) ours-lines '() #t base-marker line)]
           [(base-marker-line? line) #f]
           [else
            (let loop ([rest (cdr remaining)] [base-lines (list line)])
              (cond
                [(null? rest) #f]
                [else
                  (let ([next-line (car rest)])
                    (cond
                      [(divider-marker-line? next-line)
                      (gather-theirs (cdr rest) ours-lines base-lines #t base-marker next-line)]
                      [(base-marker-line? next-line) #f]
                      [else
                       (loop (cdr rest) (cons next-line base-lines))]))]))]))]))

  (let loop ([remaining (cdr lines)] [ours-lines '()])
    (cond
      [(null? remaining) #f]
      [else
       (let ([line (car remaining)])
         (cond
           [(base-marker-line? line) (gather-base (cdr remaining) ours-lines line)]
           [(divider-marker-line? line)
            (gather-theirs (cdr remaining) ours-lines '() #f #f line)]
           [else (loop (cdr remaining) (cons line ours-lines))]))]))
  )

(define (scan-conflicts text)
  (let loop ([remaining (scan-lines text)]
             [conflict-index 1]
             [blocks '()]
             [contains-markers #f]
             [malformed #f])
    (cond
      [(null? remaining)
       (ConflictDocument (reverse blocks) contains-markers malformed)]
      [else
       (let ([line (car remaining)])
         (cond
           [(start-marker-line? line)
            (let ([parsed (parse-conflict-block remaining conflict-index)])
              (if parsed
                  (loop (car parsed)
                        (+ conflict-index 1)
                        (cons (cdr parsed) blocks)
                        #t
                        malformed)
                  (loop (cdr remaining) conflict-index blocks #t #t)))]
           [(or (base-marker-line? line)
                (divider-marker-line? line)
                (end-marker-line? line))
            (loop (cdr remaining) conflict-index blocks #t #t)]
           [else (loop (cdr remaining) conflict-index blocks contains-markers malformed)]))])))

(define (session-conflicts session)
  (unbox (MergeConflictSession-conflicts-box session)))

(define (session-conflict-count session)
  (length (session-conflicts session)))

(define (session-current-index session)
  (unbox (MergeConflictSession-current-index-box session)))

(define (try-list-ref lst index)
  (if (and (>= index 0) (< index (length lst)))
      (list-ref lst index)
      #f))

(define (session-current-conflict session)
  (try-list-ref (session-conflicts session) (session-current-index session)))

(define (session-set-current-index! session index)
  (let ([count (session-conflict-count session)])
    (set-box! (MergeConflictSession-current-index-box session)
              (if (<= count 0)
                  0
                  (max 0 (min index (- count 1)))))))

(define (session-cycle-current-index! session delta)
  (let ([count (session-conflict-count session)])
    (when (> count 0)
      (set-box! (MergeConflictSession-current-index-box session)
                (modulo (+ (session-current-index session) delta) count)))))

(define (session-clamp-current-index! session)
  (session-set-current-index! session (session-current-index session)))

(define (session-selected-action session)
  (unbox (MergeConflictSession-selected-action-box session)))

(define (session-set-selected-action! session action)
  (set-box! (MergeConflictSession-selected-action-box session) action))

(define (session-preview-active? session)
  (unbox (MergeConflictSession-preview-active-box session)))

(define (session-set-preview-active! session active?)
  (set-box! (MergeConflictSession-preview-active-box session) active?))

(define (session-preview-length session)
  (unbox (MergeConflictSession-preview-length-box session)))

(define (session-set-preview-length! session length)
  (set-box! (MergeConflictSession-preview-length-box session) length))

(define (or-string value)
  (if value value ""))

(define (resolution-text block action)
  (cond
    [(equal? action 'ours) (ConflictBlock-ours-text block)]
    [(equal? action 'theirs) (ConflictBlock-theirs-text block)]
    [(equal? action 'base) (or-string (ConflictBlock-base-text block))]
    [(equal? action 'both)
     (string-append (ConflictBlock-ours-text block) (ConflictBlock-theirs-text block))]
    [(equal? action 'strip)
     (string-append (ConflictBlock-ours-text block)
                    (or-string (ConflictBlock-base-text block))
                    (ConflictBlock-theirs-text block))]
    [else #f]))
