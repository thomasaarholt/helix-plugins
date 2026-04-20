(require "helix/components.scm")
(require "../core/model.scm")

(provide merge-conflicts-render)

(struct UIStyles (popup text dim title ours theirs base conflict hint) #:transparent)

(define *section-preview-limit* 2)

(define (ui-styles)
  (UIStyles (theme-scope-ref "ui.popup")
            (theme-scope-ref "ui.text")
            (theme-scope-ref "ui.text.inactive")
            (style-with-bold (theme-scope-ref "ui.text"))
            (theme-scope-ref "diff.plus")
            (theme-scope-ref "diff.minus")
            (theme-scope-ref "ui.text.inactive")
            (theme-scope-ref "diff.delta.conflict")
            (theme-scope-ref "hint")))

(define (truncate-string text width)
  (cond
    [(<= width 0) ""]
    [(<= (string-length text) width) text]
    [(<= width 3) (substring text 0 width)]
    [else (string-append (substring text 0 (- width 3)) "...")]))

(define (truncate-left-string text width)
  (cond
    [(<= width 0) ""]
    [(<= (string-length text) width) text]
    [(<= width 3) (substring text (- (string-length text) width) (string-length text))]
    [else
     (string-append "..."
                    (substring text
                               (- (string-length text) (- width 3))
                               (string-length text)))]))

(define (draw-filled-line! frame x y width text style)
  (when (> width 0)
    (frame-set-string! frame x y (make-string width #\space) style)
    (frame-set-string! frame x y (truncate-string text width) style)))

(define (format-line-count count)
  (string-append (int->string count) (if (= count 1) " line" " lines")))

(define (take-lines lines remaining)
  (cond
    [(or (null? lines) (<= remaining 0)) '()]
    [else (cons (car lines) (take-lines (cdr lines) (- remaining 1)))]))

(define (draw-preview-lines! frame x y width lines style)
  (let loop ([remaining lines] [line-y y])
    (cond
      [(null? remaining) line-y]
      [else
       (draw-filled-line! frame x line-y width (string-append "  " (car remaining)) style)
       (loop (cdr remaining) (+ line-y 1))])))

(define (draw-section! frame x y width label label-style preview-lines line-count text-style dim-style)
  (draw-filled-line! frame x y width (string-append label " - " (format-line-count line-count))
                     (style-with-bold label-style))

  (let* ([visible-lines (take-lines preview-lines *section-preview-limit*)]
         [visible-count (length visible-lines)])
    (cond
      [(= line-count 0)
       (draw-filled-line! frame x (+ y 1) width "  <empty>" dim-style)
       (+ y 3)]
      [else
       (let ([next-y (draw-preview-lines! frame x (+ y 1) width visible-lines text-style)])
         (if (> line-count visible-count)
             (begin
               (draw-filled-line! frame
                                  x
                                  next-y
                                  width
                                  (string-append "  ... "
                                                 (int->string (- line-count visible-count))
                                                 " more")
                                  dim-style)
               (+ next-y 2))
              (+ next-y 1)))])))

(define (action-summary-label action)
  (cond
    [(equal? action 'ours) "OURS"]
    [(equal? action 'theirs) "THEIRS"]
    [(equal? action 'base) "BASE"]
    [(equal? action 'both) "BOTH"]
    [(equal? action 'strip) "STRIP"]
    [else "OURS"]))

(define (calculate-window-area rect conflict)
  (define max-width (max 1 (- (area-width rect) 4)))
  (define desired-width (min 96 max-width))
  (define desired-height
    (cond
      [(not conflict) 9]
      [(ConflictBlock-has-base conflict) 19]
      [else 16]))
  (define max-height (max 1 (- (area-height rect) 4)))
  (define height (min desired-height max-height))
  (define x (+ (area-x rect) (quotient (max 0 (- (area-width rect) desired-width)) 2)))
  (define y (+ (area-y rect) (quotient (max 0 (- (area-height rect) height)) 2)))

  (area x y desired-width height))

(define (merge-conflicts-render state rect frame)
  (define conflict (session-current-conflict state))
  (define styles (ui-styles))
  (define window (calculate-window-area rect conflict))
  (define x (+ (area-x window) 2))
  (define y (+ (area-y window) 1))
  (define width (max 1 (- (area-width window) 4)))

  (buffer/clear frame window)
  (block/render frame
                window
                (make-block (UIStyles-popup styles) (UIStyles-conflict styles) "all" "plain"))

  (draw-filled-line! frame x y width "Merge conflicts" (UIStyles-title styles))

  (if conflict
       (let* ([path (if (MergeConflictSession-doc-path state)
                        (MergeConflictSession-doc-path state)
                        "<unnamed buffer>")]
              [summary (string-append "Conflict "
                                      (int->string (ConflictBlock-index conflict))
                                      "/"
                                      (int->string (session-conflict-count state))
                                      "  lines "
                                      (int->string (ConflictBlock-start-line conflict))
                                      "-"
                                      (int->string (ConflictBlock-end-line conflict)))]
              [selection-summary
               (string-append "Selected "
                              (action-summary-label (session-selected-action state))
                              (if (session-preview-active? state)
                                  " | preview"
                                  " | Tab previews"))]
              [help-line-1 "Tab cycle  Enter apply  j/k move  u undo  U redo"]
              [help-line-2 (if (ConflictBlock-has-base conflict)
                               "o ours  t theirs  b base  a both  s strip  r refresh  q close"
                               "o ours  t theirs  a both  s strip  r refresh  q close")]
              [after-ours
               (draw-section! frame
                              x
                             (+ y 4)
                             width
                             "OURS"
                             (UIStyles-ours styles)
                             (ConflictBlock-ours-preview conflict)
                             (ConflictBlock-ours-line-count conflict)
                             (UIStyles-text styles)
                             (UIStyles-dim styles))]
             [after-base
              (if (ConflictBlock-has-base conflict)
                  (draw-section! frame
                                 x
                                 after-ours
                                 width
                                 "BASE"
                                 (UIStyles-base styles)
                                 (ConflictBlock-base-preview conflict)
                                 (ConflictBlock-base-line-count conflict)
                                 (UIStyles-dim styles)
                                 (UIStyles-dim styles))
                  after-ours)])
         (draw-filled-line! frame x (+ y 1) width (truncate-left-string path width) (UIStyles-dim styles))
         (draw-filled-line! frame x (+ y 2) width summary (UIStyles-conflict styles))
         (draw-filled-line! frame x (+ y 3) width selection-summary (UIStyles-hint styles))
         (draw-section! frame
                        x
                       after-base
                       width
                       "THEIRS"
                       (UIStyles-theirs styles)
                       (ConflictBlock-theirs-preview conflict)
                       (ConflictBlock-theirs-line-count conflict)
                       (UIStyles-text styles)
                       (UIStyles-dim styles))
         (draw-filled-line! frame
                            x
                            (+ (area-y window) (- (area-height window) 3))
                            width
                            help-line-1
                            (UIStyles-hint styles))
         (draw-filled-line! frame
                            x
                            (+ (area-y window) (- (area-height window) 2))
                            width
                            help-line-2
                            (UIStyles-hint styles)))
       (begin
         (draw-filled-line! frame x (+ y 2) width "No merge conflicts remain in this buffer."
                            (UIStyles-text styles))
        (draw-filled-line! frame x (+ y 4) width "Run :merge-conflicts-refresh after manual edits."
                           (UIStyles-dim styles)))))
