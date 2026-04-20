(require "helix/components.scm")
(require "../core/model.scm")

(provide dired-render
         dired-cursor)

(struct UIStyles (popup border title text dim selected directory parent hint) #:transparent)

(define (ui-styles)
  (UIStyles (theme-scope-ref "ui.popup")
            (theme-scope-ref "ui.menu")
            (style-with-bold (theme-scope-ref "ui.text"))
            (theme-scope-ref "ui.text")
            (theme-scope-ref "ui.text.inactive")
            (theme-scope-ref "ui.menu.selected")
            (style-with-bold (theme-scope-ref "ui.text"))
            (style-with-bold (theme-scope-ref "ui.text.inactive"))
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

(define (pluralize count singular plural)
  (if (= count 1) singular plural))

(define (calculate-window-area rect)
  (define max-width (max 1 (- (area-width rect) 2)))
  (define width (if (< max-width 60) max-width (min 120 max-width)))
  (define height (max 1 (- (area-height rect) 2)))
  (define x (+ (area-x rect) (quotient (max 0 (- (area-width rect) width)) 2)))
  (define y (+ (area-y rect) (quotient (max 0 (- (area-height rect) height)) 2)))
  (area x y width height))

(define (list-height window)
  (max 1 (- (area-height window) 8)))

(define (drop-items lst count)
  (cond
    [(or (null? lst) (<= count 0)) lst]
    [else (drop-items (cdr lst) (- count 1))]))

(define (take-items lst count)
  (cond
    [(or (null? lst) (<= count 0)) '()]
    [else (cons (car lst) (take-items (cdr lst) (- count 1)))]))

(define (visible-entries session)
  (take-items (drop-items (session-entries session) (session-scroll-offset session))
              (session-visible-lines session)))

(define (entry-prefix entry)
  (cond
    [(entry-parent? entry) "[U] "]
    [(entry-directory? entry) "[D] "]
    [else "[F] "]))

(define (entry-label entry)
  (cond
    [(entry-parent? entry) "../"]
    [(entry-directory? entry) (string-append (DiredEntry-name entry) "/")]
    [else (DiredEntry-name entry)]))

(define (entry-style entry styles selected?)
  (cond
    [selected? (UIStyles-selected styles)]
    [(entry-parent? entry) (UIStyles-parent styles)]
    [(entry-directory? entry) (UIStyles-directory styles)]
    [else (UIStyles-text styles)]))

(define (selection-summary session)
  (let ([count (session-entry-count session)])
    (if (> count 0)
        (string-append "Selection "
                       (int->string (+ 1 (session-current-index session)))
                       "/"
                       (int->string count))
        "Selection 0/0")))

(define (directory-summary session)
  (string-append (int->string (session-directory-count session))
                 " "
                 (pluralize (session-directory-count session) "dir" "dirs")
                 "  "
                 (int->string (session-file-count session))
                 " "
                 (pluralize (session-file-count session) "file" "files")
                 "  "
                 (selection-summary session)))

(define (dired-render state rect frame)
  (define styles (ui-styles))
  (define window (calculate-window-area rect))
  (define x (+ (area-x window) 2))
  (define y (+ (area-y window) 1))
  (define width (max 1 (- (area-width window) 4)))
  (define entries-top (+ y 4))

  (session-set-visible-lines! state (list-height window))
  (session-ensure-selection-visible! state)

  (buffer/clear frame window)
  (block/render frame
                window
                (make-block (UIStyles-popup styles) (UIStyles-border styles) "all" "plain"))

  (draw-filled-line! frame x y width "Dired" (UIStyles-title styles))
  (draw-filled-line! frame x (+ y 1) width (truncate-left-string (session-directory state) width)
                     (UIStyles-dim styles))
  (draw-filled-line! frame x (+ y 2) width (directory-summary state) (UIStyles-hint styles))

  (let loop ([remaining (visible-entries state)]
             [entry-index (session-scroll-offset state)]
             [line-y entries-top])
    (when (and (not (null? remaining))
               (< line-y (+ entries-top (session-visible-lines state))))
      (let* ([entry (car remaining)]
             [selected? (= entry-index (session-current-index state))]
             [label (string-append (entry-prefix entry) (entry-label entry))])
        (draw-filled-line! frame x line-y width label (entry-style entry styles selected?))
        (loop (cdr remaining) (+ entry-index 1) (+ line-y 1)))))

  (when (= (session-entry-count state) 0)
    (draw-filled-line! frame x entries-top width "<empty directory>" (UIStyles-dim styles)))

  (draw-filled-line! frame
                     x
                     (+ (area-y window) (- (area-height window) 3))
                     width
                     "Enter open  h parent  j/k move  g/G top/bottom  r refresh"
                     (UIStyles-hint styles))
  (draw-filled-line! frame
                     x
                     (+ (area-y window) (- (area-height window) 2))
                     width
                     "n new-file  + mkdir  R rename/move  D delete  q close"
                     (UIStyles-hint styles)))

(define (dired-cursor state rect)
  (let* ([window (calculate-window-area rect)]
         [visible-count (list-height window)]
         [relative-index (- (session-current-index state) (session-scroll-offset state))])
    (if (and (>= relative-index 0) (< relative-index visible-count))
        (position (+ (+ (area-y window) 1) 4 relative-index) (+ (area-x window) 6))
        (position (+ (area-y window) 5) (+ (area-x window) 6)))))
