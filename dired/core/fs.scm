(provide path-absolute?
         normalize-directory-path
         normalize-existing-path
         path-parent
         path-join
         resolve-path
         path-display-name
         path-descendant?
         create-empty-file!
         move-path!
         delete-path!)

(define (path-absolute? path)
  (and (> (string-length path) 0)
       (equal? (string-ref path 0) #\/)))

(define (strip-trailing-slash path)
  (if (and (> (string-length path) 1)
           (equal? (string-ref path (- (string-length path) 1)) #\/))
      (strip-trailing-slash (substring path 0 (- (string-length path) 1)))
      path))

(define (normalize-existing-path path)
  (if (and path (path-exists? path))
      (strip-trailing-slash (canonicalize-path path))
      (strip-trailing-slash path)))

(define (normalize-directory-path path)
  (cond
    [(not path) #f]
    [(and path (path-exists? path) (is-dir? path))
     (normalize-existing-path path)]
    [(and path (path-exists? path) (is-file? path))
     (path-parent (normalize-existing-path path))]
    [else (strip-trailing-slash path)]))

(define (path-parent path)
  (let ([parent (parent-name path)])
    (if (> (string-length parent) 0)
        (strip-trailing-slash parent)
        #f)))

(define (path-join base child)
  (cond
    [(or (equal? child "") (equal? child ".")) (strip-trailing-slash base)]
    [(path-absolute? child) (strip-trailing-slash child)]
    [(equal? base "/") (string-append "/" child)]
    [else (string-append (strip-trailing-slash base) "/" child)]))

(define (resolve-path base target)
  (let ([candidate (path-join base target)])
    (if (path-exists? candidate)
        (normalize-existing-path candidate)
        (strip-trailing-slash candidate))))

(define (path-display-name path)
  (let ([name (file-name path)])
    (if (> (string-length name) 0)
        name
        path)))

(define (path-descendant? ancestor candidate)
  (let* ([normalized-ancestor (strip-trailing-slash ancestor)]
         [normalized-candidate (strip-trailing-slash candidate)]
         [ancestor-length (string-length normalized-ancestor)]
         [candidate-length (string-length normalized-candidate)])
    (cond
      [(equal? normalized-ancestor "/") (> candidate-length 1)]
      [(<= candidate-length ancestor-length) #f]
      [(not (equal? (substring normalized-candidate 0 ancestor-length) normalized-ancestor)) #f]
      [else (equal? (string-ref normalized-candidate ancestor-length) #\/)])))

(define (create-empty-file! path)
  (let ([port (open-output-file path)])
    (close-output-port port)))

(define (copy-file-bytes! source target)
  (let ([input-port (open-input-file source)]
        [output-port (open-output-file target)])
    (let loop ()
      (let ([next-byte (read-byte input-port)])
        (if (eof-object? next-byte)
            (begin
              (close-input-port input-port)
              (close-output-port output-port))
            (begin
              (write-byte next-byte output-port)
              (loop)))))))

(define (move-path! source target)
  (if (is-dir? source)
      (begin
        (copy-directory-recursively! source target)
        (delete-directory! source))
      (begin
        (copy-file-bytes! source target)
        (delete-file! source))))

(define (delete-path! path)
  (if (is-dir? path)
      (delete-directory! path)
      (delete-file! path)))
