#lang frog/config
(require racket/list)

;; Called early when Frog launches. Use this to set parameters defined
;; in frog/params.
(define/contract (init)
  (-> any)
  (current-scheme/host "http://alex-hhh.github.io")
  ;; Don't expect that many blog posts, put just YEAR in the permalink ...
  ;; (current-permalink "/{year}/{title}.html")
  (current-title "The Blog of Alex Harsányi")
  (current-author "Alex Harsányi")
  (current-posts-per-page 9)
  (current-permalink "/{year}/{month}/{filename}.html"))

(define (atom? x)
  (and (not (eq? x '())) (not (cons? x))))

(define/contract (enhance-table-data xs)
  (-> (listof xexpr/c) (listof xexpr/c))
  (for/list ((x xs))
    (cond
      ((or (atom? x) (null? x)) x)
      ((or (eq? 'td (car x))
           (eq? 'th (car x)))
       (let ((attributes (second x))
             (contents (drop x 2)))
         (append
          (list
           'td
           (cond ((and (not (empty? attributes))
                       (eq? (car attributes) 'align))
                  (list
                   (list 'style (string-append "text-align: " (second attributes)))))
                 (#t '())))
           (enhance-table contents))))
      (#t
       (append
        (take x 2)
        (enhance-table-data (drop x 2)))))))

(define/contract (enhance-table xs)
  (-> (listof xexpr/c) (listof xexpr/c))
  (for/list ((x xs))
    (cond
      ((or (atom? x) (null? x))
       x)
      ((eq? 'table (car x))
       (list
        'div '((class "table-responsive"))
        (append (list 'table '((class "table table-striped table-hover")))
                (enhance-table-data (drop x 2)))))
      (#t
       (append
        (take x 2)
        (enhance-table (drop x 2)))))))

;; Called once per post and non-post page, on the contents.
(define/contract (enhance-body xs)
  (-> (listof xexpr/c) (listof xexpr/c))
  ;; Here we pass the xexprs through a series of functions.
  (~> xs
      (enhance-table)
      (syntax-highlight #:python-executable "python.exe"
                        #:line-numbers? #t
                        #:css-class "source")
      (auto-embed-tweets #:parents? #t)
      (add-racket-doc-links #:code? #t #:prose? #t)))

;; Called from `raco frog --clean`.
(define/contract (clean)
  (-> any)
  (void))
