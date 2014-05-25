(load "spice.scm")

(enable-sxml #t)

(define (interleave x lst)
  (let loop ([l lst])
   (if (null? (cdr l))
     (car l)
     (cons (car l) (cons x (loop (cdr l)))))))

(define (format-date-closing-pair p)
  (string-append (car p) ": " (number->string (cdr p))))

(define-page (main-page-path)
  (lambda ()
    (redirect-to "/stocks")))

(define-page "/stocks"
  (lambda ()
    (ajax "stocks" 'button 'click
          (lambda ()
            `(ul
               ,(map (lambda (x) `(li (b ,(s-upcase x)) ":" (br) ,(interleave '(br) (map format-date-closing-pair (get-friday-stock-prices-range x "2013-08-01" "2014-06-01")))))
                     (s-split ", " ($ 'symbols)))))
          target: "results"
          arguments: '((symbols . "$('#symbols').val()")))
    `((input (@ (type "text") (id "symbols")))
      (input (@ (type "button") (id "button") (value "fetch quotes")))
      (div (@ (id "results")))))
  use-ajax: #t)
